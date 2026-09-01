#' Check Smooth Term in GAMs
#'
#' Randomization test looking for evidence of residual pattern attributable to covariates of each smooth.
#'
#' @param x An [mgcv] GAM model.
#' @param iterations Number of permutations.
#' @param ... Other arguments to be passed to other functions (not used for now).
#'
#' @examples
#' if (require("mgcv")) {
#'   model <- mgcv::gam(Sepal.Length ~ s(Petal.Length, k = 3) + s(Petal.Width, k = 5), data = iris)
#'   check_smooth(model)
#' }
#' @export
check_smooth <- function(x, iterations = 400, ...) {
  # Based on k.check in https://github.com/cran/mgcv/blob/master/R/plots.r
  n_smooths <- length(x$smooth) # TODO: fix that for brms

  if (n_smooths == 0) stop("No smooth terms were detected.")

  rsd <- insight::get_residuals(x)
  if (insight::model_info(x)$is_bayesian) {
    # TODO: change depending on https://github.com/easystats/insight/issues/540
    rsd <- rsd[, 1]
  }

  # Initialize containers
  ve <- rep(0, iterations)
  p.val <- v.obs <- kc <- edf <- rep(0, n_smooths)
  snames <- rep("", n_smooths)

  # Data length
  n <- nrow(x$model)
  modelmatrix <- x$model

  nr <- length(rsd)

  # Iterate through smooth terms
  for (s in 1:n_smooths) {
    ok <- TRUE
    x$smooth[[s]]$by <- "NA" # Can't deal with 'by' variables
    dat <- mgcv_ExtractData(x$smooth[[s]], modelmatrix, NULL)$data

    # Sanity check that 'dat' is of the good format
    if (!is.null(attr(dat, "index")) || !is.null(attr(dat[[1]], "matrix")) || is.matrix(dat[[1]])) ok <- FALSE
    if (ok) dat <- as.data.frame(dat)

    # Get info
    snames[s] <- x$smooth[[s]]$label # Smooth name
    idx <- x$smooth[[s]]$first.para:x$smooth[[s]]$last.para # which parameter does it correspond to
    kc[s] <- length(idx)
    edf[s] <- sum(x$edf[idx])
    nc <- x$smooth[[s]]$dim

    # drop any by variables
    if (ok && ncol(dat) > nc) dat <- dat[, 1:nc, drop = FALSE]
    # Check if any factor
    for (j in 1:nc) if (is.factor(dat[[j]])) ok <- FALSE
    if (!ok) {
      p.val[s] <- v.obs[s] <- NA # can't do this test with summation convention/factors
      next # Skip iteration
    }


    if (nc == 1) {
      # 1-Dimensional term -----------
      e <- diff(rsd[order(dat[, 1])])
      v.obs[s] <- mean(e^2) / 2
      # Reshuffle n-times
      for (i in 1:iterations) {
        e <- diff(rsd[sample(1:nr, nr)]) # shuffle
        ve[i] <- mean(e^2) / 2
      }
    } else {
      # multidimensional term ---------
      # If tensor product (have to consider scaling)
      if (!is.null(x$smooth[[s]]$margin)) {
        # get the scale factors...
        beta <- stats::coef(x)[idx]
        f0 <- mgcv::PredictMat(x$smooth[[s]], dat) %*% beta
        gr.f <- rep(0, ncol(dat))
        for (i in 1:nc) {
          datp <- dat
          dx <- diff(range(dat[, i])) / 1000
          datp[, i] <- datp[, i] + dx
          fp <- mgcv::PredictMat(x$smooth[[s]], datp) %*% beta
          gr.f[i] <- mean(abs(fp - f0)) / dx
        }
        # Rescale distances
        for (i in 1:nc) {
          dat[, i] <- dat[, i] - min(dat[, i])
          dat[, i] <- gr.f[i] * dat[, i] / max(dat[, i])
        }
      }
      nn <- 3
      ni <- mgcv_nearest(nn, as.matrix(dat))$ni # TODO: this function calls an mgcv internal

      e <- rsd - rsd[ni[, 1]]
      for (j in 2:nn) e <- c(e, rsd - rsd[ni[, j]])
      v.obs[s] <- mean(e^2) / 2

      # Reshuffle n-times
      for (i in 1:iterations) {
        rsdr <- rsd[sample(1:nr, nr)] ## shuffle
        e <- rsdr - rsdr[ni[, 1]]
        for (j in 2:nn) e <- c(e, rsdr - rsdr[ni[, j]])
        ve[i] <- mean(e^2) / 2
      }
    }
    p.val[s] <- mean(ve < v.obs[s])
    v.obs[s] <- v.obs[s] / mean(rsd^2)
  }
  out <- data.frame(Term = snames, EDF_max = kc, EDF = edf, k = v.obs, p = p.val)
  out
}




# Utils -------------------------------------------------------------------


# https://github.com/cran/mgcv/blob/c263c882daf8b2ed55e6e3d1fb712cf20d79a710/R/smooth.r#L3614
#' @keywords internal
mgcv_ExtractData <- function(object, data, knots) {

  insight::check_if_installed("mgcv")

  # https://github.com/cran/mgcv/blob/c263c882daf8b2ed55e6e3d1fb712cf20d79a710/R/smooth.r#L318
  get.var <- function(txt, data, vecMat = TRUE)
                      # txt contains text that may be a variable name and may be an expression
                      # for creating a variable. get.var first tries data[[txt]] and if that
                      # fails tries evaluating txt within data (only). Routine returns NULL
                      # on failure, or if result is not numeric or a factor.
                      # matrices are coerced to vectors, which facilitates matrix arguments
  # to smooths.
  {
    x <- data[[txt]]
    if (is.null(x)) {
      x <- try(eval(parse(text = txt), data, enclos = NULL), silent = TRUE)
      if (inherits(x, "try-error")) x <- NULL
    }
    if (!is.numeric(x) && !is.factor(x)) x <- NULL
    if (is.matrix(x)) {
      if (ncol(x) == 1) {
        x <- as.numeric(x)
        ismat <- FALSE
      } else {
        ismat <- TRUE
      }
    } else {
      ismat <- FALSE
    }
    if (vecMat && is.matrix(x)) x <- x[1:prod(dim(x))] ## modified from x <- as.numeric(x) to allow factors
    if (ismat) attr(x, "matrix") <- TRUE
    x
  }


  ## `data' and `knots' contain the data needed to evaluate the `terms', `by'
  ## and `knots' elements of `object'. This routine does so, and returns
  ## a list with element `data' containing just the evaluated `terms',
  ## with the by variable as the last column. If the `terms' evaluate matrices,
  ## then a check is made of whether repeat evaluations are being made,
  ## and if so only the unique evaluation points are returned in data, along
  ## with the `index' attribute required to re-assemble the full dataset.
  knt <- dat <- list()
  ## should data be processed as for summation convention with matrix arguments?
  vecMat <- if (is.null(object$xt$sumConv)) TRUE else object$xt$sumConv
  for (i in 1:length(object$term)) {
    dat[[object$term[i]]] <- get.var(object$term[i], data, vecMat = vecMat)
    knt[[object$term[i]]] <- get.var(object$term[i], knots, vecMat = vecMat)
  }
  names(dat) <- object$term
  m <- length(object$term)
  if (!is.null(attr(dat[[1]], "matrix")) && vecMat) { ## strip down to unique covariate combinations
    n <- length(dat[[1]])
    X <- matrix(unlist(dat), n, m)
    if (is.numeric(X)) {
      X <- mgcv::uniquecombs(X)
      if (nrow(X) < n * .9) { ## worth the hassle
        for (i in 1:m) dat[[i]] <- X[, i] ## return only unique rows
        attr(dat, "index") <- attr(X, "index") ## index[i] is row of dat[[i]] containing original row i
      }
    } ## end if(is.numeric(X))
  }
  if (object$by != "NA") {
    by <- get.var(object$by, data)
    if (!is.null(by)) {
      dat[[m + 1]] <- by
      names(dat)[m + 1] <- object$by
    }
  }
  list(data = dat, knots = knt)
}



# https://github.com/cran/mgcv/blob/2db5036f529dff6ec8a4a4ba0d2df1804a4d2668/R/sparse.r#L129
#' @keywords internal
mgcv_nearest <- function(k, X, gt.zero = FALSE, get.a = FALSE) {

  insight::check_if_installed("mgcv")

  ## The rows of X contain coordinates of points.
  ## For each point, this routine finds its k nearest
  ## neighbours, returning a list of 2, n by k matrices:
  ## ni - ith row indexes the rows of X containing
  ##      the k nearest neighbours of X[i,]
  ## dist - ith row is the distances to the k nearest
  ##        neighbours.
  ## a - area associated with each point, if get.a is TRUE
  ## ties are broken arbitrarily.
  ## gt.zero indicates that neighbours must have distances greater
  ## than zero...

  if (gt.zero) {
    Xu <- mgcv::uniquecombs(X)
    ind <- attr(Xu, "index") ## Xu[ind,] == X
  } else {
    Xu <- X
    ind <- 1:nrow(X)
  }
  if (k > nrow(Xu)) stop("not enough unique values to find k nearest")
  nobs <- length(ind)
  n <- nrow(Xu)
  d <- ncol(Xu)
  dist <- matrix(0, n, k)
  if (get.a) a <- 1:n else a <- 1

  # TODO: how to get that without the call to an internal????
  oo <- .C(mgcv:::C_k_nn,
    Xu = as.double(Xu), dist = as.double(dist), a = as.double(a), ni = as.integer(dist),
    n = as.integer(n), d = as.integer(d), k = as.integer(k), get.a = as.integer(get.a)
  )

  dist <- matrix(oo$dist, n, k)[ind, ]
  rind <- 1:nobs
  rind[ind] <- 1:nobs
  ni <- matrix(rind[oo$ni + 1], n, k)[ind, ]
  if (get.a) a <- oo$a[ind] else a <- NULL
  list(ni = ni, dist = dist, a = a)
}
