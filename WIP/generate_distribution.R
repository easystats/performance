rhalfcauchy <- function(n, location, scale) {
  r <- vector(length = n)
  for (i in 1:n) {
    r_1 <- rcauchy(1, location, scale)
    while (r_1 < 0) {
      r_1 <- rcauchy(1, location, scale)
    }
    r[i] <- r_1
  }
  return(r)
}


generate_distribution <- function(
  family = "normal",
  size = 1000,
  location = 0,
  scale = 1,
  trials = 1,
  prob = .5,
  mu = 3,
  zi = .2
) {
  if (family == "normal") {
    rnorm(size, location, scale)
  } else if (family == "beta") {
    rbeta(size, location, scale)
  } else if (family == "binomial") {
    rbinom(size, size = trials, prob = prob)
  } else if (family == "bernoulli") {
    extraDistr::rbern(size, prob = prob)
  } else if (family == "binomial (zero-infl.)") {
    extraDistr::rzib(size, size = trials, prob = prob, pi = zi)
  } else if (family == "chi") {
    rchisq(size, location, scale)
  } else if (family == "exponential") {
    rexp(size, scale)
  } else if (family == "F") {
    rf(size, location, scale + 0.1)
  } else if (family == "gamma") {
    rgamma(size, location, scale)
  } else if (family == "inverse-gamma") {
    actuar::rinvgamma(size, location, scale)
  } else if (family == "cauchy") {
    rcauchy(size, location, scale)
  } else if (family == "half-cauchy") {
    rhalfcauchy(size, location, scale)
  } else if (family == "lognormal") {
    rlnorm(size, location, scale)
  } else if (family == "poisson") {
    rpois(size, lambda = mu)
  } else if (family == "t") {
    rt(size, location, scale)
  } else if (family == "weibull") {
    rweibull(size, location, scale)
  } else if (family == "tweedie") {
    tweedie::rtweedie(size, mu = mu, phi = scale, power = location)
    # mgcv::rTweedie(mu = mu, p = location + 1, phi = scale)
  } else if (family == "uniform") {
    runif(size, location, location * 2)
  } else if (family == "negative binomial") {
    rnbinom(n = size, size = trials, mu = mu)
  } else if (family == "neg. binomial (zero-infl.)") {
    extraDistr::rzinb(n = size, size = trials, prob = prob, pi = zi)
  } else if (family == "beta-binomial") {
    extraDistr::rbbinom(n = size, size = trials, alpha = scale, beta = location)
  } else if (family == "poisson (zero-infl.)") {
    extraDistr::rzip(size, location, pi = zi)
  } else if (family == "pareto") {
    VGAM::rpareto(n = size, scale = scale + 1, shape = location + 1)
  }
}


df <- data.frame()
distrs <- c(
  "normal",
  "beta",
  "chi",
  "F",
  "exponential",
  "gamma",
  "inverse-gamma",
  "lognormal",
  "poisson",
  "uniform",
  "negative binomial",
  "bernoulli",
  "cauchy",
  "half-cauchy",
  "poisson (zero-infl.)",
  "neg. binomial (zero-infl.)",
  "weibull",
  "beta-binomial",
  "binomial",
  "pareto",
  "tweedie"
)

.is.integer <- function(x) {
  tryCatch(
    expr = {
      ifelse(is.infinite(x), FALSE, x %% 1 == 0)
    },
    warning = function(w) {
      is.integer(x)
    },
    error = function(e) {
      FALSE
    }
  )
}

pb <- txtProgressBar(min = 0, max = length(distrs), style = 3)

for (di in seq_along(distrs)) {
  setTxtProgressBar(pb, di - 1)
  distribution <- distrs[di]
  cat("\n\n", sprintf("Distribution %i of %i:", di, length(distrs)), distribution, "\n")

  n_samples <- 1500

  pb2 <- txtProgressBar(min = 1, max = n_samples, style = 3)

  for (i in 1:n_samples) {
    setTxtProgressBar(pb2, i)

    size <- round(runif(1, 30, 2000))
    trials <- round(runif(1, 0, ifelse(size / 5 > 100, 100, size / 5)))
    location <- if (distribution == "tweedie") {
      runif(1, 1.0001, 1.9999)
    } else {
      runif(1, 0.01, 10)
    }
    scale <- runif(1, 0.02, 10)
    prob <- runif(1, .05, .9)
    mu <- runif(1, 1, ifelse(round(sqrt(trials)) < 2, 2, round(sqrt(trials))))
    zi <- runif(1, 0, .7)

    x <-
      generate_distribution(
        distribution,
        size = size,
        location = location,
        scale = scale,
        trials = trials,
        prob = prob,
        mu = mu,
        zi = zi
      )

    x <- as.vector(na.omit(sjmisc::zap_inf(x)))
    # x_scaled <- parameters::normalize(x, verbose = FALSE)

    if (length(x) >= 10) {
      if (all(.is.integer(x))) {
        mode <- datawizard::distribution_mode(x)
      } else {
        mode <- tryCatch(
          as.numeric(bayestestR::map_estimate(x, bw = "nrd0")),
          error = function(e) NULL
        )
        if (is.null(mode)) {
          mode <- tryCatch(
            as.numeric(bayestestR::map_estimate(x, bw = "kernel")),
            error = function(e) NULL
          )
        }
        if (is.null(mode)) {
          mode <- datawizard::distribution_mode(x)
        }
      }
      # Extract features
      data <- data.frame(
        "SD" = sd(x),
        "MAD" = mad(x, constant = 1),
        "Mean_Median_Distance" = mean(x) - median(x),
        "Mean_Mode_Distance" = mean(x) - as.numeric(mode),
        "SD_MAD_Distance" = sd(x) - mad(x, constant = 1),
        "Var_Mean_Distance" = var(x) - mean(x),
        "Range_SD" = diff(range(x)) / sd(x),
        "Range" = diff(range(x)),
        "IQR" = stats::IQR(x),
        "Skewness" = as.numeric(datawizard::skewness(x)),
        "Kurtosis" = as.numeric(datawizard::kurtosis(x)),
        "Uniques" = length(unique(x)) / length(x),
        "N_Uniques" = length(unique(x)),
        "Min" = min(x),
        "Max" = max(x),
        "Proportion_Positive" = sum(x >= 0) / length(x),
        "Integer" = all(.is.integer(x)),
        "Proportion_Zero" = sum(x == 0) / length(x)
        # "Proportion_Minimum" = sum(x == min(x)) / length(x),
        # "Proportion_Maximum" = sum(x == max(x)) / length(x)
      )

      if (length(unique(x)) == 1) {
        data$Distribution <- "uniform"
      } else {
        data$Distribution <- distribution
      }

      df <- rbind(df, data)
    }
  }
  close(pb2)
}

close(pb)

df2 <- na.omit(df)
infinite <- is.infinite(rowSums(df2[sapply(df2, is.numeric)]))
df2 <- df2[!infinite, ]

model2 <- model <- randomForest::randomForest(
  as.factor(Distribution) ~ .,
  data = df2,
  localImp = FALSE,
  importance = FALSE,
  keep.forest = TRUE,
  keep.inbag = FALSE,
  proximity = FALSE,
  maxDepth = 20,
  maxBins = 32,
  minInstancesPerNode = 1,
  minInfoGain = 0.0,
  maxMemoryInMB = 128,
  ntree = 32
)


model <- strip::strip(model, keep = "predict", use_trim = TRUE)

model$predicted <- NULL
model$y <- NULL
model$err.rate <- NULL
model$test <- NULL
model$proximity <- NULL
model$confusion <- NULL
model$localImportance <- NULL
model$importanceSD <- NULL
model$inbag <- NULL
model$votes <- NULL
model$oob.times <- NULL


classify_distribution <- model
usethis::use_data(classify_distribution, overwrite = TRUE, internal = TRUE)

sjmisc::frq(df2$Distribution)

randomForest::varImpPlot(model2)
