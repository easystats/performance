generate_distribution <- function(family = "normal", size = 1000, location = 0, scale = 1, zi = .2) {
  if (family == "normal") {
    x <- rnorm(size, location, scale)
  } else if (family == "beta") {
    x <- rbeta(size, location, scale)
  } else if (family == "binomial") {
    x <- rbinom(size, round(location) + 1, scale / 10 ^ (nchar(round(scale))))
  } else if (family == "bernoulli") {
    x <- extraDistr::rbern(size, scale / 10 ^ (nchar(round(scale))))
  } else if (family == "binomial (zero-inflated)") {
    x <-
      extraDistr::rzib(size, round(location) + 1, scale / 10 ^ (nchar(round(scale))), pi = zi)
  } else if (family == "chi") {
    x <- rchisq(size, location, scale)
  } else if (family == "exponential") {
    x <- rexp(size, scale)
  } else if (family == "F") {
    x <- rf(size, location, scale + 0.1)
  } else if (family == "gamma") {
    x <- rgamma(size, location, scale)
  } else if (family == "lognormal") {
    x <- rlnorm(size, location, scale)
  } else if (family == "poisson") {
    x <- rpois(size, location)
  } else if (family == "t") {
    x <- rt(size, location, scale)
  } else if (family == "weibull") {
    x <- rweibull(size, location, scale)
  } else if (family == "uniform") {
    x <- runif(size, location, location * 2)
  } else if (family == "negative binomial") {
    x <- rnbinom(n = size, size = scale, mu = location)
  } else if (family == "negative binomial (zero-inflated)") {
    x <-
      extraDistr::rzinb(
        n = size,
        size = scale,
        prob = scale / 10 ^ (nchar(round(scale))),
        pi = zi
      )
  } else if (family == "poisson (zero-inflated)") {
    x <- extraDistr::rzip(size, location, pi = zi)
  }
  return(x)
}


df <- data.frame()
distrs <- c(
  "normal", "beta", "chi", "F", "exponential", "gamma", "lognormal",
  "poisson", "uniform", "negative binomial", "bernoulli",
  "poisson (zero-inflated)", "negative binomial (zero-inflated)"
)

pb <- txtProgressBar(min = 0, max = length(distrs), style = 3)

for (di in 1:length(distrs)) {

  setTxtProgressBar(pb, di)
  distribution <- distrs[di]

  for (i in 1:2000) {

    size <- round(runif(1, 30, 2000))
    location <- runif(1, 0.01, 10)
    scale <- runif(1, 0.02, 10)
    zi <- runif(1, 0, .7)

    x <-
      generate_distribution(
        distribution,
        size = size,
        location = location,
        scale = scale,
        zi = zi
      )

    x[is.infinite(x)] <- 5.565423e+156
    x_scaled <- parameters::normalize(x, verbose = FALSE)

    # Extract features
    data <- data.frame(
      "SD" = sd(x_scaled),
      "MAD" = mad(x_scaled, constant = 1),
      "Mean_Median_Distance" = mean(x_scaled) - median(x_scaled),
      "Mean_Mode_Distance" = mean(x_scaled) - as.numeric(bayestestR::map_estimate(x_scaled, bw = "nrd0")),
      "SD_MAD_Distance" = sd(x_scaled) - mad(x_scaled, constant = 1),
      "Var_Mean_Distance" = var(x) - mean(x),
      "Range_SD" = diff(range(x)) / sd(x),
      "IQR" = stats::IQR(x_scaled),
      "Skewness" = parameters::skewness(x_scaled),
      "Kurtosis" = parameters::kurtosis(x_scaled),
      "Uniques" = length(unique(x)) / length(x),
      "Min" = min(x),
      "Max" = max(x),
      "Proportion_Zero" = sum(x == 0) / length(x)
    )

    if (length(unique(x)) == 1) {
      data$Distribution <- "uniform"
    } else{
      data$Distribution <- distribution
    }

    df <- rbind(df, data)
  }
}

close(pb)

df


df2 <- na.omit(df)
infinite <- is.infinite(rowSums(df2[sapply(df2, is.numeric)]))
df2 <- df2[!infinite, ]

model <-
  randomForest::randomForest(
    as.factor(Distribution) ~ .,
    data = df2,
    localImp = FALSE,
    importance = FALSE,
    keep.forest = TRUE,
    keep.inbag = FALSE,
    proximity = FALSE,
    maxDepth = 16,
    maxBins = 32,
    minInstancesPerNode = 1,
    minInfoGain = 0.0,
    maxMemoryInMB = 512,
    ntree = 32
  )


classify_distribution <- model
usethis::use_data(classify_distribution, overwrite = TRUE, internal = TRUE)
