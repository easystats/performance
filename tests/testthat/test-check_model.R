skip_on_cran()
skip_if_not_installed("see", minimum_version = "0.9.1")

d <- data.frame(
  y = c(
    101L,
    219L,
    70L,
    67L,
    31L,
    243L,
    41L,
    250L,
    140L,
    210L,
    22L,
    121L,
    282L,
    144L,
    223L,
    297L,
    20L,
    13L,
    148L,
    57L,
    143L,
    109L,
    273L,
    83L,
    28L,
    153L,
    261L,
    209L,
    134L,
    95L,
    17L,
    11L
  ),
  x = c(
    9.43240344971252,
    15.2821279871535,
    29.1169666338249,
    14.0859852649899,
    15.6464367574587,
    8.86321119888889,
    67.9427830082998,
    8.60340052656454,
    21.9029971533007,
    19.8586681899898,
    9.68086847417484,
    25.0990259928273,
    16.1398284119823,
    29.3829154122785,
    31.5592351678585,
    14.0320212214305,
    31.3815560884357,
    18.3566866719804,
    28.3822251897697,
    67.9433435559261,
    26.6656339570149,
    5.83151069454924,
    7.93210796912854,
    35.9198997836162,
    21.6330853399868,
    34.484392512508,
    35.9444483480784,
    28.4517728364097,
    27.2868577206239,
    8.1803022427107,
    46.3029232706936,
    41.516915503604
  ),
  offset = c(
    481L,
    515L,
    396L,
    451L,
    547L,
    409L,
    375L,
    430L,
    526L,
    456L,
    450L,
    425L,
    406L,
    396L,
    421L,
    417L,
    430L,
    419L,
    436L,
    517L,
    511L,
    435L,
    489L,
    417L,
    372L,
    373L,
    351L,
    367L,
    350L,
    339L,
    169L,
    63L
  )
)

suppressWarnings({
  m <- glm(y ~ x + offset(offset), family = poisson, data = d)
})

test_that("`check_model()` works if convergence issues", {
  x <- check_model(m, verbose = FALSE)
  expect_s3_class(x, "check_model")
  expect_warning(
    check_model(x = m, verbose = FALSE),
    regex = "Argument `x` is deprecated",
    fixed = TRUE
  )
})

test_that("`check_outliers()` works if convergence issues", {
  x <- check_outliers(m, verbose = FALSE)
  expect_s3_class(x, "check_outliers")
})

test_that("`check_model()` for invalid models", {
  dd <- data.frame(y = as.difftime(0:5, units = "days"))
  m1 <- lm(y ~ 1, data = dd)
  expect_message(check_model(m1), regex = "Date variables are not")
})

test_that("`check_model()` works for quantreg", {
  skip_if_not_installed("quantreg")
  data(engel, package = "quantreg")
  qm <- quantreg::rq(foodexp ~ income, data = engel)
  x <- check_model(qm, verbose = FALSE)
  expect_s3_class(x, "check_model")
})

test_that("`check_model()` warnings for tweedie", {
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("lme4")
  data(sleepstudy, package = "lme4")
  set.seed(123)
  d <- sleepstudy[sample.int(50), ]
  m <- suppressWarnings(glmmTMB::glmmTMB(
    Reaction ~ Days,
    data = d,
    family = glmmTMB::tweedie
  ))
  expect_message(
    expect_message(
      check_model(m, iterations = 2, verbose = TRUE),
      regex = "Not enough model terms"
    )
  )
})


test_that("`check_model()` warnings for zero-infl", {
  skip_if_not_installed("pscl")
  data(bioChemists, package = "pscl")
  model <- pscl::zeroinfl(
    art ~ fem + mar + kid5 + ment | kid5 + phd,
    data = bioChemists
  )
  expect_message(
    expect_message(check_model(model, verbose = TRUE), regex = "Cannot simulate"),
    regex = "Homogeneity"
  )
})


test_that("`check_model()` no warnings for quasipoisson", {
  skip_if_not_installed("datawizard")
  set.seed(250419)
  # Generate random x values
  x <- rnorm(
    n = 500,
    mean = 5,
    sd = 2
  )
  # Generate y values y = 5x + e
  y <- 5 *
    x +
    rnorm(
      n = 500,
      mean = 5,
      sd = 2
    )
  # Generate z as offset
  z <- runif(500, min = 0, max = 6719)
  mock_data <- data.frame(x, y, z) |>
    # both should be whole numbers since they're counts
    datawizard::data_modify(y = round(y), z = round(z)) |>
    datawizard::data_filter(x >= 0, y >= 0)
  # Run model
  model1 <- glm(y ~ x + offset(log(z)), family = "quasipoisson", data = mock_data)
  expect_message(check_model(model1, verbose = TRUE), regex = "Not enough")
  expect_silent(check_model(model1))
})


test_that("`check_model()` with transformed response when named as function", {
  data(mtcars)
  # rename variable, so it equals to a valid R function
  mtcars$rt <- mtcars$mpg
  model <- lm(log(rt) ~ disp, data = mtcars)
  out <- check_predictions(model)
  expect_s3_class(out, "performance_pp_check")
})
