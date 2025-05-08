test_that("check_group_variation-1", {
  skip_if_not_installed("lme4")
  set.seed(11)
  group <- rep(LETTERS[1:3], each = 3)
  variable1 <- rep(letters[1:3], each = 3)
  variable2 <- rep(letters[1:3], times = 3)
  variable3 <- letters[1:9]
  variable4 <- c(letters[1:5], letters[1:4])

  d <- data.frame(group, variable1, variable2, variable3, variable4)
  out <- check_group_variation(d, by = "group")

  out2 <- c(
    variable1 = lme4::isNested(variable1, group),
    variable2 = lme4::isNested(variable2, group),
    variable3 = lme4::isNested(variable3, group),
    variable4 = lme4::isNested(variable4, group)
  )
  expect_equal(
    out,
    data.frame(
      group = c("group", "group", "group", "group"),
      variable = c("variable1", "variable2", "variable3", "variable4"),
      type = c("between (nested)", "within", "both (nested)", "both")
    ),
    ignore_attr = TRUE
  )
  expect_equal(
    endsWith(out$type, "(nested)"),
    out2,
    ignore_attr = TRUE
  )

  set.seed(111)
  dat <- data.frame(
    id = rep(letters, each = 2),

    between_num = rep(rnorm(26), each = 2),
    within_num = rep(rnorm(2), times = 26),
    both_num = rnorm(52),

    between_fac = rep(LETTERS, each = 2),
    within_fac = rep(LETTERS[1:2], times = 26),
    both_fac = sample(LETTERS[1:5], size = 52, replace = TRUE)
  )

  out <- check_group_variation(
    dat,
    select = c(
      "between_num",
      "within_num",
      "both_num",
      "between_fac",
      "within_fac",
      "both_fac"
    ),
    by = "id"
  )
  expect_equal(
    out,
    data.frame(
      group = c("id", "id", "id", "id", "id", "id"),
      variable = c("between_num", "within_num", "both_num", "between_fac", "within_fac", "both_fac"),
      type = c("between", "within", "both", "between (nested)", "within", "both")
    ),
    ignore_attr = TRUE
  )
})


test_that("check_group_variation-2", {
  data(iris)
  set.seed(123)
  iris$ID <- sample.int(4, nrow(iris), replace = TRUE) # fake-ID
  out <- check_group_variation(
    iris,
    select = c("Sepal.Length", "Petal.Length"),
    by = "ID"
  )
  expect_equal(
    out,
    data.frame(
      group = c("ID", "ID"),
      variable = c("Sepal.Length", "Petal.Length"),
      type = c("both", "both")
    ),
    ignore_attr = TRUE
  )

  skip_if_not_installed("parameters")
  data(qol_cancer, package = "parameters")
  out <- check_group_variation(
    qol_cancer,
    select = c("age", "phq4", "QoL", "education"),
    by = "ID"
  )
  expect_equal(
    out,
    data.frame(
      group = c("ID", "ID", "ID", "ID"),
      variable = c("age", "phq4", "QoL", "education"),
      type = c("between", "both", "both", "between")
    ),
    ignore_attr = TRUE
  )

  data(qol_cancer, package = "parameters")
  qol_cancer <- datawizard::demean(qol_cancer, select = "phq4", by = "ID")
  expect_error(
    check_group_variation(
      qol_cancer,
      select = c("phq4", "phq4_within", "phq4_between"),
      by = "ID"
    ),
    regex = "One or more"
  )

  data(qol_cancer, package = "parameters")
  qol_cancer <- datawizard::demean(qol_cancer, select = "phq4", by = "ID")
  qol_cancer <- datawizard::data_rename(
    qol_cancer,
    select = c(phq4w = "phq4_within", phq4b = "phq4_between")
  )
  out <- check_group_variation(
    qol_cancer,
    select = c("age", "phq4", "QoL", "education", "phq4w", "phq4b"),
    by = "ID"
  )
  expect_equal(
    out,
    data.frame(
      group = c("ID", "ID", "ID", "ID", "ID", "ID"),
      variable = c("age", "phq4", "QoL", "education", "phq4w", "phq4b"),
      type = c("between", "both", "both", "between", "within", "between")
    ),
    ignore_attr = TRUE
  )
})


test_that("check_heterogeneity_bias", {
  skip_if_not_installed("datawizard")
  data(iris)
  set.seed(123)
  iris$ID <- sample.int(4, nrow(iris), replace = TRUE) # fake-ID
  out <- check_heterogeneity_bias(iris, select = c("Sepal.Length", "Petal.Length"), by = "ID")
  expect_equal(out, c("Sepal.Length", "Petal.Length"), ignore_attr = TRUE)
  expect_output(print(out), "Possible heterogeneity bias due to following predictors: Sepal\\.Length, Petal\\.Length")

  out <- check_heterogeneity_bias(iris, select = ~ Sepal.Length + Petal.Length, by = ~ID)
  expect_equal(out, c("Sepal.Length", "Petal.Length"), ignore_attr = TRUE)
  expect_output(print(out), "Possible heterogeneity bias due to following predictors: Sepal\\.Length, Petal\\.Length")

  m <- lm(Sepal.Length ~ Petal.Length + Petal.Width + Species + ID, data = iris)
  expect_error(
    check_heterogeneity_bias(m, select = c("Sepal.Length", "Petal.Length"), by = "ID"),
    regex = "no mixed model"
  )

  skip_if_not_installed("lme4")
  m <- lme4::lmer(Sepal.Length ~ Petal.Length + Petal.Width + Species + (1 | ID), data = iris)
  out <- check_heterogeneity_bias(m, select = c("Sepal.Length", "Petal.Length"), by = "ID")
  expect_equal(out, c("Petal.Length", "Petal.Width", "Species"), ignore_attr = TRUE)
  expect_output(
    print(out),
    "Possible heterogeneity bias due to following predictors: Petal\\.Length, Petal\\.Width, Species"
  )
  out <- check_heterogeneity_bias(m, select = ~ Sepal.Length + Petal.Length, by = ~ID)
  expect_equal(out, c("Petal.Length", "Petal.Width", "Species"), ignore_attr = TRUE)
  expect_output(
    print(out),
    "Possible heterogeneity bias due to following predictors: Petal\\.Length, Petal\\.Width, Species"
  )
})

test_that("check_heterogeneity_bias", {
  skip_if_not_installed("datawizard", minimum_version = "0.12.3")
  data(efc, package = "datawizard")
  dat <- na.omit(efc)
  dat$e42dep <- factor(dat$e42dep)
  dat$c172code <- factor(dat$c172code)

  out <- check_heterogeneity_bias(
    dat,
    select = "c12hour",
    by = c("e42dep", "c172code"),
    nested = TRUE
  )
  expect_equal(out, "c12hour", ignore_attr = TRUE)
})
