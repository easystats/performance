test_that("check_group_variation-1", {
  group <- rep(LETTERS[1:3], each = 3)
  constant <- "a"
  variable1 <- rep(letters[1:3], each = 3)
  variable1b <- rep(letters[1:2], times = c(6, 3))
  variable2 <- rep(letters[1:3], times = 3)
  variable3 <- letters[1:9]
  variable4 <- c(letters[1:5], letters[1:4])

  d <- data.frame(group, constant, variable1, variable1b, variable2, variable3, variable4)
  out <- check_group_variation(d, by = "group")

  expect_equal(
    out,
    data.frame(
      Group = rep("group", 6),
      Variable = c("constant", "variable1", "variable1b", "variable2", "variable3", "variable4"),
      Variation = c(NA, "between", "between", "within", "both", "both"),
      Design = c(NA, "nested", NA, "crossed", "nested", NA)
    ),
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
      Group = c("id", "id", "id", "id", "id", "id"),
      Variable = c("between_num", "within_num", "both_num", "between_fac", "within_fac", "both_fac"),
      Variation = c("between", "within", "both", "between", "within", "both"),
      Design = c(NA, NA, NA, "nested", "crossed", NA)
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
      Group = c("ID", "ID"),
      Variable = c("Sepal.Length", "Petal.Length"),
      Variation = c("both", "both"),
      Design = c(NA_character_)
    ),
    ignore_attr = TRUE
  )

  skip_if_not_installed("parameters")
  data(qol_cancer, package = "parameters")
  d <- qol_cancer
  out <- check_group_variation(
    d,
    select = c("age", "phq4", "QoL", "education"),
    by = "ID"
  )
  expect_equal(
    out,
    data.frame(
      Group = c("ID", "ID", "ID", "ID"),
      Variable = c("age", "phq4", "QoL", "education"),
      Variation = c("between", "both", "both", "between"),
      Design = c(NA_character_)
    ),
    ignore_attr = TRUE
  )

  d <- qol_cancer
  d <- datawizard::demean(d, select = "phq4", by = "ID")
  expect_error(
    check_group_variation(
      d,
      select = c("phq4", "phq4_within", "phq4_between"),
      by = "ID"
    ),
    regex = "One or more"
  )

  d <- qol_cancer
  d <- datawizard::demean(d, select = "phq4", by = "ID")
  d <- datawizard::data_rename(
    d,
    select = c(phq4w = "phq4_within", phq4b = "phq4_between")
  )
  out <- check_group_variation(
    d,
    select = c("age", "phq4", "QoL", "education", "phq4w", "phq4b"),
    by = "ID"
  )
  expect_equal(
    out,
    data.frame(
      Group = c("ID", "ID", "ID", "ID", "ID", "ID"),
      Variable = c("age", "phq4", "QoL", "education", "phq4w", "phq4b"),
      Variation = c("between", "both", "both", "between", "within", "between"),
      Design = c(NA_character_)
    ),
    ignore_attr = TRUE
  )
})


test_that("check_group_variation, multiple by", {
  egsingle <- data.frame(
    schoolid = factor(rep(c("2020", "2820"), times = c(18, 6))),
    lowinc = rep(c(TRUE, FALSE), times = c(18, 6)),
    childid = factor(rep(
      c("288643371", "292020281", "292020361", "295341521"),
      each = 6
    )),
    female = rep(c(TRUE, FALSE), each = 12),
    year = rep(1:6, times = 4),
    math = c(
      -3.068, -1.13, -0.921, 0.463, 0.021, 2.035,
      -2.732, -2.097, -0.988, 0.227, 0.403, 1.623,
      -2.732, -1.898, -0.921, 0.587, 1.578, 2.3,
      -2.288, -2.162, -1.631, -1.555, -0.725, 0.097
    )
  )

  out <- check_group_variation(egsingle, by = c("schoolid", "childid"))
  expect_equal(
    out,
    data.frame(
      Group = c("schoolid", "schoolid", "schoolid", "schoolid", "childid", "childid", "childid", "childid"),
      Variable = c("lowinc", "female", "year", "math", "lowinc", "female", "year", "math"),
      Variation = c("between", "both", "within", "both", "between", "between", "within", "both"),
      Design = c("nested", rep(NA_character_, 7))
    ),
    ignore_attr = TRUE
  )

  out <- check_group_variation(egsingle, by = c("schoolid", "childid"), include_by = TRUE)
  expect_equal(
    out,
    data.frame(
      Group = c(
        "schoolid", "schoolid", "schoolid", "schoolid", "schoolid",
        "childid", "childid", "childid", "childid", "childid"
      ),
      Variable = c(
        "childid", "lowinc", "female", "year", "math",
        "schoolid", "lowinc", "female", "year", "math"
      ),
      Variation = c(
        "both", "between", "both", "within", "both",
        "between", "between", "between", "within", "both"
      ),
      Design = c("nested", "nested", rep(NA_character_, 8))
    ),
    ignore_attr = TRUE
  )
})


test_that("check_group_variation, models", {
  data(iris)
  set.seed(123)
  iris$ID <- sample.int(4, nrow(iris), replace = TRUE) # fake-ID
  m <- lm(Sepal.Length ~ Petal.Length + Petal.Width + Species + ID, data = iris)
  expect_error(
    check_group_variation(m, select = c("Sepal.Length", "Petal.Length"), by = "ID"),
    regex = "no mixed model"
  )

  skip_if_not_installed("lme4")
  data(sleepstudy, package = "lme4")
  mod <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)
  out <- check_group_variation(mod)
  expect_equal(
    out,
    data.frame(
      Group = "Subject",
      Variable = "Days",
      Variation = "within",
      Design = NA_character_
    ),
    ignore_attr = TRUE
  )
})

test_that("check_group_variation, numeric_as_factor", {
  egsingle <- data.frame(
    schoolid = rep(c(2020, 2820), times = c(18, 6)),
    lowinc = rep(c(TRUE, FALSE), times = c(18, 6)),
    childid = factor(rep(
      c("288643371", "292020281", "292020361", "295341521"),
      each = 6
    )),
    female = rep(c(TRUE, FALSE), each = 12),
    year = rep(1:6, times = 4),
    math = c(
      -3.068, -1.13, -0.921, 0.463, 0.021, 2.035,
      -2.732, -2.097, -0.988, 0.227, 0.403, 1.623,
      -2.732, -1.898, -0.921, 0.587, 1.578, 2.3,
      -2.288, -2.162, -1.631, -1.555, -0.725, 0.097
    )
  )

  out1 <- check_group_variation(egsingle, by = c("schoolid", "childid"))
  out2 <- check_group_variation(
    egsingle,
    by = c("schoolid", "childid"),
    numeric_as_factor = TRUE
  )
  expect_identical(
    out1$Variation,
    c("between", "both", "within", "both", "between", "between", "within", "both")
  )
  expect_identical(
    out2$Variation,
    c("between", "both", "within", "both", "between", "between", "within", "both")
  )
  expect_identical(
    out1$Design,
    c("nested", NA, NA, NA, NA, NA, NA, NA)
  )
  expect_identical(
    out2$Design,
    c("nested", NA, "crossed", "nested", NA, NA, "crossed", NA)
  )
})
