test_that("check_group_variation-1", {
  dat1 <- data.frame(
    group = rep(LETTERS[1:3], each = 3),
    constant = "a",
    variable1 = rep(letters[1:3], each = 3),
    variable1b = rep(letters[1:2], times = c(6, 3)),
    variable2 = rep(letters[1:3], times = 3),
    variable3 = letters[1:9],
    variable4 = c(letters[1:5], letters[1:4])
  )
  out1 <- check_group_variation(dat1, by = "group")

  expect_equal(
    out1[, c("Group", "Variable", "Variation", "Design")],
    data.frame(
      Group = "group",
      Variable = c(
        "constant",
        "variable1",
        "variable1b",
        "variable2",
        "variable3",
        "variable4"
      ),
      Variation = c(NA, "between", "between", "within", "both", "both"),
      Design = c(NA, "nested", NA, "crossed", "nested", NA),
      stringsAsFactors = FALSE
    ),
    ignore_attr = TRUE
  )

  expect_identical(out1$Eta[out1$Variation %in% "between"], c(1, 1))
  expect_identical(out1$Eta[out1$Variation %in% "within"], 0)
  expect_false(any(out1$Eta[!out1$Variation %in% c("between", "within")] %in% c(1, 0)))
  expect_identical(out1$Eta[1:4], c(NA, 1, 1, 0))
  expect_true(all(0.45 < out1$Eta[5:6] & out1$Eta[5:6] < 0.75))

  set.seed(111)
  dat2 <- data.frame(
    id = rep(letters, each = 2),
    between_num = rep(rnorm(26), each = 2),
    within_num = rep(rnorm(2), times = 26),
    both_num = rnorm(52),
    between_fac = rep(LETTERS, each = 2),
    within_fac = rep(LETTERS[1:2], times = 26),
    both_fac = sample(LETTERS[1:5], size = 52, replace = TRUE)
  )
  out2 <- check_group_variation(
    dat2,
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
    out2[, c("Group", "Variable", "Variation", "Design")],
    data.frame(
      Group = "id",
      Variable = c(
        "between_num",
        "within_num",
        "both_num",
        "between_fac",
        "within_fac",
        "both_fac"
      ),
      Variation = c("between", "within", "both", "between", "within", "both"),
      Design = c(NA, NA, NA, "nested", "crossed", NA),
      stringsAsFactors = FALSE
    ),
    ignore_attr = TRUE
  )

  expect_identical(out2$Eta[out2$Variation %in% "between"], c(1, 1))
  expect_identical(out2$Eta[out2$Variation %in% "within"], c(0, 0))
  expect_false(any(out2$Eta[!out2$Variation %in% c("between", "within")] %in% c(1, 0)))
  expect_identical(out2$Eta[-c(3, 6)], c(1, 0, 1, 0))
  expect_true(all(0.7 < out2$Eta[c(3, 6)] & out2$Eta[c(3, 6)] < 0.8))
})


test_that("check_group_variation-2", {
  data(iris)
  set.seed(123)
  iris$ID <- sample.int(4, nrow(iris), replace = TRUE) # fake-ID
  out3 <- check_group_variation(
    iris,
    select = c("Sepal.Length", "Petal.Length"),
    by = "ID"
  )
  expect_equal(
    out3[, c("Group", "Variable", "Variation", "Design")],
    data.frame(
      Group = c("ID", "ID"),
      Variable = c("Sepal.Length", "Petal.Length"),
      Variation = c("both", "both"),
      Design = c(NA_character_)
    ),
    ignore_attr = TRUE
  )
  expect_equal(
    out3$Eta[1],
    sqrt(r2(lm(Sepal.Length ~ factor(ID), data = iris))[[1]]),
    ignore_attr = TRUE
  )
  expect_equal(
    out3$Eta[2],
    sqrt(r2(lm(Petal.Length ~ factor(ID), data = iris))[[1]]),
    ignore_attr = TRUE
  )

  skip_if_not_installed("parameters")
  data(qol_cancer, package = "parameters")
  out4 <- check_group_variation(
    qol_cancer,
    select = c("age", "phq4", "QoL", "education"),
    by = "ID"
  )
  expect_equal(
    out4[, c("Group", "Variable", "Variation", "Design")],
    data.frame(
      Group = c("ID", "ID", "ID", "ID"),
      Variable = c("age", "phq4", "QoL", "education"),
      Variation = c("between", "both", "both", "between"),
      Design = c(NA_character_),
      stringsAsFactors = FALSE
    ),
    ignore_attr = TRUE
  )

  dat5 <- datawizard::demean(qol_cancer, select = "phq4", by = "ID")
  expect_error(
    check_group_variation(
      dat5,
      select = c("phq4", "phq4_within", "phq4_between"),
      by = "ID"
    ),
    regex = "One or more"
  )

  dat6 <- datawizard::data_rename(
    dat5,
    select = c(phq4w = "phq4_within", phq4b = "phq4_between")
  )
  out6 <- check_group_variation(
    dat6,
    select = c("age", "phq4", "QoL", "education", "phq4w", "phq4b"),
    by = "ID"
  )
  expect_equal(
    out6[, c("Group", "Variable", "Variation", "Design")],
    data.frame(
      Group = c("ID", "ID", "ID", "ID", "ID", "ID"),
      Variable = c("age", "phq4", "QoL", "education", "phq4w", "phq4b"),
      Variation = c("between", "both", "both", "between", "within", "between"),
      Design = c(NA_character_),
      stringsAsFactors = FALSE
    ),
    ignore_attr = TRUE
  )

  expect_identical(out6$Eta[out6$Variation %in% "between"], c(1, 1, 1))
  expect_equal(out6$Eta[out6$Variation %in% "within"], 0)
  expect_false(any(out6$Eta[!out6$Variation %in% c("between", "within")] %in% c(1, 0)))
  expect_equal(out6$Eta[-(2:3)], c(1, 1, 0, 1))
  expect_true(all(0.8 < out6$Eta[2:3] & out6$Eta[2:3] < 0.85))
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
    # fmt: skip
    math = c(-3.068, -1.13, -0.921, 0.463, 0.021, 2.035, -2.732, -2.097,
             -0.988, 0.227, 0.403, 1.623, -2.732, -1.898, -0.921, 0.587,
             1.578, 2.3, -2.288, -2.162, -1.631, -1.555, -0.725, 0.097),
    stringsAsFactors = FALSE
  )

  out7 <- check_group_variation(egsingle, by = c("schoolid", "childid"))
  expect_equal(
    out7[, c("Group", "Variable", "Variation", "Design")],
    data.frame(
      Group = rep(c("schoolid", "childid"), each = 4),
      Variable = c(
        "lowinc",
        "female",
        "year",
        "math",
        "lowinc",
        "female",
        "year",
        "math"
      ),
      Variation = c(
        "between",
        "both",
        "within",
        "both",
        "between",
        "between",
        "within",
        "both"
      ),
      Design = rep(c("nested", NA_character_), c(1, 7))
    ),
    ignore_attr = TRUE
  )
  expect_identical(out7$Eta[out7$Variation %in% "between"], c(1, 1, 1))
  expect_identical(out7$Eta[out7$Variation %in% "within"], c(0, 0))
  expect_false(any(out7$Eta[!out7$Variation %in% c("between", "within")] %in% c(1, 0)))
  expect_identical(out7$Eta[-c(2, 4, 8)], c(1, 0, 1, 1, 0))
  expect_true(all(0.25 < out7$Eta[c(2, 4, 8)] & out7$Eta[c(2, 4, 8)] < 0.6))

  out8 <- check_group_variation(
    egsingle,
    by = c("schoolid", "childid"),
    include_by = TRUE
  )
  expect_equal(
    out8[, c("Group", "Variable", "Variation", "Design")],
    data.frame(
      Group = rep(c("schoolid", "childid"), each = 5),
      Variable = c(
        "childid",
        "lowinc",
        "female",
        "year",
        "math",
        "schoolid",
        "lowinc",
        "female",
        "year",
        "math"
      ),
      Variation = c(
        "both",
        "between",
        "both",
        "within",
        "both",
        "between",
        "between",
        "between",
        "within",
        "both"
      ),
      Design = rep(c("nested", NA_character_), c(2, 8))
    ),
    ignore_attr = TRUE
  )
  expect_identical(out8$Eta[out8$Variation %in% "between"], c(1, 1, 1, 1))
  expect_identical(out8$Eta[out8$Variation %in% "within"], c(0, 0))
  expect_false(any(out8$Eta[!out8$Variation %in% c("between", "within")] %in% c(1, 0)))
  expect_identical(out8$Eta[-c(1, 3, 5, 10)], c(1, 0, 1, 1, 1, 0))
  expect_true(all(0.25 < out8$Eta[c(1, 3, 5, 10)] & out8$Eta[c(1, 3, 5, 10)] < 0.8))
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
      Design = NA_character_,
      Eta = 0
    ),
    ignore_attr = TRUE
  )
})

test_that("check_group_variation, numeric_as_factor", {
  egsingle <- data.frame(
    schoolid = factor(rep(c("2020", "2820"), times = c(18, 6))),
    lowinc = rep(c(TRUE, FALSE), times = c(18, 6)),
    childid = factor(rep(
      c("288643371", "292020281", "292020361", "295341521"),
      each = 6
    )),
    female = rep(c(TRUE, FALSE), each = 12),
    year = rep(1:6, times = 4),
    # fmt: skip
    math = c(-3.068, -1.13, -0.921, 0.463, 0.021, 2.035, -2.732, -2.097,
             -0.988, 0.227, 0.403, 1.623, -2.732, -1.898, -0.921, 0.587,
             1.578, 2.3, -2.288, -2.162, -1.631, -1.555, -0.725, 0.097),
    stringsAsFactors = FALSE
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
