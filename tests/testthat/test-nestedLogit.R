skip_if_not_installed("nestedLogit")
skip_if_not_installed("carData")

data("Womenlf", package = "carData")
comparisons <- nestedLogit::logits(
  work = nestedLogit::dichotomy("not.work", working = c("parttime", "fulltime")),
  full = nestedLogit::dichotomy("parttime", "fulltime")
)
mnl <- nestedLogit::nestedLogit(
  partic ~ hincome + children,
  dichotomies = comparisons,
  data = Womenlf
)

test_that("model_performance", {
  expect_snapshot(model_performance(mnl))
})
