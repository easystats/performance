skip_on_os(c("mac", "linux"))
skip_if(packageVersion("insight") <= "0.19.5.10")
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

test_that("r2", {
  out <- r2(mnl)
  expect_equal(
    out,
    list(R2_Tjur = list(
      work = c(`Tjur's R2` = 0.137759452521642),
      full = c(`Tjur's R2` = 0.332536937208286)
    )),
    ignore_attr = TRUE,
    tolerance = 1e-4
  )

  out <- r2_tjur(mnl)
  expect_equal(
    out,
    list(R2_Tjur = list(
      work = c(`Tjur's R2` = 0.137759452521642),
      full = c(`Tjur's R2` = 0.332536937208286)
    )),
    ignore_attr = TRUE,
    tolerance = 1e-4
  )

  out <- r2_coxsnell(mnl)
  expect_equal(
    out,
    list(
      work = c(`Cox & Snell's R2` = 0.129313084315599),
      full = c(`Cox & Snell's R2` = 0.308541455410686)
    ),
    ignore_attr = TRUE,
    tolerance = 1e-4
  )

  out <- r2_nagelkerke(mnl)
  expect_equal(
    out,
    list(
      work = c(`Nagelkerke's R2` = 0.174313365512442),
      full = c(`Nagelkerke's R2` = 0.418511411473948)
    ),
    ignore_attr = TRUE,
    tolerance = 1e-4
  )
})


test_that("model_performance", {
  expect_snapshot(model_performance(mnl))
})
