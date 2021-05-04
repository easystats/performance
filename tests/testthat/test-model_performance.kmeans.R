if (require("testthat") && require("performance")) {
  test_that("model_performance.kmeans", {
    set.seed(123)
    cl <- kmeans(subset(iris, select = Sepal.Length:Petal.Width), 3)

    expect_equal(
      model_performance(cl),
      structure(
        list(
          Sum_Squares_Within = 681.3706,
          Sum_Squares_Between = 78.851441426146,
          Sum_Squares_Total = 602.519158573854,
          Iterations = 2L
        ),
        class = c("performance_model", "data.frame"),
        row.names = c(NA, -1L)
      ),
      tolerance = 0.1
    )

    set.seed(123)
    mod <- kmeans(subset(iris, select = Sepal.Length:Petal.Width),
      centers = 3, iter.max = 100, nstart = 10
    )

    expect_equal(
      model_performance(mod),
      structure(
        list(
          Sum_Squares_Within = 681.3706,
          Sum_Squares_Between = 78.851441426146,
          Sum_Squares_Total = 602.519158573854,
          Iterations = 2L
        ),
        class = c(
          "performance_model",
          "data.frame"
        ),
        row.names = c(NA, -1L)
      ),
      tolerance = 0.1
    )
  })
}
