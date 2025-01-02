# compare_performance

    Code
      print(compare_performance(lm1, lm2, lm3))
    Output
      # Comparison of Model Performance Indices
      
      Name | Model | AIC (weights) | AICc (weights) | BIC (weights) |    R2
      ---------------------------------------------------------------------
      lm1  |    lm | 231.5 (<.001) |  231.7 (<.001) | 243.5 (<.001) | 0.619
      lm2  |    lm | 106.2 (0.566) |  106.6 (0.611) | 121.3 (0.964) | 0.837
      lm3  |    lm | 106.8 (0.434) |  107.6 (0.389) | 127.8 (0.036) | 0.840
      
      Name | R2 (adj.) |  RMSE | Sigma
      --------------------------------
      lm1  |     0.614 | 0.510 | 0.515
      lm2  |     0.833 | 0.333 | 0.338
      lm3  |     0.835 | 0.330 | 0.336

---

    Code
      print(compare_performance(lm1, lm2, lm3), table_width = Inf)
    Output
      # Comparison of Model Performance Indices
      
      Name | Model | AIC (weights) | AICc (weights) | BIC (weights) |    R2 | R2 (adj.) |  RMSE | Sigma
      -------------------------------------------------------------------------------------------------
      lm1  |    lm | 231.5 (<.001) |  231.7 (<.001) | 243.5 (<.001) | 0.619 |     0.614 | 0.510 | 0.515
      lm2  |    lm | 106.2 (0.566) |  106.6 (0.611) | 121.3 (0.964) | 0.837 |     0.833 | 0.333 | 0.338
      lm3  |    lm | 106.8 (0.434) |  107.6 (0.389) | 127.8 (0.036) | 0.840 |     0.835 | 0.330 | 0.336

---

    Code
      print(compare_performance(lm1, lm2, lm3), layout = "vertical")
    Output
      # Comparison of Model Performance Indices
      
      Metric         |           lm1 |           lm2 |           lm3
      --------------------------------------------------------------
      Model          |            lm |            lm |            lm
      AIC (weights)  | 231.5 (<.001) | 106.2 (0.566) | 106.8 (0.434)
      AICc (weights) | 231.7 (<.001) | 106.6 (0.611) | 107.6 (0.389)
      BIC (weights)  | 243.5 (<.001) | 121.3 (0.964) | 127.8 (0.036)
      R2             |         0.619 |         0.837 |         0.840
      R2 (adj.)      |         0.614 |         0.833 |         0.835
      RMSE           |         0.510 |         0.333 |         0.330
      Sigma          |         0.515 |         0.338 |         0.336

---

    Code
      print(compare_performance(lm1, lm2, lm3, lm4), layout = "vertical",
      table_width = 50)
    Message
      When comparing models, please note that probably not all models were fit
        from same data.
    Output
      # Comparison of Model Performance Indices
      
      Metric         |           lm1 |           lm2
      ----------------------------------------------
      Model          |            lm |            lm
      AIC (weights)  | 231.5 (<.001) | 106.2 (0.408)
      AICc (weights) | 231.7 (<.001) | 106.6 (0.454)
      BIC (weights)  | 243.5 (<.001) | 121.3 (0.933)
      R2             |         0.619 |         0.837
      R2 (adj.)      |         0.614 |         0.833
      RMSE           |         0.510 |         0.333
      Sigma          |         0.515 |         0.338
      
      Metric         |           lm3 |           lm4
      ----------------------------------------------
      Model          |            lm |            lm
      AIC (weights)  | 106.8 (0.313) | 107.0 (0.279)
      AICc (weights) | 107.6 (0.289) | 107.8 (0.257)
      BIC (weights)  | 127.8 (0.035) | 128.0 (0.032)
      R2             |         0.840 |         0.840
      R2 (adj.)      |         0.835 |         0.834
      RMSE           |         0.330 |         0.331
      Sigma          |         0.336 |         0.337

