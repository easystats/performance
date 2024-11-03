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

