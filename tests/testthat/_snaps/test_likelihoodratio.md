# test_likelihoodratio - print p-digits

    Code
      test_likelihoodratio(m1, m2)
    Output
      # Likelihood-Ratio-Test (LRT) for Model Comparison (OLS-estimator)
      
      Name | Model | df | df_diff |  Chi2 |      p
      --------------------------------------------
      m1   |    lm |  3 |         |       |       
      m2   |    lm |  7 |       4 | 40.32 | < .001

---

    Code
      insight::print_md(test_likelihoodratio(m1, m2), p_digits = 3)
    Output
      [1] "Table: # Likelihood-Ratio-Test (LRT) for Model Comparison (OLS-estimator)"
      [2] ""                                                                         
      [3] "|Name | Model| df| df_diff|  Chi2|      p|"                               
      [4] "|:----|-----:|--:|-------:|-----:|------:|"                               
      [5] "|m1   |    lm|  3|        |      |       |"                               
      [6] "|m2   |    lm|  7|       4| 40.32| < .001|"                               
      attr(,"format")
      [1] "pipe"
      attr(,"class")
      [1] "knitr_kable" "character"  

