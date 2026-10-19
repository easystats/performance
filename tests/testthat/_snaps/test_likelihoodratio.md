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
      print_md(test_likelihoodratio(m1, m2), p_digits = 3)
    Output
      
      
      Table: Likelihood-Ratio-Test (LRT) for Model Comparison (OLS-estimator)
      
      |Name | Model| df| df_diff|  Chi2|      p|
      |:----|-----:|--:|-------:|-----:|------:|
      |m1   |    lm|  3|        |      |       |
      |m2   |    lm|  7|       4| 40.32| < .001|

