# check_collinearity, ci = NULL

    Code
      out
    Output
      # Check for Multicollinearity
      
      Low Correlation
      
       Term  VIF Increased SE Tolerance
          N 1.00         1.00      1.00
          P 1.00         1.00      1.00
          K 1.00         1.00      1.00

# check_collinearity, hurdle/zi models w/o zi-formula

    Code
      print(out)
    Output
      # Check for Multicollinearity
      
      * conditional component:
      
      Low Correlation
      
       Term  VIF   VIF 95% CI Increased SE Tolerance Tolerance 95% CI
        fem 1.06 [1.02, 1.20]         1.03      0.95     [0.83, 0.98]
        mar 1.06 [1.02, 1.20]         1.03      0.95     [0.83, 0.98]
      
      * zero inflated component:
      
      Low Correlation
      
       Term  VIF   VIF 95% CI Increased SE Tolerance Tolerance 95% CI
        fem 1.07 [1.02, 1.20]         1.03      0.94     [0.83, 0.98]
        mar 1.07 [1.02, 1.20]         1.03      0.94     [0.83, 0.98]

