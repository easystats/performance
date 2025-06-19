# item_omega

    Code
      print(omega)
    Output
      # Reliability Coefficients
      
      Statistic            | Coefficient
      ----------------------------------
      Alpha                |        0.88
      G.6                  |        0.97
      Omega (hierarchical) |        0.57
      Omega (asymptotic H) |        0.58
      Omega (total)        |        0.97

---

    Code
      print_md(omega)
    Output
      [1] "Table: Reliability Coefficients"     
      [2] ""                                    
      [3] "|Statistic            | Coefficient|"
      [4] "|:--------------------|-----------:|"
      [5] "|Alpha                |        0.88|"
      [6] "|G.6                  |        0.97|"
      [7] "|Omega (hierarchical) |        0.57|"
      [8] "|Omega (asymptotic H) |        0.58|"
      [9] "|Omega (total)        |        0.97|"
      attr(,"format")
      [1] "pipe"
      attr(,"class")
      [1] "knitr_kable" "character"  

---

    Code
      parameters::model_parameters(omega)
    Output
      # Rotated loadings from Omega (oblimin-rotation)
      
      Variable |        g |   F1* |      F2* |       F3* |   h2 |   u2 |       p2 | Complexity
      ----------------------------------------------------------------------------------------
      mpg-     |     0.58 | -0.67 |     0.09 |      0.29 | 0.88 | 0.12 |     0.38 |       2.40
      cyl      |     0.70 | -0.61 |     0.28 |      0.07 | 0.96 | 0.04 |     0.52 |       2.33
      disp     |     0.59 | -0.71 |     0.18 |      0.11 | 0.89 | 0.11 |     0.39 |       2.13
      hp       |     0.77 | -0.31 |     0.23 |      0.36 | 0.87 | 0.13 |     0.68 |       2.00
      drat-    |     0.27 | -0.79 |     0.06 |     -0.07 | 0.71 | 0.29 |     0.10 |       1.26
      wt       |     0.43 | -0.79 |    -0.04 |      0.31 | 0.91 | 0.09 |     0.20 |       1.87
      qsec-    |     0.81 |  0.19 |     0.50 |      0.06 | 0.95 | 0.05 |     0.70 |       1.81
      vs-      |     0.74 | -0.27 |     0.38 |      0.05 | 0.77 | 0.23 |     0.71 |       1.81
      am-      | 8.38e-03 | -0.89 |    -0.15 | -9.51e-03 | 0.81 | 0.19 | 8.63e-05 |       1.06
      gear     |     0.03 |  0.87 | 9.01e-03 |      0.32 | 0.87 | 0.13 | 9.03e-04 |       1.27
      carb     |     0.68 |  0.06 |     0.10 |      0.63 | 0.87 | 0.13 |     0.53 |       2.06

---

    Code
      summary(omega)
    Output
      # Omega Statistics
      
      Statistic            | Coefficient
      ----------------------------------
      Alpha                |        0.88
      G.6                  |        0.97
      Omega (hierarchical) |        0.57
      Omega (asymptotic H) |        0.58
      Omega (total)        |        0.97
      
      # Omega Coefficients
      
      Composite | Omega (total) | Omega (hierarchical) | Omega (group)
      ----------------------------------------------------------------
      g         |          0.97 |                 0.57 |          0.26
      F1*       |          0.90 |                 0.31 |          0.59
      F2*       |          0.91 |                 0.69 |          0.22
      F3*       |          0.87 |                 0.60 |          0.28
      
      # Variances
      
      Composite | Total (%) | General Factor (%) | Group Factor (%)
      -------------------------------------------------------------
      g         |     97.28 |              56.64 |            26.42
      F1*       |     90.12 |              31.07 |            59.05
      F2*       |     91.37 |              69.32 |            22.04
      F3*       |     87.36 |              59.65 |            27.71

