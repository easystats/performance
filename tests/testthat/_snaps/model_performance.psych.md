# model_performance.psych

    Code
      print(raq_fa)
    Output
      # Loadings from Factor Analysis (no rotation)
      
      Variable |   MR1 |       MR2 |      MR3 |       MR4 | Complexity | Uniqueness
      -----------------------------------------------------------------------------
      raq_01   |  0.32 |     -0.16 |     0.13 |      0.16 |       2.41 |       0.83
      raq_02   |  0.50 |      0.35 | 7.07e-03 |      0.02 |       1.80 |       0.62
      raq_03   | -0.34 |      0.18 |    -0.11 |     -0.19 |       2.43 |       0.80
      raq_04   |  0.44 |     -0.23 |     0.12 |      0.26 |       2.38 |       0.67
      raq_05   |  0.69 |     -0.16 |    -0.13 |      0.15 |       1.28 |       0.46
      raq_06   |  0.75 | -1.29e-03 |    -0.42 |     -0.06 |       1.59 |       0.27
      raq_07   |  0.53 |      0.02 |    -0.26 |     -0.05 |       1.47 |       0.65
      raq_08   |  0.63 |     -0.33 |     0.30 |     -0.40 |       2.78 |       0.25
      raq_09   |  0.46 |      0.57 |     0.29 |  7.56e-03 |       2.44 |       0.38
      raq_10   |  0.44 |     -0.05 |    -0.25 |      0.02 |       1.63 |       0.74
      raq_11   |  0.56 |     -0.27 |     0.26 |     -0.31 |       2.60 |       0.45
      raq_12   |  0.23 |     -0.11 |     0.08 |      0.20 |       2.69 |       0.89
      raq_13   |  0.51 |      0.02 |    -0.28 |  1.55e-03 |       1.56 |       0.66
      raq_14   |  0.43 |  8.03e-03 |    -0.18 |     -0.04 |       1.35 |       0.78
      raq_15   |  0.49 | -3.26e-03 |    -0.21 |     -0.03 |       1.36 |       0.71
      raq_16   |  0.35 |     -0.18 |     0.15 |      0.25 |       2.80 |       0.77
      raq_17   |  0.54 |     -0.24 |     0.22 |     -0.30 |       2.44 |       0.51
      raq_18   |  0.58 | -9.26e-03 |    -0.30 |     -0.08 |       1.53 |       0.57
      raq_19   |  0.57 |      0.41 |     0.04 |  4.45e-03 |       1.83 |       0.50
      raq_20   |  0.45 |     -0.22 |     0.15 |      0.23 |       2.28 |       0.68
      raq_21   |  0.51 |     -0.21 |     0.19 |      0.25 |       2.22 |       0.60
      raq_22   |  0.54 |      0.35 |     0.08 | -6.93e-03 |       1.78 |       0.59
      raq_23   |  0.43 |      0.57 |     0.28 |      0.03 |       2.38 |       0.41
      
      The 4 latent factors accounted for 40.12% of the total variance of the original data (MR1 = 25.38%, MR2 = 6.88%, MR3 = 4.67%, MR4 = 3.18%).

---

    Code
      print(raq_fa, threshold = 0.4)
    Output
      # Loadings from Factor Analysis (no rotation)
      
      Variable |  MR1 |  MR2 |   MR3 | MR4 | Complexity | Uniqueness
      --------------------------------------------------------------
      raq_01   |      |      |       |     |       2.41 |       0.83
      raq_02   | 0.50 |      |       |     |       1.80 |       0.62
      raq_03   |      |      |       |     |       2.43 |       0.80
      raq_04   | 0.44 |      |       |     |       2.38 |       0.67
      raq_05   | 0.69 |      |       |     |       1.28 |       0.46
      raq_06   | 0.75 |      | -0.42 |     |       1.59 |       0.27
      raq_07   | 0.53 |      |       |     |       1.47 |       0.65
      raq_08   | 0.63 |      |       |     |       2.78 |       0.25
      raq_09   | 0.46 | 0.57 |       |     |       2.44 |       0.38
      raq_10   | 0.44 |      |       |     |       1.63 |       0.74
      raq_11   | 0.56 |      |       |     |       2.60 |       0.45
      raq_12   |      |      |       |     |       2.69 |       0.89
      raq_13   | 0.51 |      |       |     |       1.56 |       0.66
      raq_14   | 0.43 |      |       |     |       1.35 |       0.78
      raq_15   | 0.49 |      |       |     |       1.36 |       0.71
      raq_16   |      |      |       |     |       2.80 |       0.77
      raq_17   | 0.54 |      |       |     |       2.44 |       0.51
      raq_18   | 0.58 |      |       |     |       1.53 |       0.57
      raq_19   | 0.57 | 0.41 |       |     |       1.83 |       0.50
      raq_20   | 0.45 |      |       |     |       2.28 |       0.68
      raq_21   | 0.51 |      |       |     |       2.22 |       0.60
      raq_22   | 0.54 |      |       |     |       1.78 |       0.59
      raq_23   | 0.43 | 0.57 |       |     |       2.38 |       0.41
      
      The 4 latent factors accounted for 40.12% of the total variance of the original data (MR1 = 25.38%, MR2 = 6.88%, MR3 = 4.67%, MR4 = 3.18%).

---

    Code
      summary(raq_fa)
    Output
      # (Explained) Variance of Components
      
      Parameter                       |   MR1 |   MR2 |   MR3 |   MR4
      ---------------------------------------------------------------
      Eigenvalues                     | 5.837 | 1.582 | 1.074 | 0.733
      Variance Explained              | 0.254 | 0.069 | 0.047 | 0.032
      Variance Explained (Cumulative) | 0.254 | 0.323 | 0.369 | 0.401
      Variance Explained (Proportion) | 0.633 | 0.172 | 0.116 | 0.079

---

    Code
      print(out)
    Output
      # Indices of model performance
      
      Chi2(167) | p (Chi2) |  RMSA | RMSA_corrected |   TLI | RMSEA |   RMSEA 90% CI |       BIC
      ------------------------------------------------------------------------------------------
      267.210   |   < .001 | 0.013 |          0.015 | 0.991 | 0.015 | [0.012, 0.019] | -1044.082

---

    Code
      print(out)
    Output
      # Indices of model performance
      
      Chi2(1) | p (Chi2) |  RMSA
      --------------------------
      4.119   |    0.042 | 0.035

---

    Code
      print(out, table_width = Inf)
    Output
      # Indices of model performance
      
      Model             |    Chi2 | df | p (Chi2) |  RMSA | RMSA_corrected |   TLI | RMSEA |   RMSEA 90% CI |     BIC |    R2 | Correlation
      -------------------------------------------------------------------------------------------------------------------------------------
      3-factor solution |  31.796 | 25 |   0.164  | 0.015 |          0.023 |       | 0.087 | [0.000, 0.181] | -54.848 |       |            
      g-model           | 264.781 | 44 |   < .001 | 0.393 |          0.440 | 0.195 | 0.395 | [0.356, 0.450] | 112.289 | 0.761 |       0.873
      
      Compare the model fit of the 3-factor solution with the g-only model.
        If the g-model has smaller RMSA and RMSEA then your items are more
        likely to describe a single unidimensional construct. If the 3-factor
        model has smaller RMSA and RMSEA then your construct is more likely to
        be made up of 3 sub-constructs.

---

    Code
      print(out, table_width = Inf)
    Output
      # Indices of model performance
      
      Model             |    Chi2 | df | p (Chi2) |  RMSA | RMSA_corrected |   TLI | RMSEA |   RMSEA 90% CI |     BIC |    R2 | Correlation
      -------------------------------------------------------------------------------------------------------------------------------------
      3-factor solution |  31.796 | 25 |   0.164  | 0.015 |          0.023 |       | 0.087 | [0.000, 0.181] | -54.848 |       |            
      g-model           | 264.781 | 44 |   < .001 | 0.393 |          0.440 | 0.195 | 0.395 | [0.356, 0.450] | 112.289 | 0.761 |       0.873
      
      Compare the model fit of the 3-factor solution with the g-only model.
        If the g-model has smaller RMSA and RMSEA then your items are more
        likely to describe a single unidimensional construct. If the 3-factor
        model has smaller RMSA and RMSEA then your construct is more likely to
        be made up of 3 sub-constructs.

