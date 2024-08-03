# check_dag

    Code
      print(dag)
    Output
      # Correct adjustments for identifying direct effects
      
      Model is correctly specified.
      - Outcome: y
      - Exposure: x
      
      No adjustment needed to estimate the direct effect of `x` on `y`.
      
      # Correct adjustments for identifying total effects
      
      Model is correctly specified.
      - Outcome: y
      - Exposure: x
      
      No adjustment needed to estimate the total effect of `x` on `y`.
      

---

    Code
      print(dag)
    Output
      # Correct adjustments for identifying direct effects
      
      Model is correctly specified.
      - Outcome: y
      - Exposure: x
      
      All minimal sufficient adjustments to estimate the direct effect were done.
      
      # Correct adjustments for identifying total effects
      
      Model is correctly specified.
      - Outcome: y
      - Exposure: x
      
      All minimal sufficient adjustments to estimate the total effect were done.
      

---

    Code
      print(dag)
    Output
      # Correct adjustments for identifying direct effects
      
      Incorrectly adjusted!
      - Outcome: y
      - Exposure: x
      
      To estimate the direct effect, also adjust for `b`.
      Currently, the model does not adjust for any variables.
      
      # Correct adjustments for identifying total effects
      
      Incorrectly adjusted!
      - Outcome: y
      - Exposure: x
      
      To estimate the total effect, also adjust for `b`.
      Currently, the model does not adjust for any variables.
      

---

    Code
      print(dag)
    Output
      # Correct adjustments for identifying direct effects
      
      Incorrectly adjusted!
      - Outcome: y
      - Exposure: x
      
      To estimate the direct effect, also adjust for `b` and `c`.
      Currently, the model currently only adjusts for `c`.
      
      # Correct adjustments for identifying total effects
      
      Incorrectly adjusted!
      - Outcome: y
      - Exposure: x
      
      To estimate the total effect, also adjust for `b` and `c`.
      Currently, the model currently only adjusts for `c`.
      

