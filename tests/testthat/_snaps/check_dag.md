# check_dag, different adjustements for total and direct

    Code
      print(dag)
    Output
      # Check for correct adjustment sets
      - Outcome: outcome
      - Exposure: exposure
      
      Identification of direct effects
      
      Incorrectly adjusted!
      To estimate the direct effect, at least adjust for `x1` and `x2`. Currently, the model does not adjust for any variables.
      
      Identification of total effects
      
      Incorrectly adjusted!
      To estimate the total effect, at least adjust for `x1`. Currently, the model does not adjust for any variables.
      

---

    Code
      print(dag)
    Output
      # Check for correct adjustment sets
      - Outcome: outcome
      - Exposure: exposure
      - Adjustment: x1
      
      Identification of direct effects
      
      Incorrectly adjusted!
      To estimate the direct effect, at least adjust for `x1` and `x2`. Currently, the model only adjusts for `x1`. You possibly also need to adjust for `x2` to block biasing paths.
      
      Identification of total effects
      
      Model is correctly specified.
      All minimal sufficient adjustments to estimate the total effect were done.
      

---

    Code
      print(dag)
    Output
      # Check for correct adjustment sets
      - Outcome: outcome
      - Exposure: exposure
      - Adjustment: x2
      
      Identification of direct effects
      
      Incorrectly adjusted!
      To estimate the direct effect, at least adjust for `x1` and `x2`. Currently, the model only adjusts for `x2`. You possibly also need to adjust for `x1` to block biasing paths.
      
      Identification of total effects
      
      Incorrectly adjusted!
      To estimate the total effect, do not adjust for `x2`.
      

---

    Code
      print(dag)
    Output
      # Check for correct adjustment sets
      - Outcome: outcome
      - Exposure: exposure
      - Adjustments: x1 and x2
      
      Identification of direct effects
      
      Model is correctly specified.
      All minimal sufficient adjustments to estimate the direct effect were done.
      
      Identification of total effects
      
      Incorrectly adjusted!
      To estimate the total effect, do not adjust for `x1` and `x2`.
      

