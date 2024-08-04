# check_dag

    Code
      print(dag)
    Output
      # Check for correct adjustment sets
      - Outcome: y
      - Exposure: x
      
      Identification of direct effects
      
      Model is correctly specified.
      No adjustment needed to estimate the direct effect of `x` on `y`.
      
      Identification of total effects
      
      Model is correctly specified.
      No adjustment needed to estimate the total effect of `x` on `y`.
      

---

    Code
      print(dag)
    Output
      # Check for correct adjustment sets
      - Outcome: y
      - Exposure: x
      - Adjustment: b
      
      Identification of direct effects
      
      Model is correctly specified.
      All minimal sufficient adjustments to estimate the direct effect were done.
      
      Identification of total effects
      
      Model is correctly specified.
      All minimal sufficient adjustments to estimate the total effect were done.
      

---

    Code
      print(dag)
    Output
      # Check for correct adjustment sets
      - Outcome: y
      - Exposure: x
      
      Identification of direct effects
      
      Incorrectly adjusted!
      To estimate the direct effect, also adjust for `b`.
      Currently, the model does not adjust for any variables.
      
      Identification of total effects
      
      Incorrectly adjusted!
      To estimate the total effect, also adjust for `b`.
      Currently, the model does not adjust for any variables.
      

---

    Code
      print(dag)
    Output
      # Check for correct adjustment sets
      - Outcome: y
      - Exposure: x
      - Adjustment: c
      
      Identification of direct effects
      
      Incorrectly adjusted!
      To estimate the direct effect, also adjust for `b` and `c`.
      Currently, the model only adjusts for `c`.
      
      Identification of total effects
      
      Incorrectly adjusted!
      To estimate the total effect, also adjust for `b` and `c`.
      Currently, the model only adjusts for `c`.
      

---

    Code
      print(dag)
    Output
      # Check for correct adjustment sets
      - Outcome: y
      - Exposure: x
      - Adjustment: c
      
      Identification of direct effects
      
      Incorrectly adjusted!
      To estimate the direct effect, also adjust for `b` and `c`.
      Currently, the model only adjusts for `c`.
      
      Identification of total effects
      
      Incorrectly adjusted!
      To estimate the total effect, also adjust for `b` and `c`.
      Currently, the model only adjusts for `c`.
      

---

    Code
      print(dag)
    Output
      # Check for correct adjustment sets
      - Outcome: mpg
      - Exposure: wt
      - Adjustments: cyl, disp and gear
      
      Identification of direct effects
      
      Model is correctly specified.
      All minimal sufficient adjustments to estimate the direct effect were done.
      
      Identification of total effects
      
      Model is correctly specified.
      All minimal sufficient adjustments to estimate the total effect were done.
      

