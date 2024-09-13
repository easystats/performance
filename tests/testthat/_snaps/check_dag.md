# check_dag

    Code
      print(dag)
    Output
      # Check for correct adjustment sets
      - Outcome: y
      - Exposure: x
      
      Identification of direct and total effects
      
      Model is correctly specified.
      No adjustment needed to estimate the direct and total effect of `x` on `y`.
      

---

    Code
      print(dag)
    Output
      # Check for correct adjustment sets
      - Outcome: y
      - Exposure: x
      - Adjustment: b
      
      Identification of direct and total effects
      
      Incorrectly adjusted!
      To estimate the direct and total effect, do not adjust for `b`.
      

---

    Code
      print(dag)
    Output
      # Check for correct adjustment sets
      - Outcome: y
      - Exposure: x
      
      Identification of direct and total effects
      
      Incorrectly adjusted!
      To estimate the direct and total effect, do not adjust for .
      

---

    Code
      print(dag)
    Output
      # Check for correct adjustment sets
      - Outcome: y
      - Exposure: x
      - Adjustment: c
      
      Identification of direct and total effects
      
      Incorrectly adjusted!
      To estimate the direct and total effect, do not adjust for `c`.
      

---

    Code
      print(dag)
    Output
      # Check for correct adjustment sets
      - Outcome: y
      - Exposure: x
      - Adjustment: c
      
      Identification of direct and total effects
      
      Incorrectly adjusted!
      To estimate the direct and total effect, do not adjust for `c`.
      

---

    Code
      print(dag)
    Output
      # Check for correct adjustment sets
      - Outcome: mpg
      - Exposure: wt
      - Adjustments: cyl, disp and gear
      
      Identification of direct and total effects
      
      Model is correctly specified.
      No adjustment needed to estimate the direct and total effect of `wt` on `mpg`.
      

# check_dag, multiple adjustment sets

    Code
      print(dag)
    Output
      # Check for correct adjustment sets
      - Outcome: exam
      - Exposure: podcast
      
      Identification of direct and total effects
      
      Incorrectly adjusted!
      To estimate the direct and total effect, do not adjust for .
      

---

    Code
      print(dag)
    Output
      # Check for correct adjustment sets
      - Outcome: exam
      - Exposure: podcast
      - Adjustments: alertness and prepared
      
      Identification of direct and total effects
      
      Incorrectly adjusted!
      To estimate the direct and total effect, do not adjust for `alertness` and `prepared`.
      

# check_dag, different adjustements for total and direct

    Code
      print(dag)
    Output
      # Check for correct adjustment sets
      - Outcome: outcome
      - Exposure: exposure
      
      Identification of direct effects
      
      Incorrectly adjusted!
      To estimate the direct effect, do not adjust for .
      
      Identification of total effects
      
      Incorrectly adjusted!
      To estimate the total effect, do not adjust for .
      

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
      To estimate the direct effect, do not adjust for `x1`.
      
      Identification of total effects
      
      Incorrectly adjusted!
      To estimate the total effect, do not adjust for `x1`.
      

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
      To estimate the direct effect, do not adjust for `x2`.
      
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
      
      Incorrectly adjusted!
      To estimate the direct effect, do not adjust for `x1` and `x2`.
      
      Identification of total effects
      
      Incorrectly adjusted!
      To estimate the total effect, do not adjust for `x1` and `x2`.
      

# check_dag, collider bias

    Code
      print(dag)
    Output
      # Check for correct adjustment sets
      - Outcome: SMD_ICD11
      - Exposure: agegroup
      - Adjustments: edgroup3, gender_kid, pss4_kid_sum_2sd and residence
      
      Identification of direct effects
      
      Model is correctly specified.
      No adjustment needed to estimate the direct effect of `agegroup` on `SMD_ICD11`.
      
      Identification of total effects
      
      Model is correctly specified.
      No adjustment needed to estimate the total effect of `agegroup` on `SMD_ICD11`.
      

---

    Code
      print(dag)
    Output
      # Check for correct adjustment sets
      - Outcome: SMD_ICD11
      - Exposure: agegroup
      - Adjustments: edgroup3, gender_kid, pss4_kid_sum_2sd, residence and sm_h_total_kid
      - Collider: sm_h_total_kid
      
      Identification of direct effects
      
      Incorrectly adjusted!
      Your model adjusts for a (downstream) collider, `sm_h_total_kid`. To estimate the direct effect, do not adjust for it, to avoid collider-bias.
      
      Identification of total effects
      
      Incorrectly adjusted!
      Your model adjusts for a (downstream) collider, `sm_h_total_kid`. To estimate the total effect, do not adjust for it, to avoid collider-bias.
      

