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
      
      Model is correctly specified.
      All minimal sufficient adjustments to estimate the direct and total effect were done.
      

---

    Code
      print(dag)
    Output
      # Check for correct adjustment sets
      - Outcome: y
      - Exposure: x
      
      Identification of direct and total effects
      
      Incorrectly adjusted!
      To estimate the direct and total effect, at least adjust for `b`.
      Currently, the model does not adjust for any variables.
      

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
      To estimate the direct and total effect, at least adjust for `b` and `c`.
      Currently, the model only adjusts for `c`.
      

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
      To estimate the direct and total effect, at least adjust for `b` and `c`.
      Currently, the model only adjusts for `c`.
      

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
      All minimal sufficient adjustments to estimate the direct and total effect were done.
      

# check_dag, multiple adjustment sets

    Code
      print(dag)
    Output
      # Check for correct adjustment sets
      - Outcome: exam
      - Exposure: podcast
      
      Identification of direct and total effects
      
      Incorrectly adjusted!
      To estimate the direct and total effect, at least adjust for one of the following sets:
      - alertness, prepared
      - alertness, skills_course
      - mood, prepared
      - mood, skills_course.
      Currently, the model does not adjust for any variables.
      

---

    Code
      print(dag)
    Output
      # Check for correct adjustment sets
      - Outcome: exam
      - Exposure: podcast
      - Adjustments: alertness and prepared
      
      Identification of direct and total effects
      
      Model is correctly specified.
      All minimal sufficient adjustments to estimate the direct and total effect were done.
      

# check_dag, different adjustements for total and direct

    Code
      print(dag)
    Output
      # Check for correct adjustment sets
      - Outcome: outcome
      - Exposure: exposure
      
      Identification of direct effects
      
      Incorrectly adjusted!
      To estimate the direct effect, at least adjust for `x1` and `x2`.
      Currently, the model does not adjust for any variables.
      
      Identification of total effects
      
      Incorrectly adjusted!
      To estimate the total effect, at least adjust for `x1`.
      Currently, the model does not adjust for any variables.
      

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
      To estimate the direct effect, at least adjust for `x1` and `x2`.
      Currently, the model only adjusts for `x1`.
      
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
      To estimate the direct effect, at least adjust for `x1` and `x2`.
      Currently, the model only adjusts for `x2`.
      
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
      

