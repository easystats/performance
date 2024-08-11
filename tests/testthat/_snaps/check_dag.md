# check_dag

    Code
      print(dag)
    Output
      # Check for correct adjustment sets
      - Outcome: y
      - Exposure: x
      
      Identification of {.i direct and total} effects
      
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
      
      Identification of {.i direct and total} effects
      
      Model is correctly specified.
      All minimal sufficient adjustments to estimate the direct and total effect were done.
      

---

    Code
      print(dag)
    Output
      # Check for correct adjustment sets
      - Outcome: y
      - Exposure: x
      
      Identification of {.i direct and total} effects
      
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
      
      Identification of {.i direct and total} effects
      
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
      
      Identification of {.i direct and total} effects
      
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
      
      Identification of {.i direct and total} effects
      
      Model is correctly specified.
      All minimal sufficient adjustments to estimate the direct and total effect were done.
      

# check_dag, multiple adjustment sets

    Code
      print(dag)
    Output
      # Check for correct adjustment sets
      - Outcome: exam
      - Exposure: podcast
      
      Identification of {.i direct and total} effects
      
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
      
      Identification of {.i direct and total} effects
      
      Model is correctly specified.
      All minimal sufficient adjustments to estimate the direct and total effect were done.
      

