# see

<details>

* Version: 0.6.2
* GitHub: https://github.com/easystats/see
* Source code: https://github.com/cran/see
* Date/Publication: 2021-02-04 18:00:02 UTC
* Number of recursive dependencies: 192

Run `revdep_details(, "see")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in 'see-Ex.R' failed
    The error most likely occurred in:
    
    > ### Name: plot.see_performance_roc
    > ### Title: Plot method for ROC curves
    > ### Aliases: plot.see_performance_roc
    > 
    > ### ** Examples
    > 
    > library(performance)
    ...
      6. |     \-ggplot2:::f(l = layers[[i]], d = data[[i]])
      7. |       \-l$compute_aesthetics(d, plot)
      8. |         \-ggplot2:::f(..., self = self)
      9. |           \-base::lapply(aesthetics, eval_tidy, data = data, env = env)
     10. |             \-rlang:::FUN(X[[i]], ...)
     11. +-Specifity
     12. +-rlang:::`$.rlang_data_pronoun`(.data, Specifity)
     13. | \-rlang:::data_pronoun_get(x, nm)
     14. \-rlang:::abort_data_pronoun(x)
    Execution halted
    ```

