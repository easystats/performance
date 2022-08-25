#' \item **Isolation Forest**:
#'  The outliers are detected using the anomaly score of an isolation forest (a
#'  class of random forest). The default threshold of 0.025 will classify as
#'  outliers the observations located at `qnorm(1-0.025) * MAD)` (a robust
#'  equivalent of SD) of the median (roughly corresponding to the 2.5\% most
#'  extreme observations). Requires the \pkg{solitude} package.
#'
#'  \item **Local Outlier Factor**:
#'  Based on a K nearest neighbors algorithm, LOF compares the local density of
#'  a point to the local densities of its neighbors instead of computing a
#'  distance from the center (Breunig et al., 2000). Points that have a
#'  substantially lower density than their neighbors are considered outliers. A
#'  LOF score of approximately 1 indicates that density around the point is
#'  comparable to its neighbors. Scores significantly larger than 1 indicate
#'  outliers. The default threshold of 0.025 will classify as outliers the
#'  observations located at `qnorm(1-0.025) * SD)` of the log-transformed
#'  LOF distance. Requires the \pkg{dbscan} package.
