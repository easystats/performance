#' @title Check correct model adjustment for identifying causal effects
#' @name check_dag
#'
#' @description Check correct model adjustment for identifying causal effects.
#'
#' @export
check_dag <- function(..., outcome = NULL, exposure = NULL) {
  insight::check_if_installed(
    c("ggdag", "dagitty"),
    reason = "to check correct adjustments for identifying causal effects."
  )
}

# Setup -------------------------------------------
# -------------------------------------------------

library(easystats)
library(glmmTMB)
library(ggeffects)
library(ggdag)

load("nest_imputiert.RData")

# Variablen anschauen
data_seek(nest, "fabel")
colnames(nest)
data_tabulate(nest, c("familienstand", "einkommen", "casmin3"))


# DAG - Thereotisches Modell ----------------------
# -------------------------------------------------

dag <- dagify(
  fabel ~ alter + kind_pflegegrad + geschlecht + einkommen + casmin3 + gruppe + familienstand,
  einkommen ~ casmin3 + geschlecht + alter,
  outcome = "fabel",
  exposure = "gruppe"
)
# theoretisches modell abbilden
ggdag_status(dag) + theme_dag()

ggdag_status(dag, text = FALSE, use_labels = "name") +
  ggplot2::guides(color = "none") +  # Turn off legend
  theme_dag()

# wofür müssen wir adjustieren, wenn wir am Gesamteffekt interessiert sind?
dagitty::adjustmentSets(dag)
ggdag_adjustment_set(dag)
# wofür müssen wir adjustieren, wenn wir am direkten Effekt interessiert sind?
dagitty::adjustmentSets(dag, effect = "direct")
ggdag_adjustment_set(dag, effect = "direct")
# incorrect adjustement - adjustmentSets() liefert leeres Element und
# adjustedNodes() hat mehr als 1 Element
r <- dagitty::adjustmentSets(dag)
incorrectly_adjusted <- (!length(r) && length(dagitty::adjustedNodes(dag)) > 0)
incorrectly_adjusted
r <- dagitty::adjustmentSets(dag, effect = "direct")
incorrectly_adjusted <- (!length(r) && length(dagitty::adjustedNodes(dag)) > 0)
incorrectly_adjusted
# Wir müssen nicht adjustieren - hi
