library(lme4)
library(dplyr)
library(r2mlm)

model <- lmer(satisfaction ~ salary_c + (1 + salary_c | schoolID), data = r2mlm::teachsat)



# Utils -------------------------------------------------------------------


add_interaction_vars_to_data <- function(data, interaction_vars) {

  for (whole in interaction_vars) {
    pieces <- stringr::str_split_fixed(whole, ":", 2)
    newcol <- dplyr::pull(data[pieces[1]] * data[pieces[2]])
    data <- dplyr::mutate(data, !!whole := newcol)
  }
  data
}




r2mlm:::get_interaction_vars(lm(Sepal.Length ~ Species * Petal.Width, data=iris))

# Step 1: Components Extraction ------------------------------------------------------
# TODO: this needs to be simplified and "generalized"

r2mlm_init <- function(model) {

  # Step 1) check if model has_intercept
  has_intercept <- insight::has_intercept(model)

  # Step 2) Pull all variable names from the formula
  all_variables <- all.vars(formula(model))
  cluster_variable <- all_variables[length(all_variables)] # pull cluster, we'll need it later

  # Step 3a) pull and prepare data (r2mlm:::prepare_data(model, "lme4", cluster_variable))
  modelframe <- model.frame(model)
  interaction_vars <- insight::find_interactions(model, flatten = TRUE)
  modelframe <- add_interaction_vars_to_data(modelframe, interaction_vars)
  data <- dplyr::group_by(modelframe, modelframe[cluster_variable])



  # Step 3b) determine whether data is appropriate format. Only the cluster variable can be a factor, for now
  # a) Pull all variables except for cluster

  outcome_and_predictors <- all_variables[1:(length(all_variables) - 1)]

  # b) If any of those variables is non-numeric, then throw an error

  for (variable in outcome_and_predictors) {

    if (!(class(data[[variable]]) == "integer") && !(class(data[[variable]]) == "numeric")) {
      stop("Your data must be numeric. Only the cluster variable can be a factor.")
    }

  }

  # Step 4) Fill l1 and l2 vectors

  # * Step 4a) Define variables you'll be sorting
  # take the outcome out of the predictors with [2:length(outcome_and_predictors)], then add interaction terms

  # sometimes the outcome_and_predictors is only an outcome variable (for the null model). If that's the case, then
  #     predictors is null, just call get_interaction_vars just in case

  if (length(outcome_and_predictors) == 1) {
    predictors <- insight::find_interactions(model, flatten = TRUE)
  } else {
    predictors <- append(outcome_and_predictors[2:length(outcome_and_predictors)], insight::find_interactions(model, flatten = TRUE))
  }

  # * Step 4b) Create and fill vectors
  l1_vars <- r2mlm:::sort_variables(data, predictors, cluster_variable)$l1_vars
  l2_vars <- r2mlm:::sort_variables(data, predictors, cluster_variable)$l2_vars

  # Step 5) pull variable names for L1 predictors with random slopes into a variable called random_slope_vars

  random_slope_vars <- r2mlm:::get_random_slope_vars(model, has_intercept, "lme4")

  # Step 6) determine value of centeredwithincluster

  if (is.null(l1_vars)) {
    centeredwithincluster <- TRUE
  } else {
    centeredwithincluster <- r2mlm:::get_cwc(l1_vars, cluster_variable, data)
  }

  # Step 7) pull column numbers for _covs variables
  # 7a) within_covs (l1 variables)
  # for (each value in l1_vars list) {match(value, names(data))}

  within <- r2mlm:::get_covs(l1_vars, data)

  # 7b) pull column numbers for between_covs (l2 variables)

  between <- r2mlm:::get_covs(l2_vars, data)

  # 7c) pull column numbers for random_covs (l1 variables with random slopes)

  random <- r2mlm:::get_covs(random_slope_vars, data)

  # Step 8) pull gamma values (fixed slopes)
  # 8a) gamma_w, fixed slopes for L1 variables (from l1_vars list)
  gammaw <- c()
  i = 1
  for (variable in l1_vars) {
    gammaw[i] <- fixef(model)[variable]
    i = i + 1
  }

  # 8b) gamma_b, intercept value if hasintercept = TRUE, and fixed slopes for L2 variables (from between list)
  gammab <- c()
  if (has_intercept == TRUE) {
    gammab[1] <- fixef(model)[1]
    i = 2
  } else {
    i = 1
  }
  for (var in l2_vars) {
    gammab[i] <- fixef(model)[var]
    i = i + 1
  }

  # Step 9) Tau matrix, results from VarCorr(model)

  vcov <- lme4::VarCorr(model)
  tau <- as.matrix(Matrix::bdiag(vcov))

  # Step 10) sigma^2 value, Rij

  sigma2 <- lme4::getME(model, "sigma")^2

  # Step 11) input everything into r2MLM

  list(data = as.data.frame(data),
       within_covs = within,
       between_covs = between,
       random_covs = random,
       gamma_w = gammaw,
       gamma_b = gammab,
       Tau = tau,
       sigma2 = sigma2,
       has_intercept = has_intercept,
       clustermeancentered = centeredwithincluster)
}


# Step 2 computation ------------------------------------------------------


l <- r2mlm_init(model)

r2mlm_manual(data = l$data, within_covs = l$within_covs, between_covs = l$between_covs, random_covs = l$random_covs, gamma_w = l$gamma_w, gamma_b = l$gamma_b, Tau = l$Tau, sigma2 = l$sigma2, has_intercept = l$has_intercept, clustermeancentered = l$clustermeancentered)



# Gold standard -----------------------------------------------------------


r2mlm::r2mlm(model)
