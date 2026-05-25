library(lme4)
library(r2mlm)


source("r2mlm_init.R")


# Example -----------------------------------------------------------------

model <- lmer(satisfaction ~ salary_c + (1 + salary_c | schoolID), data = r2mlm::teachsat)

l <- r2mlm_init(model) # Get list of ingredients
r2mlm::r2mlm_manual(
  data = l$data,
  within_covs = l$within_covs,
  between_covs = l$between_covs,
  random_covs = l$random_covs,
  gamma_w = l$gamma_w,
  gamma_b = l$gamma_b,
  Tau = l$Tau,
  sigma2 = l$sigma2,
  has_intercept = l$has_intercept,
  clustermeancentered = l$clustermeancentered
)

# Testing -----------------------------------------------------------------

test_it <- function(model) {
  l <- r2mlm_init(model)
  rez <- r2mlm::r2mlm_manual(
    data = l$data,
    within_covs = l$within_covs,
    between_covs = l$between_covs,
    random_covs = l$random_covs,
    gamma_w = l$gamma_w,
    gamma_b = l$gamma_b,
    Tau = l$Tau,
    sigma2 = l$sigma2,
    has_intercept = l$has_intercept,
    clustermeancentered = l$clustermeancentered
  )

  truth <- r2mlm::r2mlm(model)
  if (all(rez$Decompositions == truth$Decompositions) & all(rez$R2s == truth$R2s)) {
    print("Identical.")
  } else {
    print("Error!")
  }
}


model <- lmer(satisfaction ~ salary_c + (1 + salary_c | schoolID), data = r2mlm::teachsat)
test_it(model)
model <- lmer(
  satisfaction ~ salary_c * control_c + (1 + salary_c | schoolID),
  data = r2mlm::teachsat
)
test_it(model)
# model <- lmer(satisfaction ~ 0 + salary_c + (1 + salary_c | schoolID), data = r2mlm::teachsat)
# test_it(model)

# model <- nlme::lme(satisfaction ~ salary_c, random = ~ 1 + salary_c | schoolID, data = teachsat)
# test_it(model)
