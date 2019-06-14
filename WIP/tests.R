library(glmmTMB)
library(sjmisc)
library(lme4)
library(see)
library(performance)

dat <- data.frame(
  outcome = rbinom(n = 100, size = 1, prob = 0.35),
  var_binom = as.factor(rbinom(n = 100, size = 1, prob = 0.2)),
  var_cont = rnorm(n = 100, mean = 10, sd = 7),
  group = sample(letters[1:4], size = 100, replace = TRUE)
)

dat$var_cont <- sjmisc::std(dat$var_cont)

data(Salamanders)
data(sleepstudy)
data(efc)

# should be poisson (ZI) or neg bin (ZI)
m1 <- glm(count ~ spp + mined, family = poisson, data = Salamanders)
check_distribution(m1)
plot(check_distribution(m1))

m2 <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
check_distribution(m2)
plot(check_distribution(m2))

m3 <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
check_distribution(m3)
plot(check_distribution(m3))

# response is clearly normal distributed
m4 <- lm(Sepal.Width ~ Petal.Width, data = iris)
check_distribution(m4)
plot(check_distribution(m4))

# should be poisson-alike
m5 <- lm(tot_sc_e ~ neg_c_7 + barthtot + c161sex, data = efc)
check_distribution(m5)
plot(check_distribution(m5))

# should be poisson-alike
m6 <- glm(tot_sc_e ~ neg_c_7 + barthtot + c161sex, data = efc, family = poisson())
check_distribution(m6)
plot(check_distribution(m6))

# normal
m7 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
check_distribution(m7)
plot(check_distribution(m7))

# bernoulli
m8 <- glmer(outcome ~ var_binom + var_cont + (1 | group), data = dat, family = binomial())
check_distribution(m8)
plot(check_distribution(m8))
