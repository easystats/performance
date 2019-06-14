library(glmmTMB)
library(sjmisc)

data(Salamanders)
data(efc)

# should be poisson (ZI) or neg bin (ZI)
m1 <- glm(count ~ spp + mined, family = poisson, data = Salamanders)
check_distribution(m1)

m2 <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
check_distribution(m2)

m3 <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
check_distribution(m3)

# response is clearly normal distributed
m4 <- lm(Sepal.Width ~ Petal.Width, data = iris)
check_distribution(m4)

# should be poisson-alike
m5 <- lm(tot_sc_e ~ neg_c_7 + barthtot + c161sex, data = efc)
check_distribution(m5)

# should be poisson-alike
m6 <- glm(tot_sc_e ~ neg_c_7 + barthtot + c161sex, data = efc, family = poisson())
check_distribution(m6)
