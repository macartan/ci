rm(list = ls())

library(DeclareDesign)
library(tidyverse)

N = 100
MI <-
  declare_model(
    N = N,
    U = rnorm(N, sd = 0.1),
    X = runif(N, min = 0, max = 1),
    potential_outcomes(Y ~ 0.2 * Z + 1 * X + 0.1*X*Z + U)
  ) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) 


DA_1 <-
  declare_assignment(Z = complete_ra(N = N)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z, model = lm_robust, inquiry = "ATE")

DA_2 <-
  declare_measurement(blocks = as.numeric(X > 0.5)) +
  declare_assignment(Z = block_ra(blocks = blocks)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z, model = lm_robust, inquiry = "ATE")





diagnosis <- 
diagnose_designs(complete = MI + DA_1,
                 blocked = MI + DA_2)



dat <- draw_data(MI + DA_2)

glance(lm_robust(Y ~ blocks, data = dat))


