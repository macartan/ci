rm(list = ls())

library(DeclareDesign)
library(tidyverse)

N = 100
design <-
  declare_model(
    N = N,
    U = rnorm(N),
    X = runif(N, min = 0, max = 1),
    potential_outcomes(Y ~ 0.2 * Z + 0.1 * X + 0.1*X*Z + U)
  ) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_assignment(Z = complete_ra(N = N)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z, model = lm_robust, inquiry = "ATE")

# one draw of data
dat <- draw_data(design)

# one run of the design
run_design(design)

# run 500 simulations and summarize
diagnosis <- diagnose_design(design)

# redesign over levels of N
designs <- redesign(design, N = seq(100, 1000, by = 200))
diagnosis <- diagnose_designs(designs)

# make a picture
gg_df <-
  diagnosis %>%
  tidy() %>%
  filter(diagnosand == "power")

ggplot(gg_df, aes(N, estimate)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  theme_bw()
