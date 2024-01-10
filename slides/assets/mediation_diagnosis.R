rm(list = ls())
library(DeclareDesign)
library(tidyverse)
library(mvtnorm)

N = 100
a = 0.5
b = 0.5
d = 0.5
rho = 0

design <-
  declare_model(
    N,
    draw_multivariate(c(E_1, E_3) ~
                        MASS::mvrnorm(
                          n = N,
                          mu = c(0, 0),
                          Sigma = matrix(c(1, rho, rho, 1), ncol = 2)
                        )),
    potential_outcomes(M ~ rbinom(
      n = N,
      size = 1,
      prob = pnorm(a * Z + E_1)
    )),
    potential_outcomes(Y ~ d * Z + b * M + E_3,
                       conditions = list(Z = c(0, 1),
                                         M = c(0, 1)))
  ) +
  declare_inquiry(
    ATE_M = mean(M_Z_1 - M_Z_0),
    ATE_Y =
      mean(c(Y_Z_1_M_1[M_Z_1 == 1], Y_Z_1_M_0[M_Z_1 == 0])) -
      mean(c(Y_Z_0_M_1[M_Z_0 == 1], Y_Z_0_M_0[M_Z_0 == 0])),
    average_direct_effect_at_M_Z_0 =
      mean(c(Y_Z_1_M_1[M_Z_0 == 1], Y_Z_1_M_0[M_Z_0 == 0])) -
      mean(c(Y_Z_0_M_1[M_Z_0 == 1], Y_Z_0_M_0[M_Z_0 == 0]))
  ) +
  declare_assignment(Z = complete_ra(N)) +
  declare_measurement(M = reveal_outcomes(M ~ Z),
                      Y = reveal_outcomes(Y ~ Z + M)) +
  declare_estimator(M ~ Z,
                    model = lm_robust,
                    inquiry = "ATE_M",
                    label = "ATE_M") +
  declare_estimator(Y ~ Z,
                    model = lm_robust,
                    inquiry = "ATE_Y",
                    label = "ATE_Y") +
  declare_estimator(
    Y ~ Z + M,
    model = lm_robust,
    term = "Z",
    inquiry = "average_direct_effect_at_M_Z_0",
    label = "direct_effect"
  )


designs <-redesign(design, rho = seq(-0.9, 0.9, length.out = 9))

diagnosis <- diagnose_designs(designs)



gg_df <- 
  diagnosis %>%
  tidy() %>% 
  filter(diagnosand == "bias")

g <- 
ggplot(gg_df, aes(rho, estimate)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  theme_bw() +
  theme(strip.background = element_blank()) +
  facet_wrap(~ inquiry) +
  labs(y = "Diagnosand: Bias", 
       x = "Correlation between e1 and e3")

ggsave("mediation_bias.png", g, width = 6.5, height = 3.5)
