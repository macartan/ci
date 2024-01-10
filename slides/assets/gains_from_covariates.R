library(DeclareDesign)
library(tidyverse)
N = 100
r_sq = 0.1
# b = 0.1

design <-
  declare_model(N = N,
                draw_multivariate(c(U, X) ~ MASS::mvrnorm(
                  n = N,
                  mu = c(0, 0),
                  Sigma = matrix(c(1, sqrt(r_sq), sqrt(r_sq), 1), 2, 2)
                )), 
                potential_outcomes(Y ~ 0.1*Z + U)) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_assignment(Z = complete_ra(N)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y~Z, covariates = ~X, model = lm_lin, inquiry = "ATE")

run_design(design)

designs <- redesign(design, N = seq(100, 1000, by = 100),
                    r_sq = seq(0.0, 0.9, by = 0.2))
diagnosis <-
  diagnose_designs(designs)


gg_df <- 
diagnosis %>% 
  tidy %>% 
  filter(diagnosand %in% c("sd_estimate", "power"),
         estimator == "Lin") %>% 
  mutate(diagnosand_label = 
           case_when(diagnosand == "sd_estimate" ~ "True standard error",
                     diagnosand == "power" ~ "Statistical power"))

label_df <-
  gg_df %>% 
  filter(N == 1000) %>% 
  mutate(label = paste0("R^2 = ", DeclareDesign:::format_num(r_sq, 1)))



g <- 
ggplot(gg_df, aes(N, estimate, group = r_sq, color = r_sq, fill = r_sq)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, color = NULL), alpha = 0.2) +
  geom_text(data = label_df, aes(label = label), nudge_x = 20, hjust = 0) +
scale_x_continuous(limits = c(0, 1200), breaks = seq(0, 1000, length.out = 6)) +
  theme_bw() +
  theme(strip.background = element_blank(),
        panel.grid.minor = element_blank()) +
  facet_wrap(~diagnosand, scales = "free_y") +
  labs(y = "Diagnosand estimate",
       x = "Sample size")
g

ggsave("gains_from_covariates.png", g, width = 7,height = 3.5)
