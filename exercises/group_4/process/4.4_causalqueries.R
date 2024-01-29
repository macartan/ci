library(CausalQueries)
library(fabricatr)
library(kableExtra)

model <- make_model("X -> M -> Y <-> X")

plot(model)

data_1 <- fabricate(N = 1000,
                  X = rep(0:1, N/2),
                  M = rbinom(N, 1, .9*X),
                  Y = rbinom(N, 1,  .9*M))

data_2 <- fabricate(N = 1000,
                        X = rep(0:1, N/2),
                        M = rbinom(N, 1,  1*X),
                        Y = rbinom(N, 1,  1*M))

model_1 <- model |> update_model(data_1, refresh = 0)
model_2 <- model |> update_model(data_2, refresh = 0)

query_model(
  model = model_1, 
  using = c("priors", "posteriors"),
  query = "Y[X=1] - Y[X=0]")

query_model(
  model = model_2, 
  using = c("priors", "posteriors"),
  query = "Y[X=1] - Y[X=0]")
