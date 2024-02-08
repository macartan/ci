trend = 0

library(DeclareDesign)
library(tidyverse)
design <- 
  declare_model(
    unit = add_level(N = 2, ui = rnorm(N), I = 1:N),
    time = add_level(N = 6, ut = rnorm(N), T = 1:N, nest = FALSE),
    obs = cross_levels(by = join_using(unit, time))) +
  declare_model(
    potential_outcomes(Y ~ trend*T + (1+Z)*(I == 2))) +
  declare_assignment(Z = 1*((I == 1) * (T>3) + (I == 2) * (T>5))) +
  declare_measurement(Y = reveal_outcomes(Y~Z), 
                      I_c = I - mean(I)) +
  declare_inquiry(mean(Y_Z_1 - Y_Z_0)) +
  declare_estimator(Y ~ Z, label = "naive") + 
  declare_estimator(Y ~ Z + I, label = "FE1") + 
  declare_estimator(Y ~ Z + as.factor(T), label = "FE2") + 
  declare_estimator(Y ~ Z + I + as.factor(T), label = "FE3") + 
  declare_estimator(Y ~ Z*I_c + as.factor(T), label = "Sat")  

diagnose_design(design, sims = 3)



data <- draw_data(design)

Assume first there are *no* time trends

diagnose_design(design, sims = 3)

The estimand is .5 -- this comes from weighting the effect for unit 1 (0) and the effect for unit 2 (1) equally

The naive estimate is wildly off because it does not take into account that units with different treatment shares have differnt average levels in outcomes

The estimate when we control for unit is .36: this comes from weighting the unit=stratum level effects according to the variance of assignment to each stratum:

  ```{r}
data |> group_by(unit) |> summarize(var = mean(Z)*(1-mean(Z))) |>
  mutate(weight = var/sum(var))
```

The estimate when we control for time  is -1: this comes from weighting the time-stratum level effects according to the variance of assignment to each stratum: in fact it makes use of period 4 and 5 only and weights these equally we end up just getting the difference in the avera outcome for unit 1 in treatment (0) and group 2 in control (1)
  
```{r}
data |> group_by(time) |> summarize(var = mean(Z)*(1-mean(Z))) |>
  mutate(weight = var/sum(var))
```

The estimate when we control for time  *and* unit is 0.25 this is harder to interpret:
  
We can figure out what it is form the Goodman=Bacon decomposition in 
https://www.nber.org/system/files/working_papers/w25018/w25018.pdf

In this case:
  
$$\hat\beta^{DD} = s_{12}\left(\mu_{12}\hat\beta^{2 \times 2, 1}_{12} +  (1=\mu_{12})\hat\beta^{2 \times 2, 2}_{12}\right)$$
  
where   $\mu_{12} = \frac34$ and $s_{kl} = $
  
$$3/4\hat\beta^{2 \times 2, 1}_{12} +   1/4 \hat\beta^{2 \times 2, 2}_{12}$$

$\hat\beta^{2 \times 2, 1}_{12}$ is $(\overline{y}_1^{MID(1,2)} - \overline{y}_1^{PRE(1)}) - (\overline{y}_2^{MID(1,2)} - \overline{y}_2^{PRE(1)})$ 

which in the simple example without time trends is $(0 - 0) - (1=1) = 0$ 

meanwhile

$\hat\beta^{2 \times 2, 2}_{12}$ is $(\overline{y}_2^{POST(2)} - \overline{y}_2^{MID(1,2)}) - (\overline{y}_1^{POST(2)} - \overline{y}_1^{MID(1,2)})$ 

which is $(2 = 1) - (0 - 0) = 1$

In all $(3/4) \times 0  + (1/4) \times 1 = 0.25$
  
thus a 3/4 weight on the early to mid diff in diff (0) and a 1/4 weight on the mid to late diff in diff (1)



The interpretation 