---
title: "power notes"
format: html
---


Simplest intuition on power:

```{r}
power <- function(b, alpha = 0.05, critical = qnorm(1-alpha/2))  

  1 - pnorm(critical, mean = abs(b)) + pnorm(-critical, mean = abs(b))

```

This is essentially what is done by `pwrss::power.z.test` -- and it produces nice graphs!

See:

```{r}
pwrss::power.z.test(ncp = 1.96, alpha = 0.05, 
             alternative = "not equal", plot = TRUE)
```

# Experimental design

Say $N$ subject are divided into two groups and potential outcomes have standard deviation $\sigma$ in treatment and control. Then the conservative variance of  the treatment effect is (approx / conservatively):

$$Var(\tau)=\frac{\sigma^2}{N/2} + \frac{\sigma^2}{N/2} = 4\frac{\sigma^2}{N}$$

and so the standard error is:

$$\sigma_\tau=\frac{2\sigma}{\sqrt{N}}$$

```{r}

se <- function(sd, N) (N/(N-1))^.5*2*sd/(N^.5)


power_2 <- function(b, alpha = .05, sd = 1, N = 100, critical = qnorm(1-alpha/2), se = 2*sd/N^.5)  

  1 - pnorm(critical, mean = abs(b)/se(sd, N)) + pnorm(-critical, mean = abs(b)/se(sd, N))

power_2(0)
```

This is done e.g. like this



```{r}
pwrss::pwrss.t.2means(mu1 = .2, mu2 = .1, sd1 = 1, sd2 = 1, 
               n2 = 50, alpha = 0.05,
               alternative = "not equal")

power_2(.10, N = 100)


x <- sapply(seq(0, 1, .05), function(j)
c(pwrss::pwrss.t.2means(mu1 = 0, mu2 = j, sd1 = 1, sd2 = 1, n2 = 50)$power, 
power_2(j, N = 100))) |> t() |> data.frame() |> plot()


x <- sapply(seq(0, 1, .05), function(j)
c(pwrss::pwrss.t.2means(mu1 = 0, mu2 = j, sd1 = 1, sd2 = 1, n2 = 50)$power, 
power_2(j, N = 100))) |> t() |> data.frame() |> plot()

```