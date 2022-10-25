
library(data.table)
library(ggplot2)
library(magrittr)

rm(list=ls())

## Monte Carlo 

rho <- 0.09    ## correlation factor of portfolio...assuming at 0.09 for trial

m       <- 1000000
alpha   <- 0.5
f_value <- -1.0

sim_dt <- data.table(
  iteration = seq(1, m),
  w1 = rep(alpha, m), 
  v1 = rep(f_value, m),
  w2 = sqrt(1+alpha^2),
  v2 = rnorm(m, mean=0, sd=1)
)  

sim_dt[, w_tot:= w1 + w2]
sim_dt[, t_value := (w1/w_tot)*v1 + (w2/w_tot)*v2]

sim_dt %>% ggplot(aes(x=t_value)) + geom_density()


## Defining variables
rho <- 0.09    ## correlation factor of portfolio...assuming at 0.09 for trial
X <- numeric(M)
threshold <- numeric(M)
iteration <- numeric(M)
set.seed(777)
Z <- rnorm(M, mean=0, sd=1)   ## generating common risk factor
Zvar <- rnorm(M, mean=0, sd=1)  ## generating N idiosyncratic risk factors

for (m in 1:M) {
  iteration[m] <- m
  X[m] <- sqrt(rho)*Z[m] + sqrt(1-rho)*Zvar[m]
  threshold[m] <- qnorm(0.08, mean=0, sd=1)  ## PD set at 8%
}
sim <- data.table(cbind(iteration,X,threshold))

library(dplyr)
sim <- mutate(sim, Default = (X < threshold))




## Monte Carlo 
M <- 10000  

## Defining variables
rho <- 0.09    ## correlation factor of portfolio...assuming at 0.09 for trial
X <- numeric(M)
threshold <- numeric(M)
iteration <- numeric(M)
set.seed(777)
Z <- rnorm(M, mean=0, sd=1)   ## generating common risk factor
Zvar <- rnorm(M, mean=0, sd=1)  ## generating N idiosyncratic risk factors

for (m in 1:M) {
  iteration[m] <- m
  X[m] <- sqrt(rho)*Z[m] + sqrt(1-rho)*Zvar[m]
  threshold[m] <- qnorm(0.08, mean=0, sd=1)  ## PD set at 8%
}
sim <- data.table(cbind(iteration,X,threshold))

library(dplyr)
sim <- mutate(sim, Default = (X < threshold))