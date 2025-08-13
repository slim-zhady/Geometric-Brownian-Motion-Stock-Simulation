# n = days, t = time in years, t_steps = (steps), drift = mu, volatility = sigma,
# current_price = initial stock price.
geometric_bm <- function(n, t, t_steps, drift, volatility, current_price) {
  std_bm <- c(0, cumsum(rnorm(n, 0, sqrt(t/n))))  # Standard Brownian Motion
  
  bm_with_drift <- (drift * t_steps) + (volatility * std_bm)  # Brownian Motion with drift
  
  geom_bm <- current_price * exp(bm_with_drift)  # Geometric Brownian Motion
  
  return(geom_bm)
}

# Expected value of the lognormal distribution (page 353).
logn_mean <- function(init, mu, sigma, x) {
  return(init * exp(x * ((mu + sigma^2) / 2)))
}

# SD of the lognormal distribution (page 353).
logn_sd <- function(init, mu, sigma, x) {
  return(sqrt(init^2 * exp(x * (mu + sigma^2)) * (exp(x * sigma^2) - 1)))
}

# n = days, t = time in years, drift = mu, volatility = sigma,
# current_price = initial stock price, replications = no. of simulations.
stock_sim <- function(n, t, drift, volatility, current_price, replications, ticker) {
  t_steps <- seq(0, t, length=n+1)  # t values (steps)
  
  # mean plus-minus 1.96 standard deviations (used for plot bounds, "xlab")
  errors <- logn_mean(current_price, drift, volatility, t) + (c(-1,1) * (1.96 * logn_sd(current_price, drift, volatility, t)))

  for (i in 1:replications) {  # generate random samples
    gbm_sample_i <- geometric_bm(n, t, t_steps, drift, volatility, current_price)  # Geometric BM from sample i
    
    if (i == 1) {  # create the plot in the first iteration of the loop
      plot(t_steps, gbm_sample_i, type="l", col="gray", ylab="Price",
           xlab="Time (years)", ylim=c(errors[1] - (current_price / 3), errors[2] + ((2 * current_price) / 3)),
           main=paste("Geometric Brownian Motion\nSimulation", " (", ticker, ")", sep=""))
    }
    lines(t_steps, gbm_sample_i, type="l", col="gray")
  }
  abline(h=0)  # draw a line at price = $0
  legend("topleft", legend=c("Actual","Simulation", "Lognormal mean & 95% CI"),
         lty=c(1, 1, 2), col=c("red","gray","black"), cex=0.8)
  curve(current_price * exp(x * (drift + volatility^2) / 2), from=0, to=t, lty=2, add=T)  # Lognormal Mean
  curve(current_price * exp((drift * x) - (1.96 * sqrt(x) * volatility)), from=0, to=t, lty=2, add=T)  # 95% CI lower bound
  curve(current_price * exp((drift * x) + (1.96 * sqrt(x) * volatility)), from=0, to=t, lty=2, add=T)  # 95% CI upper bound
}
