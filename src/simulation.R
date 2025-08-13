library(quantmod)
# The following function uses helper functions located in GBM.R.
ticker_sim <- function(ticker, replications, train_dates, score_dates) {
  trn_stock_data <- getSymbols(ticker, src="yahoo", from=train_dates[1], to=train_dates[2], auto.assign=F)
  scr_stock_data <- getSymbols(ticker, src="yahoo", from=score_dates[1], to=score_dates[2], auto.assign=F)
  
  trn_closing_prices <- as.numeric(Cl(trn_stock_data))  # training data
  scr_closing_prices <- as.numeric(Cl(scr_stock_data))  # scoring (actual data)
  recorded_trading_days <- length(scr_closing_prices)
  
  today <- Sys.Date()
  predict_date <- as.Date(score_dates[2])
  
  if (today < predict_date) {  # if the simulation goes into the future
    sim_date_range <- as.double(predict_date - today)
    trading_days <- recorded_trading_days + floor(0.69 * sim_date_range)  # estimate the amount of future trading days
  } else {
    trading_days <- recorded_trading_days
  }
  
  # plot of the price path (closing price)
  plot(1:recorded_trading_days, scr_closing_prices, type="l", col="red",
       xlab="Trading Day", ylab="Closing Price", main=ticker)
  
  mean_price <- mean(trn_closing_prices)
  sd <- sd(trn_closing_prices)
  vol <- sd / mean_price  # volatility estimate
  
  returns <- c()  # percent daily returns
  for (i in 1:(length(trn_closing_prices) - 1)) {
    R_i <- (trn_closing_prices[i + 1] / trn_closing_prices[i]) - 1
    returns <- c(returns, R_i)
  }
  mu <- (1 / (length(trn_closing_prices) - 1)) * sum(returns)  # Arithmetic Expected Return estimate
  
  t <- trading_days / 252  # convert time units to year(s) (generally 252 trading days in a year)
  init_price = scr_closing_prices[1]
  
  # simulation and plot
  stock_sim(trading_days, t, mu, vol, init_price, replications, ticker)
  lines((1:recorded_trading_days) / trading_days,
        scr_closing_prices, type="l", col="red")
}

trn_date <- c("2020-1-1", "2024-12-31")  # choose a date range to compute estimates
scr_date <- c("2025-01-01", "2025-12-31")  # choose a date range to simulate

popular_tickers <- c("AAPL", "MSFT", "NVDA", "AMZN",
                     "GOOG", "META", "TSLA", "F", "WMT")

for (i in 1:length(popular_tickers)) {  # loop through popular companies
  ticker_sim(popular_tickers[i], 50, trn_date, scr_date)
}

ticker_sim("META", 50, trn_date, scr_date)



