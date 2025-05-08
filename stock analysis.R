rm(list=ls())
install.packages("quantmod")
library(quantmod)
library(ggplot2)

MA_analysis <- function(ticker_symbol, start_date, end_date){
  ticker_data = getSymbols(ticker_symbol, src="yahoo", from=start_date, to=end_date, auto.assign=FALSE)
  head(ticker_data)
  tail(ticker_data)
  summary(ticker_data)
  str(ticker_data)

  # create time series graph
  ggplot(ticker_data, aes(x=index(ticker_data), y=ticker_data[,6])) + 
    geom_line(color="darkgreen") + 
    ggtitle("Prices Series") + 
    xlab("Date") + ylab("Price") + 
    theme(plot.title=element_text(hjust=0.5)) + 
    scale_x_date(date_labels="%b %Y", date_breaks="1 year")

  # calculate moving averages
  # MA^q_t = (1/q)summ(i=0 to q-1) of (x_(t-1))
  # bigger day window corresponds to smaller MA responsiveness to price var
  # smaller day window corresponds to bigger MA responsiveness to price var

  ticker_mm = subset(ticker_data, index(ticker_data) >= "2016-01-01")

  # 10 day window
  ticker_mm10 = rollmean(ticker_mm[,6], 10, fill=list(NA, NULL, NA), align="right")
  ticker_mm30 = rollmean(ticker_mm[,6], 30, fill=list(NA, NULL, NA), align="right")

  ticker_mm$mm10 = coredata(ticker_mm10)
  ticker_mm$mm30 = coredata(ticker_mm30)

  # when short term MA cross long term upwards --> BUY
  # when short term MA cross long term downwards --> SELL

  ggplot(ticker_mm, aes(x=index(ticker_mm))) +
    geom_line(aes(y=ticker_mm[,6], color="ticker")) +
    ggtitle("ticker Prices Series") +
    geom_line(aes(y=ticker_mm$mm10, color="MM10")) +
    geom_line(aes(y=ticker_mm$mm30, color="MM30")) +
    xlab("Date") + ylab("Price") +
    theme(plot.title=element_text(hjust=0.5), panel.border=element_blank()) +
    scale_x_date(date_labels="%b %Y", date_breaks="6 months") +
    scale_color_manual("Series", values=c("ticker"="red4", "MM10"="cyan4", "MM30"="grey40"))
    
  # returns
  # calculate log-return of stock
  ticker_ret = diff(log(ticker[,6])) # diff calculates all values in any vector
  ticker_ret = ticker_ret[-1,]

  # quantmod functions
  Op(ticker) # opening values
  Cl(ticker) # closing values
  Ad(ticker) # adjusted values
  Hi(ticker) # max price
  Lo(ticker) # min price
  Vo(ticker) # volume of transactions

  dailyReturn(ticker)
  weeklyReturn(ticker)
  monthlyReturn(ticker)
  quarterlyReturn(ticker)
  yearlyReturn(ticker)

  summary(ticker_ret)
  sd(ticker_ret)

  # returns over time
  ggplot(ticker_ret, aes(x=index(ticker_ret), y=ticker_ret)) +
    geom_line(color="lightblue4") +
    ggtitle("ticker Returns Series") +
    xlab("Date") + ylab("Return") +
    theme(plot.title=element_text(hjust=0.5)) +
    scale_x_date(date_labels="%b %Y", date_breaks="1 year")

  # look at returns in 2020
  ticker_ret20 = subset(ticker_ret, index(ticker_ret) > "2020-01-01" & index(ticker_ret) < "2021-01-01")
  ggplot(ticker_ret20, aes(x=index(ticker_ret20), y=ticker_ret20)) +
    geom_line(color="lightblue4") +
    ggtitle("ticker Return Series in 2020") +
    xlab("Date") + ylab("Return") +
    theme(plot.title=element_text(hjust=0.5)) +
    scale_x_date(date_labels="%b %Y", date_breaks="1 month")

  summary(ticker_ret20)
  sd(ticker_ret20)

}




MA_analysis("APPL", "2010-01-01", "2023-08-31")