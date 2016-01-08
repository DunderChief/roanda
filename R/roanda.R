library(RCurl)
library(jsonlite)
library(xts)

# Set SSL certs globally
options(RCurlOptions=list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

##################
##  Get Equity  ##
##################
getEquity <- function(acct, auth_id, acct_type) 
{
  ##  Athorization
  auth <- c(Authorization=paste('Bearer', auth_id))
  
  url <- paste0('https://api-', acct_type, '.oanda.com/v1/accounts/', acct)
  equity <- fromJSON(getURL(url, httpheader=auth))$balance
  return(equity)
}


#################
##  Get Price  ## 
#################
getPrice <- function(instrument='EUR_USD', auth_id, acct_type)
{
  ##  Athorization
  auth <- c(Authorization=paste('Bearer', auth_id))
  ##  Get Current price:
  url <- paste0('https://api-', acct_type, '.oanda.com/v1/prices?instruments=', 
                instrument)
  current_price <- fromJSON(getURL(url, httpheader=auth))
  return(current_price$prices)
}


#####################
##  Get pip value  ##
#####################
getPipValue <- function(instrument='EUR_USD', acct, auth_id, acct_type)
{
  auth <- c(Authorization=paste('Bearer', auth_id))
  url <- paste0('https://api-', acct_type, '.oanda.com/v1/instruments?accountId=',
                acct, '&instruments=', instrument)
  pipval <- fromJSON(getURL(url, httpheader=auth))
  return(as.numeric(pipval$instruments$pip))
}


##########################
##  Get Open Positions  ##
##########################
getPositions <- function(instrument, acct, auth_id, acct_type)
{
  auth <- c(Authorization=paste('Bearer', auth_id))
  url <- paste0('https://api-', acct_type, '.oanda.com/v1/accounts/',
                acct, '/positions?instrument=', instrument)
  orders <- fromJSON(getURL(url, httpheader=auth))$positions
  return(orders)
}

#######################
##  Get Open Trades  ##
#######################
getOpenTrades <- function(instrument, acct, auth_id, acct_type)
{
  auth <- c(Authorization=paste('Bearer', auth_id))
  url <- paste0('https://api-', acct_type, '.oanda.com/v1/accounts/',
                acct, '/trades?instrument=', instrument)
  trades <- fromJSON(getURL(url, httpheader=auth))$trades
  # If no trades open, exit
  if(length(trades)==0) return(trades)
  # convert to xts
  trades$side[trades$side=='buy'] <- 1
  trades$side[trades$side=='sell'] <- -1
  trades$side <- as.integer(trades$side)
  time <- as.POSIXct(trades$time, format='%Y-%m-%dT%H:%M:%S')
  trades <- subset(trades, select= -c(instrument, time))
  trades <- xts(trades, order.by=time)
  return(trades)
}


####################
##  Close Trades  ##
####################
closeAllTrades <- function(instrument, acct, auth_id, acct_type)
{
  auth <- c(Authorization=paste('Bearer', auth_id))
  url <- paste0('https://api-', acct_type, '.oanda.com/v1/accounts/',
                acct, '/positions/', instrument) 
  closed <- fromJSON(getURL(url, httpheader=auth, customrequest='DELETE'))
  return(closed)
}

closeTrade <- function(orderID, acct, auth_id, acct_type)
{
  auth <- c(Authorization=paste('Bearer', auth_id))
  url <- paste0('https://api-', acct_type, '.oanda.com/v1/accounts/',
                acct, '/trades/', orderID) 
  closed <- fromJSON(getURL(url, httpheader=auth, customrequest='DELETE'))
  return(closed)
}
  
#########################
##  Get Past N Orders  ##
#########################
getPastOrders <- function(instrument='EUR_USD',
                          count=1, 
                          acct, 
                          auth_id, 
                          acct_type
)
{
  auth <- c(Authorization=paste('Bearer', auth_id))
  options <- paste0('instrument=', instrument,
                    '&count=', count)
  url <- paste0('https://api-', acct_type, '.oanda.com/v1/accounts/',
                acct, '/transactions?', options) 
  orders <- fromJSON(getURL(url, httpheader=auth))$transactions
  return(orders)
}
##########################
##  Get Past N Candles  ##
##########################
pastCandles <- function(instrument='EUR_USD',
                        granularity, # Time frame S5, M1, H4, D, W, M, etc.
                        count,  ##  Number of candles -- max 5000
                        candleFormat='bidask',
                        isAsk=TRUE, ##  Do you want the bid or ask prices
                        auth_id,
                        acct_type
)
{
  require(xts)
  ##  Athorization
  auth <- c(Authorization=paste('Bearer', auth_id))
  
  options <- paste0('instrument=', instrument,
                    '&granularity=', granularity,
                    '&count=', count,
                    '&candleFormat=', candleFormat
  )
  url <- paste0('https://api-', acct_type, '.oanda.com/v1/candles?', options)
  
  hist <- fromJSON(getURL(url, httpheader=auth))$candles
  hist <- hist[hist$complete==TRUE, ]
  if(isAsk){
    ohlc <- data.frame(Open=hist$openAsk, High=hist$highAsk, 
                       Low=hist$lowAsk, Close=hist$closeAsk, Volume=hist$volume)
  } else {
    ohlc <-   data.frame(Open=hist$openBid, High=hist$highBid, 
                         Low=hist$lowBid, Close=hist$closeBid, Volume=hist$volume)
  }
  
  ##  Convert to XTS object
  dates <- as.POSIXct(hist$time, format='%Y-%m-%dT%H:%M:%S', tz='UTC')
  ohlc_xts <- xts(ohlc, order.by=dates)
  return(ohlc_xts)
}

###########################
##  Get Historical Data  ##
###########################
getCandlesByTime <- function(instrument='EUR_USD',
                             granularity='M1', # Time frame S5, M1, H4, D, W, M, etc.
                             start, ## '2014-06-19T15%3A47%3A40Z'
                             end,
                             includeFirst=TRUE, ## Include the first candle
                             candleFormat='bidask',
                             isAsk=TRUE, ##  Do you want the bid or ask prices
                             auth_id,
                             acct_type
)
{
  
  ##  Athorization
  auth <- c(Authorization=paste('Bearer', auth_id))
  
  ##  Convert includeFirst to string of 'true' or 'false'
  includeFirst <- ifelse(includeFirst, 'true', 'false')
  
  options <- paste0('instrument=', instrument,
                    '&granularity=', granularity,
                    '&start=', start,
                    '&end=', end,
                    '&includeFirst=', includeFirst,
                    '&candleFormat=', candleFormat
  )
  url <- paste0('https://api-', acct_type, '.oanda.com/v1/candles?', options)
  for(iter in 1:20) {
    hist <- getURL(url, httpheader=auth)
    if(hist=='') next # We bad, start next loop
    hist <- fromJSON(hist)$candles
    hist <- hist[hist$complete==TRUE, ]
    if(NROW(hist)!=0) break # We good, break loop
    Sys.sleep(.5)
  }
  if(NROW(hist==0)) stop('Zero candles return 20 times in a row...')
  if(isAsk){
    ohlc <- data.frame(Open=hist$openAsk, High=hist$highAsk, 
                       Low=hist$lowAsk, Close=hist$closeAsk, Volume=hist$volume)
  } else {
    ohlc <-   data.frame(Open=hist$openBid, High=hist$highBid, 
                         Low=hist$lowBid, Close=hist$closeBid, Volume=hist$volume)
  }
  
  ##  Convert to XTS object
  dates <- as.POSIXct(hist$time, format='%Y-%m-%dT%H:%M:%S', tz='UTC')
  ohlc_xts <- xts(ohlc, order.by=dates)
  return(ohlc_xts)
}
##########################
##  Place Market Order  ##
##########################
marketOrder <- function(instrument='EUR_USD',
                        units=10000, #number of units to buy
                        side, #buy, sell
                        slippage=2, ## (pips)
                        SL=10, ## pips
                        TP=10, ## pips
                        #trailingStop, ## Not supported yet
                        acct,
                        auth_id,
                        acct_type
)
{
  ##  Set order type to 'market'
  type <- 'market'
  
  ##  Get market price and pip value
  curr_price <- getPrice('EUR_USD', auth_id=auth_id, acct_type=acct_type)
  pip_value <- getPipValue('EUR_USD', auth_id=auth_id, acct=acct, acct_type=acct_type)
  
  ##  Convert TP and SL to price values
  if(side=='buy'){
    stopLoss <- curr_price$ask - (SL * pip_value)
    takeProfit <- curr_price$ask + (TP * pip_value)
    slipUp <- curr_price$ask + (slippage * pip_value)
    slipDown <- curr_price$ask - (slippage * pip_value)
  } else if(side=='sell') {
    stopLoss <- curr_price$bid + (SL * pip_value)
    takeProfit <- curr_price$bid - (TP * pip_value)
    slipUp <- curr_price$bid + (slippage * pip_value)
    slipDown <- curr_price$bid - (slippage * pip_value)
  } else {
    stop('Need to specify whether to Buy or Sell')
  }
  
  if(!stopLoss > 0) stop('No stop loss value!')
  
  ##  Athorization
  auth <- c(Authorization=paste('Bearer', auth_id))
  
  ##  Past together the proper URL
  url <- paste0('https://api-', acct_type, '.oanda.com/v1/accounts/',
                acct, '/orders')
  
  ##  Place the order
  order <- fromJSON(
    postForm(url,
             style='POST',
             .params=c(instrument=instrument,
                       units=units,
                       side=side,
                       type=type,
                       lowerBound=slipDown,
                       upperBound=slipUp,
                       takeProfit=takeProfit,
                       stopLoss=stopLoss), 
             .opts=list(httpheader=auth))
  )
  
  return(order)
}