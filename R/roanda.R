library(httr)
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
  for(i in 1:15) {
    http <- tryCatch(GET(url=url, add_headers(auth)), error=function(e) {
        Sys.sleep(1)
        next
      }
    )
    if(http$status_code==200) {
      out <- content(http, encoding='UTF-8')$balance
      return(out)
    } else{
      cat(warn_for_status(http), 'roanda::getEquity() | ')
      Sys.sleep(.5)
    }
  }
  print(http)
  stop("Couldn't get the proper response from Oanda server: roanda::getEquity()")
}

  ########################
 ##  Get Account Info  ##
########################
getAccount <- function(acct, auth_id, acct_type) 
{
  ##  Athorization
  auth <- c(Authorization=paste('Bearer', auth_id))
  
  url <- paste0('https://api-', acct_type, '.oanda.com/v1/accounts/', acct)
  for(i in 1:15) {
    http <- tryCatch(GET(url=url, add_headers(auth)), error=function(e) {         
        Sys.sleep(1)         
        next       
      }     
    )
    if(http$status_code==200) {
      out <- content(http, encoding='UTF-8')
      return(data.frame(out))
    } else{
      cat(warn_for_status(http), 'roanda::getAccount() | ')
      Sys.sleep(.5)
    }
  }
  print(http)
  stop("Couldn't get the proper response from Oanda server: roanda::getEquity()")
}
#################
##  Get Price  ## 
#################
getPrice <- function(instrument, auth_id, acct_type)
{
  ##  Athorization
  auth <- c(Authorization=paste('Bearer', auth_id))
  
  ##  Get Current price:
  url <- paste0('https://api-', acct_type, '.oanda.com/v1/prices?instruments=', 
                instrument)
  for(i in 1:15) {
    http <- tryCatch(GET(url=url, add_headers(auth)), error=function(e) {         
        Sys.sleep(1)         
        next       
      }     
    )
    if(http$status_code==200) {
      out <- fromJSON(content(http, encoding='UTF-8', type='text'))$prices
      return(out)
    } else{
      cat(warn_for_status(http), 'roanda::getPrice() | ')
      Sys.sleep(.5)
    }
  }
  print(http)
  stop("Couldn't get the proper response from Oanda server: roanda::getPrice()")
}


#####################
##  Get pip value  ##
#####################
getPipValue <- function(instrument, acct, auth_id, acct_type)
{
  auth <- c(Authorization=paste('Bearer', auth_id))
  url <- paste0('https://api-', acct_type, '.oanda.com/v1/instruments?accountId=',
                acct, '&instruments=', instrument)
  for(i in 1:15) {
    http <- tryCatch(GET(url=url, add_headers(auth)), error=function(e) {         
        Sys.sleep(1)         
        next       
      }     
    )
    if(http$status_code==200) {
      out <- fromJSON(content(http, encoding='UTF-8', type='text'))$instruments$pip
      return(as.numeric(out))
    } else{
      cat(warn_for_status(http), 'roanda::getPipValue() | ')
      Sys.sleep(.5)
    }
  }
  print(http)
  stop("Couldn't get the proper response from Oanda server: roanda::getPipValue()")
}


##########################
##  Get Open Positions  ##
##########################
getPositions <- function(instrument, acct, auth_id, acct_type)
{
  auth <- c(Authorization=paste('Bearer', auth_id))
  url <- paste0('https://api-', acct_type, '.oanda.com/v1/accounts/',
                acct, '/positions?instrument=', instrument)
  for(i in 1:15) {
    http <- tryCatch(GET(url=url, add_headers(auth)), error=function(e) {
        Sys.sleep(1)
        next
      }
    )
      
    if(http$status_code==200) {
      out <- fromJSON(content(http, encoding='UTF-8', type='text'))$positions
      return(out)
    } else{
      cat(warn_for_status(http), 'roanda::getPositions() | ')
      Sys.sleep(1)
    }
  }
  print(http)
  stop("Couldn't get the proper response from Oanda server: roanda::getPositions()")
}

#######################
##  Get Open Trades  ##
#######################
getOpenTrades <- function(instrument, acct, auth_id, acct_type)
{
  auth <- c(Authorization=paste('Bearer', auth_id))
  url <- paste0('https://api-', acct_type, '.oanda.com/v1/accounts/',
                acct, '/trades?instrument=', instrument)
  for(i in 1:15) {
    http <- tryCatch(GET(url=url, add_headers(auth)), error=function(e) {         
        Sys.sleep(1)         
        next       
      }     
    )
    if(http$status_code==200) {
      trades <- fromJSON(content(http, encoding='UTF-8', type='text'))$trades
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
    } else{
      cat(warn_for_status(http), 'roanda::getOpenTrades() | ')
      Sys.sleep(.5)
    }
  }
  print(http)
  stop("Couldn't get the proper response from Oanda server: roanda::getOpenTrades()")
}

####################
##  Close Trades  ##
####################
closeAllTrades <- function(instrument, acct, auth_id, acct_type)
{
  auth <- c(Authorization=paste('Bearer', auth_id))
  url <- paste0('https://api-', acct_type, '.oanda.com/v1/accounts/',
                acct, '/positions/', instrument)
  for(i in 1:15) {
    http <- tryCatch(DELETE(url=url, add_headers(auth)), error=function(e) {         
        Sys.sleep(1)         
        next       
      }     
    )
    if(http$status_code==200) {
      closed <- fromJSON(content(http, encoding='UTF-8', type='text'))
      return(closed)
    } else{
      cat(warn_for_status(http), 'roanda::closeAllTrades() | ')
      Sys.sleep(.5)
    }
  }
  print(http)
  stop("Couldn't get the proper response from Oanda server: roanda::closeAllTrades()")
}

##########################
##  Close Single Trade  ##
##########################
closeTrade <- function(orderID, acct, auth_id, acct_type)
{
  auth <- c(Authorization=paste('Bearer', auth_id))
  url <- paste0('https://api-', acct_type, '.oanda.com/v1/accounts/',
                acct, '/trades/', orderID)
  for(i in 1:15) {
    http <- tryCatch(DELETE(url=url, add_headers(auth)), error=function(e) {         
        Sys.sleep(1)         
        next       
      }     
    )
    if(http$status_code==200) {
      closed <- fromJSON(content(http, encoding='UTF-8', type='text'))
      return(closed)
    } else{
      cat(warn_for_status(http), 'roanda::closeTrade() | ')
      Sys.sleep(.5)
    }
  }
  print(http)
  stop("Couldn't get the proper response from Oanda server: roanda::closeTrade()")
}
  
#########################
##  Get Past N Orders  ##
#########################
getPastOrders <- function(instrument,
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
  for(i in 1:15) {
    http <- tryCatch(GET(url=url, add_headers(auth)), error=function(e) {         
        Sys.sleep(1)         
        next       
      }     
    )
    if(http$status_code==200) {
      out <- fromJSON(content(http, encoding='UTF-8', type='text'))$transactions
      # Need to deal with a strange column that is actually 2 columns
      tradeOpened <- out$tradeOpened
      out$tradeOpened_id <- tradeOpened$id
      out$tradeOpened_units <- tradeOpened$units
      out <- subset(out, select=-tradeOpened)
      return(out)
    } else if(http$status_code==53 | http$status_code==429){ # This request has a 60 sec cooldown
      Sys.sleep(61)
    } else {
      cat(warn_for_status(http), 'roanda::getPastOrders() | ')
      Sys.sleep(.5)
    }
  }
  print(http)
  stop("Couldn't get the proper response from Oanda server: roanda::getPositions()")
}
##########################
##  Get Past N Candles  ##
##########################
pastCandles <- function(instrument,
                        granularity, # Time frame S5, M1, H4, D, W, M, etc.
                        count,  ##  Number of candles -- max 5000
                        candleFormat='bidask',
                        isAsk=TRUE, ##  Do you want the bid or ask prices
                        auth_id,
                        acct_type,
                        only_complete=TRUE
)
{
  ##  Athorization
  auth <- c(Authorization=paste('Bearer', auth_id))
  
  options <- paste0('instrument=', instrument,
                    '&granularity=', granularity,
                    '&count=', count,
                    '&candleFormat=', candleFormat
  )
  url <- paste0('https://api-', acct_type, '.oanda.com/v1/candles?', options)
  for(i in 1:15) {
    http <- tryCatch(GET(url=url, add_headers(auth)), error=function(e) {         
        Sys.sleep(1)         
        next       
      }     
    )
    if(http$status_code==200) {
      hist <- fromJSON(content(http, encoding='UTF-8', type='text'))$candles
      if(only_complete){
        hist <- hist[hist$complete==TRUE, ]
      }
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

    } else{
      cat(warn_for_status(http), 'roanda::pastCandles() | ')
      Sys.sleep(.5)
    }
  }
  print(http)
  stop("Couldn't get the proper response from Oanda server: roanda::pastCandles()")
}

###########################
##  Get Historical Data  ##
###########################
getCandlesByTime <- function(instrument,
                             granularity='M1', # Time frame S5, M1, H4, D, W, M, etc.
                             start, ## '2014-06-19T15%3A47%3A40Z'
                             end,
                             includeFirst=TRUE, ## Include the first candle
                             candleFormat='bidask',
                             isAsk=TRUE, ##  Do you want the bid or ask prices
                             auth_id,
                             acct_type,
                             only_complete=TRUE
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
  
  for(i in 1:15) {
    http <- tryCatch(GET(url=url, add_headers(auth)), error=function(e) {         
        Sys.sleep(1)         
        next       
      }     
    )
    if(http$status_code==200) {
      hist <- fromJSON(content(http, encoding='UTF-8', type='text'))$candles
      if(only_complete){
        hist <- hist[hist$complete==TRUE, ]
      }
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
    } else if(http$status_code==204) { # No content
      return(NULL)
    } else{
      cat(warn_for_status(http), 'roanda::getCandlesByTime() | ')
      Sys.sleep(.5)
    }
  }
  print(http)
  stop("Couldn't get the proper response from Oanda server: roanda::pastCandlesByTime()")
}
##########################
##  Place Market Order  ##
##########################
marketOrder <- function(instrument,
                        units=1000, #number of units to buy
                        side, #buy, sell
                        slippage=4, ## (pips)
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
  curr_price <- getPrice(instrument, auth_id=auth_id, acct_type=acct_type)
  pip_value <- getPipValue(instrument, auth_id=auth_id, acct=acct, acct_type=acct_type)
  
  if(!is.numeric(curr_price$ask)) stop('Current ask price is not numeric for some reason')
  if(!is.numeric(curr_price$bid)) stop('Current bid price is not numeric for some reason')
  if(!is.numeric(pip_value)) stop('Pip value is not numeric for some reason')
  
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
  body <- list(instrument=instrument,
               units=units,
               side=side,
               type=type,
               lowerBound=slipDown,
               upperBound=slipUp,
               takeProfit=takeProfit,
               stopLoss=stopLoss)
  
  for(i in 1:15) {
    http <- tryCatch(POST(url=url, add_headers(auth), body=body, encode='form'),
                     error=function(e) {         
                       Sys.sleep(1)         
                       next       
                     }     
    )
    if(http$status_code==200) {
      order <- fromJSON(content(http, encoding='UTF-8', type='text'))
      return(order)
    } else{
      cat(warn_for_status(http), 'roanda::marketOrder() | ')
      Sys.sleep(1)
    }
  }
  print(http)
  stop("Couldn't get the proper response from Oanda server: roanda::marketOrder()")
} 