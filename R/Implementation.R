options(stringsAsFactors=FALSE)
library(xts)
library(quantmod)
library(roanda)
library(lubridate)
library(foreach)
Sys.setenv(TZ='UTC')

# Return whether the current time is in undesireable trading hours
# ________________________________________________________________
isWeekend <- function() {
  systime <- Sys.time()
  this.day <- wday(systime)
  this.hour <- hour(systime)
  return(this.day %in% c(1,7) |           # Sat. or Sun. or...
         (this.day==6 & this.hour > 13) | # Fridays after 9am
         (this.day==2 & this.hour < 7)    # Monday before 3 am
  )   
}


# Return whether there's a fresh candle on my timescale
# _____________________________________________________
updateCandles <- function(hist, instrument='EUR_USD', granularity='H1') {
  # If historical data not supplied, get some
  if(missing(hist)) {
    hist <- getHistorical(instrument=instrument, granularity=granularity)
  }
  systime <- Sys.time()
  this.day <- wday(systime)
  this.hour <- hour(systime)
  this.minute <- minute(systime)
  this.second <- as.integer(second(systime))

  # Find the closest hour, minute, second, etc.
  timestring <- substring(granularity, 1, 1)
  timeint <- as.numeric(substring(granularity, 2, nchar(granularity)))
  lub_time <- switch(timestring,
                     'S' = 'second',
                     'M' = 'minute',
                     'H' = 'hour',
                     'D' = 'day',
                     'W' = 'week')
  period_secs <- switch(timestring,
                        'S' = 1,
                        'M' = 60,
                        'H' = 60*60,
                        'D' = 60*60*24,
                        'W' = 60*60*24*7)
  # Some Oanda granularity have no int, there fore need to skip
  if(!is.na(timeint)) period_secs <- period_secs * timeint
  newCandleTime <- align.time(Sys.time(), period_secs, drop.time=FALSE)
  
  # Sleep until we're whithin 1 sec of that time
  sleep_dur <- difftime(newCandleTime-1, Sys.time(), units='secs')
  if(sleep_dur > 1) Sys.sleep(sleep_dur)
  
    
  while(Sys.time() < newCandleTime) {
    cat('waiting for correct time')
    Sys.sleep(.3)
  }
  while(index(xts::last(hist)) < (newCandleTime - period_secs)){
    cat('getting new candles')
    hist <- updateHistorical(hist, instrument=instrument, granularity=granularity)
    Sys.sleep(.3)
  }
  return(hist)
}

# Returns the units needed given a stop loss and risk level 
# _________________________________________________________
getUnits <- function(instrument, current_price, stoploss, risk=.02)
{
  if(risk > .10) stop("Whoa there cowbody!!! Greater than 10% risk is more than this little function is willing to allow.")
  price <- as.vector(current_price)
  ##  Is USD the base currency (1st pair)?
  isUSDbase <- which(strsplit(instrument, '_')[[1]] =='USD') == 1
  ##  How many units = 1 USD?
  units_per_dollar <- ifelse(isUSDbase, 
                             pipvalue * (1/price),
                             pipvalue)
  eq <- getEquity(acct=acct, auth_id=auth_id, acct_type=acct_type)
  dollars_per_pip_to_risk <- eq * risk / stoploss  
  units <- dollars_per_pip_to_risk / units_per_dollar
  # Set a cap of 1 million units
  if(units > 1e6) units <- 1e6
  return(as.integer(units))
}

# Given an order, how long ago was it placed?
# ___________________________________________
howOldIsOrder <- function(order) {
  how_old <- as.numeric(difftime(Sys.time(), index(order), units='mins'))
  return(how_old)
}



