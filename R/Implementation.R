

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
updateCandles <- function(hist, instrument='EUR_USD', granularity='H1', 
                          acct, auth_id, acct_type) {
  # If historical data not supplied, get some
  if(missing(hist)) {
    hist <- getHistorical(instrument=instrument, granularity=granularity,
                          acct=acct, auth_id=auth_id, acct_type=acct_type)
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
    Sys.sleep(.3)
  }
  
  while(xts:::index.xts(xts::last(hist)) < (newCandleTime - period_secs)){
    start <- xts:::index.xts(xts::last(hist)) - period_secs*4
    hist <- updateHistorical(hist, start=start, instrument=instrument, granularity=granularity,
                             acct=acct, auth_id=auth_id, acct_type=acct_type)
    Sys.sleep(.3)
  }
  return(hist)
}

# Returns the units needed given a stop loss and risk level 
# _________________________________________________________
getUnits <- function(instrument, current_price, stoploss, risk=.02)
{
  pipvalue <- getPipValue(instrument, acct=acct, auth_id=auth_id, acct_type=acct_type)
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
  how_old <- as.numeric(difftime(Sys.time(), xts:::index.xts(order), units='mins'))
  return(how_old)
}

# function to find when one indicator crosses another: -1 fast cross down, 1 fast cross up 
# ___________________________________________________
getCrossover <- function(fast, slow) {
  
  isFastGreater <- na.omit(fast > slow)
  isFastGreater[, 1] <- as.numeric(isFastGreater[, 1])
  
  # Find crossovers
  isFastGreater$Lag1 <- Lag(isFastGreater[, 1]) * -1 # * -1 gives us rowDiffs()
  crossovers <- xts(rowSums(isFastGreater), order.by=index(isFastGreater))
  colnames(crossovers) <- 'signal'
  return(na.omit(crossovers))
}

