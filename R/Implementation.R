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
detectNewCandle <- function(instrument='EUR_USD', granularity='H1') {
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
  
  # Sleep until we're close to that time
  sleep_dur <- difftime(newCandleTime-1, Sys.time(), units='secs')
  if(sleep_dur > 1) Sys.sleep(sleep_dur)
  
  while(index(xts::last(historical)) < newCandleTime){
    historical <- getHistorical(instrument, granularity=granularity,
                                start.time=Sys.time()-(5*period_secs),
                                end.time=newCandleTime)
    Sys.sleep(.3)
  }
  
}


# Params -------------------------------------------------------------------------------
# acct=522939
# auth_id='5a5e826fd8fc396e0847de25dbba7d25-0c737c44d5c1019a8a2c5de7067fba69'
# acct_type='fxtrade'
acct=727313
auth_id='414ffc620fc8932dde851e20b7d67e86-e9526690bcc14965de0dda51d800ae65'
acct_type='fxpractice'

ma_type='EMA'
instrument <- 'EUR_USD'
pipvalue <- getPipValue(instrument, acct=acct, auth_id=auth_id, acct_type=acct_type)
time_frame='H1'
max_trade_time <- 60*5 #  in minutes
risk <- 0.02 ##  Percent of equity that we want to risk per trade

max_trades <- 3
#TPSL <- readRDS('objs/opt_TPSL.rds')
TP <- 45
SL <- 15
cat('Starting while loop:', as.character(Sys.time()), '\n')
while(TRUE){
  this.day <- wday(Sys.time())
  this.hour <- hour(Sys.time())
  this.minute <- minute(Sys.time())
  this.second <- as.integer(second(Sys.time()))
  # Don't trade on weekends or most of Friday
  if(this.day %in% c(1,7) | 
     (this.day==6 & this.hour > 13) | # Fridays until 9am
     (this.day==2 & this.hour < 7)) { # Monday 3am
    Sys.sleep(60*55)
    next
  }
  
  # The Meat --------------------------------------------------------------------------
  if(this.minute == 0 & this.second==1) { # Evaluate every hour