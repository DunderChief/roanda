options(stringsAsFactors=FALSE)
library(xts)
library(quantmod)
library(roanda)
source('code/Cluster_by_Hour/Implementation_functions.R')
Sys.setenv(TZ='UTC')
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