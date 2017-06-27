library(xts)
library(quantmod)
library(lubridate)
library(foreach)

# Historacial Commitment of traders CFTC
getCommitmentOfTraders <- function(instrument='EUR_USD',
                                   acct, auth_id, acct_type) {
  auth <- c(Authorization=paste('Bearer', auth_id))
  
  url <- paste0('https://api-', acct_type, '.oanda.com/labs/v1/commitments_of_traders?', 
                paste0('instrument=', instrument))
  for(i in 1:15) {
    http <- GET(url=url, add_headers(auth))
    if(http$status_code==200) {
      out <- fromJSON(content(http, type='text', encoding='UTF-8'))[[1]]
      out$date <- as.POSIXct(out$date, origin="1970-01-01")
      colnames(out) <- c('unit', 'NonCommercialShort', 'date', 'price',
                         'NonCommercialLong', 'OverallInterest')
      return(out)
    } else{
      cat(warn_for_status(http), 'roanda::getEconomicEvents() | ')
      Sys.sleep(.5)
    }
  }
  print(http)
  stop("Couldn't get the proper response from Oanda server: roanda::getEquity()")
}
# Get Economic Events
getEconomicEvents <- function(instrument=NA, period=31536000,
                              acct, auth_id, acct_type){
  ##  Athorization
  auth <- c(Authorization=paste('Bearer', auth_id))
  
  url <- paste0('https://api-', acct_type, '.oanda.com/labs/v1/calendar?', 
                ifelse(is.na(instrument), '', 
                       paste0('instrument=', instrument, '&')),
                'period=', period)
  for(i in 1:15) {
    http <- GET(url=url, add_headers(auth))
    if(http$status_code==200) {
      out <- fromJSON(content(http, encoding='UTF-8', type='text'))
      out$timestamp <- as.POSIXct(out$timestamp, origin="1970-01-01")
      return(out)
    } else{
      cat(warn_for_status(http), 'roanda::getEconomicEvents() | ')
      Sys.sleep(.5)
    }
  }
  print(http)
  stop("Couldn't get the proper response from Oanda server: roanda::getEquity()")
}

# Historical position ratios
getPositionRatios <- function(instrument='EUR_USD', period=31536000,
                              acct, auth_id, acct_type){
  auth <- c(Authorization=paste('Bearer', auth_id))
  
  url <- paste0('https://api-', acct_type, 
                '.oanda.com/labs/v1/historical_position_ratios?', 
                paste0('instrument=', instrument, '&'),
                'period=', period)
  for(i in 1:15) {
    http <- GET(url=url, add_headers(auth))
    if(http$status_code==200) {
      out <- fromJSON(content(http, type='text', encoding='UTF-8'))
      out <- data.frame(out[[1]][[1]][[2]])
      colnames(out) <- c('timestamp', 'LongPosRatio', 'ExchangeRate')
      out$timestamp <- as.POSIXct(out$timestamp, origin="1970-01-01")
      return(out)
    } else{
      cat(warn_for_status(http), 'roanda::getEconomicEvents() | ')
      Sys.sleep(.5)
    }
  }
  print(http)
  stop("Couldn't get the proper response from Oanda server: roanda::getEquity()")
}
# Historical candles
getHistorical <- function(
  instrument='EUR_USD',
  start.time=Sys.Date() - 5,
  end.time=Sys.Date(),
  granularity='M1',
  acct,
  auth_id,
  acct_type,
  isAsk=TRUE,
  only_complete=TRUE)
{
  
  ##  We want to get the 30 second data from July 1, 2013 - July 12, 2014,
  ##  but Oanda only lets us get 5000 time points at a time
  options(stringsAsFactors=FALSE)
  
  # Split the granularity string
  gran.num <- as.numeric(substring(granularity, 2, nchar(granularity)))
  if(is.na(gran.num)) gran.num <- 1
  gran.time <- substring(granularity, 1, 1)
  
  gran.time <- switch(gran.time,
                     'S' = 'sec',
                     'M' = 'min',
                     'H' = 'hour',
                     'D' = 'day',
                     'W' = 'week')
  
  # this is meant to break the total time into bins with 5000 points in between
  start.time <- as.POSIXct(start.time)
  end.time <- as.POSIXct(end.time)
  
  start_dates <- seq.POSIXt(start.time, end.time, by=paste(5000*gran.num, gran.time))
  
  dat <- foreach(start=start_dates) %do% {
    sec_multiplier <- switch(gran.time,
                             'sec' = 1,
                             'min' = 60,
                             'hour' = 60*60,
                             'day' = 60*60*24,
                             'week' = 60*60*24*7)
    end <- start + seconds(5000*sec_multiplier*gran.num)
    start.f <- format(start, format='%Y-%m-%dT%H:%M:%SZ')
    end.f <- format(end, format='%Y-%m-%dT%H:%M:%SZ')

    start.f <- gsub(':', '%3A', start.f)
    end.f <- gsub(':', '%3A', end.f)

    out <- getCandlesByTime(instrument=instrument, granularity=granularity, isAsk=isAsk,
                            start=start.f, end=end.f, auth_id=auth_id, acct_type=acct_type,
                            only_complete=only_complete)
    Sys.sleep(0.1)
    return(as.data.frame(out))
  }
  has.candles <- lapply(dat, function(xx) nrow(xx)!=1)
  has.candles <- do.call(c, has.candles)
  if(sum(has.candles) < 1) {
    warning('No candles found!!! Returning NA')
    return(NA)
  }
  dat_2 <- dat[has.candles]
  dat_2 <- do.call(rbind, dat_2)
  dat <- xts(dat_2, order.by=as.POSIXct(rownames(dat_2), format='%Y-%m-%d %H:%M:%S', tz='UTC'))
  return(dat)
}

# Takes an xts object and downloads appends new candles up to current date
updateHistorical <- function(dat.xts, auth_id, acct, acct_type, 
                             instrument='EUR_USD', granularity='H1', 
                             isAsk=TRUE, start, only_complete=TRUE) {
  library(roanda)
  # Subtract one day to avoid 00:00:00 where there may be no candles available yet
  if(missing(start)){
    start <- floor_date(xts:::index.xts(xts::last(dat.xts)) - 60*60*24, 'day')
  }
  newdata <- getHistorical(instrument, 
                           start.time=start, 
                           end.time=Sys.Date() + 1,
                           granularity=granularity,
                           auth_id=auth_id,
                           acct=acct,
                           acct_type=acct_type,
                           isAsk=isAsk,
                           only_complete=only_complete)
  if(NCOL(newdata)==1){
    warning('No candles returned for this time period, returning original dataset.')
    return(dat.xts)
  }
  updated <- rbind(dat.xts, newdata)
  updated <- updated[!duplicated(xts:::index.xts(updated)),]
  return(updated)
}

