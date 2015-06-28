library(xts)
library(quantmod)
library(lubridate)
library(foreach)
library(stringr)

getHistorical <- function(
  instrument='EUR_USD',
  start.time=Sys.Date() - 5,
  end.time=Sys.Date(),
  granularity='M1',
  acct=727313,
  auth_id='414ffc620fc8932dde851e20b7d67e86-e9526690bcc14965de0dda51d800ae65',
  acct_type='fxpractice')
{
  
  ##  We want to get the 30 second data from July 1, 2013 - July 12, 2014,
  ##  but Oanda only lets us get 5000 time points at a time
  options(stringsAsFactors=FALSE)
  
  # Split the granularity string
  gran.num <- as.integer(str_extract(granularity, '[0-9]+'))
  gran.time <- str_extract(granularity, '[A-Z]')
  
  gran.time <- switch(gran.time,
                     'S' = 'sec',
                     'M' = 'min',
                     'H' = 'hour',
                     'D' = 'day',
                     'W' = 'week')
  
  # this is meant to break the total time into bins with 5000 points in between
  start_dates <- seq.POSIXt(ymd(start.time), ymd(end.time), by=paste(5000*gran.num, gran.time))
  
  dat <- foreach(start=start_dates) %do% {
    sec_multiplier <- switch(gran.time,
                             'sec' = 1,
                             'min' = 60,
                             'hour' = 60*60,
                             'day' = 60*60*24,
                             'week' = 60*60*24*7)
    end <- start + seconds(5000*sec_multiplier)
    start.f <- format(start, format='%Y-%m-%dT%H:%M:%SZ')
    end.f <- format(end, format='%Y-%m-%dT%H:%M:%SZ')

    start.f <- gsub(':', '%3A', start.f)
    end.f <- gsub(':', '%3A', end.f)

    out <- getCandlesByTime(instrument=instrument, granularity=granularity, 
                            start=start.f, end=end.f, auth_id=auth_id, acct_type=acct_type)
    Sys.sleep(0.1)
    return(as.data.frame(out))
  }
  has.candles <- lapply(dat, function(xx) nrow(xx)!=1)
  has.candles <- do.call(c, has.candles)
  dat_2 <- dat[has.candles]
  dat_2 <- do.call(rbind, dat_2)
  dat <- xts(dat_2, order.by=as.POSIXct(rownames(dat_2), format='%Y-%m-%d %H:%M:%S', tz='UTC'))
  return(dat)
}

# Takes an xts object and downloads appends new candles up to current date
updateHistorical <- function(dat.xts, instrument='EUR_USD', granularity='H1') {
  library(roanda)
  # Subtract one day to avoid 00:00:00 where there may be no candles available yet
  start <- floor_date(index(xts::last(dat.xts)) - 60*60*24, 'day')
  newdata <- getHistorical(instrument, 
                           start.time=as.character(start), 
                           end.time=Sys.Date(),
                           granularity='H1')
  updated <- rbind(dat.xts, newdata)
  updated <- updated[!duplicated(index(updated)),]
  return(updated)
}