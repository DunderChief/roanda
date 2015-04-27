\name{getCandlesByTime}
\alias{getCandlesByTime}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
blah blah
}
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getCandlesByTime(instrument = "EUR_USD", granularity = "M1", start, end, includeFirst = TRUE, candleFormat = "bidask", isAsk = TRUE, auth_id, acct_type = "fxpractice")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{instrument}{
     ~~Describe \code{instrument} here~~
}
  \item{granularity}{
     ~~Describe \code{granularity} here~~
}
  \item{start}{
     ~~Describe \code{start} here~~
}
  \item{end}{
     ~~Describe \code{end} here~~
}
  \item{includeFirst}{
     ~~Describe \code{includeFirst} here~~
}
  \item{candleFormat}{
     ~~Describe \code{candleFormat} here~~
}
  \item{isAsk}{
     ~~Describe \code{isAsk} here~~
}
  \item{auth_id}{
     ~~Describe \code{auth_id} here~~
}
  \item{acct_type}{
     ~~Describe \code{acct_type} here~~
}
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
 ...
}
\references{
 ~put references to the literature/web site here ~
}
\author{
  ~~who you are~~
}
\note{
  ~~further notes~~
}

 ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (instrument = "EUR_USD", granularity = "M1", start, 
    end, includeFirst = TRUE, candleFormat = "bidask", isAsk = TRUE, 
    auth_id, acct_type = "fxpractice") 
{
    require(xts)
    auth <- c(Authorization = paste("Bearer", auth_id))
    includeFirst <- ifelse(includeFirst, "true", "false")
    options <- paste0("instrument=", instrument, "&granularity=", 
        granularity, "&start=", start, "&end=", end, "&includeFirst=", 
        includeFirst, "&candleFormat=", candleFormat)
    url <- paste0("https://api-", acct_type, ".oanda.com/v1/candles?", 
        options)
    hist <- getURL(url, httpheader = auth)
    if (hist == "") 
        return("No Candles")
    hist <- fromJSON(hist)$candles
    hist <- hist[hist$complete == TRUE, ]
    if (isAsk) {
        ohlc <- data.frame(Open = hist$openAsk, High = hist$highAsk, 
            Low = hist$lowAsk, Close = hist$closeAsk, Volume = hist$volume)
    }
    else {
        ohlc <- data.frame(Open = hist$openBid, High = hist$highBid, 
            Low = hist$lowBid, Close = hist$closeBid, Volume = hist$volume)
    }
    dates <- as.POSIXct(hist$time, format = "\%Y-\%m-\%dT\%H:\%M:\%S", 
        tz = "UTC")
    ohlc_xts <- xts(ohlc, order.by = dates)
    return(ohlc_xts)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
