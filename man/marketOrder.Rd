\name{marketOrder}
\alias{marketOrder}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  ~~function to do ... ~~
}
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
marketOrder(instrument = "EUR_USD", units = 10000, side, slippage = 2, SL = 10, TP = 10, acct, auth_id, acct_type = "fxpractice")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{instrument}{
     ~~Describe \code{instrument} here~~
}
  \item{units}{
     ~~Describe \code{units} here~~
}
  \item{side}{
     ~~Describe \code{side} here~~
}
  \item{slippage}{
     ~~Describe \code{slippage} here~~
}
  \item{SL}{
     ~~Describe \code{SL} here~~
}
  \item{TP}{
     ~~Describe \code{TP} here~~
}
  \item{acct}{
     ~~Describe \code{acct} here~~
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
function (instrument = "EUR_USD", units = 10000, side, slippage = 2, 
    SL = 10, TP = 10, acct, auth_id, acct_type = "fxpractice") 
{
    type <- "market"
    curr_price <- getPrice("EUR_USD", auth_id = auth_id)
    pip_value <- getPipValue("EUR_USD", auth_id = auth_id, acct = acct)
    if (side == "buy") {
        stopLoss <- curr_price$ask - (SL * pip_value)
        takeProfit <- curr_price$ask + (TP * pip_value)
        slipUp <- curr_price$ask + (slippage * pip_value)
        slipDown <- curr_price$ask - (slippage * pip_value)
    }
    else if (side == "sell") {
        stopLoss <- curr_price$bid + (SL * pip_value)
        takeProfit <- curr_price$bid - (TP * pip_value)
        slipUp <- curr_price$bid + (slippage * pip_value)
        slipDown <- curr_price$bid - (slippage * pip_value)
    }
    else {
        stop("Need to specify whether to Buy or Sell")
    }
    auth <- c(Authorization = paste("Bearer", auth_id))
    url <- paste0("https://api-", acct_type, ".oanda.com/v1/accounts/", 
        acct, "/orders")
    order <- fromJSON(postForm(url, style = "POST", .params = c(instrument = instrument, 
        units = units, side = side, type = type, lowerBound = slipDown, 
        upperBound = slipUp, takeProfit = takeProfit, stopLoss = stopLoss), 
        .opts = list(httpheader = auth)))
    return(order)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
