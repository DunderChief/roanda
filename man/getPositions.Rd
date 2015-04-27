\name{getPositions}
\alias{getPositions}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  ~~function to do ... ~~
}
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getPositions(instrument, acct, auth_id, acct_type = "fxpractice")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{instrument}{
     ~~Describe \code{instrument} here~~
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
function (instrument, acct, auth_id, acct_type = "fxpractice") 
{
    auth <- c(Authorization = paste("Bearer", auth_id))
    url <- paste0("https://api-", acct_type, ".oanda.com/v1/accounts/", 
        acct, "/positions?instrument=", instrument)
    orders <- fromJSON(getURL(url, httpheader = auth))$positions
    return(orders)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
