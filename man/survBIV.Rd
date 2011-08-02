\name{survBIV}
\alias{survBIV}
\alias{is.survBIV}
\title{Create a survBIV object}
\description{Creates a survBIV object, usually used as input to other functions.}
\usage{
survBIV(time1, event1, time2, event2)
is.survBIV(object)
}

\arguments{
\item{time1}{time of the intermediate event/censoring time or first gap time.}
\item{event1}{indicator of transition to the intermediate event; 0 if the first gap time is censored and 1 otherwise.}
\item{time2}{the second gap time.}
\item{event2}{censoring indicator for the second event; 0 if the second gap time is censored and 1 otherwise.}
\item{object}{any R object.}
}

%\details{
%  Typical usages are \preformatted{survBIV(time1, event1, time2, event2)}
%}

\value{
An object of class \code{survBIV}.  There are methods for \code{summary} and \code{plot}.

\code{survBIV} objects are implemented as a single element list \item{data}{a data.frame
with \code{time1}, \code{event1}, \code{time2}, \code{event2} and \code{Stime} as columns.}

In the case of \code{is.survBIV}, a logical value \code{TRUE} if \code{object}
inherits from class \code{survBIV}, otherwise \code{FALSE}.
}

\author{Ana Moreira, Artur Agostinho Araujo and Luis Meira Machado}

\seealso{
\code{\link{plot.survBIV}},
\code{\link{summary.survBIV}}.
}

\examples{
data(bladderBIV)
bladderBIV.obj <- with( bladderBIV, survBIV(time1, event1, time2, event2) )
#or
bladderBIV.obj <- survBIV(bladderBIV$time1, bladderBIV$event1, bladderBIV$time2,
bladderBIV$event2)
}
