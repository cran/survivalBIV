\name{bladderBIV}
\alias{bladderBIV}
\docType{data}
\title{Bladder cancer recurrences}
\description{This contains the bladder cancer recurrences data in a different format. In this study, patients had superficial bladder tumors that were removed by transurethral resection. Many patients had multiple recurrences (up to a maximum of 9) of tumors during the study, and new tumors were removed at each visit. Only the first two recurrence times (in months) and the corresponding gap times are considered.}
\usage{data("bladderBIV")}

\format{
A data frame with 85 observations on the following 4 variables.
\describe{
\item{\code{time1}}{time until first recurrence/censoring time.}
\item{\code{event1}}{first recurrence indicator.}
\item{\code{time2}}{elapsed time between first and second recurrence/censoring time.}
\item{\code{event2}}{second recurrence indicator.}
}
}

%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}

%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}

\references{LJ Wei, DY Lin, L Weissfeld (1989). "Regression analysis of multivariate incomplete failure time data by modeling marginal distributions", \emph{Journal of the American Statistical Association}, \bold{84}.}

%\examples{
%data(bladderBIV)
%## maybe str(bladderBIV) ; plot(bladderBIV) ...
%}

%\keyword{datasets}
