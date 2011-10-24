\name{bivCKM}
\alias{bivCKM}
\title{Conditional Kaplan-Meier Estimator}
\description{Provides estimates for the bivariate distribution function for the Conditional Kaplan-Meier estimator, CKM.}
\usage{bivCKM(object, t1, t2, conf = FALSE, n.boot = 1000, conf.level = 0.95,
method.boot = "percentile")}

\arguments{
\item{object}{an object of class \code{survBIV}.}
\item{t1}{the first time for obtaining estimates for the bivariate distribution function. If missing, 0 will be used.}
\item{t2}{the second time for obtaining estimates for the bivariate distribution function. If missing, the maximum of \code{time2} will be used.}
\item{conf}{provides pointwise confidence bands. Defaults to \code{FALSE}.}
\item{n.boot}{the number of bootstrap samples. Defaults to 1000 samples.}
\item{conf.level}{level of confidence. Defaults to 0.95 (correponding to 95\%).}
\item{method.boot}{method used to compute bootstrap confidence intervals. Possible options are "percentile" and "basic". Defaults to "percentile".}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%}

\references{
%% ~put references to the literature/web site here ~
de Una-Alvarez J, Meira Machado LF (2008). "A Simple Estimator of the Bivariate Distribution Function for Censored Gap Times", \emph{Statistical and Probability Letters}, \bold{78}, 2440-2445.

Meira Machado LF, Moreira A (2010). "Estimation of the Bivariate Distribution Function for Censored Gap Times", \emph{Proceedings of the 19th International Conference on Computational Statistics}, pp. 1367-1374.

Davison, A.C. and Hinkley, D.V. (1997) "Bootstrap Methods and Their Application", Chapter 5. Cambridge University Press.
}

\author{Ana Moreira, Artur Agostinho Araujo and Luis Meira Machado}

%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{bivIPCW}},
\code{\link{bivKMPW}},
\code{\link{bivKMW}}.
}

\examples{
data("bladderBIV")
bladderBIV_obj <- with(bladderBIV, survBIV(time1, event1, time2, event2))
bivCKM(object = bladderBIV_obj, t1 = 5, t2 = 20)
#or
bladderBIV_obj <- survBIV(bladderBIV$time1, bladderBIV$event1, bladderBIV$time2,
bladderBIV$event2)
bivCKM(object = bladderBIV_obj, t1 = 5, t2 = 20, conf = TRUE, conf.level = 0.9)
}
