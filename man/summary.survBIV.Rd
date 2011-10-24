\name{summary.survBIV}
\alias{summary.survBIV}
\title{Summary methods for a survBIV object}
\description{Provides results for an object of class \code{survBIV}. It gives the estimated bivariate distribution of the gap times and marginal distribution of the second gap time.}
\usage{summary.survBIV(object, t1, t2, conf = FALSE, n.boot = 1000, conf.level = 0.95,
method.boot = "percentile", method.cens = "KM", method = "all", ...)}

\arguments{
\item{object}{an object of class \code{survBIV}.}
\item{t1}{the first time for obtaining estimates for the bivariate distribution function. If missing, 0 will be used.}
\item{t2}{the second time for obtaining estimates for the bivariate distribution function. If missing, the maximum of \code{time2} will be used.}
\item{conf}{provides pointwise confidence bands. Defaults to \code{FALSE}.}
\item{n.boot}{the number of bootstrap samples. Defaults to 1000 samples.}
\item{conf.level}{level of confidence. Defaults to 0.95 (correponding to 95\%).}
\item{method.boot}{method used to compute bootstrap confidence intervals. Possible options are "percentile" and "basic". Defaults to "percentile".}
\item{method.cens}{method for computation of the survival of censoring times. Defaults to "KM" where the Kaplan-Meier estimator is used. \code{method.cens="prodlim"} uses \pkg{prodlim} package. For \code{IPCW} method only.}
\item{method}{a character string specifying which estimator to fit. Possible values are "CKM", "IPCW", "KMPW", "KMW" and "all". Defaults to "all".}
\item{\dots}{further arguments for summary.}
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
%~~ Literature or other references for background information ~~
de Una-Alvarez J, Amorim AP (2011). "A Semiparametric Estimator of the Bivariate Distribution Function for Censored Gap Times", \emph{Biometrical Journal}, \bold{53}, 113-127.

de Una-Alvarez J, Meira Machado LF (2008). "A Simple Estimator of the Bivariate Distribution Function for Censored Gap Times", \emph{Statistical and Probability Letters}, \bold{78}, 2440-2445.

Gerds, TA, Schumacher, M (2006). "Consistent estimation of the expected bries score in general survival models with right-censored event times", \emph{Biometrical Journal}, \bold{48}, 1029-1040.

Lin D, Sun W, Ying Z (1999). "Nonparametric Estimation of the Time Distributions for Serial Events with Censored Data", \emph{Biometrika}, \bold{86}, 59-70.

Meira Machado LF, Moreira A (2010). "Estimation of the Bivariate Distribution Function for Censored Gap Times". \emph{Proceedings of the 19th International Conference on Computational Statistics, pp. 1367-1374}.

Davison, A.C. and Hinkley, D.V. (1997) "Bootstrap Methods and Their Application", Chapter 5. Cambridge University Press.
}

\author{Ana Moreira, Artur Agostinho Araujo and Luis Meira Machado}

%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{bivCKM}},
\code{\link{bivIPCW}},
\code{\link{bivKMPW}},
\code{\link{bivKMW}},
\code{\link{survBIV}}.
}

\examples{
data("bladderBIV")
bladderBIV_obj <- with(bladderBIV, survBIV(time1, event1, time2, event2))
summary(bladderBIV_obj, t1 = 5, t2 = 20, method = "all")
}
