\name{bivKMW}
\alias{bivKMW}
\title{Kaplan-Meier Weighted Estimators}
\description{Provides estimates for the bivariate distribution function for the estimators based on Kaplan-Meier Weights: Kaplan-Meier Weighted Estimator (KMW) and the Kaplan-Meier Presmoothed Weighted Estimator (KMPW).}
\usage{bivKMW(mydata, t1, t2)}

\arguments{
  \item{mydata}{the input data. A data frame with at least 5 variables: time1 (time of the intermediate event/censoring time or first gap time), delta (indicator of transition to the intermediate event; 0 if the first gap time is censored and 1 otherwise), time2 (the second gap time), status (censoring indicator: "dead"=1, "alive"=0) and Stime (time1 + time2). The remaining variables in the data.frame are left for the covariates.}
  \item{t1}{the first time for obtaining estimates for the bivariate distribution function. If NULL is equivalent to 0.}
  \item{t2}{the second time for obtaining estimates for the bivariate distribution function. If NULL is equivalent to maximum of Stime.}
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
de Una-Alvarez J, Amorim AP (2011). "A Semiparametric Estimator of the Bivariate Distribution Function for Censored Gap Times", \emph{Biometrical Journal}.
}

\author{Ana Moreira and Luis Meira Machado}
%\note{
%%  ~~further notes~~
%}
%% ~Make other sections like Warning with \section{Warning }{....} ~
%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}

\examples{
data(bladder2)
bladderBIV<-adapt(data=bladder2)
bivKMW(mydata=bladderBIV, t1=5, t2=20)
}