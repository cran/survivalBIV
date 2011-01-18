\name{adapt}
\alias{adapt}
\title{Adapt Database}
\description{Provides the adequate dataset for implementing all methods of the bivariate distribution for censored gap times. Returns a new dataset of class \code{bivariate}.}
\usage{adapt(data=NULL)}

\arguments{
  \item{data}{the input data. A data frame with at least 5 variables: time1 (time of the intermediate event/censoring time or first gap time), delta (indicator of transition to the intermediate event; 0 if the first gap time is censored and 1 otherwise), time2 (the second gap time), status (censoring indicator: "dead"=1, "alive"=0) and Stime (time1 + time2). The remaining variables in the data.frame are left for the covariates.}
}

\details{This function is to be used only once for each dataset.}
%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%}
%\references{
%% ~put references to the literature/web site here ~
%}
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
summary(bladderBIV, t1=5, t2=20, method="all")
}