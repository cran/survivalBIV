\name{summary.bivariate}
\alias{summary.bivariate}
\title{Summary methods for a bivariate object}
\description{Provides results for an object of class \code{bivariate}. It gives the estimated bivariate distribution of the gap times and marginal distribution of the second gap time.}
\usage{
summary.bivariate(object, t1, t2, method, ...)}

\arguments{
  \item{object}{an object of class \code{bivariate}.}
  \item{t1}{the first time for computing estimates for the bivariate distribution function. If NULL is equivalent to 0.}
  \item{t2}{the second time for computing estimates for the bivariate distribution function. If NULL is equivalent to maximum of time2.}
  \item{method}{a character string specifying which estimator to fit. Possible values are "KMW", "IPCW", "CKM", "KMPW" and "all". If NULL all the estimators will be implemented.}
  \item{\dots}{further arguments for plot.}
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
