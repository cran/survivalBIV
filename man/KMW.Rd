\name{KMW}
\alias{KMW}
\title{Kaplan-Meier Weights}
\description{Calculates the Kaplan-Meier weights.}
\usage{KMW(time, status)}

\arguments{
  \item{time}{the follow up time.}
  \item{status}{the status indicator, normally 0=censored 1=event.}
}
%\details{
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
KMW(bladder2$Stime, bladder2$status) 
}
