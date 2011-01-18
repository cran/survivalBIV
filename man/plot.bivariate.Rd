\name{plot.bivariate}
\alias{plot.bivariate}
\title{Plot method for a bivariate object}
\description{Provides the plots for the bivariate distribution function and marginal distribution of the second time.}
\usage{
plot.bivariate(x, plot.marginal=NULL, plot.bivariate=NULL, xlab,
ylab, zlab, col, method, grid.x=NULL, grid.y=NULL, col.biv=NULL,
xlim=NULL, ylim=NULL, ...)}

\arguments{
  \item{x}{an object of class bivariate.}
  \item{plot.marginal}{If TRUE, plots the marginal distribution of the second gap time.}
  \item{plot.bivariate}{If TRUE, plots the bivariate distribution function.}
  \item{xlab}{x-axis label.}
  \item{ylab}{y-axis label.}
  \item{zlab}{z-axis label.}
  \item{col}{colour for plot.}
  \item{method}{a character string specifying which estimator to fit. Possible values are "KMW", "KMPW", "IPCW" and "CKM". If NULL all the estimators will be implemented.}
  \item{grid.x}{vector of points to build the plot for the bivariate distribution function. For \code{IPCW} method only.}
  \item{grid.y}{vector of points to build the plot for the bivariate distribution function. For \code{IPCW} method only.}
  \item{col.biv}{a logical variable indicating whether you want color to be used in the filled.contour plot. By default col.biv = FALSE.}
  \item{xlim}{x limits for the plot for the marginal distribution of the second gap time.}
  \item{ylim}{y limits for the plot for the marginal distribution of the second gap time.}
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
#Example 
plot(bladderBIV, plot.marginal=TRUE, method="CKM")
plot(bladderBIV, plot.bivariate=TRUE, method="CKM")
plot(bladderBIV, plot.marginal=TRUE, method="KMW")
plot(bladderBIV, plot.bivariate=TRUE, method="KMPW")

}