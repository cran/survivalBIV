\name{plot.survBIV}
\alias{plot.survBIV}
\title{Plot methods for a survBIV object}
\description{Provides the plots for the bivariate distribution function and marginal distribution of the second time.}
\usage{
\method{plot}{survBIV}(x, plot.marginal = FALSE, plot.bivariate = FALSE, 
plot.persp = FALSE, plot.contour = FALSE, method, method.cens = "KM", xlab,
ylab, zlab, main = method, col, grid.x = NULL, grid.y = NULL, col.biv = FALSE,
xlim = NULL, ylim = NULL, ...)}

\arguments{
\item{x}{an object of class \code{survBIV}.}
\item{plot.marginal}{if \code{TRUE}, plots the marginal distribution of the second gap time. Defaults to \code{FALSE}.}
\item{plot.bivariate}{if \code{TRUE}, plots the bivariate distribution function. Defaults to \code{FALSE}.}
\item{plot.persp}{if \code{TRUE}, draws the perspective plot of the bivariate distribution function. Defaults to \code{FALSE}.}
\item{plot.contour}{if \code{TRUE}, draws the contour plot of the bivariate distribution function. Defaults to \code{FALSE}.}
\item{method}{a character string specifying which estimator to fit. Possible values are "CKM", "IPCW", "KMPW" and "KMW".}
\item{method.cens}{a character string specifying which estimator to use in the computation. Possible values are "KM" and "prodlim". Defaults to "KM". For \code{IPCW} method only.}
\item{xlab}{x-axis label.}
\item{ylab}{y-axis label.}
\item{zlab}{z-axis label.}
\item{main}{title for the plot. Defaults to \code{method}.}
\item{col}{colour for plot.}
\item{grid.x}{vector of points to build the plot for the bivariate distribution function.}
\item{grid.y}{vector of points to build the plot for the bivariate distribution function.}
\item{col.biv}{a logical variable indicating whether you want color to be used in the filled.contour plot. Defaults to \code{FALSE}.}
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

\author{Ana Moreira, Artur Agostinho Araujo and Luis Meira Machado}

%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{survBIV}}.
}

\examples{
data("bladderBIV")
bladderBIV_obj <- with(bladderBIV, survBIV(time1, event1, time2, event2))

op <- par(mfrow = c(2, 2))
plot(bladderBIV_obj, plot.marginal = TRUE, method = "CKM")
plot(bladderBIV_obj, plot.marginal = TRUE, method = "IPCW")
plot(bladderBIV_obj, plot.marginal = TRUE, method = "KMPW")
plot(bladderBIV_obj, plot.marginal = TRUE, method = "KMW")
par(op)

plot(bladderBIV_obj, plot.marginal = TRUE, plot.bivariate = TRUE, method = "CKM")
plot(bladderBIV_obj, plot.bivariate = TRUE, method = "IPCW")
plot(bladderBIV_obj, plot.persp = TRUE, method = "KMPW")
plot(bladderBIV_obj, plot.contour = TRUE, method = "KMW")
}
