\name{dgpBIV}
\alias{dgpBIV}
\title{Generates Bivariate Survival Data}
\description{Generates bivariate censored gap times from some known copula functions.}
\usage{dgpBIV(n, corr, dist, model.cens, cens.par, dist.par,
to.data.frame = FALSE)}

\arguments{
\item{n}{sample size.}
\item{corr}{correlation parameter. Possible values for the bivariate exponential distribution are between 0 (for independency) and 1. Any value between 0 (not included) and 1 is accepted for the bivariate Weibull distribution.}
\item{dist}{distribution. Possible bivariate distributions are "exponential" and "weibull".}
\item{model.cens}{model for censorship. Possible values are "uniform" and "exponential".}
\item{cens.par}{parameter for the censorship distribution. For censure model equal to "exponential" the argument \code{cens.par} must be greater than 0. For censure model equal to "uniform" the argument must be greater or equal than 0.}
\item{dist.par}{vector of parameters for the allowed distributions. Two (rate) parameters for the bivariate exponential distribution and four (2 location parameters and 2 scale parameters) for the bivariate Weibull distribution. See details below.}
\item{to.data.frame}{if \code{TRUE} returns a data.frame, if \code{FALSE} returns an object of class \code{survBIV}. Defaults to \code{FALSE}.}
}

\details{
The bivariate exponential distribution, also known as Farlie-Gumbel-Morgenstern distribution is given by
\deqn{F(x,y)=F_1(x)F_2(y)[1+\delta((1-F_1(x))(1-F_2(y)))]}{F(x,y)=F_1(x)F_2(y)[1+d((1-F_1(x))(1-F_2(y)))}
for \eqn{x\ge0}{x\ge0} and \eqn{y\ge0}{y\ge0}. Where the marginal distribution functions \eqn{F_1}{F_1} and \eqn{F_2}{F_2} are exponential with rate parameters \eqn{\lambda_1}{l_1} and \eqn{\lambda_2}{l_2} and correlation parameter \eqn{\delta}{d}, \eqn{-1 \le \delta \le 1}{-1 \le \delta \le 1}. \cr

The bivariate Weibull distribution with two-parameter marginal distributions. It's survival function is given by
\deqn{S(x,y)=P(X>x,Y>y)=e^{-[(\frac{x}{\theta_1})^\frac{\beta_1}{\delta}+(\frac{y}{\theta_2})^\frac{\beta_2}{\delta}]^\delta}}{S(x,y)=P(X>x,Y>y)=exp^(-[(x/t_1)^\beta_1/d+(y/t_2)^b_2/d]^d)}

Where \eqn{0 < \delta \le 1}{0 < d \le 1} and each marginal distribution has shape parameter \eqn{\beta_i}{b_i} and a scale parameter \eqn{\theta_i}{t_i}, \eqn{i = 1, 2}.
}

%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%}

\references{
Johnson N, Kotz S (1972). "Distribution in statistics: continuous multivariate distribution", John Wiley and Sons.

Lu J, Bhattacharya G (1990). "Some new constructions of bivariate weibull models", Annals of Institute of Statistical Mathematics, \bold{42}, 543-559.
}

\author{Ana Moreira, Artur Agostinho Araujo and Luis Meira Machado}

%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{corrBIV}}.
}

\examples{
#Example for the bivariate Exponential distribution
dgpBIV(n = 100, corr = 1, dist = "exponential", model.cens = "uniform", 
cens.par = 3, dist.par = c(1, 1), to.data.frame = TRUE)
#Example for the bivariate Weibull distribution
dgpBIV(n = 100, corr = 1, dist = "weibull", model.cens = "exponential",
cens.par = 0.08, dist.par = c(2, 7, 2, 7))
}
