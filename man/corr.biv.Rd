\name{corr.biv}
\alias{corr.biv}
\title{Correlation between two gap times}
\description{Provides the correlation between the bivariate times for some copula distributions.}
\usage{corr.biv(dist, corr, dist.par)}

\arguments{
  \item{dist}{the distribution. Possible bivariate distributions are "exponential" and "weibull".}
  \item{corr}{correlation parameter. Possible values for the bivariate exponential distribution are 0 (for independency) and 1 for a maximum correlation of 25\%. Any positive value is accepted for the bivariate Weibull distribution.}
  \item{dist.par}{vector of parameters for the allowed distributions. Two (rate) parameters for the bivariate exponential distribution and four (2 location parameters and 2 scale parameters) for the bivariate Weibull distribution. See details below.}
}
\details{
The bivariate exponential distribution, also known as Failie-Gumbel-Morgenstern distribution is given by
\deqn{F(x,y)=F_1(x)F_2(y)[1+\delta((1-F_1(x))(1-F_2(y)))]}{F(x,y)=F_1(x)F_2(y)[1+\delta((1-F_1(x))(1-F_2(y)))}
for \eqn{x\ge0}{x\ge0} and \eqn{y\ge0}{y\ge0}. Where the marginal distribution functions \eqn{F_1}{F_1} and \eqn{F_2}{F_2} are exponential with rate parameters \eqn{\lambda_1}{\lambda_1} and \eqn{\lambda_2}{\lambda_2} and correlation parameter \eqn{\delta}{\delta}. \cr

The bivariate Weibull distribution with two-parameter marginal distributions. It's survival function is given by
\deqn{S(x,y)=P(X>x,Y>y)=e^{-[(\frac{x}{\theta_1})^\frac{\beta_1}{\delta}+(\frac{y}{\theta_2})^\frac{\beta_2}{\delta}]^\delta}}{S(x,y)=P(X>x,Y>y)=exp^(-[(x/\theta_1)^\beta_1/\delta+(y/\theta_2)^\beta_2/\delta]^\delta)}

Where \eqn{0 < \delta \le 1}{0 < \delta \le 1} and each marginal distribution has shape parameter \eqn{\beta_i}{\beta_i} and a scale parameter \eqn{\theta_i}{\theta_i}, \eqn{i = 1, 2}.
}
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
#Example for the bivariate Weibull distribution 
corr.biv(dist="weibull", corr=0.5, c(2,7,2,7))
#Example for the bivariate Exponential distribution
corr.biv(dist="exponential", corr=-1, c(1,1))
}