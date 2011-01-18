\name{data.gen}
\alias{data.gen}
\title{Generates Bivariate Survival Data}
\description{Generates bivariate censored gap times from some known copula functions.}
\usage{data.gen(n, corr, dist, model.cens, cens.par, dist.par)}

\arguments{
  \item{n}{sample size.}
  \item{corr}{correlation parameter. Possible values for the bivariate exponential distribution are 0 (for independency) and 1 for a maximum correlation of 25\%. Any positive value is accepted for the bivariate Weibull distribution.}
  \item{dist}{distribution. Possible bivariate distributions are "exponential" and "weibull".}
  \item{model.cens}{model for censorship. Possible values are "uniform" and "exponential".}
  \item{cens.par}{parameter for the censorship distribution.}
  \item{dist.par}{vector of parameters for the allowed distributions. Two (rate) parameters for the bivariate exponential distribution and four (2 location parameters and 2 scale parameters) for the bivariate Weibull distribution. See details below.}
}
\details{
The bivariate exponential distribution, also known as Failie-Gumbel-Morgenstern distribution is given by
\deqn{F(x,y)=F_1(x)F_2(y)[1+\delta((1-F_1(x))(1-F_2(y)))]}{F(x,y)=F_1(x)F_2(y)[1+d((1-F_1(x))(1-F_2(y)))}
for \eqn{x\ge0}{x\ge0} and \eqn{y\ge0}{y\ge0}. Where the marginal distribution functions \eqn{F_1}{F_1} and \eqn{F_2}{F_2} are exponential with rate parameters \eqn{\lambda_1}{l_1} and \eqn{\lambda_2}{l_2} and correlation parameter \eqn{\delta}{d}. \cr

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
#Example for the bivariate Exponential distribution
data.gen(n=100, corr=1, dist="exponential", model.cens="uniform",
cens.par=3, dist.par=c(1,1))
#Example for distribution weibull
data.gen(n=100, corr=1, dist="weibull", model.cens="uniform",
cens.par=3, dist.par=c(2,7,2,7)) 
}