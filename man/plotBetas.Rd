% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotGlmnet.R
\name{plotBetas}
\alias{plotBetas}
\title{Plot the coefficients from glmnet}
\usage{
plotBetas(glmnet, labelLambda = 0, ylab = "Coefficient",
  transformFunc = function(x) x, minBeta = 0,
  xlim = rev(range(log10(glmnet$lambda))) + c(0, -0.2), ylim = range(betas[,
  inXlim]), ...)
}
\arguments{
\item{glmnet}{a glmet object from \code{\link[glmnet]{glmnet}}}

\item{labelLambda}{label all variables that are not 0 at this the closest lambda <= labelLambda}

\item{ylab}{label for y axis}

\item{transformFunc}{a functions to adjust y-axis labels (e.g. \code{exp} to show the axis as e^beta or \code{function(x)x^2} for 2^beta)}

\item{minBeta}{the minimum absolute necessary to count beta as different from zero}

\item{xlim}{minimum and maximum for limits of the x axis. Leave empty to set automatically}

\item{ylim}{minimum and maximum for limits of the y axis. Leave empty to set automatically}

\item{...}{additional arguments for \code{\link{plot}}}
}
\description{
Take a glmnet object and plot out the path of the coefficient for each variable as model complexity increases.
}
\examples{
#example from cv.glmet
set.seed(1010)
n=1000;p=100
nzc=trunc(p/10)
x=matrix(rnorm(n*p),n,p)
beta=rnorm(nzc)
fx= x[,seq(nzc)] \%*\% beta
eps=rnorm(n)*5
y=drop(fx+eps)
px=exp(fx)
px=px/(1+px)
ly=rbinom(n=length(px),prob=px,size=1)
set.seed(1011)
cvob1=glmnet::cv.glmnet(x,y)
plotBetas(cvob1$glmnet.fit,cvob1$lambda.1se)
}
