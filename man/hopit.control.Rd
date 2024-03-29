% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hopit.R
\name{hopit.control}
\alias{hopit.control}
\title{Auxiliary for controlling the fitting of a \code{hopit} model}
\usage{
hopit.control(
  grad.eps = 3e-05,
  bgfs.maxit = 10000,
  cg.maxit = 10000,
  nlm.maxit = 150,
  bgfs.reltol = 5e-10,
  cg.reltol = 5e-10,
  nlm.gradtol = 1e-07,
  nlm.steptol = 1e-07,
  fit.methods = "BFGS",
  nlm.fit = FALSE,
  trace = TRUE,
  transform.latent = "none",
  transform.thresh = "none"
)
}
\arguments{
\item{grad.eps}{an epsilon parameter ("a very small number") used to calculate the Hessian from the gradient function.}

\item{bgfs.maxit, cg.maxit, nlm.maxit}{the maximum number of iterations.
See \code{\link{optim}} and \code{\link{nlm}} for details.}

\item{bgfs.reltol, cg.reltol}{the relative convergence tolerances for the BFGS and the CG methods.
See \code{\link{optim}} for details.}

\item{nlm.gradtol, nlm.steptol}{a tolerance at which the scaled gradient is
considered close enough to zero and
a minimum allowable relative step length for the nlm method. See \code{\link{nlm}}.}

\item{fit.methods}{"CG", "BFGS", or both. If both, the CG is run first, followed by the BFGS. See \code{\link{optim}}.}

\item{nlm.fit}{a logical; if FALSE (default) the \code{nlm} optimization method
is omitted and only the BFGS and/or the CG methods are run.}

\item{trace}{a logical for whether to trace the process of model fitting.}

\item{transform.latent, transform.thresh}{a type of transformation applied to
the all of the latent's or all of the threshold's numeric variables. Possible values:
\itemize{
\item "none" : no transformation
\item "min" : subtract the minimum from a variable
\item "scale_01" : transform the variable to fit the range from 0 to 1
\item "standardize" or "standardise" : subtract the mean from a variable then divide it by its standard deviation
\item "standardize_trunc" or "standardise_trunc" : subtract the minimum from a variable then divide it by its standard deviation
}}
}
\description{
An auxiliary function for controlling the fitting of a \code{hopit} model.
Use this function to set the control
parameters of the \code{\link{hopit}} and other related functions.
}
\seealso{
\code{\link{hopit}}
}
\author{
Maciej J. Danko
}
