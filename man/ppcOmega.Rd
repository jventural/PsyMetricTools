\name{ppcOmega}
\alias{ppcOmega}
\title{Posterior Predictive Check for Omega 1-Factor Model}

\description{
This function is based on the first version of the Bayesrel library published on January 20, 2019 by author Julius Pfadt and Donvanden Bergh. However, since it does not appear in the latest version of the Bayesrel library, it has been modified to be compatible with it.

This function, inspired by the methodologies used in the Bayesrel library, performs a posterior predictive check for the Omega 1-Factor Model. It is not originally included in the latest version of the Bayesrel library but has been adapted for compatibility. The function focuses on assessing the fit of a one-factor model by comparing the observed eigenvalues of the data with those simulated from the model's Bayesian posterior estimates. This comparison aids in understanding the adequacy of the one-factor model in capturing the underlying structure of the data.
}

\usage{
ppcOmega(data, res)
}

\arguments{
  \item{data}{A data frame or matrix of observed variables.}
  \item{res}{An object containing Bayesian model results, specifically the posterior distributions of loadings and residual variances.}
}

\details{
The function `ppcOmega` calculates the eigenvalues from the observed data and from simulations based on the Bayesian posterior estimates of loadings and residual variances. It uses these eigenvalues to create a visual comparison between the observed data structure and the structure implied by the model. This graphical representation is crucial for evaluating the fit of the one-factor model and understanding its implications in the context of the data.
}

\value{
A ggplot object that displays a scatter plot of observed eigenvalues and error bars representing the range of simulated eigenvalues. This plot provides a visual assessment of how well the one-factor model captures the variance structure of the data.
}

\examples{
# Assuming 'data' is a dataset and 'model_results' contains Bayesian posterior distributions
ppcOmega(data, model_results)
}

\author{
Professor José Ventura León
}
