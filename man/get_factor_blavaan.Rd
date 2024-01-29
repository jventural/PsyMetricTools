\name{get_factor_blavaan}
\alias{get_factor_blavaan}
\title{Extraction of Standardized Parameters and CrI for Bayesian Factor Models}

\description{
This function is tailored for Bayesian factor analysis models using the 'blavaan' package. It extracts and summarizes the standardized factor loadings and their 95 percent Credible Intervals (CrI). The function also provides the Potential Scale Reduction Factor (PSRF) statistic for each factor loading, assisting in assessing convergence in Bayesian models. This utility is particularly useful for interpreting the relationships between observed variables and latent factors in a Bayesian framework.
}

\usage{
get_factor_blavaan(fit_model, credMass = 0.95)
}

\arguments{
\item{fit_model}{A 'blavaan' model object, typically a Bayesian factor analysis model, from which standardized parameters are to be extracted.}
\item{credMass}{The credible mass for the Credible Interval calculation. Defaults to 0.95, representing a 95 percent credible interval.}
}

\details{
The function starts by extracting the PSRF statistic for factor loadings, indicating model convergence. It then retrieves the posterior distributions of the standardized factor loadings. For each loading, it calculates the mean and computes its 95 percent Credible Interval using the HDInterval package. The inclusion of PSRF values offers additional insights into the reliability and convergence status of each factor loading in the model.
}

\value{
A data frame containing factor names, their posterior means, 95 percent Credible Intervals, and PSRF values. This detailed summary enables a clear interpretation of factor loadings within Bayesian factor analysis models, highlighting the estimated magnitude and uncertainty of each loading.
}

\examples{

Assuming 'bayes_factor_model' is a Bayesian factor analysis model fitted with blavaan
factor_summary <- get_factor_blavaan(bayes_factor_model, credMass = 0.95)
print(factor_summary)
}

\author{
Professor José Ventura León
}
