\name{get_combined_Fit_Indices_Blavaan2}
\alias{get_combined_Fit_Indices_Blavaan2}
\title{Aggregate Fit Indices for Multiple Bayesian SEM Models with Custom Fit Objects}

\description{
This function aggregates and summarizes key fit indices from a list of Bayesian Structural Equation Models (SEM) fitted using the 'blavaan' package, alongside a corresponding list of custom fit objects. It facilitates the analysis and comparison of multiple models simultaneously by calculating mean values and Highest Density Intervals (HDI) for critical fit indices including chi-square (chisq), number of estimated parameters (pD), degrees of freedom (df), posterior predictive p-value (ppp), Bayesian RMSEA (BRMSEA), and Bayesian CFI (BCFI).
}

\usage{
get_combined_Fit_Indices_Blavaan(models_list, credMass = 0.95, fits)
}

\arguments{
\item{models_list}{A list of 'blavaan' model objects from which fit indices are to be extracted and combined.}
\item{credMass}{The credible mass for the Highest Density Interval (HDI) calculation, with a default of 0.95, representing a 95 percent credible interval.}
\item{fits}{A list of custom fit objects corresponding to each 'blavaan' model in 'models_list'. These objects are used for extracting the posterior predictive p-value (ppp) and other relevant fit measures.}
}

\details{
The function computes mean values of chisq, pD, df, BRMSEA, and BCFI for each model in 'models_list', utilizing the respective fit object from 'fits'. It leverages the HDInterval package to calculate the HDIs for BRMSEA and BCFI, providing an in-depth comparison of the models' fit. This comprehensive assessment aids in evaluating the goodness-of-fit, parameter estimates, and overall reliability of multiple Bayesian SEM models, with HDIs offering insights into the uncertainty and precision of the fit indices estimates.
}

\value{
A data frame where each row corresponds to one model from 'models_list', containing mean values of chisq, pD, df, ppp, BRMSEA, and BCFI, along with their respective HDIs. This structured format enables a straightforward comparison and evaluation of the fit indices across multiple models.
}

\examples{
# Assuming 'model_list' is a list of Bayesian SEM models fitted with blavaan,
# and 'fits' is a corresponding list of custom fit objects for these models:
combined_fit_summary <- get_combined_Fit_Indices_Blavaan2(list(bnull, bfit1), credMass = 0.95, fit = c(fit0,fit1))
print(combined_fit_summary)
}

\author{
Professor José Ventura León
}

