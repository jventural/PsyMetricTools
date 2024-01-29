\name{get_combined_Fit_Indices_Blavaan}
\alias{get_combined_Fit_Indices_Blavaan}
\title{Aggregate Fit Indices for Multiple Bayesian SEM Models}

\description{
This function is designed to aggregate and summarize key fit indices from a list of Bayesian Structural Equation Models (SEM) fitted using the 'blavaan' package. It provides an efficient way to analyze and compare multiple models simultaneously. The function calculates mean values for chi-square (chisq), number of estimated parameters (pD), degrees of freedom (df), Bayesian RMSEA (BRMSEA), and Bayesian CFI (BCFI) for each model. Additionally, it computes the Highest Density Intervals (HDI) for both BRMSEA and BCFI, offering a comprehensive comparison across different models.
}

\usage{
get_combined_Fit_Indices_Blavaan(models_list, credMass = 0.95)
}

\arguments{
\item{models_list}{A list of 'blavaan' model objects from which fit indices are to be extracted and combined.}
\item{credMass}{The credible mass for the Highest Density Interval (HDI) calculation. Defaults to 0.95, representing a 95 percent credible interval}
}

\details{
For each model in the provided list, the function calculates mean values of chisq, pD, df, BRMSEA, and BCFI. It also employs the HDInterval package to compute the HDIs for BRMSEA and BCFI. These aggregated statistics enable a comprehensive assessment of multiple models' goodness-of-fit, parameter estimates, and overall reliability. The HDIs provide a Bayesian perspective on the uncertainty and precision of the BRMSEA and BCFI estimates across different models.
}

\value{
A data frame where each row corresponds to one model from the input list, containing the mean values of chisq, pD, df, BRMSEA, and BCFI, along with their respective HDIs. This format facilitates easy comparison and evaluation of multiple models' fit indices.
}

\examples{

Assuming 'model_list' is a list of Bayesian SEM models fitted with blavaan
combined_fit_summary <- get_combined_Fit_Indices_Blavaan(model_list, credMass = 0.95)
print(combined_fit_summary)
}

\author{
Professor José Ventura León
}
