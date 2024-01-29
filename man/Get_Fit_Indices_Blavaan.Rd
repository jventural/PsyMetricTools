\name{Get_Fit_Indices_Blavaan}
\alias{Get_Fit_Indices_Blavaan}
\title{Extraction of Fit Indices and HDI Intervals for Bayesian SEM Models}

\description{
This function is designed for extracting and summarizing key fit indices from Bayesian Structural Equation Models (SEM) fitted with the 'blavaan' package. It calculates the mean values for chi-square (chisq), the number of estimated parameters (pD), degrees of freedom (df), posterior predictive p-value (ppp), Bayesian RMSEA (BRMSEA), and Bayesian CFI (BCFI). Additionally, it computes the Highest Density Intervals (HDI) for BRMSEA and BCFI, providing a comprehensive overview of the model's fit.
}

\usage{
Get_Fit_Indices_Blavaan(model, credMass = 0.95)
}

\arguments{
\item{model}{A 'blavaan' model object from which fit indices are to be extracted.}
\item{credMass}{The credible mass for the Highest Density Interval (HDI) calculation. Defaults to 0.95, representing a 95 percent credible interval.}
}

\details{
The function first calculates mean values of chisq, pD, df, BRMSEA, and BCFI from the model's outputs. It then utilizes the HDInterval package to compute the HDIs for BRMSEA and BCFI, presenting these as formatted strings. These statistics provide insights into the model's goodness-of-fit, parameter estimates, and overall reliability. The HDIs offer a Bayesian approach to understanding the uncertainty and precision of the BRMSEA and BCFI estimates.
}

\value{
A data frame with a single row, containing the mean values of chisq, pD, ppp, df, BRMSEA, and BCFI, along with their respective HDIs. This summary provides a concise view of the model's performance and fit indices.
}

\examples{

Assuming 'bayes_model' is a Bayesian SEM model fitted with blavaan
fit_summary <- Get_Fit_Indices_Blavaan(bayes_model, credMass = 0.95)
print(fit_summary)
}

\author{
Professor José Ventura León
}
