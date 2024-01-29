\name{calculate_mcmc_standard_error}
\alias{calculate_mcmc_standard_error}
\title{Calculation of MCMC Standard Error for Bayesian Posterior Distributions}

\description{
This function calculates the Markov Chain Monte Carlo (MCMC) standard error for posterior distributions obtained from Bayesian models. It is designed to work with matrices or data frames containing posterior samples. The function provides a critical assessment of the precision of MCMC estimates, which is essential in Bayesian analysis to ensure the robustness and reliability of the results.
}

\usage{
calculate_mcmc_standard_error(stpost1)
}

\arguments{
\item{stpost1}{A matrix or data frame of posterior samples from a Bayesian model. Each column should represent a different parameter or variable.}
}

\details{
The function iterates over each column (parameter) of the input matrix or data frame, applying the mcse function from the mcmcse package to calculate the standard error for each parameter's posterior distribution. It also performs a check to determine if the standard errors are within an acceptable range (less than 5% of the standard deviation of the posterior samples). The function outputs the minimum and maximum calculated standard errors and a formatted list of all standard errors for further analysis.
}

\value{
A list containing the minimum and maximum standard errors calculated (excluding zero values), a logical vector indicating if each parameter's standard error is within the acceptable range, and a formatted vector of all calculated standard errors. This comprehensive output allows for an effective evaluation of the precision of MCMC estimates in Bayesian models.
}

\examples{
se_results <- calculate_mcmc_standard_error(posterior_samples)
print(se_results)
}

\author{
Professor José Ventura León
}







