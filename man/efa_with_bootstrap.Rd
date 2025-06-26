\name{efa_with_bootstrap}
\alias{efa_with_bootstrap}
\title{Exploratory Factor Analysis with Optional Bootstrap Assessment}
\description{
The efa_with_bootstrap function performs an exploratory factor analysis (EFA) on a dataset using lavaan, with options to rotate the solution and apply loading thresholds. When enabled, it also conducts a bootstrap procedure to assess the stability of fit indices, factor loadings, and interfactor correlations across resampled datasets.
}
\usage{
efa_with_bootstrap(n_factors,
n_items,
name_items,
data,
apply_threshold,
estimator = "WLSMV",
rotation = "oblimin",
exclude_items = NULL,
bootstrap = FALSE,
n_bootstrap = 1000,
bootstrap_seed = 123)
}
\arguments{
\item{n_factors}{An integer specifying the number of factors to extract in the EFA.}
\item{n_items}{An integer giving the total number of observed variables (items) in the analysis.}
\item{name_items}{A character vector of length \code{n_items} containing the column names of the items in \code{data}.}
\item{data}{A data frame with at least \code{n_items} columns corresponding to the observed variables.}
\item{apply_threshold}{A numeric value (e.g., 0.30) to threshold small factor loadings in the standardized solution.}
\item{estimator}{A character string specifying the estimator for \code{lavaan} (default \code{"WLSMV"}).}
\item{rotation}{A character string indicating the rotation method to apply (default \code{"oblimin"}).}
\item{exclude_items}{An optional character vector of item names to exclude from the analysis (default \code{NULL}).}
\item{bootstrap}{Logical; if \code{TRUE}, perform \code{n_bootstrap} bootstrap replications (default \code{FALSE}).}
\item{n_bootstrap}{An integer giving the number of bootstrap samples to draw when \code{bootstrap = TRUE} (default 1000).}
\item{bootstrap_seed}{An integer seed for reproducibility of the bootstrap sampling (default 123).}
}
\details{
Upon invocation, \code{efa_with_bootstrap} first fits an exploratory factor model to the full dataset via \code{lavaan}, extracting fit indices, the standardized loading matrix (with thresholding), raw loadings, and the interfactor correlation matrix. If \code{bootstrap = TRUE}, the function then resamples the data with replacement \code{n_bootstrap} times and reruns the EFA on each bootstrap sample. Successful iterations are collected, and summary statistics (means, medians, standard deviations, quantiles, and confidence intervals) are computed for:
\itemize{
\item Fit indices (e.g., RMSEA, CFI).
\item Factor loadings for each item–factor combination.
\item Interfactor correlations across all factor pairs.
}
Internally, progress is shown via a text progress bar, and failed iterations are skipped.
}
\value{
A list with components:
\itemize{
\item \code{Bondades_Original}: Fit indices from the original EFA.
\item \code{Specifications}: The lavaan model objects used for the original analysis.
\item \code{InterFactor}: The original interfactor correlation matrix.
\item \code{result_df}: A data frame of thresholded standardized loadings from the original model.
\item \code{Bootstrap} (only if \code{bootstrap = TRUE}): A list containing:
\itemize{
\item \code{n_bootstrap_successful}: Number of successful bootstrap iterations.
\item \code{fit_indices_summary}: Summary statistics for each fit index.
\item \code{loadings_summary}: A list of per-factor loading summaries (means, SD, CIs, etc.).
\item \code{interfactor_summary}: Summary of interfactor correlation statistics.
\item \code{global_loadings_summary}: Combined item-level loading summaries across all factors.
\item \code{global_interfactor_summary}: Combined summaries of all interfactor correlations.
\item \code{raw_bootstrap_data}: Raw lists of \code{bondades}, \code{loadings}, and \code{interfactor} matrices.
}
}
}
\examples{
library(lavaan)
library(dplyr)

# Simulate example data: 200 observations, 6 items
set.seed(42)
example_data <- data.frame(
i1 = rnorm(200), i2 = rnorm(200), i3 = rnorm(200),
i4 = rnorm(200), i5 = rnorm(200), i6 = rnorm(200)
)

# Perform EFA with 2 factors, threshold loadings at 0.30, no bootstrap
results <- efa_with_bootstrap(
n_factors = 2,
n_items = 6,
name_items = paste0("i", 1:6),
data = example_data,
apply_threshold = 0.30,
estimator = "WLSMV",
rotation = "oblimin",
bootstrap = FALSE
)

# Perform EFA with bootstrap stability assessment (200 replications)
results_boot <- efa_with_bootstrap(
n_factors = 2,
n_items = 6,
name_items = paste0("i", 1:6),
data = example_data,
apply_threshold = 0.30,
estimator = "WLSMV",
rotation = "oblimin",
bootstrap = TRUE,
n_bootstrap = 200,
bootstrap_seed = 2025
)

head(results_boot$Bootstrap$fit_indices_summary$summary)
}
\author{
Dr. José Ventura-León
}
