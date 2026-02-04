\name{boot_cfa}
\alias{boot_cfa}
\title{Bootstrap Confirmatory Factor Analysis (CFA) on a Dataset}

\description{
  This function performs a bootstrap-based Confirmatory Factor Analysis (CFA) on a given dataset to assess the stability and reliability of measurement models across multiple replications. By resampling the dataset with replacement, it computes CFA models to evaluate the fit and reliability of the constructs within the data, using specified model parameters and item prefixes. This approach is ideal for robustly estimating model parameters and assessing the reliability of measurement constructs in the presence of sampling variability.
}

\usage{
boot_cfa(new_df, model_string, item_prefix, seed = 2023,
         n_replications = 1000, ordered = TRUE,
         estimator = "WLSMV")
}

\arguments{
  \item{new_df}{The dataset on which the bootstrap CFA will be conducted.}
  \item{model_string}{A string representing the CFA model specification.}
  \item{item_prefix}{Prefix of the items to be included in the CFA model.}
  \item{seed}{An optional seed for the random number generator to ensure reproducibility. Default is 2023.}
  \item{n_replications}{The number of bootstrap replications to perform. Default is 1000.}
  \item{ordered}{Logical indicating whether the data is ordinal. Default is TRUE.}
  \item{estimator}{The estimator to be used in the CFA. Default is "WLSMV".}
}

\details{
  The function initializes by setting a seed for reproducibility and loading necessary libraries. It then replicates the input dataset `n_replications` times with replacement, prepares the data for CFA modeling by filtering based on item prefixes, and computes the CFA models. Post-calculation, it assesses the convergence, validity, and fit of each model replication, summarizing these through various fit measures. Finally, it calculates the reliability of the measurement model using omega coefficients, producing a comprehensive evaluation of the model's performance across bootstrap samples.
}

\value{
  A data frame containing the results of the bootstrap CFA analyses for each replication, including convergence status, model validity, fit measures, and omega reliability coefficients. This comprehensive output facilitates in-depth evaluation of the measurement model's stability and reliability across different samples.
}

\examples{
  model_original <- "F1 =~ PHQ1+PHQ2+PHQ3+PHQ4+PHQ5+PHQ6+PHQ7+PHQ8"

  # Assuming 'my_dataset' is your dataset, and you have a model string and item prefix defined
  results <- boot_cfa(new_df = my_dataset,
                      model_string = model_original,
                      item_prefix = "PHQ",
                      seed = 2023,
                      n_replications = 1000,
                      ordered = TRUE,
                      estimator = "WLSMV")

  # To view the summary of CFA results
  summary(results)
}

\author{
  Dr. José Ventura-León
}

