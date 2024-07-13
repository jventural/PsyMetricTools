\name{boot_and_evaluate}
\alias{boot_and_evaluate}
\title{Bootstrapping and Evaluation of Network Estimation Algorithms}

\description{
This function performs bootstrapping and evaluation of network estimation algorithms using various correlation methods and sample sizes. It generates performance metrics for each combination of algorithm, correlation method, and sample size over multiple simulations. The results provide a comprehensive assessment of the algorithms' performance in terms of sensitivity, specificity, precision, correlation, absolute correlation, and bias.
}

\usage{
boot_and_evaluate(data, true_network, algorithms, correlation_methods,
                  sample_sizes = c(100, 250, 500, 1000), n_simulations = 100, seed = 123, n_cores = 8)
}

\arguments{
  \item{data}{A dataframe containing the dataset to be used for bootstrapping.}
  \item{true_network}{An EGAnet object where the real network will be extracted.}
  \item{algorithms}{A character vector specifying the network estimation algorithms to be evaluated.}
  \item{correlation_methods}{A character vector specifying the correlation methods to be used in network estimation.}
  \item{sample_sizes}{A numeric vector indicating the sample sizes to be used in each simulation. Default is \code{c(100, 250, 500, 1000)}.}
  \item{n_simulations}{An integer indicating the number of simulations to be performed. Default is 100.}
  \item{seed}{An integer for setting the seed for reproducibility. Default is 123.}
  \item{n_cores}{An integer that depends on the number of cores in the user's computer. Default is 8.}
}

\details{
The function begins by loading the necessary libraries and defining internal helper functions for calculating correlation, bias, and network performance metrics. It then initializes an empty list to store the results and sets the random seed for reproducibility.

For each simulation, the function iterates over the specified sample sizes, algorithms, and correlation methods. It samples the data according to the current sample size, estimates the network using the current algorithm and correlation method, and then evaluates the performance of the estimated network against the true network using various metrics.

The performance metrics include sensitivity, specificity, precision, correlation, absolute correlation, and bias. The results of each simulation are compiled into a dataframe, which is then returned as the final output.
}

\value{
A dataframe containing the performance metrics for each combination of algorithm, correlation method, sample size, and simulation. The metrics include sensitivity, specificity, precision, correlation, absolute correlation, and bias.
}

\examples{
# Example usage with a sample dataset and true network

# Upload data
data_bfi <- psych::bfi
data_bfi <- dplyr::select(data_bfi, -gender, -education, -age)
data_bfi <- na.omit(data_bfi)

# Examining dimensions
ega.wmt <- EGA(data = data_bfi,
               iter = 1000,
               model = "glasso",
               corr = "pearson",
               algorithm = "walktrap")

# Network matrix
ega.wmt$network

# Running the simulation and evaluation
results <- boot_and_evaluate(
  true_network = ega.wmt,
  data = data_bfi,
  sample_sizes = seq(100, 500, by = 100),
  algorithms = c("walktrap", "leiden", "louvain", "fast_greedy"),
  correlation_methods = c("pearson", "spearman", "cor_auto"),
  n_simulations = 100,
  seed = 2024,
  n_cores = 8
)

}

\author{
Dr. José Ventura-León
}
