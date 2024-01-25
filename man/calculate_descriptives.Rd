\name{calculate_descriptives}
\alias{calculate_descriptives}
\title{Calculation of descriptive statistics}

\description{
This function calculates descriptive statistics for a specified range of variables within a dataset. It is designed to provide a comprehensive summary of the data, including measures such as mean, standard deviation, minimum, maximum, skewness, kurtosis, and the percentage of the mean to the maximum value. This function is particularly useful in the initial stages of data analysis and for understanding the distribution of each variable.
}

\usage{
calculate_descriptives(data, start_col, end_col)
}

\arguments{
  \item{data}{The data frame containing the variables to be analyzed.}
  \item{start_col}{The name of the first column in the range of variables to be included in the analysis.}
  \item{end_col}{The name of the last column in the range of variables to be included in the analysis.}
}

\details{
The function selects the range of columns specified, computes the descriptive statistics using the `psych` package, and then formats the results into a reader-friendly format. The percentage column represents the mean as a percentage of the maximum value for each variable.
}

\value{
A data frame containing the descriptive statistics for each variable in the specified range. The columns include the mean (Media), standard deviation (DE), minimum (Min.), maximum (Max.), skewness (g1), kurtosis (g2), and percentage mean to maximum ("%").
}

\examples{
descriptives <- calculate_descriptives(data, "Depression", "Anxiety")
print(descriptives)
}

\author{
Professor José Ventura León
}
