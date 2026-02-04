\name{summarise_column}
\alias{summarise_column}
\title{Summary Statistics for Specified Column Excluding Zeros}

\description{
  The `summarise_column` function computes summary statistics (mean, standard deviation, minimum, and maximum) for a specified column in a dataframe, excluding any zero values. This function is designed to provide a quick overview of non-zero data distributions, aiding in the evaluation of variable characteristics while ignoring non-applicable or absent observations.
}

\usage{
summarise_column(data, col_name)
}

\arguments{
  \item{data}{A dataframe containing the observed variables to be analyzed.}
  \item{col_name}{The name of the column for which summary statistics are to be calculated, provided as a string.}
}

\details{
  This function first filters out zero values from the specified column to ensure that summary statistics reflect only non-zero observations. It then calculates the mean, standard deviation, minimum, and maximum values for the remaining data, providing a concise statistical summary. This process helps in identifying key characteristics of the data distribution, such as central tendency and variability, while excluding potentially misleading zero-value entries.
}

\value{
  Returns a dataframe with a single row containing the calculated summary statistics for the specified column. The dataframe also includes a column indicating the name of the analyzed variable, facilitating the identification of the statistics in the context of larger analyses or when processing multiple variables sequentially.
}

\examples{
# Assuming 'dataFrame' is a dataframe with numerical columns
summaryStatsF1 <- summarise_column(dataFrame, "f1")
print(summaryStatsF1)  # Display summary statistics for column 'f1'

summaryStatsF2 <- summarise_column(dataFrame, "f2")
print(summaryStatsF2)  # Display summary statistics for column 'f2'
}

\author{
Dr. José Ventura-León
}
