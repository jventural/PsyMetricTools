\name{split_data_two}
\alias{split_data_two}
\title{Divide Data into Exploratory and Confirmatory Sets}

\description{
This function divides a dataset into two subsets intended for exploratory analysis and confirmatory analysis, respectively. It ensures a reproducible partition of the dataset according to specified proportions. This method is particularly useful in research designs where the dataset needs to be split for initial exploration and subsequent validation or confirmation, facilitating a streamlined approach to data analysis without a pilot testing phase.
}

\usage{
split_data_two(df, perc_exploratorio = 0.5, perc_confirmatorio = 0.5, seed = NULL)
}

\arguments{
  \item{df}{The dataset to be split into subsets.}
  \item{perc_exploratorio}{The proportion of data to be used for the exploratory subset. Default is 0.5.}
  \item{perc_confirmatorio}{The proportion of data to be used for the confirmatory subset. Default is 0.5.}
  \item{seed}{An optional seed for the random number generator to ensure reproducibility. Default is NULL, which does not set a seed.}
}

\details{
The function verifies that the provided proportions sum to 1, issuing an error if they do not. After optionally setting a seed for reproducibility, it calculates the number of observations for each subset, using floor rounding to ensure integer counts. The dataset is then randomly shuffled and split into the two subsets according to the calculated sizes. This process allows for a randomized and reproducible partitioning of the data, suitable for the distinct phases of exploratory and confirmatory research.
}

\value{
A list containing two elements:
\itemize{
  \item{exploratorio}{A dataframe containing the exploratory subset of the data.}
  \item{confirmatorio}{A dataframe containing the confirmatory subset of the data.}
}
Each subset can be directly accessed by its name in the list.
}

\examples{
# Assuming 'df' is a dataframe you wish to split
data_partitions <- split_data_two(df)

# To access each subset
exploratory_data <- data_partitions$exploratorio
confirmatory_data <- data_partitions$confirmatorio
}

\author{
Professor José Ventura León
}
