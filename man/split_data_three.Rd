\name{split_data_three}
\alias{split_data_three}
\title{Divide Data into Pilot, Exploratory, and Confirmatory Sets}

\description{
This function divides a dataset into three subsets intended for pilot testing, exploratory analysis, and confirmatory analysis, respectively. It ensures a reproducible partition of the dataset according to specified proportions. This is particularly useful in research designs where different stages require separate datasets, such as in the development and validation of questionnaires or models.
}

\usage{
split_data_three(df, perc_piloto = 0.10, perc_exploratorio = 0.45, perc_confirmatorio = 0.45, seed = NULL)
}

\arguments{
  \item{df}{The dataset to be split into subsets.}
  \item{perc_piloto}{The proportion of data to be used for the pilot subset. Default is 0.10.}
  \item{perc_exploratorio}{The proportion of data to be used for the exploratory subset. Default is 0.45.}
  \item{perc_confirmatorio}{The proportion of data to be used for the confirmatory subset. Default is 0.45.}
  \item{seed}{An optional seed for the random number generator to ensure reproducibility. Default is NULL, which does not set a seed.}
}

\details{
The function first checks that the provided proportions sum to 1, throwing an error if they do not. After optionally setting a seed for reproducibility, it calculates the number of observations for each subset, rounding down to ensure integer counts. It then randomly shuffles the dataset and splits it into the three subsets according to the calculated sizes. This allows for a randomized and reproducible partitioning of the data for different phases of research.
}

\value{
A list containing three elements:
\itemize{
  \item{piloto}{A dataframe containing the pilot subset of the data.}
  \item{exploratorio}{A dataframe containing the exploratory subset of the data.}
  \item{confirmatorio}{A dataframe containing the confirmatory subset of the data.}
}
Each subset can be directly accessed by its name in the list.
}

\examples{
# Assuming 'df' is a dataframe that you wish to split
data_partitions <- split_data_three(df)

# To access each subset
pilot_data <- data_partitions$piloto
exploratory_data <- data_partitions$exploratorio
confirmatory_data <- data_partitions$confirmatorio
}

\author{
Professor José Ventura León
}
