\name{split_data_stratified_clustered}
\alias{split_data_stratified_clustered}
\title{Divide Data into Stratified and Clustered Exploratory and Confirmatory Sets}

\description{
This function divides a dataset into two subsets intended for exploratory and confirmatory analysis, respectively, based on stratification and clustering criteria. It is designed to ensure a reproducible and methodologically sound partition of the dataset according to specified proportions and grouping variables. This approach is particularly useful in complex survey designs or studies that require careful consideration of population subgroups, enhancing the representativeness and relevance of both exploratory and confirmatory analyses.
}

\usage{
split_data_stratified_clustered(df_filtered, strat_var = "Region_reside", cluster_var = "Facultad", perc_exploratorio = 0.5, seed = NULL)
}

\arguments{
  \item{df_filtered}{The dataset to be split into subsets.}
  \item{strat_var}{The variable used for stratification. Default is "Region_reside".}
  \item{cluster_var}{The variable used for clustering. Default is "Facultad".}
  \item{perc_exploratorio}{The proportion of data to be used for the exploratory subset. Default is 0.5.}
  \item{seed}{An optional seed for the random number generator to ensure reproducibility. Default is NULL.}
}

\details{
After setting an optional seed for reproducibility, the function creates a unique grouping variable by interacting the specified stratification and clustering variables. It then divides the dataset within each group according to the specified proportion for exploratory analysis. This process ensures that the resulting subsets are representative across the specified strata and clusters, facilitating more nuanced and accurate exploratory and confirmatory analyses.
}

\value{
A list containing two elements:
\itemize{
  \item{exploratorio:}{ A dataframe containing the stratified and clustered exploratory subset of the data.}
  \item{confirmatorio:}{ A dataframe containing the remaining data for confirmatory analysis, following the same stratification and clustering criteria.}
}
These subsets can be directly accessed by their names in the list, supporting seamless integration into further analysis workflows.
}

\examples{
# Assuming 'df_filtered' is a dataframe you wish to split with variables 'Region_reside' and 'Facultad'
resultados <- split_data_stratified_clustered(df_filtered,
                                              strat_var = "Region_reside",
                                              cluster_var = "Facultad",
                                              perc_exploratorio = 0.5, seed = 2024)

# To access each subset
exploratory_data <- resultados$exploratorio
confirmatory_data <- resultados$confirmatorio
}

\author{
Professor José Ventura León
}
