\name{split_data_stratified_clustered}
\alias{split_data_stratified_clustered}
\title{Divide Data into Stratified and Clustered Sets}

\description{
Divides a dataset into exploratory and confirmatory subsets using stratified and clustered sampling.
}

\usage{
split_data_stratified_clustered(df, strat_var = "Region_reside",
                                cluster_var = "Facultad",
                                perc_exploratorio = 0.5, seed = NULL)
}

\arguments{
  \item{df}{Data frame to split.}
  \item{strat_var}{Stratification variable (default "Region_reside").}
  \item{cluster_var}{Cluster variable (default "Facultad").}
  \item{perc_exploratorio}{Proportion for exploratory subset (default 0.5).}
  \item{seed}{Random seed (default NULL).}
}

\value{
A list with exploratorio and confirmatorio data frames.
}

\examples{
\dontrun{
result <- split_data_stratified_clustered(mydata)
}
}

\author{
Dr. Jose Ventura-Leon
}

