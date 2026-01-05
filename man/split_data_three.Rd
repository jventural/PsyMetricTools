\name{split_data_three}
\alias{split_data_three}
\title{Split Data into Three Subsets}

\description{
Splits a data frame into pilot, exploratory and confirmatory subsets.
}

\usage{
split_data_three(df, perc_piloto = 0.1, perc_exploratorio = 0.45,
                 perc_confirmatorio = 0.45, seed = NULL)
}

\arguments{
  \item{df}{Data frame to split.}
  \item{perc_piloto}{Proportion for pilot subset (default 0.1).}
  \item{perc_exploratorio}{Proportion for exploratory subset (default 0.45).}
  \item{perc_confirmatorio}{Proportion for confirmatory subset (default 0.45).}
  \item{seed}{Random seed for reproducibility (default NULL).}
}

\value{
A list with piloto, exploratorio and confirmatorio data frames.
}

\examples{
\dontrun{
result <- split_data_three(mydata)
}
}

\author{
Dr. Jose Ventura-Leon
}

