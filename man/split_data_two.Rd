\name{split_data_two}
\alias{split_data_two}
\title{Split Data into Two Subsets}

\description{
Splits a data frame into exploratory and confirmatory subsets.
}

\usage{
split_data_two(df, perc_exploratorio = 0.5, perc_confirmatorio = 0.5, seed = NULL)
}

\arguments{
  \item{df}{Data frame to split.}
  \item{perc_exploratorio}{Proportion for exploratory subset (default 0.5).}
  \item{perc_confirmatorio}{Proportion for confirmatory subset (default 0.5).}
  \item{seed}{Random seed for reproducibility (default NULL).}
}

\value{
A list with exploratorio and confirmatorio data frames.
}

\examples{
\dontrun{
result <- split_data_two(mydata)
}
}

\author{
Dr. Jose Ventura-Leon
}

