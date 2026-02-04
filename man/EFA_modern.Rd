\name{EFA_modern}
\alias{EFA_modern}
\title{Modern Exploratory Factor Analysis}

\description{
Performs exploratory factor analysis using lavaan with rotation.
}

\usage{
EFA_modern(n_factors, n_items, name_items, data, apply_threshold,
           estimator = "WLSMV", rotation = "oblimin", exclude_items = NULL)
}

\arguments{
  \item{n_factors}{Number of factors to extract.}
  \item{n_items}{Number of items.}
  \item{name_items}{Item name prefix.}
  \item{data}{Data frame for analysis.}
  \item{apply_threshold}{Apply threshold to loadings.}
  \item{estimator}{Estimator (default "WLSMV").}
  \item{rotation}{Rotation method (default "oblimin").}
  \item{exclude_items}{Items to exclude (default NULL).}
}

\value{
A list with Bondades_Original, Specifications, InterFactor, and result_df.
}

\examples{
\dontrun{
results <- EFA_modern(n_factors = 3, n_items = 12,
                        name_items = "item", data = mydata,
                        apply_threshold = TRUE)
}
}

\author{
Dr. Jose Ventura-Leon
}

