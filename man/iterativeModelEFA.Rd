\name{iterativeModelEFA}
\alias{iterativeModelEFA}
\title{Iterative Model EFA}

\description{
Performs iterative EFA removing items with multiple loadings.
}

\usage{
iterativeModelEFA(data, n_factors, estimator = "WLSMV", name_items = "RAS",
                  n_items = NULL, specific_items = NULL, rotation = "oblimin",
                  verbose = FALSE)
}

\arguments{
  \item{data}{Data frame for EFA.}
  \item{n_factors}{Number of factors.}
  \item{estimator}{Estimator (default "WLSMV").}
  \item{name_items}{Item prefix (default "RAS").}
  \item{n_items}{Number of items (default NULL).}
  \item{specific_items}{Specific items (default NULL).}
  \item{rotation}{Rotation method (default "oblimin").}
  \item{verbose}{Verbose output (default FALSE).}
}

\value{
A list with final_model, final_items, excluded_items, and iteration details.
}

\examples{
\dontrun{
result <- iterativeModelEFA(mydata, n_factors = 3)
}
}

\author{
Dr. Jose Ventura-Leon
}

