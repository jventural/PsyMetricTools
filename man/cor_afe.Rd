\name{cor_afe}
\alias{cor_afe}
\title{Extract Factor Correlations from Factor Analysis}

\description{
Extracts the factor correlation matrix (Phi) from a factor analysis object.
}

\usage{
cor_afe(x)
}

\arguments{
  \item{x}{A factor analysis object containing a Phi matrix.}
}

\value{
A correlation matrix of factors rounded to 2 decimal places.
}

\examples{
\dontrun{
correlations <- cor_afe(fa_result)
print(correlations)
}
}

\author{
Dr. Jose Ventura-Leon
}

