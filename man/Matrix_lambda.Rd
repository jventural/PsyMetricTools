\name{Matrix_lambda}
\alias{Matrix_lambda}
\title{Factor Loadings Matrix Creation Tool}

\description{
This function generates a factor loadings matrix (Lambda) based on the specified number of items, factors, and block sizes. It is designed to facilitate the creation of structured matrices commonly used in factor analysis and structural equation modeling (SEM). This tool is particularly useful for defining the relationships between observed variables and latent factors in a clear, organized manner.
}

\usage{
Matrix_lambda(n_items, factores, n_tamanos)
}

\arguments{
  \item{n_items}{An integer representing the total number of observed variables (items).}
  \item{factores}{An integer representing the total number of latent factors.}
  \item{n_tamanos}{A numeric vector specifying the size of each block of items corresponding to each factor. The length of this vector must match the number of factors, and the sum of its elements must equal the number of items.}
}

\details{
The function begins by validating the input parameters to ensure that the length of `n_tamanos` matches the number of `factores`, and that the sum of `n_tamanos` equals the number of `n_items`. It then creates a matrix of zeros with dimensions specified by `n_items` and `factores`. The matrix is populated with ones in the appropriate blocks as specified by `n_tamanos`, representing the factor loadings. This structured approach helps in clearly defining which items load onto which factors, a fundamental step in confirmatory factor analysis and other SEM applications.
}

\value{
A matrix of size with ones in specified blocks indicating factor loadings, and zeros elsewhere. This matrix can be used directly in SEM software packages for model specification.
}

\examples{
# Example usage with 25 items and 5 factors, each factor associated with 5 items
Lambda <- Matrix_lambda(25, 5, c(5, 5, 5, 5, 5))
print(Lambda)
}

\author{
José Ventura-León
}
