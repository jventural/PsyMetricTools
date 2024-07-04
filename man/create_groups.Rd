\name{create_groups}
\alias{create_groups}
\title{Group Index Assignment Tool}

\description{
This function creates a list of groups with corresponding indices based on specified names and values. It is designed to facilitate the organization of observed variables into predefined groups, which is particularly useful in factor analysis, structural equation modeling (SEM), and network analysis. This tool provides a clear and structured method for defining groups of items or variables.
}

\usage{
create_groups(names, values)
}

\arguments{
  \item{names}{A character vector containing the names of the groups. Each name represents a different group.}
  \item{values}{A numeric vector specifying the number of items in each group. The length of this vector must match the length of `names`.}
}

\details{
The function begins by checking that the lengths of `names` and `values` are the same. It then initializes an empty list and iterates over the provided names and values to assign a sequence of indices to each group. The result is a structured list where each element is named after a group and contains a sequence of indices corresponding to the items in that group. This structured grouping is essential for various multivariate statistical techniques.
}

\value{
A list of groups, each containing a sequence of indices. The list is named according to the `names` vector, and each element contains indices as specified by the `values` vector.
}

\examples{
# Example usage with group names and values
names <- c("agree", "conscientious", "extraversion", "neuroticism", "openness")
values <- c(5, 5, 5, 5, 5)
groups <- create_groups(names, values)
print(groups)
}

\author{
José Ventura-León
}
