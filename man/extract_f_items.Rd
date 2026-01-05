\name{extract_f_items}
\alias{extract_f_items}
\title{Factorial Structure Item Extraction Tool}

\description{
This function is designed to organize and extract items associated with different factors in a factorial structure. It is particularly useful for analyzing pattern matrices or factor structures in exploratory or confirmatory factor analyses. The function identifies items corresponding to specified factor prefixes and generates a structured representation of each factor's associated items.
}

\usage{
extract_f_items(data, prefixes)
}

\arguments{
  \item{data}{A data frame or matrix representing the pattern matrix or factorial structure. It should contain a column named 'Items' and other columns with factor loadings.}
  \item{prefixes}{A vector of character strings representing the prefixes of factor names. These prefixes are used to identify columns in the 'data' corresponding to each factor.}
}

\details{
For each factor prefix provided, the function searches the data for columns starting with that prefix. It then filters out items from these columns where factor loadings are non-zero, indicating a relationship with the factor. The function concatenates these items into a formatted string representing the factorial structure for each factor.
}

\value{
A string containing the formatted representation of the factorial structure. Each line in the string represents a factor, showing its name and the associated items concatenated with ' + '.
}

\examples{
# Example usage with a hypothetical pattern matrix 'Patron'
Patron <- data.frame(
  Items = c("Item1", "Item2", "Item3", "Item4", "Item5"),
  f1 = c(0.4, 0, 0.6, 0, 0),
  f2 = c(0, 0.5, 0, 0.7, 0),
  f3 = c(0, 0, 0, 0, 0.8)
)
prefixes <- c("f1", "f2", "f3")
extracted_structure <- extract_f_items(Patron, prefixes)
print(extracted_structure)
}

\author{
Dr. José Ventura-León
}

