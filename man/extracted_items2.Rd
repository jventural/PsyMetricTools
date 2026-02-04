\name{extracted_items2}
\alias{extracted_items2}
\title{Factor Item Extraction for Reliability Analysis}

\description{
This function is designed to extract and organize items based on their highest factor loadings from a given dataset, typically used in the context of exploratory factor analysis (EFA). The function identifies the factor with the highest loading for each item and groups items accordingly. This grouping is particularly useful for subsequent reliability analysis, such as in the `calcula_omega_all` function, to assess the internal consistency of each factor.
}

\usage{
extracted_items2(data)
}

\arguments{
  \item{data}{A dataframe containing factor loadings for items. It should include both numeric columns representing factor loadings and a column named 'Items' with item identifiers.}
}

\details{
The function first isolates numeric columns, assuming these represent factor loadings. For each item (row) in the dataset, it identifies the factor (column) with the highest loading. Items are then grouped under these factors. This approach helps in constructing factor-specific item lists, essential for reliability and validity analysis of factor models.
}

\value{
A list where each element represents a factor identified by its column name. The elements contain vectors of item names that have the highest loading on the respective factor.
}

\examples{
# Assuming 'data' is a dataframe with factor loadings and an 'Items' column
factor_items <- extracted_items2(data)
print(factor_items)
}

\author{
Dr. José Ventura-León
}

