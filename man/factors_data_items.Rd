\name{factors_data_items}
\alias{factors_data_items}
\title{Factor Structure Item Integration Tool}

\description{
This function is designed to integrate item phrasing with their corresponding factor loadings and structure. It takes a summary of factor analysis results and merges this with the detailed item data. This is particularly useful for visualizing and understanding how specific items relate to each factor in the analysis, especially when interpreting the factor loadings in the context of item content.
}

\usage{
factors_data_items(summary_data, data_items, num_factors,
                   apply_threshold = TRUE)
}

\arguments{
  \item{summary_data}{A data frame obtained from the `factor_summary` function, containing factor loadings, communalities, and uniquenesses.}
  \item{data_items}{A data frame containing the items with their phrasings. This data should correspond to the items analyzed in the factor analysis.}
  \item{num_factors}{The number of factors included in the factor analysis.}
  \item{apply_threshold}{Optional. A logical value indicating whether to apply a threshold to factor loadings. Defaults to TRUE. When set to TRUE, loadings below 0.30 are set to zero.}
}

\details{
The function first filters the `data_items` data frame to include only those items present in `summary_data`. Then, it merges the filtered item data with `summary_data`, aligning items with their respective factor loadings. If `apply_threshold` is TRUE, factor loadings below 0.30 are set to zero, which can help in identifying more significant item-factor relationships.
}

\value{
A tibble that combines item phrasing with their factor loadings. This tibble includes columns for each factor, as well as the communalities (`h2`), uniquenesses (`u2`), and the phrasing of each item.
}

\examples{
# Load item data from an Excel file
data_items <- read_excel("data_items.xlsx")

# Assuming 'summary_data' is a data frame with factor analysis results
factor_data <- factors_data_items(summary_data, data_items, num_factors = 5)
print(factor_data)
}

\author{
Dr. José Ventura-León
}

