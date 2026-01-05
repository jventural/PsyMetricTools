\name{extract_ExcludedItems_boot}
\alias{extract_ExcludedItems_boot}
\title{Extract Excluded Items from Bootstrap EFA Results}

\description{
  This function extracts and organizes items excluded from each bootstrap sample during exploratory factor analysis (EFA). It processes a structured list of bootstrap results, identifying items that were excluded in each sample due to reasons such as significant cross-loadings or inadequate fit. The primary objective is to compile this information for further analysis, thereby enhancing the understanding of item performance and stability across bootstrap samples within EFA contexts.
}

\usage{
extract_ExcludedItems_boot(resultados_bootstrap)
}

\arguments{
  \item{resultados_bootstrap}{A list containing the results of bootstrap analyses for EFA. Each element of the list represents a structured result from a single bootstrap sample, detailing items that were excluded in that particular analysis.}
}

\details{
  Through iteration over each set of bootstrap results contained in `resultados_bootstrap`, the function extracts excluded items, transforms them into a structured tabular format, and labels them with their corresponding sample identifier. The outcome is an aggregated dataframe that lists excluded items across all bootstrap samples, organized for easy analysis or reporting. This operation leverages the capabilities of the `purrr`, `dplyr`, and `tibble` packages to facilitate efficient data handling and transformation.
}

\value{
  A dataframe that compiles the excluded items from each bootstrap sample into a single, structured format, including identifiers for each sample. This comprehensive overview aids in the analysis or visualization of item exclusion patterns, offering insights into the consistency and reliability of the EFA model across different bootstrap iterations.
}

\examples{
# Assuming 'resultados_bootstrap' is the output from your bootstrap EFA analysis
excludedItems_df <- extract_ExcludedItems_boot(resultados_bootstrap)

# To examine the first few entries of the excluded items dataframe
head(excludedItems_df)
}

\author{
  Dr. José Ventura-León
}

