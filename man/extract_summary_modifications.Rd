\name{extract_summary_modifications}
\alias{extract_summary_modifications}
\title{Summary Extraction of Modification Indices from Combined Results}

\description{
This function aggregates modification indices and statistical summaries from combined results of confirmatory factor analysis (CFA) modifications. Designed to work with a dataframe that consolidates modification suggestions across multiple analyses or bootstrap samples, it facilitates the identification of frequently suggested modifications and their impact, as measured by the Modification Index (MI). By highlighting modifications that exceed a specified occurrence threshold, it aids in pinpointing potential areas for model improvement.
}

\usage{
extract_summary_modifications(combined_results, min_count = 10)
}

\arguments{
  \item{combined_results}{A dataframe containing combined modification indices and their respective MI values from multiple CFA analyses or bootstrap samples.}
  \item{min_count}{An integer specifying the minimum number of occurrences for a modification to be included in the summary output. Default is 10.}
}

\details{
The function groups modifications based on their identifiers within the 'combined_results' dataframe, calculating for each modification the total count of suggestions, average MI, standard deviation of MI, minimum MI, and maximum MI. It then filters to include only those modifications suggested more frequently than the 'min_count' threshold, ordering the remaining modifications by their average MI in descending order. This process highlights the most consistently suggested modifications that could significantly improve model fit.
}

\value{
A dataframe summarizing modification indices for modifications that meet or exceed the 'min_count' threshold. The dataframe includes columns for modification identifier, count of occurrences, average MI, standard deviation of MI, minimum MI, and maximum MI, enabling comprehensive analysis and comparison of modifications across different analyses or bootstrap samples.
}

\examples{
# Assuming 'combined_results' is a dataframe of modification indices from CFA analyses
summary_modifications <- extract_summary_modifications(
  combined_results,
  min_count = 15
)

# To view the summarized modification indices
head(summary_modifications)
}

\author{
Dr. José Ventura-León
}
