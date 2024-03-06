\name{Plot_and_Table_absolute}
\alias{Plot_and_Table_absolute}
\title{Comparative Plot and Table of Absolute Fit Measures}

\description{
This function creates a comparative visualization and statistical summary table for absolute fit measures including RMSEA (Root Mean Square Error of Approximation), SRMR (Standardized Root Mean Square Residual), and CRMR (a typographical error, possibly meant to be another fit measure like CFI or TLI, but since CRMR is not a standard fit measure, it could be a custom or mistakenly included term). It's designed to provide insights into the absolute fit of models from a dataset of bootstrap confirmatory factor analysis (CFA) replications, highlighting the distribution and statistical properties (mean, standard deviation, minimum, and maximum) of these fit indices. This tool aids in the detailed evaluation and comparison of model fit to empirical data, enhancing the selection of optimal measurement models based on absolute fit criteria.
}

\usage{
Plot_and_Table_absolute(df_repli)
}

\arguments{
  \item{df_repli}{The dataset containing the results of the bootstrap CFA, specifically the absolute fit measures calculated for each replication.}
}

\details{
Leveraging a suite of R packages for data manipulation and visualization, the function begins by extracting RMSEA, SRMR, and possibly a corrected term for CRMR, from the provided dataset. It rounds these values for precision and computes summary statistics for each measure. A boxplot for each of the absolute fit measures is then generated to visually represent their distribution across replications. Finally, the statistical summary table and the boxplot are combined into a single output, offering a comprehensive overview of the absolute fit measures' performance across multiple model replications.
}

\value{
A list containing two elements: a table summarizing the statistical properties of the absolute fit measures and a boxplot visualizing their distribution. This output provides a nuanced view of the models' absolute fit to the data, facilitating the comparison and assessment of different measurement models in terms of how well they approximate the empirical data structure.
}

\examples{
# Assuming 'df_repli' is your dataset with CFA absolute fit measures
results <- Plot_and_Table_absolute(df_repli)

# To access the summary table of absolute fit measures
fit_table <- results$table

# To view the comparative plot
plot <- results$plot
print(plot)
}

\author{
Professor José Ventura León
}
