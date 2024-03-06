\name{Plot_and_Table_comparative}
\alias{Plot_and_Table_comparative}
\title{Comparative Plot and Table of CFI and TLI Measures}

\description{
This function generates a comparative visualization and statistical summary table for the Comparative Fit Index (CFI) and Tucker-Lewis Index (TLI) measures from a dataset of bootstrap confirmatory factor analysis (CFA) replications. It aims to provide a clear comparison of model fit across multiple replications by visualizing the distribution of CFI and TLI values and summarizing these values statistically, including their mean, standard deviation, minimum, and maximum. This tool is particularly useful for evaluating and comparing the goodness-of-fit of measurement models, aiding in the selection of the most appropriate model based on fit criteria.
}

\usage{
Plot_and_Table_comparative(df_repli)
}

\arguments{
  \item{df_repli}{The dataset containing the results of the bootstrap CFA, specifically the fit measures calculated for each replication.}
}

\details{
Utilizing a series of R packages for data manipulation and visualization, the function first extracts the CFI and TLI values from the provided dataset. It then rounds these values for presentation and calculates summary statistics for each measure. Additionally, it creates a boxplot for both CFI and TLI values to visually depict their distribution across replications. The function concludes by combining the statistical summary table and the boxplot into a single graphical output, providing a comprehensive overview of the model fit measures' performance.
}

\value{
A list containing two elements: a table summarizing the statistical properties of the CFI and TLI measures and a boxplot visualizing their distribution. This combination offers a nuanced understanding of the measurement models' goodness-of-fit across different bootstrap samples, facilitating comparisons and assessments of model adequacy.
}

\examples{
# Assuming 'df_repli' is your dataset with CFA fit measures
results <- Plot_and_Table_comparative(df_repli)

# To access the summary table of CFI and TLI
fit_table <- results$table

# To view the comparative plot
plot <- results$plot
print(plot)
}

\author{
Professor José Ventura León
}
