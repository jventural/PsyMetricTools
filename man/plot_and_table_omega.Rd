\name{plot_and_table_omega}
\alias{plot_and_table_omega}
\title{Plot and Table of Omega Reliability Coefficients}

\description{
This function generates a graphical representation and a statistical summary table of omega (\u03C9) reliability coefficients from a dataset of bootstrap confirmatory factor analysis (CFA) replications. It visualizes the distribution of omega values across multiple replications and provides a detailed summary of these values, including their mean, standard deviation, minimum, and maximum. This aids in assessing the stability and reliability of measurement models, offering insights into the internal consistency of the constructs measured.
}

\usage{
plot_and_table_omega(df_repli, omega_ymin_annot = NULL, omega_ymax_annot = NULL)
}

\arguments{
  \item{df_repli}{The dataset containing the results of the bootstrap CFA, specifically the omega reliability coefficients calculated for each replication.}
  \item{omega_ymin_annot}{Optional. A numeric value to set the minimum y-axis limit for the annotation in the plot. If NULL, a default calculation based on the dataset is used.}
  \item{omega_ymax_annot}{Optional. A numeric value to set the maximum y-axis limit for the annotation in the plot. If NULL, a default value is used.}
}

\details{
The function utilizes several R packages to prepare and visualize the data. It starts by selecting the relevant columns prefixed with "Rel" (representing the omega reliability coefficients) from the provided dataset. These columns are then summarized to calculate the mean, standard deviation, minimum, and maximum values. Additionally, the function prepares a boxplot of the omega values to visually represent their distribution across replications. The statistical table and the boxplot are then combined into a single visualization, providing a comprehensive overview of the reliability coefficients' behavior. The function allows for adjustment of the ymin and ymax annotations on the plot, offering flexibility in the visualization's detail level.
}

\value{
A list containing two elements: a table summarizing the statistical properties of the omega reliability coefficients and a boxplot visualizing their distribution. This combination of statistical summary and graphical visualization facilitates a detailed assessment of the measurement model's reliability.
}

\examples{
# Assuming 'df_repli' is your dataset with omega reliability coefficients
results <- plot_and_table_omega(df_repli)

# To access the summary table
omega_table <- results$table

# To view the plot
plot <- results$plot
print(plot)
}

\author{
Professor José Ventura León
}
