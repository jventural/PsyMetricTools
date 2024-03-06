\name{boot_cfa_plot_and_save}
\alias{boot_cfa_plot_and_save}
\title{Comprehensive Visualization and Optional Saving of Bootstrap CFA Results}

\description{
This enhanced function generates a comprehensive graphical display of bootstrap Confirmatory Factor Analysis (CFA) results, integrating visualizations of omega reliability coefficients, Comparative Fit Index (CFI), Tucker-Lewis Index (TLI) measures, as well as absolute fit measures like RMSEA and SRMR. It utilizes the functionality of `plot_and_table_omega`, `Plot_and_Table_comparative`, and `Plot_and_Table_absolute` to create individual plots for each set of measures, which are then arranged side by side for a holistic comparative analysis. An additional feature of this function is the optional saving of the generated figure to a file, allowing for both immediate visualization and persistent storage of the analysis results.
}

\usage{
boot_cfa_plot_and_save(df, save = TRUE, dpi = 600)
}

\arguments{
  \item{df}{The dataset containing the results of the bootstrap CFA, including fit measures and omega reliability coefficients for each replication.}
  \item{save}{Logical flag indicating whether to save the generated figure to a file. Defaults to TRUE.}
  \item{dpi}{Dots per inch for the saved image file. Used only if save is TRUE. Defaults to 600 for high resolution.}
}

\details{
Upon invocation, the function methodically calls upon the three designated plotting functions to generate visuals corresponding to distinct aspects of the CFA results: omega reliability coefficients, CFI and TLI measures, and absolute fit measures (RMSEA, SRMR). These visuals are subsequently merged into a singular figure comprising three panels, labeled A, B, and C, correlating to the omega reliability plot, the comparative fit plot, and the absolute fit plot, respectively. This amalgamated visual representation facilitates a concurrent review of diverse model evaluation metrics, providing comprehensive insights into the model’s consistency, fitting accuracy, and overall suitability. If the `save` parameter is set to TRUE, the combined figure is saved to a JPEG file using specified `dpi` and default dimensions, otherwise, it is returned for display in the R console.
}

\value{
A singular graphical object encapsulating the combined plots of omega reliability coefficients, comparative fit indices, and absolute fit measures. Depending on the `save` parameter, this object is either saved to a file or returned for immediate display within the R environment, offering a detailed visual summary of the CFA analysis outcomes suitable for report inclusion or further examination.
}

\examples{
# Assuming 'df' is your dataset with CFA results
figure <- boot_cfa_plot_and_save(df, save = TRUE, dpi = 600)

# To display the combined figure (if not saved)
if (!save) {
  print(figure)
}
}

\author{
Professor José Ventura León
}
