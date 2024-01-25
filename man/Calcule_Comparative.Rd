\name{Calcule_Comparative}
\alias{Calcule_Comparative}
\title{Comparative Statistics Calculation}

\description{
This function is designed to analyze and compare two groups within a dataset by calculating various statistics, including mean differences, t-tests, and Cohen's d effect sizes. It is particularly useful for assessing the statistical significance and effect size of differences between groups across multiple variables. The function also offers an option to compute robust Cohen's d, providing more reliable estimates in the presence of outliers or non-normal distributions.
}

\usage{
Calcule_Comparative(data, cols, group_var, Robust = FALSE)
}

\arguments{
  \item{data}{The data frame containing the dataset for analysis.}
  \item{cols}{A vector of column names representing the variables under study.}
  \item{group_var}{The name of the column in the dataset that represents the grouping variable for comparison.}
  \item{Robust}{Logical. If TRUE, uses robust estimation for Cohen's d (suitable for non-normal distributions or when outliers are present); if FALSE, uses the standard Cohen's d. Default is FALSE.}
}

\details{
The function performs a long-format conversion of the selected columns, computes descriptive statistics for each variable within each group, and conducts t-tests to assess the significance of differences. The Cohen's d effect size (standard or robust, depending on the `Robust` parameter) is calculated to measure the magnitude of the difference.
}

\value{
A data frame that includes comparative statistics for each variable in the specified columns, detailing mean values, standard deviations, t-test results, Cohen's d values, and their interpretations.
}

\examples{
result <- Calcule_Comparative(
  data = Data,
  cols = c("Soledad", "Columna2", "Columna3", "Columna4", "Columna5"),
  group_var = "Grupo_Edad",
  Robust = FALSE
)
print(result)
}

\author{
[Your Name or Affiliation]
}
