\name{calculate_per_fit}
\alias{calculate_per_fit}
\title{Calculate Percentage of Models Meeting Specific Fit Criteria}

\description{
  A versatile function designed to calculate the percentage of models from a replication study that meet specified fit criteria. It allows for the evaluation of model fit across a range of thresholds for common fit indices, facilitating an aggregate understanding of model performance in simulation studies or multi-model analyses.
}

\usage{
calculate_per_fit(df_repli, thresholds_str)
}

\arguments{
  \item{df_repli}{A list containing elements of fit measures for each replicated model. Expected to include a nested data frame of fit measures.}
  \item{thresholds_str}{A string specifying the fit indices, their desired direction ("<" for below a threshold, ">" for above a threshold), and their thresholds. Fit measures and thresholds should be separated by commas.}
}

\details{
  This function interprets a user-defined string of thresholds and fit measures, converts it into a structured data frame, and then compares each model's fit measures against these thresholds to determine whether they meet the specified criteria. It efficiently processes a collection of models' fit indices, offering insights into the proportion of models that adhere to or exceed given fit standards. The function supports common fit measures such as CFI, TLI, SRMR, and RMSEA.
}

\value{
  Returns a data frame where each column represents a specific fit measure and each cell contains the percentage of models that meet the criteria defined in the input string. Percentages are formatted as strings with a percent sign to facilitate easy interpretation.
}

\examples{
# Assuming 'results' contains your replication study's fit measures
measure_thresholds_str <- "CFI < .95, TLI < .95, SRMR > .08, RMSEA > .08"
result_table <- calculate_per_fit(results, measure_thresholds_str)
print(result_table)
# This will print a table showing the percentage of models meeting the specified thresholds.
}

\author{
Dr. José Ventura-León
}
