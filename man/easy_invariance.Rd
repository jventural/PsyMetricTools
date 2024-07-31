\name{easy_invariance}
\alias{easy_invariance}
\title{Testing Measurement Invariance Across Multiple Levels}

\description{
This function performs measurement invariance testing across multiple levels (configural, thresholds, metric, scalar, strict) using structural equation modeling. It returns a comprehensive table with model fit indices and statistical terms for each level of invariance tested.
}

\usage{
easy_invariance(model, data, estimator, ordered, ID.cat, group, levels_of_invariance, group.partial = NULL)
}

\arguments{
  \item{model}{A string representing the model to be tested for invariance.}
  \item{data}{A dataframe containing the dataset to be used for invariance testing.}
  \item{estimator}{A string specifying the estimator to be used in the SEM analysis.}
  \item{ordered}{A boolean indicating whether the data contains ordered categorical variables.}
  \item{ID.cat}{A string specifying the method for identifying categories in the SEM analysis.}
  \item{group}{A string specifying the grouping variable for testing invariance.}
  \item{levels_of_invariance}{A character vector specifying the levels of invariance to be tested (e.g., "configural", "threshold", "metric", "scalar", "strict").}
  \item{group.partial}{A character vector specifying parameters to be freed across groups for partial invariance. Default is NULL.}
}

\details{
The function begins by loading the necessary libraries and defining an internal helper function to run the \code{semTools::measEq.syntax} function for different levels of invariance. It initializes a list to store the results for each level of invariance specified.

The function iterates over the specified levels of invariance, running the invariance test for each level and storing the results. After running the tests, it compares the models using \code{semTools::compareFit} and extracts the fit indices (CFI, RMSEA) and their differences for each model comparison.

The final output is a combined dataframe with the model fit indices and statistical terms for each level of invariance tested, with columns renamed to statistical terms (χ²(df), Δχ², Δdf, p, CFI, RMSEA, ΔCFI, ΔRMSEA).
}

\value{
A dataframe containing the model fit indices and statistical terms for each level of invariance tested. The columns include χ²(df), Δχ², Δdf, p, CFI, RMSEA, ΔCFI, and ΔRMSEA.
}

\examples{
# Example usage with a sample dataset and model

# Define the model
model_original <- "F1 =~ A1+A2+A3+A4+A5
                   F2 =~ C1+C2+C3+C4+C5
                   F3 =~ E1+E2+E3+E4+E5
                   F4 =~ N1+N2+N3+N4+N5
                   F5 =~ O1+O2+O3+O4+O5
"

# Example dataset
big5 <- as_tibble(psych::bfi)
big5 <- na.omit(big5)
big5 <- select(big5, -education, -age)
big5 <- slice(big5, 1:500)

# Run the invariance testing function
results <- easy_invariance(
  model = model_original,
  data = big5,
  estimator = "WLSMV",
  ordered = TRUE,
  ID.cat = "Wu.Estabrook.2016",
  group = "gender",
  levels_of_invariance = c("configural", "threshold", "metric", "scalar", "strict")
)

# Print the results
print(results$combined_data)
}

\author{
Dr. José Ventura-León
}
