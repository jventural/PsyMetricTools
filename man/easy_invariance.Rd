\name{easy_invariance}
\alias{easy_invariance}
\title{Testing Measurement Invariance Across Multiple Levels}

\description{
This function performs measurement invariance testing across multiple levels (configural, thresholds, metric, scalar, strict) using structural equation modeling.
}

\usage{
easy_invariance(model, data, estimator, ordered, ID.cat,
                group, levels_of_invariance,
                group.partial = NULL,
                fit_indices = c("CFI", "TLI", "RMSEA", "SRMR"))
}

\arguments{
  \item{model}{A string representing the model to be tested for invariance.}
  \item{data}{A dataframe containing the dataset to be used for invariance testing.}
  \item{estimator}{A string specifying the estimator to be used in the SEM analysis.}
  \item{ordered}{A boolean indicating whether the data contains ordered categorical variables.}
  \item{ID.cat}{A string specifying the method for identifying categories in the SEM analysis.}
  \item{group}{A string specifying the grouping variable for testing invariance.}
  \item{levels_of_invariance}{A character vector specifying the levels of invariance to be tested.}
  \item{group.partial}{A character vector specifying parameters to be freed across groups. Default is NULL.}
  \item{fit_indices}{Character vector of fit indices to report (default c("CFI", "TLI", "RMSEA", "SRMR")).}
}

\value{
A dataframe containing the model fit indices and statistical terms for each level of invariance tested.
}

\examples{
\dontrun{
model <- "F1 =~ A1+A2+A3"
results <- easy_invariance(model, data = mydata, estimator = "WLSMV",
                           ordered = TRUE, ID.cat = "Wu.Estabrook.2016",
                           group = "gender", levels_of_invariance = c("configural"))
}
}

\author{
Dr. Jose Ventura-Leon
}

