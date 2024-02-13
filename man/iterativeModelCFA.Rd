\name{iterativeModelCFA}
\alias{iterativeModelCFA}
\title{Iterative Model Fit for Confirmatory Factor Analysis (CFA)}

\description{
This function conducts an iterative process of model adjustment for confirmatory factor analysis (CFA) using the lavaan package. It is designed to iteratively enhance the model by adding modifications based on the highest modification indices (MI) exceeding a specified threshold. This iterative process aids in refining the CFA model to better fit the data, especially for complex models where initial model specifications might not capture all relevant pathways or correlations.
}

\usage{
iterativeModelCFA(data, initialModel, MAX_ITERATIONS = 10, MI_THRESHOLD = 5)
}

\arguments{
  \item{data}{The dataset to be analyzed, where rows represent observations and columns represent variables involved in the CFA model.}
  \item{initialModel}{A character string specifying the initial CFA model. This should follow the model specification syntax used by the lavaan package.}
  \item{MAX_ITERATIONS}{An integer specifying the maximum number of iterations for model adjustments. Default is 10.}
  \item{MI_THRESHOLD}{A numeric value specifying the minimum modification index (MI) value that a modification must exceed to be considered for inclusion in the model. Default is 5.}
}

\details{
The function starts with the specified initial CFA model and performs CFA using the 'cfa' function from the lavaan package. At each iteration, it checks for possible model modifications (i.e., additional pathways or correlations) that could improve model fit, based on modification indices (MI). Only modifications with an MI value greater than the specified threshold are considered. The process stops when either the maximum number of iterations is reached or no further modifications exceeding the MI threshold are identified.

The output includes the final adjusted model, fit measures for each iteration, standardized solutions, and a record of modifications applied throughout the iterations.
}

\value{
A list containing:
\itemize{
  \item{FinalModel}{The final adjusted CFA model as a character string.}
  \item{FitMeasuresDf}{A dataframe of fit measures for each iteration, including chi-square, degrees of freedom, SRMR, WRMR, CFI, TLI, and RMSEA.}
  \item{StandardizedSolutions}{A list of standardized solutions for each iteration.}
  \item{ModificationsDf}{A dataframe listing all modifications applied to the model during the iterative process, including the iteration number, the modification made, and the modification index (MI).}
}
}

\examples{
# Define the initial CFA model
initialModel <- '
  Factor1 =~ x1 + x2 + x3
  Factor2 =~ x4 + x5 + x6
'

# Run the iterative CFA model adjustment
results <- iterativeModelCFA(data = myData, initialModel = initialModel)

# View the final model
cat(results$FinalModel)

# Examine fit measures
print(results$FitMeasuresDf)

# View the modifications applied
print(results$ModificationsDf)
}

\author{
Professor José Ventura León
}
