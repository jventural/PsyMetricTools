\name{crearSintaxisCFA}
\alias{crearSintaxisCFA}
\title{Construct Confirmatory Factor Analysis (CFA) Model Syntax}

\description{
  This function generates the model syntax for a confirmatory factor analysis (CFA) based on a given dataframe. It identifies unique factors within the dataset and constructs the CFA model syntax by aggregating items under their corresponding factors. Designed for use in psychometric analysis, this tool facilitates the creation of complex CFA models by automating the syntax generation process, thus saving time and reducing the potential for manual errors.
}

\usage{
  crearSintaxisCFA(df)
}

\arguments{
  \item{df}{A dataframe that contains at least two columns: `Items` and `Factores`. `Items` are the observed variables to be included in the model, and `Factores` denotes the latent variables (factors) to which each item belongs.}
}

\details{
  The function iterates through each unique factor in the `Factores` column of the provided dataframe. For each factor, it aggregates the corresponding items (observed variables) listed in the `Items` column. These items are then formatted into the CFA model syntax, where each factor is represented as a latent variable (e.g., F1, F2, F3) related to its items through the `=~` operator. The resulting model syntax can be directly used in functions from the `lavaan` package for conducting confirmatory factor analysis.
}

\value{
  A single string containing the complete CFA model syntax. This string is formatted to be compatible with the `lavaan` package's model specification requirements, facilitating easy integration into CFA analysis workflows.
}

\examples{
# Assuming 'df' is your dataset with 'Items' and 'Factores' columns
model_syntax <- crearSintaxisCFA(df)

# To view the generated model syntax
print(model_syntax)
}

\author{
Dr. José Ventura-León
}
