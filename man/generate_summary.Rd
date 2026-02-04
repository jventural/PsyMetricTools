\name{generate_summary}
\alias{generate_summary}
\title{Generate Summary Statistics for Categorical and Numeric Variables}

\description{
The `generate_summary` function computes summary statistics for both categorical and numeric variables in a given dataset. For categorical variables, the function generates counts and percentages, while for numeric variables (e.g., age), it calculates the mean and standard deviation. This summary information is returned as a list of tables, providing a quick and comprehensive overview of the selected variables.
}

\usage{
generate_summary(data, variables)
}

\arguments{
  \item{data}{A dataframe containing the dataset to be summarized.}
  \item{variables}{A character vector specifying the variables to be included in the summary. Categorical variables will be summarized using counts and percentages, and numeric variables will be summarized using means and standard deviations.}
}

\details{
The function starts by creating an empty list called `results` to store the summary tables. For each variable in the `variables` argument, if the variable is categorical, it calculates the count of each category and its percentage relative to the total observations. This information is stored in a summary table for each categorical variable.

For numeric variables such as "Edad" (age), the function computes the mean and standard deviation using the `mean()` and `sd()` functions, respectively, while ignoring missing values (`na.rm = TRUE`). This summary is then added to the results list as a separate entry.

The final output is a named list containing a summary table for each variable specified in `variables`, providing a comprehensive view of the distribution of both categorical and numeric data.
}

\value{
A list containing summary tables for each variable specified in the \code{variables} argument. Each table provides either counts and percentages for categorical variables or mean and standard deviation for numeric variables.
}

\examples{
# Example usage with a sample dataframe

# Sample dataset
data <- data.frame(
  Edad = c(25, 30, 22, 40, 35, NA, 28, 26),
  Sexo = c("M", "F", "F", "M", "M", "F", "M", "M"),
  Facultad = c("Psychology", "Law", "Medicine", "Engineering",
               "Psychology", "Law", "Medicine", "Psychology"),
  Residencia = c("Urban", "Rural", "Urban", "Urban",
                 "Rural", "Rural", "Urban", "Urban"),
  Tipo_Universidad = c("Public", "Private", "Public", "Private",
                       "Public", "Public", "Private", "Public")
)

# Specify variables to summarize
variables <- c("Edad", "Sexo", "Facultad", "Residencia", "Tipo_Universidad")

# Generate summary statistics
summary_results <- generate_summary(data, variables)

# Print results for Sexo
print(summary_results[["Sexo"]])
}

\author{
Dr. José Ventura-León
}
