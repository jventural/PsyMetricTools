\name{normality_test_SW}
\alias{normality_test_SW}
\title{Shapiro-Wilk Normality Test Implementation}

\description{
This function is tailored to perform the Shapiro-Wilk test for assessing the normality of distributions in a dataset. It applies the test to multiple variables and provides a comprehensive summary, including the Shapiro-Wilk statistic, p-value, and a conclusion on normality. This function is particularly useful for preliminary data analysis where checking the normality assumption is crucial for further statistical procedures.
}

\usage{
normality_test_SW(data, variables)
}

\arguments{
  \item{data}{A data frame containing the data to be analyzed.}
  \item{variables}{A vector of variable names within the data frame to be tested for normality. These should be numeric variables.}
}

\details{
The function reshapes the data to apply the Shapiro-Wilk test to each specified variable. It returns the Shapiro-Wilk statistic, a formatted p-value (indicating values lower than 0.001 as 'p < .001'), and a determination of normality based on the p-value (with a threshold of 0.05). This assists in quickly identifying variables that may violate the normality assumption.
}

\value{
A data frame with each row representing a variable. Columns include the variable name, Shapiro-Wilk statistic, p-value, and a normality assessment ('Normal' or 'No-normal').
}

\examples{
# Assuming 'df_new_renombrado' is a data frame with relevant variables
test_results <- normality_test_SW(df_new_renombrado, c("Ansiedad de separación", "Autoestima"))
print(test_results)
}

\author{
Professor José Ventura León
}
