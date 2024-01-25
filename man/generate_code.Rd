\name{generate_code}
\alias{generate_code}
\title{Dynamic R Code Generator for Summation of Factor Items}

\description{
This function dynamically generates R code to calculate the sum of items associated with each factor in a dataset. It parses a structured text input, where each line represents a factor and its corresponding items, and then constructs R code using `dplyr` syntax. This code can be directly executed to compute the sum of item responses for each factor in a dataset, making it particularly useful in preparing data for factor analysis or scoring scales.
}

\usage{
generate_code(text, name = "Data")
}

\arguments{
  \item{text}{A string where each line contains a factor name followed by a colon and a comma-separated list of its corresponding items.}
  \item{name}{Optional. The name of the data frame in the generated code. This should correspond to the dataset on which the code will be applied. Default is "Data".}
}

\details{
The function interprets each line of the input `text` as a separate factor. Factor names and their associated item names are extracted, and R code is constructed to use `dplyr` functions for row-wise summation of these items. This generated code is particularly useful for creating new variables in a dataset that represent the aggregate scores of different factors.
}

\value{
A string containing the R code that, when executed, will calculate the sum of responses for each factor in the specified dataset.
}

\examples{
# Example text input
text <- "Validation: E8, E16
Comprehensibility: E5, E10, E33, E45
Guilt and embarrassment: E4, E14, E26, E31
Simplicity vs. Complexity: E18, E35, E38, E47
Relationship with high values: E21, E25, E42
Controllability: E7, E27, E44
Insensitivity: E15, E32
Rationality and anti-emotionality: E17, E30, E46
Duration of strong feelings: E13, E29
Consensus with others: E3, E19, E39, E41
Acceptance or inhibition: E2, E9, E12, E20, E28, E40
Rumination vs. instrumental style: E1, E36, E37, E48
Expression: E6, E23
Blaming others: E11, E34"

# Generate code for the 'Data_Ansiedad' dataset
code <- generate_code(text, "Data_Ansiedad")
cat(code)
}

\author{
Professor José Ventura León
}

