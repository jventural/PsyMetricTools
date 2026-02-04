\name{Preliminar_table}
\alias{Preliminar_table}
\title{Frequency Distribution and Percentage Conversion Table}

\description{
  This function computes the frequency distribution of items and converts these frequencies into percentages. It is intended to process a data frame with selected items and generate a tidy table with added item identifiers and rounded percentage values. This function is useful in psychological and social science research for preparing data for further analysis or reporting.
}

\usage{
Preliminar_table(data)
}

\arguments{
  \item{data}{A data frame with variables of interest. The variables should be numeric and represent response options that are to be tabulated and converted into percentage frequencies.}
}

\details{
  The `Preliminar_table` function first calculates the response frequencies using the `psych::response.frequencies` function. It then converts these frequencies to a tibble, removes any missing data indicators, and adds a numeric identifier for each item. It converts the frequencies to percentages and rounds the values to two decimal places. The function is part of a preliminary data analysis pipeline, preparing the data for more complex procedures or for generating reports.
}

\value{
  A tibble representing the frequency distribution of items as percentages. Each row corresponds to an item with its identifier, and the columns represent the percentage frequencies of response options, rounded to two decimal places.
}

\examples{
# Example usage with a sample data frame
sample_data <- data.frame(
  SWB1 = sample(1:5, 100, replace = TRUE),
  SWB2 = sample(1:5, 100, replace = TRUE),
  SWB3 = sample(1:5, 100, replace = TRUE)
)
percentage_table <- Preliminar_table(sample_data)
print(percentage_table)
}

\author{
Dr. José Ventura-León
}


