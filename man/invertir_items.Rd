\name{invertir_items}
\alias{invertir_items}
\title{Invert Specific Items' Values in a DataFrame Based on Response Scale}

\description{
  This function inverts the values of specific columns in a dataframe, accommodating different scales of measurement that either include or exclude zero. It's particularly useful for adjusting the directionality of survey or psychometric questionnaire items, ensuring that lower values are transformed to higher ones and vice versa, in alignment with the desired measurement scale.
}

\usage{
invertir_items(df, items, num_respuestas, comienza_con_cero = TRUE)
}

\arguments{
  \item{df}{A dataframe containing the data to be transformed.}
  \item{items}{A character vector specifying the names of the items within the dataframe whose values will be inverted.}
  \item{num_respuestas}{An integer indicating the number of response options. This determines the range of values for inversion.}
  \item{comienza_con_cero}{A logical flag indicating whether the response scale starts with 0 (\code{TRUE}) or 1 (\code{FALSE}). Defaults to \code{TRUE}.}
}

\details{
  Based on the specified number of responses and whether the scale starts with zero, the function dynamically calculates the maximum inversion value for each column. This approach allows for flexible adaptation to various scales, ensuring accurate value inversion across different data collection instruments. It is particularly adept at handling psychometric scales or survey items that require inversion for consistent analytical interpretation.
}

\value{
  The function returns a dataframe with the specified columns' values inverted according to the provided scale information. This modification aids in the consistent analysis and interpretation of survey or questionnaire data.
}

\examples{
# Assuming df_survey is your dataframe
# Invert items 'q1' and 'q4' with a 5-point scale
items <- c("q1", "q4")
df_survey_modified <- invertir_items(
  df_survey, items,
  num_respuestas = 5,
  comienza_con_cero = FALSE
)
print(df_survey_modified)
# Print the dataframe with inverted values
}

\author{
  Dr. José Ventura-León
}
