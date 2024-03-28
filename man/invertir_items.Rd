\name{invertir_items}
\alias{invertir_items}
\title{Invert Specific Items' Values in a DataFrame}

\description{
  A function designed to invert the values of specific columns in a dataframe, taking into account whether the values include zero. It is useful for transforming measurement scales where the lowest value becomes the highest and vice versa, especially in the context of surveys or psychometric questionnaires where one wishes to adjust the directionality of items.
}

\usage{
invertir_items(df, items)
}

\arguments{
  \item{df}{A dataframe containing the data to be transformed.}
  \item{items}{A character vector specifying the names of the columns within the dataframe whose values will be inverted.}
}

\details{
  The function first checks whether the value 0 is present in the data of the specified columns. If 0 is present, it assumes a scale of 0 to 3 for inverting values. Otherwise, it calculates the maximum value from the unique data in each column to determine the inversion value, thus dynamically adjusting to different data ranges. This functionality is particularly useful in processing questionnaire items where some items are formulated inversely and need to be recalculated for consistent analysis.
}

\value{
  Returns a modified dataframe where the specified columns have been transformed, inverting their values according to the described logic. The transformed values facilitate subsequent data interpretation and analysis.
}

\examples{
# Assuming Data_Final is your dataframe and you want to invert the items 'phq1' and 'phq2'
items_to_invert <- c("phq1", "phq2")
Data_Final_modified <- invertir_items(Data_Final, items_to_invert)
print(Data_Final_modified)
# This will print the dataframe with the values of 'phq1' and 'phq2' inverted according to the function's logic.
}

\author{
  Professor José Ventura León
}
