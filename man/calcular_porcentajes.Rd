\name{calcular_porcentajes}
\alias{calcular_porcentajes}
\title{Calculation of Percentage Distributions}

\description{
This function is designed to calculate the percentage distribution for specified columns within a dataset. It is useful for understanding the relative frequencies of different categories or values in each variable. By providing a clear breakdown of percentages, this function aids in the initial stages of categorical data analysis and helps in visualizing the distribution across various groups.
}

\usage{
calcular_porcentajes(data, columnas)
}

\arguments{
  \item{data}{The data frame containing the variables for which percentage distributions are to be calculated.}
  \item{columnas}{A vector of column names within the data frame for which the percentage distributions are to be calculated. These should be categorical or discrete variables.}
}

\details{
For each specified column, the function groups the data by the unique values in that column, calculates the count of each value, and then computes the percentage that each count represents of the total. The results are particularly valuable for categorical data analysis, enabling a quick assessment of the proportion each category occupies within the dataset.
}

\value{
A list where each element corresponds to a specified column and contains a data frame. Each data frame includes the unique values of the variable, their counts (n), and their respective percentage distributions (Porcentaje).
}

\examples{
# Assuming 'data' is a data frame and 'columnas' is a vector of column names
porcentajes <- calcular_porcentajes(data, columnas)
print(porcentajes)
}

\author{
Professor José Ventura León
}
