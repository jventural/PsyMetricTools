\name{grafico_boxplots}
\alias{grafico_boxplots}
\title{Box Plot Visualization Tool}

\description{
This function is designed to create box plots for a selection of variables within a dataset. It is useful for a quick visual examination of the distribution, range, and central tendencies of multiple variables. The function generates a series of box plots, each corresponding to a different variable, facilitating comparative analysis across the variables.
}

\usage{
grafico_boxplots(data, cols)
}

\arguments{
  \item{data}{The dataset in which the box plots will be generated. It should be a data frame with variables of interest.}
  \item{cols}{A vector containing the names of the variables (columns) for which box plots are to be created. These should be numeric or ordinal variables.}
}

\details{
The function reshapes the data into a long format suitable for `ggplot2` box plot visualization. Each variable specified in `cols` will have its own box plot, displayed in a facet grid layout. This layout allows for easy comparison of distributions across different variables.
}

\value{
A `ggplot` object representing the box plots of the specified variables. This object can be further modified or directly plotted using ggplot2 functions.
}

\examples{
# Assuming 'Data' is a data frame and 'columnas' is a vector of column names
grafico <- grafico_boxplots(Data, columnas)
print(grafico)
}

\author{
Professor José Ventura León
}
