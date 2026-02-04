\name{EFA_plot}
\alias{EFA_plot}
\title{Exploratory Factor Analysis Plot with Complexity Indication}

\description{
This function generates a plot for visualizing the results of an Exploratory Factor Analysis (EFA), highlighting items with complex loadings across multiple factors. The plot displays the standardized factor loadings and confidence intervals for each item, with complex items indicated by a red outline.
}

\usage{
EFA_plot(specifications, item_prefix)
}

\arguments{
  \item{specifications}{A list containing the standardized factor loadings and associated data, typically obtained from an EFA analysis.}
  \item{item_prefix}{A string representing the prefix used in item labels, which will be removed to isolate the item number.}
}

\details{
The function first prepares the data by identifying complex items, defined as those with standardized loadings greater than 0.30 on more than one factor. It then creates a plot using \code{ggplot2}, where each item's standardized loading is represented with a point and its confidence interval as a horizontal line.

Items that are identified as complex are highlighted with a red outline to distinguish them from simpler items. The plot also includes customizable axis labels, titles, and facet labels to improve readability and presentation. The generated plot can be used to visually inspect the factor structure and to identify potential issues with item complexity.

The function returns the plot as a ggplot object, which can be further customized or saved using \code{ggsave}.
}

\value{
A \code{ggplot} object representing the EFA plot with complexity indication for each item. The plot includes:
\itemize{
  \item Standardized factor loadings (lambda) on the x-axis.
  \item Item numbers on the y-axis.
  \item Complex items highlighted with a red outline.
  \item Confidence intervals for factor loadings.
}
}

\examples{
# Example usage of the EFA_plot function

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(stringr)

# Example specifications list (replace with actual data)
specifications_example <- list(list(
  est.std = runif(100, -1, 1),
  ci.lower = runif(100, -1, 0),
  ci.upper = runif(100, 0, 1),
  lhs = rep("f1", 100),
  rhs = paste0("IS", 1:100),
  op = rep("=~", 100)
))

# Generate the EFA plot
EFA_plot(specifications = specifications_example, item_prefix = "IS")
}

\author{
Dr. José Ventura-León
}
