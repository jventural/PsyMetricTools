\name{Multivariate_plot}
\alias{Multivariate_plot}
\title{QQ-Plot and Mardia Coefficient Visualization Tool}

\description{
This function generates a combined visualization consisting of a QQ-Plot for Mahalanobis distances and a table displaying Mardia's skewness and kurtosis coefficients. It is useful for assessing multivariate normality in a dataset, which is an essential assumption in many statistical analyses. The QQ-Plot helps visualize how closely the dataset conforms to a multivariate normal distribution, while the Mardia coefficients provide a numerical assessment of skewness and kurtosis.
}

\usage{
Multivariate_plot(data, xmin = 30, xmax = 40, ymin = 2, ymax = 7)
}

\arguments{
  \item{data}{The data frame or matrix containing the variables to be analyzed for multivariate normality.}
  \item{xmin}{Optional. The minimum value on the x-axis for positioning the Mardia coefficients table in the plot. Default is 30.}
  \item{xmax}{Optional. The maximum value on the x-axis for positioning the Mardia coefficients table in the plot. Default is 40.}
  \item{ymin}{Optional. The minimum value on the y-axis for positioning the Mardia coefficients table in the plot. Default is 2.}
  \item{ymax}{Optional. The maximum value on the y-axis for positioning the Mardia coefficients table in the plot. Default is 7.}
}

\details{
The function calculates Mahalanobis distances for the given dataset and plots these against theoretical quantiles of the chi-squared distribution to create the QQ-Plot. The Mardia coefficients (skewness and kurtosis) are calculated and displayed in a table overlaid on the plot. This combined visualization aids in diagnosing deviations from multivariate normality.
}

\value{
A ggplot object representing the QQ-Plot and overlaid table of Mardia coefficients. This object can be further modified or directly plotted using ggplot2 functions.
}

\examples{
# Assuming 'data' is a data frame suitable for multivariate normality analysis
plot <- Multivariate_plot(data)
print(plot)
}

\author{
Professor José Ventura León
}
