\name{calcular_correlaciones}
\alias{calcular_correlaciones}
\title{Calculate correlation}

\description{
This function calculates a correlation matrix using either Pearson, Spearman, or Winsorized Pearson methods. It is designed to work with a range of columns within a dataset, allowing for flexible analysis of variable relationships. The function can handle both normal and non-parametric data distributions, making it suitable for a wide range of statistical applications.
}

\usage{
calcular_correlaciones(data, columna_inicial, columna_final, method = c("spearman", "pearson"), winsorize = FALSE)
}

\arguments{
  \item{data}{The dataset in which the correlation will be calculated.}
  \item{columna_inicial}{The name of the first column indicating the start of the range of variables for correlation analysis.}
  \item{columna_final}{The name of the last column indicating the end of the range of variables for correlation analysis.}
  \item{method}{The method of correlation calculation. Options are "spearman" for Spearman's rank correlation or "pearson" for Pearson's correlation coefficient. Default is "spearman".}
  \item{winsorize}{A logical value. If TRUE and the method is "pearson", the function calculates a Winsorized Pearson correlation coefficient. Default is FALSE.}
}

\details{
When `winsorize` is set to TRUE, the Pearson correlation is adjusted to minimize the effect of outliers by Winsorizing the data (replacing extreme values) before calculation. This can be particularly useful in datasets with significant outliers.
}

\value{
A data frame containing the correlation matrix with the specified method. The matrix includes the correlations between each pair of variables in the specified range.
}

\examples{
resultado <- calcular_correlaciones(df_new_renombrado, "Ansiedad de separación", "Búsqueda de atención", method = "pearson", winsorize = TRUE)
print(resultado)
}

\author{
Professor José Ventura León
}
