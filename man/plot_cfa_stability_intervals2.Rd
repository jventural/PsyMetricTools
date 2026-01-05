\name{plot_cfa_stability_intervals2}
\alias{plot_cfa_stability_intervals2}
\title{Generation of CFA Stability Panels with Confidence Intervals}

\description{
This function creates three panels of plots to visualize the stability of fit indices and reliability in Confirmatory Factor Analysis (CFA). Given a data frame of results (percentages of cases and index values), it computes the 2.5 % and 97.5 % percentiles and the mean for each index, and displays confidence ribbons around lines and points. It supports renaming factors and adjusting ribbon transparency.
}

\usage{
plot_cfa_stability_intervals2(resultados, num_factors = 1,
                              alpha_ribbon = 0.2,
                              rename_factors = NULL, ...)
}

\arguments{
\item{resultados}{A data frame containing a \code{Porcentaje} column and columns for indices: \code{chisq.scaled}, \code{srmr}, \code{wrmr}, \code{cfi.scaled}, \code{crmr}, \code{tli.scaled}, \code{rmsea.scaled}, and \code{F1}, \code{F2}, … according to \code{num_factors}.}
\item{num_factors}{Integer specifying the number of factors (F1, F2, …) present in the results. Default is \code{1}.}
\item{alpha_ribbon}{Numeric transparency value for the confidence ribbons. Default is \code{0.2}.}
\item{rename_factors}{Optional named vector to rename factor labels in the plots.}
\item{\dots}{Additional arguments passed internally (not used directly).}
}

\details{

Loads \code{dplyr}, \code{tidyr}, and \code{ggplot2}.

Defines the set of indices to evaluate (fit and reliability).

Converts \code{Porcentaje} to numeric and reshapes data to long format.

Applies factor renaming if \code{rename_factors} is provided and sets factor levels.

Groups by \code{Porcentaje} and each index to compute the 2.5 % and 97.5 % percentiles and the mean.

Splits indices into three groups by value range:

Group 1: \code{cfi.scaled}, \code{tli.scaled} (range 0–1)

Group 2: \code{rmsea.scaled}, \code{srmr}, \code{crmr} (range 0–0.20)

Group 3: Factors \code{F1}, \code{F2}, … (range 0–1)

For each group, generates a plot with the mean line, points, and confidence ribbon.

Returns a list with three \code{ggplot2} objects:
\itemize{
\item \code{cfi_tli}: panel of CFI and TLI indices
\item \code{rm_srmr_crmr}: panel of RMSEA, SRMR, and CRMR indices
\item \code{factors}: panel of factor reliability indices
}
}

\value{
A list of three \code{ggplot2} objects:
\describe{
\item{cfi_tli}{Plot of \code{cfi.scaled} and \code{tli.scaled}.}
\item{rm_srmr_crmr}{Plot of \code{rmsea.scaled}, \code{srmr}, and \code{crmr}.}
\item{factors}{Plot of factor indices \code{F1}, \code{F2}, … .}
}
}

\examples{
\dontrun{

Assuming 'res' contains Porcentaje, cfi.scaled, tli.scaled, rmsea.scaled, srmr, crmr, F1
plots <- plot_cfa_stability_intervals2(res, num_factors = 1)
print(plots$cfi_tli)
print(plots$rm_srmr_crmr)
print(plots$factors)
}
}

\author{
Dr. José Ventura-León
}
