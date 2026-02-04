\name{agrupar_por_factor}
\alias{agrupar_por_factor}
\title{Automatic Grouping of Items by Factor Loadings Above a Threshold}
\description{
This function takes a \code{data.frame} containing item identifiers and one or more numeric columns representing factor loadings, then automatically assigns each item to all factors for which its loading exceeds a specified \code{threshold}. The output is a named list where each element corresponds to a factor column and contains the vector of item names whose loading in that factor is greater than \code{threshold}.
}
\usage{
agrupar_por_factor(df, item_col = "Items", threshold = 0)
}
\arguments{
\item{df}{A \code{data.frame} that includes at least:
\describe{
\item{A column with item identifiers (e.g., “Items”).}{These are the names or codes of each item.}
\item{One or more numeric columns.}{Each numeric column is interpreted as a factor loading for the corresponding item.}
}
}
\item{item_col}{Character. The name of the column in \code{df} that contains item identifiers. Default: \code{"Items"}.}
\item{threshold}{Numeric. The cutoff value for each factor loading. Only items whose loading in a given factor column is strictly greater than \code{threshold} will be included in that factor’s group. Default: \code{0}.}
}
\details{
\code{agrupar_por_factor} follows these steps internally:
\enumerate{
\item Identify all columns in \code{df} except \code{item_col}.
\item Among those, keep only the columns that are numeric—these are treated as “factor” columns.
\item For each numeric factor column \code{fac}, create a logical mask \code{df[[fac]] > threshold}.
\item Use that mask to subset the values in \code{df[[item_col]]}, producing all item identifiers whose loading in \code{fac} exceeds \code{threshold}.
\item Store the resulting vector of item names under the list element named after \code{fac}.
}
}
\value{
A named \code{list} of character vectors. Each element’s name is one factor column (e.g., \code{"f1"} or \code{"f2"}), and its value is a character vector of item names whose corresponding numeric loading exceeds \code{threshold} in that factor.
}
\examples{
\dontrun{
# Example data.frame of loadings:
resultado <- agrupar_por_factor(Patron1, item_col = "Items")
# resultado will be a list with factors as elements
}
}
\author{
Dr. José Ventura-León
}
