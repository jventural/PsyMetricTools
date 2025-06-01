\name{calcula_omega_mcdonald}
\alias{calcula_omega_mcdonald}
\title{Compute McDonald’s Omega Reliability for Specified Item Groups}
\description{
This function calculates McDonald’s omega reliability coefficient for one or more sets of items (groups) based on their factor loadings. It supports two methods for estimating communality (\eqn{h^2}):

\code{"comunalidad"}: \eqn{h^2 = (\sum \lambda)^2}

\code{"sum_loadings"}: \eqn{h^2 = \sum \lambda}

Given a \code{data.frame} of item loadings and a specification of groups (either \code{NULL}, a single character vector of item names, or a list of such vectors), the function returns both per‐item statistics (generalized loading, communality, uniqueness) and the overall omega for each group.
}
\usage{
calcula_omega_mcdonald(loadings_df,
groups = NULL,
item_col = NULL,
method = c("comunalidad", "sum_loadings"))
}
\arguments{
\item{loadings_df}{A \code{data.frame} containing item names and their factor loadings. The first column (or the column specified by \code{item_col}) must list item identifiers, and the remaining numeric columns represent loadings on one or multiple factors.}
\item{groups}{Specifies which items belong to each group. Can be:
\describe{
\item{\code{NULL}}{All items in \code{loadings_df} are treated as a single group named “All.”}
\item{A character vector}{Interpreted as a single unnamed group (“Group1”) containing the items in that vector.}
\item{A list of character vectors}{Each element is a group of item names. If unnamed, groups are automatically named “Group1,” “Group2,” etc.}
}
}
\item{item_col}{Character. Name of the column in \code{loadings_df} that contains item identifiers. If \code{NULL}, the first column of \code{loadings_df} is used.}
\item{method}{Choice of \code{"comunalidad"} or \code{"sum_loadings"}.
\describe{
\item{\code{"comunalidad"}}{Compute communality as \eqn{h^2 = (\sum \lambda)^2} for each item.}
\item{\code{"sum_loadings"}}{Compute communality as \eqn{h^2 = \sum \lambda} for each item.}
}
The chosen method affects the calculation of item uniqueness (\eqn{\theta = 1 - h^2}).
}
}
\details{
\code{calcula_omega_mcdonald} proceeds in these steps:
\enumerate{
\item Identify the item column: if \code{item_col} is \code{NULL}, the first column of \code{loadings_df} is assumed to contain item names.
\item Prepare \code{groups_list}:
\itemize{
\item If \code{groups = NULL}, create a single group “All” containing all item names.
\item If \code{groups} is a character vector, create a single group named “Group1.”
\item If \code{groups} is a list of character vectors, use it directly. Any unnamed elements are auto‐named “Group1,” “Group2,” etc.
}
\item For each group in \code{groups_list}, subset \code{loadings_df} to only those rows whose item name appears in the group.
\item Form a matrix \code{mat} of numeric loadings by dropping the \code{item_col} and converting the remaining columns to a numeric matrix.
\item Compute \code{gen_loading} for each item as the row sum of \code{mat}: \eqn{\lambda_{\text{gen}} = \sum_{j} \lambda_{ij}}.
\item Depending on \code{method}:
\itemize{
\item If \code{"comunalidad"}: \eqn{h^2 = (\lambda_{\text{gen}})^2}.
\item If \code{"sum_loadings"}: \eqn{h^2 = \lambda_{\text{gen}}}.
}
Then compute item uniqueness: \eqn{\theta = 1 - h^2}.
\item Compute group omega:
\deqn{\omega = \frac{\sum_i h_i^2}{\sum_i h_i^2 + \sum_i \theta_i}}
\item Construct \code{item_stats}, a \code{data.frame} with columns:
\describe{
\item{\code{Item}}{Item identifier.}
\item{\code{gen_loading}}{Generalized factor loading for that item.}
\item{\code{h2}}{Communality (either squared sum or sum, per \code{method}).}
\item{\code{theta}}{Uniqueness (\eqn{1 - h^2}).}
}
\item Repeat steps 3–8 for every group in \code{groups_list}.
}
}
\value{
A \code{list} with two components:
\describe{
\item{\code{omegas}}{A \code{data.frame} with two columns:
\describe{
\item{\code{group}}{Group name (as in \code{groups_list}).}
\item{\code{omega}}{Calculated McDonald’s omega for that group.}
}
}
\item{\code{item_stats}}{A \code{list} of \code{data.frame}s, one per group. Each \code{data.frame} contains item‐level statistics (\code{Item}, \code{gen_loading}, \code{h2}, \code{theta}) for that group.}
}
}
\examples{
\dontrun{

# Supongamos un data.frame de cargas:
loadings_df <- data.frame(
Items = c("EAF1","EAF2","EAF3","EAF4","EAF5","EAF6","EAF8"),
f1 = c(0.91, 0.86, 0, 0.66, 0, 0, 0.79),
f2 = c(0, 0, 0.81, 0, 0.73, 0.79, 0)
)

# Definir manualmente grupos de ítems:
groups <- list(
f1 = c("EAF1","EAF2","EAF4","EAF8"),
f2 = c("EAF3","EAF5","EAF6")
)

# Calcular omegas usando communality method
resultado <- calcula_omega_mcdonald(
loadings_df = loadings_df,
groups = groups,
item_col = "Items",
method = "comunalidad"
)

# Ver los omegas en un data.frame
resultado$omegas

group omega
1 f1 0.8079501
2 f2 0.7772510

# Ver los item_stats para el grupo “f1”
resultado$item_stats$f1

Item gen_loading h2 theta
1 EAF1 0.91 0.828100 0.1719000
2 EAF2 0.86 0.739600 0.2604000
3 EAF4 0.66 0.435600 0.5644000
4 EAF8 0.79 0.624100 0.3759000

}
}
\author{
Professor José Ventura-León
}
