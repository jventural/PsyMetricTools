\name{Density_report}
\alias{Density_report}
\title{Network Density Calculation}

\description{
This function calculates the density of a network based on the edge matrix provided. Network density is a measure of the proportion of actual connections in the network relative to the total possible connections. It takes an edge matrix as input and computes the density by determining the number of non-zero connections. This function is useful in network analysis, allowing researchers to quantify how densely connected a network is.
}

\usage{
Density_report(edge.matrix)
}

\arguments{
  \item{edge.matrix}{A matrix representing the edges in the network, where rows and columns represent nodes, and cell values indicate the presence (and possibly the weight) of an edge between nodes.}
}

\details{
The function performs the following steps:
1. Determines the total number of possible connections in the network, which is calculated as  \eqn{n * (n-1) / 2}, where \eqn{n} is the number of nodes in the network.
2. Calculates the number of actual connections different from zero by summing the entries in the edge matrix that are non-zero and dividing by 2, to account for the bidirectionality in an undirected network.
3. Computes the network density as the ratio of the number of non-zero connections to the total possible connections, expressed as a percentage.
4. Returns a string summarizing the number of non-zero connections and the calculated density of the network.
}

\value{
A character string summarizing the total number of non-zero connections out of the total possible connections and the density of the network as a percentage.
}

\examples{
edge.matrix <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), nrow = 3, byrow = TRUE)
result <- Density_report(edge.matrix)
print(result)
}

\note{
The function assumes an undirected network and counts each connection only once. Ensure that the edge matrix is symmetric and correctly represents the network's connections.
}

\author{
Professor José Ventura León
}

