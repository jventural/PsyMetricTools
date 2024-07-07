\name{plot_lvm}
\alias{plot_lvm}
\title{Latent Variable Model Visualization Tool}

\description{
This function provides a comprehensive visualization of various aspects of a latent variable model (LVM) using the `psychonetrics` package. It allows for plotting the estimated network, factor loadings, residual correlations, residual partial correlations, and latent variables. The visualizations are essential for understanding the structural relationships within the model and the residual dependencies among observed variables.
}

\usage{
plot_lvm(
  x,
  plot = c("network", "loadings", "residcors", "residpcors", "latents"),
  ask,
  rotation = promax,
  vsize = c(5, 5),
  palette = "Set1",
  ...
)
}

\arguments{
  \item{x}{An object of class `psychonetrics` containing the latent variable model to be visualized.}
  \item{plot}{A character vector specifying the types of plots to generate. Options include "network", "loadings", "residcors", "residpcors", and "latents".}
  \item{ask}{A logical value indicating whether to prompt for user input before generating each plot when multiple plots are specified.}
  \item{rotation}{A rotation method for the factor loadings. Defaults to `promax`.}
  \item{vsize}{A numeric vector of length two specifying the sizes of the nodes in the plots.}
  \item{palette}{A character string specifying the color palette from RColorBrewer to use for the plots.}
  \item{...}{Additional arguments passed to the `qgraph` and `qgraph.loadings` functions.}
}

\details{
The function leverages the `qgraph` and `RColorBrewer` packages to generate the visualizations. It begins by checking if the specified color palette is available and retrieves the corresponding colors. It then prepares the data for plotting, including setting appropriate row names for the lambda matrix and identifying observed variables.

The function provides different types of plots:
\itemize{
  \item{network: }{Plots the estimated network of the latent variable model.}
  \item{loadings: }{Plots the factor loadings after applying the specified rotation.}
  \item{residcors: }{Plots the residual correlations.}
  \item{residpcors: }{Plots the residual partial correlations.}
  \item{latents: }{Plots the network of latent variables.}
}

Each plot type uses specific settings for node shapes and sizes, tailored to the type of data being visualized. The resulting plots are stored in a list and returned invisibly.
}

\value{
A list of `qgraph` objects representing the specified visualizations of the latent variable model. This allows for a detailed examination of the model's structure and the residual correlations among observed variables.
}

\examples{
# Assuming `lvm` is an object of class `psychonetrics`
plot_lvm(mod2, "network", groups = groups, palette = "Dark2")
plot_lvm(mod2, "loadings", groups = groups, palette = "Dark2", vsize = c(3, 12), edge.width = 2)
plot_lvm(mod2, "residcors", groups = groups, palette = "Dark2")
plot_lvm(mod2, "residpcors", groups = groups, palette = "Dark2")
plot_lvm(mod2, "latents", groups = groups, vsize = 8, vsize2 = 8, edge.labels = T, palette = "Dark2")
}

\author{
José Ventura-León
}
