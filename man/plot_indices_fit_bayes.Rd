\name{plot_indices_fit_bayes}
\alias{plot_indices_fit_bayes}
\title{Density Plot Generation Tool for Bayesian Fit Indices}

\description{
  This function generates density plots for the Bayesian fit indices BRMSEA and BCFI from a Bayesian model object. It is designed to aid in visualizing the distribution of fit indices, which are crucial for assessing model fit in a Bayesian framework. The function allows for a customizable color palette to enhance the visual distinction between different fit indices.
}

\usage{
plot_indices_fit_bayes(bfit1, custom_palette = NULL)
}

\arguments{
  \item{bfit1}{A Bayesian model object containing fit indices. The object is expected to have a list or similar structure where fit indices can be accessed using `@indices[["BRMSEA"]]` and `@indices[["BCFI"]]`.}
  \item{custom_palette}{An optional argument for a color palette. This can be a vector of color names or a function returning a color palette. If provided, it is used to fill the density plots for the BRMSEA and BCFI indices. Default is `NULL`, which uses a color palette from RColorBrewer.}
}

\details{
  The `plot_indices_fit_bayes` function processes the Bayesian fit indices from the given model object, transforming them into a long format suitable for ggplot2 visualization. It creates a density plot for each fit index, displayed in a facet layout. This visualization is helpful for assessing the uncertainty and distribution of the fit indices derived from Bayesian analysis.
}

\value{
  A ggplot object representing the density plots of Bayesian fit indices. The x-axis displays the value of the fit indices, and the y-axis shows their density.
}

\examples{
# Example of use with a custom color palette
indices_density_plot <- plot_indices_fit_bayes(bfit1, custom_palette = c("darkorange", "dodgerblue"))
print(indices_density_plot)

# Or using a palette of RColorBrewer
indices_density_plot <- plot_indices_fit_bayes(bfit1, custom_palette = RColorBrewer::brewer.pal(3, "Set1"))
print(indices_density_plot)
}

\author{
Professor José Ventura León
}


