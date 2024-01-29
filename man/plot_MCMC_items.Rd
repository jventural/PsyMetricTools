\name{plot_MCMC_items}
\alias{plot_MCMC_items}
\title{Density Plot Generation Tool for Standardized Bayesian MCMC Samples}

\description{
  This function generates density plots for standardized Bayesian MCMC samples extracted from a Bayesian model object. It is designed to help in visualizing the distribution of standardized parameter estimates across MCMC iterations, particularly useful for evaluating the uncertainty and variability of the estimates in a Bayesian framework. The function allows for a customizable color palette to enhance the visual representation of different items or parameters.
}

\usage{
plot_MCMC_items(fit2, custom_palette = NULL)
}

\arguments{
  \item{fit2}{A Bayesian model object containing MCMC samples. The object is expected to have a structure that can be converted into a data frame with relevant parameter estimates.}
  \item{custom_palette}{An optional argument for a color palette. This can be a vector of color names or a function returning a color palette. If provided, it is used to fill the density plots for different items or parameters. Default is `NULL`, which uses a pastel color palette from RColorBrewer.}
}

\details{
  The `plot_MCMC_items` function processes the MCMC samples from the given Bayesian model object, transforming them into a long format suitable for ggplot2 visualization. It creates a series of density plots, one for each item or parameter, displayed in a facet layout. This visualization aids in understanding the posterior distribution of the parameters.
}

\value{
  A ggplot object representing the density plots of MCMC samples for each parameter or item. The x-axis displays the value of the standardized estimates, and the y-axis shows the density.
}

\examples{
# Example of use with a custom color palette
mi_density_plot <- plot_MCMC_items(fit2, custom_palette = c("lightblue", "pink", "lightgreen"))
print(mi_density_plot)

# Or using a palette of RColorBrewer
mi_density_plot <- plot_MCMC_items(fit2, custom_palette = RColorBrewer::brewer.pal(3, "Pastel1"))
print(mi_density_plot)
}

\author{
Professor José Ventura León
}

