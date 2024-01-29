\name{traceplot_bayesrel}
\alias{traceplot_bayesrel}
\title{Traceplot Generation Tool for Bayesian MCMC Samples}

\description{
  This function generates traceplots for Bayesian MCMC samples extracted from a Bayesian model object. It is designed to help in visualizing and analyzing the convergence and mixing properties of MCMC chains in Bayesian models, particularly useful in the context of Bayesian reliability analysis. The function offers the flexibility to customize the color palette, enhancing the visual distinction between different MCMC chains.
}

\usage{
traceplot_bayesrel(res, paleta_colores = NULL)
}

\arguments{
  \item{res}{A Bayesian model object containing MCMC samples. This object is expected to have a specific structure, with the MCMC samples located at `res[["Bayes"]][["samp"]][["Bayes_omega"]]`.}
  \item{paleta_colores}{An optional argument for a color palette. This can be a vector of color names or a function returning a color palette. If provided, it is used to color the lines in the traceplot for different MCMC chains. Default is `NULL`, which uses the ggplot2 default color palette.}
}

\details{
  The `traceplot_bayesrel` function processes the MCMC samples from the given Bayesian model object, transforming them into a format suitable for ggplot2 visualization. It creates a traceplot where each line represents an MCMC chain, allowing for an assessment of chain convergence and overlap. This is crucial for diagnosing the reliability of the Bayesian model estimation.
}

\value{
  A ggplot object representing the traceplot of MCMC samples. Each line in the plot corresponds to a separate chain, with iterations on the x-axis and sampled values on the y-axis.
}

\examples{
# Example of use with a custom color palette
mi_traceplot <- crear_traceplot_bayes(res, paleta_colores = c("red", "blue", "green"))
print(mi_traceplot)

# Or using a palette of RColorBrewer
mi_traceplot <- crear_traceplot_bayes(res, paleta_colores = RColorBrewer::brewer.pal(3, "Set1"))
print(mi_traceplot)

}

\author{
Professor José Ventura León
}
