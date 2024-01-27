\name{plotstrel}
\alias{plotstrel}
\title{Bayesian posterior reliability plot}

\description{
This function is based on the first version of the Bayesrel library published on January 20, 2019 by author Julius Pfadt and Donvanden Bergh. However, since it does not appear in the latest version of the Bayesrel library, it has been modified to be compatible with it.
This function provides a display of the Posterior graph. It is designed to facilitate the understanding of the reliability of estimates, comparing prior and posterior distributions, and the impact of different thresholds on these estimates. The function allows for the application of thresholds to emphasize significant changes in the estimates, thereby aiding in a clearer understanding of the reliability structure.
}

\usage{
plotstrel(x, estimate, blackwhite = FALSE, criteria = TRUE, cuts = c(.70, .80), twopie = FALSE)
}

\arguments{
  \item{x}{An object containing Bayesian model results, including samples and prior information.}
  \item{estimate}{The specific estimate to be analyzed, identified by its name in the model.}
  \item{blackwhite}{Logical. If TRUE, plots are generated in black and white. Default is FALSE.}
  \item{criteria}{Logical. If TRUE, applies predefined criteria for significance thresholds. Default is TRUE.}
  \item{cuts}{A numeric vector specifying the thresholds for analysis. Default is c(.70, .80).}
  \item{twopie}{Logical. If TRUE, generates a two-pie chart for visual comparison. Default is FALSE.}
}

\details{
The function plotstrel extracts and processes Bayesian reliability estimates, providing visual insights into their distribution and robustness. It compares prior and posterior distributions with adjustable thresholds, offering a detailed examination of the estimates' reliability. The option to include pie charts further enhances the interpretability of the results, especially useful in the context of decision-making and sensitivity analysis.
}

\value{
Graphical output depicting the density plots of the prior and posterior distributions of the estimate, with optional pie charts for a visual summary. The graphical representation helps in understanding the distribution and significance of the reliability estimates.
}

\examples{
# Assuming 'model_output' is an object containing Bayesian model results
plotstrel(model_output, estimate = "reliability", blackwhite = FALSE, criteria = TRUE, cuts = c(.70, .80), twopie = TRUE)
}

\author{
Professor José Ventura León
}

