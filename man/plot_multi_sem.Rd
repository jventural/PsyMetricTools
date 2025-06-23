\name{plot_multi_sem}
\alias{plot_multi_sem}
\title{Plot Multiple SEM Models with Fit Indices and Customization}
\description{
This function takes a \code{list} of fitted \code{lavaan} model objects and displays them side by side as path diagrams, with control over layout, label type, node and edge styling, automatic annotation of fit indices (e.g.\ CFI, TLI, RMSEA, SRMR), custom panel titles, and optional saving of the combined plot to file in various formats.
}
\usage{
plot_multi_sem(
  models,
  titles = NULL,
  layout = "tree2",
  rotation = 2,
  whatLabels = "std",
  residuals = FALSE,
  intercepts = FALSE,
  thresholds = FALSE,
  sizeMan = 10,
  sizeMan2 = 5,
  sizeLat = 12,
  label.cex = 1,
  edge.label.cex = 2,
  edge.color = "grey40",
  color = list(lat = "grey80", man = "grey90"),
  mar = c(4,4,4,4),
  outerMar = c(1,1,3,1),
  show_fit_indices = TRUE,
  fit_indices = c("cfi","tli","rmsea","srmr"),
  use_scaled = FALSE,
  model_descriptions = NULL,
  custom_titles = NULL,
  title.cex = 0.8,
  title.font = 2,
  save_plot = FALSE,
  filename = "sem_models_plot",
  file_format = "png",
  width_per = 4,
  height = 4,
  dpi = 300,
  units = "in"
)
}
\arguments{
  \item{models}{A \code{list} of fitted \code{lavaan} model objects to be plotted.}
  \item{titles}{Optional \code{character} vector of panel titles when \code{show_fit_indices = FALSE}.}
  \item{layout}{\code{character} specifying the \code{semPaths} layout algorithm.}
  \item{rotation}{\code{numeric} (1–4) controlling diagram rotation.}
  \item{whatLabels}{\code{character} indicating which estimates to label ("std", "est", etc.).}
  \item{residuals}{\code{logical} whether to display residual variances.}
  \item{intercepts}{\code{logical} whether to display intercepts.}
  \item{thresholds}{\code{logical} whether to display thresholds for ordinal indicators.}
  \item{sizeMan}{\code{numeric} size of primary manifest variable nodes.}
  \item{sizeMan2}{\code{numeric} size of secondary manifest variable nodes.}
  \item{sizeLat}{\code{numeric} size of latent variable nodes.}
  \item{label.cex}{\code{numeric} character expansion for variable labels.}
  \item{edge.label.cex}{\code{numeric} character expansion for path labels.}
  \item{edge.color}{\code{character} color for the path arrows.}
  \item{color}{\code{list} with elements \code{lat} and \code{man} specifying fill colors.}
  \item{mar}{\code{numeric} vector for inner margins of each panel.}
  \item{outerMar}{\code{numeric} vector for outer margins around the multi‐panel layout.}
  \item{show_fit_indices}{\code{logical} whether to extract and display fit indices in the title.}
  \item{fit_indices}{\code{character} vector of fit measure names for \code{lavaan::fitMeasures()}.}
  \item{use_scaled}{\code{logical} if \code{TRUE}, use the \code{*.scaled} versions of CFI/TLI/RMSEA.}
  \item{model_descriptions}{\code{character} vector of descriptive text for each model (first title line).}
  \item{custom_titles}{\code{character} vector of fully custom title text (overrides other titles).}
  \item{title.cex}{\code{numeric} character expansion for the title/fit‐index text.}
  \item{title.font}{\code{integer} font style for the title (1=plain, 2=bold, 3=italic).}
  \item{save_plot}{\code{logical} if \code{TRUE}, open a graphics device and save the output to file.}
  \item{filename}{\code{character} base name (without extension) for the saved file.}
  \item{file_format}{\code{character} one of "png", "pdf", "tiff", or "jpeg".}
  \item{width_per}{\code{numeric} width (in specified \code{units}) for each individual panel.}
  \item{height}{\code{numeric} height of the output device in the specified \code{units}.}
  \item{dpi}{\code{numeric} resolution in dots per inch (for bitmap formats).}
  \item{units}{\code{character} units for \code{width_per} and \code{height}, e.g. "in" or "cm".}
}
\details{
Internally, \code{plot_multi_sem} sets up a \code{1 × n} plotting layout via \code{par(mfrow, mar, outerMar)}, loops over each model calling \code{semPlot::semPaths()} with the specified styling options, optionally extracts and formats fit indices via \code{lavaan::fitMeasures()}, adds them in a two‐line panel title (or uses custom titles), restores the original graphics parameters, and closes the device if \code{save_plot = TRUE}.
}
\value{
Invisible \code{NULL}. The side effect is the display of SEM path diagrams on the current graphics device or creation of a saved file when \code{save_plot = TRUE}.
}
\examples{
\dontrun{
# Basic two‐panel plot
plot_multi_sem(models = list(fit1, fit2))

# With custom descriptions and larger text
plot_multi_sem(
  models = list(fitA, fitB),
  model_descriptions = c("Uni‐dimensional", "Bi‐dimensional"),
  sizeMan = 14,
  label.cex = 1.2,
  title.cex = 1.0,
  title.font = 2
)

# Save as high‐res JPEG
plot_multi_sem(
  models = list(fit1, fit2),
  save_plot   = TRUE,
  filename    = "sem_compare",
  file_format = "jpeg",
  width_per   = 5,
  height      = 4,
  dpi         = 300
)
}
}
\author{
Professor José Ventura\-León
}
