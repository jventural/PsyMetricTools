\name{combine_likert_sem}
\alias{combine_likert_sem}
\title{Combine a Likert Plot with a SEM Path Diagram into a Single Figure}
\description{
This function merges a Likert‐style bar chart (any \code{ggplot} object) and a structural equation model path diagram (from \code{semPlot::semPaths}) into a single composite figure. It provides flexible control over layout, relative panel widths, annotation tags, custom \code{semPaths} arguments, and explicit node labeling.
}
\usage{
combine_likert_sem(plot_likert, fit_sem,
                   sem_args     = list(),
                   nodeLabels   = NULL,
                   ncol         = 2,
                   widths       = rep(1, ncol),
                   tag_levels   = "A",
                   tag_suffix   = "",
                   tag_pos      = c(0.05, 0.95),
                   tag_offset_y = 0.02,
                   tag_size     = 16,
                   tag_face     = "bold")
}
\arguments{
  \item{plot_likert}{A \code{ggplot} object representing the Likert‐style bar chart.}
  \item{fit_sem}{A fitted \code{lavaan} (or compatible) object to be visualized by \code{semPlot::semPaths}.}
  \item{sem_args}{A named list of additional arguments to pass to \code{semPaths} (e.g., \code{sizeMan}, \code{edge.label.cex}, \code{style}).}
  \item{nodeLabels}{An optional character vector of node labels for the SEM diagram; passed directly to the \code{nodeLabels} argument of \code{semPlot::semPaths}.}
  \item{ncol}{Number of columns in the combined figure. Default is \code{2}.}
  \item{widths}{Numeric vector of relative widths for each column; default is \code{rep(1, ncol)}.}
  \item{tag_levels}{Starting tag letter for annotation (e.g., \code{"A"} produces tags \code{"A"}, \code{"B"}, …).}
  \item{tag_suffix}{Optional suffix appended after each tag. Default is \code{""}.}
  \item{tag_pos}{Numeric vector \code{c(x, y)} giving the base position of tags within each panel (in normalized device coordinates, 0–1).}
  \item{tag_offset_y}{Vertical offset added to \code{tag_pos[2]} to lift tags above the plot; default is \code{0.02}.}
  \item{tag_size}{Font size for the annotation tags; default is \code{16}.}
  \item{tag_face}{Font face for the annotation tags (e.g., \code{"bold"}); default is \code{"bold"}.}
}
\details{
\code{combine_likert_sem} first ensures that \pkg{ggplotify}, \pkg{patchwork}, and \pkg{semPlot} are installed and loaded.
It constructs a \pkg{ggplot2} object for the SEM diagram by wrapping \code{semPlot::semPaths} via \code{ggplotify::as.ggplot}, allowing the two panels to be combined under the same \pkg{patchwork} layout.
Users may override any of the default \code{semPaths} parameters via \code{sem_args}, and may supply explicit node labels through \code{nodeLabels}. Annotation tags (“A”, “B”, …) are added at user‐specified positions with optional vertical offset.
}
\value{
A \code{patchwork} object (which inherits from \code{ggplot}) representing the composite figure. It can be printed or saved using standard \pkg{ggplot2} functions.
}
\examples{
\dontrun{
library(gglikert)
library(lavaan)

# 1. Create a Likert plot
plot_likert <- gglikert(my_df) +
  labs(x = "Percentage", y = "Items") +
  theme_minimal()

# 2. Fit a simple SEM
model <- 'F1 =~ y1 + y2 + y3'
fit1  <- lavaan::cfa(model, data = my_df)

# 3. Combine with custom node labels and semPaths args
combined_plot <- combine_likert_sem(
  plot_likert = plot_likert,
  fit_sem     = fit1,
  sem_args    = list(sizeMan = 6, edge.label.cex = 1.2),
  nodeLabels  = c("Y1","Y2","Y3","F1"),
  ncol        = 2,
  widths      = c(0.8, 1),
  tag_levels  = "A",
  tag_pos     = c(0.05, 0.90),
  tag_offset_y = 0.03,
  tag_size    = 14
)

# 4. Display or save
print(combined_plot)
ggsave("combined_figure.png", combined_plot, width = 10, height = 5)
}
}
\author{
Dr. José Ventura-León
}
