\name{combine_likert_sem}
\alias{combine_likert_sem}
\title{Combine a Likert Plot with a SEM Path Diagram into a Single Figure}

\description{
This function merges a Likert‐style bar chart (created with \code{gglikert} or any \code{ggplot} object) and a structural equation model path diagram (from \code{semPlot::semPaths}) into a single composite figure. It provides flexible control over layout, relative panel widths, annotation tags, and custom arguments to pass to \code{semPaths}.
}

\usage{
combine_likert_sem(plot_likert, fit_sem,
                   sem_args     = list(),
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
  \item{sem_args}{A named list of additional arguments to pass to \code{semPaths} (e.g., \code{sizeMan}, \code{edge.label.cex}).}
  \item{ncol}{Number of columns in the combined figure. Default is \code{2}.}
  \item{widths}{Numeric vector of relative widths for each column. Default is \code{rep(1, ncol)}.}
  \item{tag_levels}{Character string indicating the starting tag letter (e.g., \code{"A"} yields \code{"A"}, \code{"B"}).}
  \item{tag_suffix}{Optional suffix appended to each tag. Default is \code{""}.}
  \item{tag_pos}{Numeric vector \code{c(x, y)} giving the base position of the tags within each panel (0–1 scale).}
  \item{tag_offset_y}{Numeric offset added to the \code{y}-coordinate of tags, to lift them above the plot area.}
  \item{tag_size}{Font size for the annotation tags.}
  \item{tag_face}{Font face for the annotation tags (e.g., \code{"bold"}).}
}

\details{
\code{combine_likert_sem} begins by ensuring required packages (\code{ggplotify}, \code{patchwork}, \code{semPlot}) are installed and loaded.
It constructs a secondary \code{ggplot} object for the SEM diagram by wrapping \code{semPlot::semPaths} via \code{ggplotify::as.ggplot}, allowing the two panels to be combined under the same \code{ggplot2}/\code{patchwork} framework.
Custom \code{semPaths} parameters may be supplied through \code{sem_args} to override defaults.
Finally, the function arranges the Likert plot and SEM diagram side by side (or in multiple columns), adds annotation tags (“A”, “B”, …) at user‐specified positions (with optional vertical offset), and returns the combined \code{patchwork} object.
}

\value{
A \code{patchwork} object (which inherits from \code{ggplot}) representing the composite figure. It can be printed or saved using standard \code{ggplot2} functions.
}

\examples{
\dontrun{
library(gglikert)
library(lavaan)

# 1. Create a Likert plot
plot_likert <- gglikert(df_variables) +
  labs(x = "Percentage of responses", y = "Items") +
  theme_minimal()

# 2. Fit a simple SEM
model <- 'F1 =~ y1 + y2 + y3'
fit1  <- lavaan::cfa(model, data = mydata)

# 3. Combine plots with custom settings
combined_plot <- combine_likert_sem(
  plot_likert   = plot_likert,
  fit_sem       = fit1,
  sem_args      = list(sizeMan = 6, edge.label.cex = 1),
  ncol          = 2,
  widths        = c(0.8, 1),
  tag_levels    = "A",
  tag_pos       = c(0.05, 0.90),
  tag_offset_y  = 0.03,
  tag_size      = 14
)

# 4. Display or save
print(combined_plot)
ggsave("combined_figure.png", combined_plot, width = 10, height = 5)
}
}

\author{
Dr. José Ventura-León
}
