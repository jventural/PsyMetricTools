\name{boot_cfa_plot}
\alias{boot_cfa_plot}
\title{Bootstrap CFA Plot}

\description{
Creates boxplot visualizations for bootstrap CFA results.
}

\usage{
boot_cfa_plot(df, save = TRUE, path = "Plot_boot_cfa.jpg", dpi = 600,
              omega_ymin_annot = NULL, omega_ymax_annot = NULL,
              comp_ymin_annot = NULL, comp_ymax_annot = NULL,
              abs_ymin_annot = NULL, abs_ymax_annot = NULL,
              palette = "grey", exclude_indices = NULL,
              show_tables = TRUE, ...)
}

\arguments{
  \item{df}{Data frame with results from boot_cfa().}
  \item{save}{Logical. Save the plot (default TRUE).}
  \item{path}{Path to save the plot (default "Plot_boot_cfa.jpg").}
  \item{dpi}{Resolution in DPI (default 600).}
  \item{omega_ymin_annot}{Y-axis minimum for omega annotation.}
  \item{omega_ymax_annot}{Y-axis maximum for omega annotation.}
  \item{comp_ymin_annot}{Y-axis minimum for comparative indices annotation.}
  \item{comp_ymax_annot}{Y-axis maximum for comparative indices annotation.}
  \item{abs_ymin_annot}{Y-axis minimum for absolute indices annotation.}
  \item{abs_ymax_annot}{Y-axis maximum for absolute indices annotation.}
  \item{palette}{Color palette (default "grey").}
  \item{exclude_indices}{Indices to exclude from plots (default NULL).}
  \item{show_tables}{Show summary tables in plots (default TRUE).}
  \item{...}{Additional arguments passed to ggsave.}
}

\value{
A combined ggplot object (invisibly).
}

\examples{
\dontrun{
boot_cfa_plot(results)
}
}

\author{
Dr. Jose Ventura-Leon
}

