\name{boot_cfa_plot}
\alias{boot_cfa_plot}
\title{Comprehensive Visualization and Optional Saving of Bootstrap CFA Results}
\description{
This function generates a comprehensive graphical display of bootstrap Confirmatory Factor Analysis (CFA) results, integrating three panels:
1. Omega reliability coefficients (plot and summary table),
2. Comparative Fit Index (CFI) and Tucker–Lewis Index (TLI) measures (plot and summary table),
3. Absolute fit measures (RMSEA, SRMR, CRMR) (plot and summary table).
The panels are arranged side by side for holistic comparison. An optional \code{save} parameter allows the figure to be written to a file (JPEG) with specified resolution.
}
\usage{
boot_cfa_plot(df,
              save = TRUE,
              path = "Plot_boot_cfa.jpg",
              dpi = 600,
              omega_ymin_annot = NULL,
              omega_ymax_annot = NULL,
              comp_ymin_annot = NULL,
              comp_ymax_annot = NULL,
              abs_ymin_annot = NULL,
              abs_ymax_annot = NULL,
              palette = "grey", ...)
}
\arguments{
  \item{df}{A \code{data.frame} containing bootstrap CFA results. Must include a column \code{fit_measures1} (a list of fit‐index tibbles) and numeric columns for omega coefficients in each replication.}
  \item{save}{Logical. If \code{TRUE}, saves the combined figure to \code{path}. If \code{FALSE}, returns the combined \code{ggplot} object without saving. Default: \code{TRUE}.}
  \item{path}{Character. File path (including filename and extension) for saving the figure, e.g.\ \code{"Output/combined_plot.jpg"}. Default: \code{"Plot_boot_cfa.jpg"}.}
  \item{dpi}{Numeric. Resolution in dots per inch for the saved image. Used only if \code{save = TRUE}. Default: \code{600}.}
  \item{omega_ymin_annot}{Numeric or \code{NULL}. Minimum y‐axis position for annotation inset in the omega panel. If \code{NULL}, set automatically.}
  \item{omega_ymax_annot}{Numeric or \code{NULL}. Maximum y‐axis position for annotation inset in the omega panel. If \code{NULL}, set automatically.}
  \item{comp_ymin_annot}{Numeric or \code{NULL}. Minimum y‐axis position for annotation inset in the comparative fit (CFI/TLI) panel. If \code{NULL}, set automatically.}
  \item{comp_ymax_annot}{Numeric or \code{NULL}. Maximum y‐axis position for annotation inset in the comparative fit panel. If \code{NULL}, set automatically.}
  \item{abs_ymin_annot}{Numeric or \code{NULL}. Minimum y‐axis position for annotation inset in the absolute fit (RMSEA/SRMR/CRMR) panel. If \code{NULL}, set automatically.}
  \item{abs_ymax_annot}{Numeric or \code{NULL}. Maximum y‐axis position for annotation inset in the absolute fit panel. If \code{NULL}, set automatically.}
  \item{palette}{Character. Color palette for the boxplots. Accepts a single color name (e.g.\ \code{"red"}), a hex code (e.g.\ \code{"#FF0000"}), \code{"grey"} for grayscale, or any palette name from the \pkg{wesanderson} package (e.g.\ \code{"Zissou1"}). Default: \code{"grey"}.}
  \item{\dots}{Additional arguments passed to \code{ggsave()} when \code{save = TRUE}, such as \code{device}, \code{units}, or custom filename settings.}
}
\details{
\code{boot_cfa_plot} internally calls three helper functions:
\itemize{
  \item \code{plot_and_table_omega}: Generates the omega reliability plot and summary table.
  \item \code{plot_and_table_comp}: Generates the CFI/TLI comparative fit plot and summary table.
  \item \code{plot_and_table_abs}: Generates the RMSEA/SRMR/CRMR absolute fit plot and summary table.
}
These three panels are combined into a single figure with labels “A”, “B”, and “C”.
If \code{save = TRUE}, the figure is written to \code{path} as a JPEG using the specified \code{dpi}; otherwise, the combined \code{ggplot} object is returned invisibly for on‐screen display.
The \code{palette} argument customizes boxplot fill colors, and any extra arguments in \dots{} are forwarded to \code{ggsave()} (e.g.\ to override file type or units).
}
\value{
A single \code{ggplot} (via \pkg{ggpubr}) object containing the three‐panel composite figure.
If \code{save = TRUE}, the object is returned invisibly after writing the JPEG file; if \code{save = FALSE}, it is returned visibly for immediate rendering.
}
\examples{
\dontrun{
# Assume 'results' is your bootstrap CFA output
# with components 'fit_measures1' (list of tibbles) and numeric omegas.
figure <- boot_cfa_plot(results,
                        save = TRUE,
                        path = "Output/combined_plot.jpg",
                        dpi = 600,
                        omega_ymin_annot = 0.6,
                        omega_ymax_annot = 0.9,
                        comp_ymin_annot  = 0.95,
                        comp_ymax_annot  = 0.99,
                        abs_ymin_annot   = 0.02,
                        abs_ymax_annot   = 0.12,
                        palette          = "Zissou1",
                        units            = "cm",
                        height           = 16,
                        width            = 22)
# If not saved, display on screen:
# print(figure)
}
}
\author{
Professor José Ventura-León
}
