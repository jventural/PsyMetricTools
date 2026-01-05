\name{plot_cfa_stability}
\alias{plot_cfa_stability}
\title{Plot Stability of CFA Model Fit Indices Across Varying Sample Sizes}

\description{
Generates line plots to visualize the stability of CFA model fit indices across different sample sizes.
}

\usage{
plot_cfa_stability(resultados, num_factors = 3, y_min_omega = 0.7,
                   y_max_omega = 1, y_breaks_omega = 0.05, y_min_cfi = 0.9,
                   y_max_cfi = 1, y_min_tli = 0.9, y_max_tli = 1,
                   y_min_rmsea = 0, y_max_rmsea = 0.15, y_min_srmr = 0,
                   y_max_srmr = 0.15, y_min_crmr = 0, y_max_crmr = 0.15,
                   y_min_reliability = 0.5, y_max_reliability = 1,
                   y_breaks_comparative = 0.02, y_breaks_absolutes = 0.05,
                   hline_color = "red", xlab_size = 12, ylab_size = 12)
}

\arguments{
  \item{resultados}{Data frame with bootstrap CFA results.}
  \item{num_factors}{Number of factors in the model (default 3).}
  \item{y_min_omega}{Minimum y-axis value for omega plot (default 0.7).}
  \item{y_max_omega}{Maximum y-axis value for omega plot (default 1).}
  \item{y_breaks_omega}{Y-axis breaks for omega plot (default 0.05).}
  \item{y_min_cfi}{Minimum y-axis value for CFI plot (default 0.9).}
  \item{y_max_cfi}{Maximum y-axis value for CFI plot (default 1).}
  \item{y_min_tli}{Minimum y-axis value for TLI plot (default 0.9).}
  \item{y_max_tli}{Maximum y-axis value for TLI plot (default 1).}
  \item{y_min_rmsea}{Minimum y-axis value for RMSEA plot (default 0).}
  \item{y_max_rmsea}{Maximum y-axis value for RMSEA plot (default 0.15).}
  \item{y_min_srmr}{Minimum y-axis value for SRMR plot (default 0).}
  \item{y_max_srmr}{Maximum y-axis value for SRMR plot (default 0.15).}
  \item{y_min_crmr}{Minimum y-axis value for CRMR plot (default 0).}
  \item{y_max_crmr}{Maximum y-axis value for CRMR plot (default 0.15).}
  \item{y_min_reliability}{Minimum y-axis value for reliability plot (default 0.5).}
  \item{y_max_reliability}{Maximum y-axis value for reliability plot (default 1).}
  \item{y_breaks_comparative}{Y-axis breaks for comparative indices (default 0.02).}
  \item{y_breaks_absolutes}{Y-axis breaks for absolute indices (default 0.05).}
  \item{hline_color}{Color for reference lines (default "red").}
  \item{xlab_size}{X-axis label size (default 12).}
  \item{ylab_size}{Y-axis label size (default 12).}
}

\value{
A combined ggplot object with line plots for CFI, TLI, RMSEA, SRMR, CRMR, and omega.
}

\examples{
\dontrun{
resultados <- boot_cfa_stability(model, data = df, seed = 2023)
figure <- plot_cfa_stability(resultados, num_factors = 3)
}
}

\author{
Dr. Jose Ventura-Leon
}

