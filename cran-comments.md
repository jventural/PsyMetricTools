## Submission summary

Resubmission of PsyMetricTools (version 1.2.2), a package that provides
psychometric and statistical analysis tools for the social sciences
(data preprocessing, exploratory and confirmatory factor analysis,
reliability, measurement invariance, Likert-scale visualization, and
multi-class imbalance handling).

This resubmission addresses all points raised by Konstanze Lauseker in the
manual review of version 1.2.1 (2026-07-28):

* Description text: no longer starts with "A comprehensive package...";
  it now starts with "Provides tools for psychometric and statistical
  analysis...".

* \dontrun{}: all \dontrun{} wrappers were replaced with \donttest{}
  (58 files). Examples were rewritten to be fully self-contained (simulated
  data included) so they are executable by the user; short examples were
  unwrapped entirely. Bootstrap-based examples remain in \donttest{} because
  they refit CFA/EFA models many times and exceed 5 seconds by nature; their
  replication counts were reduced so they remain executable in reasonable
  time. Parallel examples use no more than 2 cores.

* print()/cat() (R/efa_with_bootstrap.R, R/boot_cfa_density.R): cat() calls
  were replaced with message(). In boot_cfa_density() the statistics table
  is no longer printed to the console; it is attached to the returned ggplot
  object as attribute "stats" so users can extract and print() it.

* Writing to the user's home filespace: plotting/export functions no longer
  write by default. Defaults changed from save = TRUE with a fixed filename
  to save = FALSE and path = NULL (boot_cfa_plot, boot_cfa_density,
  boot_cfa_raincloud, boot_cfa_plot_enhanced, boot_efa_plot,
  boot_efa_forest_plot, plot_multi_sem); export_summary_tables() now
  requires an explicit file path (no default). If saving is requested
  without a path, the function stops with a message suggesting tempdir().
  All examples that demonstrate saving write to tempdir().

* par() (R/plot_multi_sem.R): the function now stores the user's settings
  with oldpar <- par(no.readonly = TRUE) and restores them with an immediate
  on.exit(par(oldpar)) instead of resetting to default par(). Likewise,
  boot_cfa_stability() now restores the user's future::plan() with an
  immediate on.exit().

* set.seed() within functions (R/boot_cfa_raincloud.R): the fixed
  set.seed(123) was removed. Additionally, all seed arguments with fixed
  numeric defaults (boot_cfa, boot_cfa_stability, boot_efa,
  efa_with_bootstrap) now default to NULL and only call set.seed() when the
  user supplies a value.

## Test environments

* Local: Windows 11, R 4.4.1 — R CMD check --as-cran (donttest examples
  executed).
* win-builder (R-devel).

## R CMD check results

0 errors | 0 warnings | notes:

* "New submission" — resubmission of the 1.2.x line.
* "Possibly misspelled words in DESCRIPTION": 'Estabrook' is an author
  surname (Wu & Estabrook, 2016) and 'Likert' is the standard name of the
  rating-scale format.

## Downstream dependencies

There are currently no downstream dependencies on CRAN.
