## Submission summary

Update of PsyMetricTools from 1.2.2 (on CRAN since 2026-08-06) to 1.2.3.

Changes in this version:

* Bug fix in boot_cfa(): since lavaan 0.6-14 the 'test' option is stored as a
  vector (for example c("standard", "scaled.shifted") for WLSMV). The internal
  helpers inspected only its first element, which is always "standard", so
  robust fits were reported with the non-robust CFI, TLI and RMSEA instead of
  the scaled ones. They now test every declared statistic.
* New function run_sempowerlab(), which launches a Shiny application
  (inst/shiny/sempowerlab) that plans the sample size of a structural model
  with latent variables by wrapping semPower::semPower.powerRegression().
  shiny, bslib and semPower are in Suggests and are checked with
  requireNamespace() before the application starts; the example only runs
  inside if (interactive()).

## Test environments

* Local: Windows 11 x64, R 4.4.1 (R CMD check --as-cran --run-donttest,
  PDF manual built with pdflatex)
* win-builder: R-devel

## R CMD check results

win-builder (R-devel): Status OK (0 errors | 0 warnings | 0 notes).

Local (R 4.4.1, --run-donttest): 0 errors | 0 warnings | 2 notes

* "unable to verify current time": local environment only.
* "Examples with CPU or elapsed time > 5s": these are the \donttest{}
  examples of the bootstrap and EFA functions (unchanged since 1.2.2), which
  refit factor models many times; the note only appears because
  --run-donttest executes them.

## Downstream dependencies

There are currently no downstream dependencies on CRAN.
