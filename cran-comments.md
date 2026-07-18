## Submission summary

Resubmission of PsyMetricTools (version 1.2.1), a package that provides
psychometric and statistical analysis tools for the social sciences
(data preprocessing, exploratory and confirmatory factor analysis,
reliability, measurement invariance, Likert-scale visualization, and
multi-class imbalance handling).

The 1.2.0 submission of 2026-07-01 was archived because the PDF manual
failed to build (LaTeX errors caused by the Unicode characters U+2265 and
U+2264 in Rd files). Changes in this resubmission:

* All Unicode symbols not supported by LaTeX (>=, <=, ->, R^2) were
  rewritten in portable form in the affected Rd files; the PDF manual now
  builds cleanly (verified locally with pdflatex and on win-builder
  R-devel).
* DESCRIPTION now declares R (>= 4.1.0), as required by the use of the
  native pipe |> and function shorthand \(...) syntax (this addresses the
  corresponding NOTE from the incoming checks).

Regarding the "Possibly misspelled words" NOTE: "Estabrook" is an author
surname (Wu & Estabrook, 2016, cited with its DOI in the Description) and
"Likert" is the standard name of the rating-scale format; both are spelled
correctly.

## Test environments

* Local: Windows 11 x64, R 4.4.1 (R CMD check --as-cran)
* win-builder: R-devel (Windows Server 2022 x64)

## R CMD check results

0 errors | 0 warnings | 2 notes

* NOTE: "New submission" — expected.
* NOTE: "Possibly misspelled words in DESCRIPTION: Estabrook, Likert" —
  proper nouns, spelled correctly (see above).

## Downstream dependencies

There are currently no downstream dependencies on CRAN.
