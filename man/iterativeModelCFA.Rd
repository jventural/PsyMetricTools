\name{iterativeModelCFA}
\alias{iterativeModelCFA}
\title{Iterative Model CFA}

\description{
Performs iterative CFA model adjustment based on modification indices.
}

\usage{
iterativeModelCFA(data, initialModel, MAX_ITERATIONS = 10, MI_THRESHOLD = 5)
}

\arguments{
  \item{data}{Data frame for CFA.}
  \item{initialModel}{Initial CFA model specification.}
  \item{MAX_ITERATIONS}{Maximum iterations (default 10).}
  \item{MI_THRESHOLD}{MI threshold (default 5).}
}

\value{
A list with FinalModel, FitMeasuresDf, StandardizedSolutions, and ModificationsDf.
}

\examples{
\dontrun{
result <- iterativeModelCFA(mydata, "F1 =~ x1 + x2 + x3")
}
}

\author{
Dr. Jose Ventura-Leon
}

