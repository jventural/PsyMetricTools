\name{diag_aba_na}
\alias{diag_aba_na}
\title{Matrix Lower Triangle Formatting Tool}

\description{
This function reformats a given matrix to display only the lower triangle, including the diagonal. It modifies the row and column headers for clarity and replaces the upper triangle values with 'NA'. The diagonal values are set to a placeholder ('-'). This is particularly useful for visualizing symmetric matrices like correlation matrices, where the upper triangle is redundant.
}

\usage{
diag_aba_na(matriz)
}

\arguments{
  \item{matriz}{A numeric or data matrix to be reformatted. Typically used with symmetric matrices such as correlation or covariance matrices.}
}

\details{
The function first calls an internal function `cambiar_encabezados` to rename the row and column headers numerically for better readability. Then, it replaces diagonal values with '-', and sets all values in the upper triangle of the matrix to 'NA', leaving only the lower triangle and diagonal visible.
}

\value{
A reformatted matrix with modified headers, '-' on the diagonal, and 'NA' in the upper triangle. The lower triangle remains unchanged, displaying the original values.
}

\examples{
# Example matrix
matriz <- matrix(rnorm(16), 4, 4)
colnames(matriz) <- c("Var1", "Var2", "Var3", "Var4")
rownames(matriz) <- c("Var1", "Var2", "Var3", "Var4")

# Apply function
formatted_matrix <- diag_aba_na(matriz)
print(formatted_matrix)
}

\author{
Professor José Ventura León
}
