\name{crear_modelo_lavaan}
\alias{crear_modelo_lavaan}
\title{Lavaan Model Creation Tool}

\description{
This function facilitates the creation of a structural equation model (SEM) syntax for the `lavaan` package. It constructs the model by specifying latent variables and their observed indicators. This is particularly useful for quickly assembling large SEM models where variables and their corresponding item numbers are systematically named.
}

\usage{
crear_modelo_lavaan(nombre, ...)
}

\arguments{
  \item{nombre}{Prefix of the item or indicator names in the dataset.}
  \item{...}{Additional arguments where each argument name is a latent variable and its value is a vector containing the suffixes of the observed indicators corresponding to this latent variable.}
}

\details{
The function iterates over each latent variable provided in the arguments. For each latent variable, it constructs a part of the lavaan model syntax by concatenating the `nombre` prefix with the specified item suffixes. The resulting string represents a part of the SEM model where each latent variable is defined by its observed indicators.
}

\value{
A string representing the lavaan model syntax. This string can be directly used in functions from the `lavaan` package to specify a structural equation model.
}

\examples{
model_syntax <- crear_modelo_lavaan(
  nombre = "CCOV",
  F1 = c(3, 6, 2),
  F2 = c(1, 4, 5)
)
print(model_syntax)
}

\author{
Dr. José Ventura-León
}
