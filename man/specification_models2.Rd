\name{specification_models2}
\alias{specification_models2}
\title{Ajusta Modelos Especificados en Datos Usando 'lavaan'}

\description{
Esta función ajusta una serie de modelos especificados a un conjunto de datos utilizando la librería 'lavaan'. Está diseñada para facilitar el análisis de datos estructurales mediante la especificación de modelos, incluyendo la estimación por el método especificado y la rotación oblicua 'oblimin'. Permite una fácil instalación y carga de 'lavaan' y suprime mensajes y advertencias para mantener la salida limpia y enfocada en los resultados del ajuste del modelo.
}

\usage{
specification_models2(modelos, data, estimator)
}

\arguments{
  \item{modelos}{Una lista de cadenas de texto que especifican los modelos a ser ajustados.}
  \item{data}{El conjunto de datos sobre el cual ajustar los modelos.}
  \item{estimator}{El método de estimación a utilizar para el ajuste de modelos.}
}

\details{
La función primero instala y carga la librería 'lavaan' si aún no está presente en el entorno de R. Luego, itera sobre la lista de modelos proporcionados, ajustando cada uno al conjunto de datos especificado utilizando el estimador y opciones de modelo dados, incluyendo rotación 'oblimin' y tratamiento de variables ordenadas. Los mensajes y advertencias generados durante el ajuste de modelos son suprimidos para evitar la saturación de la salida con información no esencial.
}

\value{
Una lista de objetos de ajuste de modelo generados por 'lavaan' para cada modelo especificado. Cada elemento de la lista corresponde a un modelo ajustado y puede ser utilizado para análisis posteriores o para extraer medidas de bondad de ajuste.
}

\examples{
# Asumiendo que 'data' es un conjunto de datos y 'model_list' es una lista de modelos especificados como cadenas de texto
adjusted_models <- specification_models2(model_list, data, "MLM")

# Para acceder a los resultados de un modelo específico
model_results <- adjusted_models[[1]]
}

\author{
Profesor José Ventura León
}

