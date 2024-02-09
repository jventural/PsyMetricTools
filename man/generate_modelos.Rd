\name{filtrar_aberrantes}
\alias{filtrar_aberrantes}
\title{Outlier Detection and Data Filtering Tool}

\description{
This function identifies and removes outliers from a dataset based on the Mahalanobis distance, specifically tailored for use in psychometric and behavioral data analysis. It utilizes the `mahad` function from the `careless` package to detect outliers and filters them out from the dataset, ensuring a cleaner data set for subsequent analysis. The function is designed to work with datasets where specific items (variables) need to be analyzed for outlier detection.
}

\usage{
filtrar_aberrantes(data, items)
}

\arguments{
  \item{data}{A dataframe containing the dataset from which outliers will be identified and removed.}
  \item{items}{A character vector specifying the column names in `data` that should be used for outlier detection based on the Mahalanobis distance.}
}

\details{
The function first checks if the dataset contains a column named `ID`; if not, it automatically generates this column based on rownames. It then proceeds to calculate the Mahalanobis distance for the specified items, flags outliers based on a confidence threshold of 0.999, and filters these from the dataset. The function returns a list containing the filtered dataset and a dataframe of flagged outliers, including their Mahalanobis distance (`d_sq`), flagged status, and values for the specified items.
}

\value{
A list with two elements:
\itemize{
  \item \code{data_filtrada}: The filtered dataset with outliers removed.
  \item \code{tabla_aberrantes}: A dataframe showing the outliers, including their ID, the specified items' values, Mahalanobis distance (`d_sq`), and flagged status.
}
}

\examples{
# Assuming 'df_final' is your dataset and 'RP1:RP16' are the items of interest
resultado <- filtrar_aberrantes(df_final, paste0("RP", 1:16))
df_filtered <- resultado$data_filtrada
tabla_aberrantes <- resultado$tabla_aberrantes

# To view the filtered dataset
print(df_filtered)

# To view the dataframe of flagged outliers
print(tabla_aberrantes)
}

\author{
Professor José Ventura León
}
