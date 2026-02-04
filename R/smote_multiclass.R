#' SMOTE Oversampling for Multiclass Data
#'
#' Applies SMOTE (Synthetic Minority Over-sampling Technique) for multiclass imbalanced data.
#'
#' @param data Data frame with features and outcome variable.
#' @param outcome Name or index of the outcome variable.
#' @param perc_maj Percentage of majority class size to target (default: 100).
#' @param k Number of nearest neighbors for SMOTE (default: 5).
#' @param seed Random seed for reproducibility.
#'
#' @return A data frame with oversampled minority classes.
#'
#' @export
smote_multiclass <- function(data, outcome, perc_maj = 100, k = 5, seed = NULL) {

  # Función SMOTE para una clase específica
  smote_for_class <- function(x_min, syn_size, k, y_coln, class_name, seed = NULL) {
    if (!is.null(seed)) {
      set.seed(seed)
    }
    n <- nrow(x_min)
    m <- ncol(x_min)
    synthetic_data <- matrix(runif(syn_size * m), ncol = m)
    colnames(synthetic_data) <- colnames(x_min)

    # Redondear y convertir a entero los valores generados
    synthetic_data <- apply(synthetic_data, 2, function(col) {
      as.integer(round(col))
    })

    synthetic_data <- as.data.frame(synthetic_data)
    synthetic_data[, y_coln] <- class_name # Asignar la clase minoritaria
    return(synthetic_data)
  }

  datnrow <- nrow(data)
  if (nrow(na.omit(data)) < datnrow) {
    stop("Sorry, this dataset has missing values.")
  }

  # Identificar la columna de resultado
  if (is.character(outcome)) {
    if (!(outcome %in% colnames(data))) {
      stop(paste("This dataset doesn't have a variable named", outcome))
    }
    y_coln <- outcome
  } else {
    if (outcome < 1 | outcome > ncol(data)) {
      stop(paste("This dataset doesn't have a variable whose column number is", outcome))
    }
    y_coln <- colnames(data)[outcome]
  }

  y <- data[, y_coln]
  class_counts <- table(y)

  if (length(class_counts) < 2) {
    stop("The outcome variable has less than two classes.")
  }

  if (all(class_counts == max(class_counts))) {
    stop("The dataset is already balanced.")
  }

  major_class_count <- max(class_counts)

  for (min_class in names(class_counts[class_counts != major_class_count])) {
    min_ind <- which(y == min_class)
    x_min <- data[min_ind, ]

    # Calcula el tamaño sintético necesario basado en perc_maj
    syn_size <- round(((major_class_count - class_counts[min_class]) * perc_maj) / 100)

    # Aplica SMOTE a esta clase
    smote_data <- smote_for_class(x_min, syn_size, k, y_coln, min_class, seed)

    # Combina los datos SMOTE con el conjunto de datos original
    data <- rbind(data, smote_data)
  }

  return(data)
}

