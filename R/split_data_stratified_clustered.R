#' @title Split Data with Stratified Clustering
#' @description Splits a data frame using stratified and clustered sampling.
#' @param df Data frame to split.
#' @param strat_var Stratification variable name (default "Region_reside").
#' @param cluster_var Cluster variable name (default "Facultad").
#' @param perc_exploratorio Proportion for exploratory subset (default 0.5).
#' @param seed Random seed for reproducibility (default NULL).
#' @return A list with exploratorio and confirmatorio data frames.
#' @examples
#' \dontrun{
#' # Create sample data with stratification and clustering variables
#' set.seed(123)
#' data <- data.frame(
#'   ID = 1:500,
#'   Item1 = sample(1:5, 500, replace = TRUE),
#'   Item2 = sample(1:5, 500, replace = TRUE),
#'   Item3 = sample(1:5, 500, replace = TRUE),
#'   Region_reside = sample(c("Norte", "Sur", "Centro"), 500, replace = TRUE),
#'   Facultad = sample(c("Ingenieria", "Medicina", "Derecho"), 500, replace = TRUE)
#' )
#'
#' # Split with stratification by Region and clustering by Facultad
#' splits <- split_data_stratified_clustered(
#'   df = data,
#'   strat_var = "Region_reside",
#'   cluster_var = "Facultad",
#'   perc_exploratorio = 0.5,
#'   seed = 123
#' )
#'
#' # Access subsets
#' efa_data <- splits$exploratorio
#' cfa_data <- splits$confirmatorio
#'
#' # Verify stratification is maintained
#' table(efa_data$Region_reside, efa_data$Facultad)
#' table(cfa_data$Region_reside, cfa_data$Facultad)
#'
#' # Different proportions
#' splits2 <- split_data_stratified_clustered(
#'   df = data,
#'   strat_var = "Region_reside",
#'   cluster_var = "Facultad",
#'   perc_exploratorio = 0.6,
#'   seed = 456
#' )
#' }
#' @export
split_data_stratified_clustered <- function(df,
                                            strat_var = "Region_reside",
                                            cluster_var = "Facultad",
                                            perc_exploratorio = 0.5, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Añadiendo una columna de grupos para facilitar el muestreo estratificado
  df <- df |>
    dplyr::mutate(group = interaction(.data[[strat_var]], .data[[cluster_var]]))

  # Inicializar listas para almacenar los resultados
  exploratorio <- list()
  confirmatorio <- list()

  # Dividir cada grupo por separado
  unique_groups <- unique(df$group)
  for (group in unique_groups) {
    group_df <- df |> dplyr::filter(group == !!group)
    n <- nrow(group_df)
    n_exploratorio <- floor(perc_exploratorio * n)
    ids_exploratorio <- sample(seq_len(n), size = n_exploratorio, replace = FALSE)

    exploratorio[[as.character(group)]] <- group_df[ids_exploratorio, ]
    confirmatorio[[as.character(group)]] <- group_df[-ids_exploratorio, ]
  }

  # Combinar las listas de dataframes en dos dataframes
  exploratorio_df <- dplyr::bind_rows(exploratorio)
  confirmatorio_df <- dplyr::bind_rows(confirmatorio)

  return(list(exploratorio = exploratorio_df, confirmatorio = confirmatorio_df))
}
