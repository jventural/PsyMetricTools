split_data_stratified_clustered <- function(df,
                                            strat_var = "Region_reside",
                                            cluster_var = "Facultad",
                                            perc_exploratorio = 0.5, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Añadiendo una columna de grupos para facilitar el muestreo estratificado
  df <- df %>%
    mutate(group = interaction(.data[[strat_var]], .data[[cluster_var]]))

  # Inicializar listas para almacenar los resultados
  exploratorio <- list()
  confirmatorio <- list()

  # Dividir cada grupo por separado
  unique_groups <- unique(df$group)
  for (group in unique_groups) {
    group_df <- df %>% filter(group == !!group)
    n <- nrow(group_df)
    n_exploratorio <- floor(perc_exploratorio * n)
    ids_exploratorio <- sample(seq_len(n), size = n_exploratorio, replace = FALSE)

    exploratorio[[as.character(group)]] <- group_df[ids_exploratorio, ]
    confirmatorio[[as.character(group)]] <- group_df[-ids_exploratorio, ]
  }

  # Combinar las listas de dataframes en dos dataframes
  exploratorio_df <- bind_rows(exploratorio)
  confirmatorio_df <- bind_rows(confirmatorio)

  return(list(exploratorio = exploratorio_df, confirmatorio = confirmatorio_df))
}
