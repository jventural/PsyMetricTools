#' Calculate McDonald's Omega Coefficient
#'
#' Calculates McDonald's omega reliability coefficient from factor loadings.
#'
#' @param loadings_df Data frame with items and factor loadings.
#' @param groups NULL for all items, vector of items, or list of item vectors for groups.
#' @param item_col Name of the column containing item names (default: first column).
#' @param method Either "comunalidad" (h2 = sum of loadings squared) or "sum_loadings".
#'
#' @return A list with omega values and item statistics.
#'
#' @export
calcula_omega_mcdonald <- function(loadings_df,
                                   groups = NULL,
                                   item_col = NULL,
                                   method = c("comunalidad", "sum_loadings")) {
  # loadings_df: data.frame con columna de items y cargas factoriales
  # groups: NULL, vector de items, o lista de tales vectores
  # item_col: si NULL, se toma la primera columna como “Items”
  # method: "comunalidad" (h2 = (sum λ)^2) o "sum_loadings" (h2 = sum λ)

  method <- match.arg(method)

  # 1) Determinar columna de items
  if (is.null(item_col)) {
    item_col <- names(loadings_df)[1]
  }
  if (!item_col %in% names(loadings_df)) {
    stop("No existe columna '", item_col, "' en loadings_df.")
  }

  # 2) Preparar lista de grupos
  if (is.null(groups)) {
    groups_list <- list(All = loadings_df[[item_col]])
  } else if (is.character(groups) && !is.list(groups)) {
    groups_list <- list(Group1 = groups)
  } else if (is.list(groups)) {
    groups_list <- groups
    if (is.null(names(groups_list)) || any(names(groups_list) == "")) {
      names(groups_list) <- paste0("Group", seq_along(groups_list))
    }
  } else {
    stop("'groups' debe ser NULL, un vector de caracteres o una lista de ellos.")
  }

  # 3) Funcion interna para un grupo
  calc_one <- function(items_vec) {
    sub_df <- loadings_df[loadings_df[[item_col]] %in% items_vec, , drop = FALSE]
    if (nrow(sub_df) == 0) {
      stop("Ningun item coincide con: ", paste(items_vec, collapse = ", "))
    }

    # Matriz de cargas (todas las columnas excepto item_col)
    mat <- as.matrix(sub_df[ , setdiff(names(sub_df), item_col), drop = FALSE ])
    gen_loading <- rowSums(mat)

    # 4) Comunalia o suma de cargas segun metodo
    if (method == "comunalidad") {
      h2 <- gen_loading^2
    } else { # "sum_loadings"
      h2 <- gen_loading
    }
    theta <- 1 - h2

    # 5) Omega del grupo
    omega_total <- sum(h2) / ( sum(h2) + sum(theta) )

    # 6) Stats por item
    item_stats <- data.frame(
      Item        = sub_df[[item_col]],
      gen_loading = gen_loading,
      h2          = h2,
      theta       = theta,
      stringsAsFactors = FALSE
    )

    list(item_stats = item_stats,
         omega      = omega_total)
  }

  # 7) Aplicar a cada grupo
  raw_results <- lapply(groups_list, calc_one)

  # 8) Construir data.frame con todos los omegas
  omegas_df <- data.frame(
    group = names(raw_results),
    omega = sapply(raw_results, function(x) x$omega),
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  # 9) Extraer la lista de item_stats para cada grupo
  item_stats_list <- lapply(raw_results, function(x) x$item_stats)

  # 10) Retornar lista integrada
  return(list(
    omegas      = omegas_df,
    item_stats  = item_stats_list
  ))
}
