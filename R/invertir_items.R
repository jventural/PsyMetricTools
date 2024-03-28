invertir_items <- function(df, items) {
  df %>%
    mutate(across(all_of(items), ~ {
      val_uniq = unique(.)
      if(0 %in% val_uniq) {
        return(3 - .)
      } else {
        max_val = max(val_uniq)
        return(max_val + 1 - .)
      }
    }))
}
