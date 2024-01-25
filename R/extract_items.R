extract_items <- function(text, prefix = "") {
  extracted <- list()
  for (line in text) {
    words <- strsplit(line, ": ")[[1]][-1]
    tag <- strsplit(line, ": ")[[1]][1]
    values <- unlist(strsplit(words, ", "))

    # Agregar prefijo solo a las letras mayúsculas seguidas de una o más letras minúsculas
    values <- gsub("(?<=[A-Z])(?=[a-z])", paste0(prefix, ""), values, perl = TRUE)

    extracted[[tag]] <- values
  }
  return(extracted)
}
