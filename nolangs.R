nolangs <- function(vector, langs=("en")) {
  faltantes <- function(vector, langs) {
    ref <- unlist(strsplit(langs, "\\|"))
    val <- unlist(strsplit(vector, "\\|"))
    faltan <- setdiff(ref, val)
    if(length(faltan)>0) paste0("!", faltan, collapse = "|") else ""
  }
  V <- sapply(vector, function(x) faltantes(x, langs))
  names(V) <- NULL
  return(V)
}

