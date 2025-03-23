library(wikiTools)

load("Autores.RData")
Wikis <- w_Wikipedias(base$Q, wikilangs="es|ca|eu|gl|ast|en|fr|pt|it|de")
ww <- unique(unlist(strsplit(Wikis$langs, "\\|")))


faltantes <- function(modelo, dato) {
  ref <- unlist(strsplit(modelo, "\\|"))
  val <- unlist(strsplit(dato, "\\|"))
  faltan <- setdiff(ref, val)
  paste(faltan, collapse = "|")
}

wikilangs <- "es|ca|eu|gl|ast|en|fr|pt|it|de"

autor$nolangs <- sapply(autor$langs, function(x) faltantes(wikilangs, x))