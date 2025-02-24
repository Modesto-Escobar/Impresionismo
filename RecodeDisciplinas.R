#### Modesto Escobar
# Sun Feb 23 20:34:03 2025 ------------------------------
# Recodifica
library("readxl")
M <- base
p <- as.data.frame(read_excel("OcupacionesI.xlsx", sheet="Ocupaciones"))
p <- p[!is.na(p$categoría),] # Evita NA
cates <- names(table(p$categoría))
changes <-list()
M$Ocupación <- tolower(M$occupation)i
for(i in cates[c(3, 2, 4, 6, 5, 1, 8, 7)]){
  changes[[i]]  <- p[p$categoría==i, "ocupación"]
  for(j in 1:nrow(M)) {
  x <- unlist(strsplit(M$Ocupación[j],"\\|"))
  if(length(intersect(x, changes[[i]]))>0) M$disc[j] <- paste0(M$disc[j], "|", i)
  }
}
M$disciplina <- sub("^\\|","", M$disc)
if(exists("base")) base$Disciplina <- M$disciplina
