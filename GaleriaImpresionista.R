# Mon Feb 17 12:30:34 2025 ------------------------------
# Galería de impresionistas
# Prácticas de grado
# José Antonio Robles

library(netCoin)
library(wikiTools)
library(tidyverse)

sites <- data.frame(
  url=c("wikipedia.org","wikidata.org","brumario.usal.es","museoreinasofia.es","viaf.org", "bne.es", "isni.org"),
  name=c("Wikipedia","Wikidata","USAL","MNCARS","VIAF", "BNE", "ISNI"),
  icon=c("https://www.wikipedia.org/static/favicon/wikipedia.ico","https://www.wikidata.org/static/favicon/wikidata.ico",
         "https://bibliotecas.usal.es/?q=system/files/imagecache/nodoniv2_thumb/nodoniv2/app.png",
         "https://static5.museoreinasofia.es/sites/all/themes/mrs_twitter_bootstrap/images/misc/favicon-32x32.png",
         "https://viaf.org/viaf/images/viaf.ico", 
         "https://datos.bne.es/img/logoBNEpositivo.jpg",
         "https://isni.org/images/isni-logo.png")
)

# names <- readLines("nombres")
# 
# baseQ <- getWikiInf(names)

# base <- w_EntityInfo(baseQ$Q, langsorder="es", wikilangs="es")
# 
# input <- data.frame(name=base$entity, url=gsub("\\|.*","", base$pic))
# getFiles(input, path="imgs", ext="jpg")
# 
# base$image <- paste0("imgs/",base$entity,".jpg")
# base$ventana <- get_template2(base, title="label", text="description")

library(readxl)
base <- read_excel("prueba.xlsx")
baseO <- read_excel("obras.xlsx")

baseO$Nombre_esp <- ifelse(is.na(baseO$Nombre_esp), baseO$Nombre_VO, baseO$Nombre_esp)

# library(rvest)
# library(polite)
# 
# base <- base %>%
#   mutate(wiki = sapply(wikipedias, function(url) {
#     tryCatch({
#       session <- bow(url)
#       page <- scrape(session)
#       text <- page %>% html_nodes("p") %>% html_text() %>% .[1]
#       return(text)
#     }, error = function(e) return("No disponible"))
#   }))

# base <- gsub("\\|?", "", base$occupation)

base <- base %>% 
  mutate(
    description=ifelse(is.na(descripción), description, descripción),
    description=paste0(toupper(substr(description, 1, 1)), substr(description, 2, nchar(description))),
    image=ifelse(pic=="NA" | pic=="", "imgs/pintor.jpg", paste0("imgs/", entity, ".jpg"))) %>%
  select(label, descripción, description, occupation, wikipedias, wiki, image) %>% 
  rename(nombre = label, ocupación = occupation) %>% 
  as.data.frame() 



base$description <- gsub("\\s*\\(.*?\\)", "", base$description)

base$wikipedias <- sub("es\\.wiki", "es.m.wiki", base$wikipedias)
base$Wiki <- ifelse(base$wikipedias=="NA", NA, base$wikipedias)
base$text <- ifelse(is.na(base$Wiki), "", paste0(base$wiki, renderLinks(base, "Wiki", target="mainframe", sites=sites)))


#Recodificación de ocupaciones
M <- base
p <- as.data.frame(read_excel("OcupacionesI.xlsx", sheet="Ocupaciones"))
p <- p[!is.na(p$categoría),] # Evita NA
cates <- names(table(p$categoría))
changes <-list()
M$Ocupación <- tolower(M$ocupación)
M$disc <- ""
for(i in cates[c(3, 2, 4, 6, 5, 1, 8, 7)]){
  changes[[i]]  <- p[p$categoría==i, "ocupación"]
  for(j in 1:nrow(M)) {
    x <- unlist(strsplit(M$Ocupación[j],"\\|"))
    if(length(intersect(x, changes[[i]]))>0) M$disc[j] <- paste0(M$disc[j], "|", i)
  }
}

M$disciplina <- sub("^\\|","", M$disc)
if(exists("base")) base$Disciplina <- M$disciplina


base$ventana <- get_template2(base, title="nombre", title2 = "description", text="text")

autores <- exhibit(base, name="nombre", ntext="ventana", image="image", main = "Autores del impresionismo",
                   language = "es") %>% plot(dir="~/temp")

baseO$vetana_obras <- get_template2(baseO, title = "Nombre_esp", title2 = "Nombre_VO")

# obras <- exhibit(baseO, name="Nombre_esp") %>% plot(dir="~/temp2")
# 
# multigraphCreate("Autores" = autores, "Obras" = obras) %>% plot(dir="~/temp2")






