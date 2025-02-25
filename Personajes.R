# Mon Feb 17 12:30:34 2025 ------------------------------
# Galería de impresionistas
# Prácticas de grado
# José Antonio Robles

library(netCoin)
library(wikiTools)
library(tidyverse)

if(FALSE) {
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

names <- readLines("nombres")

baseQ <- getWikiInf(names)

base <- w_EntityInfo(baseQ$Q, langsorder="es", wikilangs="es")
base$info <- ifelse(is.na(base$wikipedias),"",extractWiki(base$label,language="es"))

# input <- data.frame(name=base$entity, url=gsub("\\|.*","", base$pic))
# getFiles(input, path="imgs", ext="jpg")

base$description <- paste0(toupper(substr(base$description, 1, 1)), substr(base$description, 2, nchar(base$description)))
base$wikidata <- paste0("https://m.wikidata.org/wiki/",base$entity)
base$wiki <- sub("\\.wikipedia",".m.wikipedia", base$wikipedias)
base$image <- ifelse(is.na(base$pic), "imgs/pintor.jpg", paste0("imgs/", base$entity, ".jpg"))
base$links <- paste0(base$info, 
                     '</p><h3>ENLACES:</h3>', renderLinks(base, c("wikidata", "wiki"), NULL, "mainframe", sites=sites))
base$ventana <- get_template2(base, title="label", title2="description", text="links")
} else{
  load("Autores.RData")
}

f_A <- c("entity", "label", "description", "sex", "byear", "bcountry", "dyear", "dcountry", "occupation", "bplaceLat", "bplaceLon", "pic",  "wikipedias")
f_E <- c("Q", "Nombre", "Descripción", "Sexo", "Nace", "País nace", "Muere", "País fallece", "Ocupación", "lat", "lon", "imagen", "wiki")

campos <- c(f_E[1:9], "image", "ventana")

indexes <- match(f_A, names(base))
names(base)[indexes] <- f_E
source("RecodeDisciplinas.R")

autores <- exhibit(base[,campos], name="Nombre", ntext="ventana", image="image", main = "Autores del impresionismo",
                   language = "es") %>% plot(dir="~/temp")

