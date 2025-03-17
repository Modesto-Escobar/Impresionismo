## Operaciones previas
## Modesto Escobar
# Sat Mar 15 08:05:10 2025 ------------------------------

# Carga de paquetes
library(netCoin)
library(wikiTools)
library(tidyverse)

# Función ventana

pop_up <- function(data, title="name", title2=NULL, info=TRUE, entity="entity", links=c("wikidata", "wiki"), 
                   wikilangs="en") {
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
  
  langs <- unlist(strsplit(wikilangs, "\\|"))
  for(e in links) {
    if(e=="wikidata" & !is.element(e, names(data))) {
      data$wikidata <- ifelse(substr(data[[entity]], 1, 1)!="Q", NA, paste0("https://m.wikidata.org/wiki/", data[[entity]]))
    }
    if(e=="wiki" & !is.element(e, names(data))){
      wikis <- w_Wikipedias(data[[entity]], wikilangs=wikilangs)[,c(1,6)]
      wikis$wiki <- sub("\\.wikipedia",".m.wikipedia", sub("\\|.*","", wikis$pages))
      if (info) {
        names <- ifelse(is.na(wikis$wiki) | wikis$wiki=="", " ", sub(".*/","", wikis$wiki))
        wikis$info <- sub("character\\(0\\)", "", as.character(extractWiki(names,language=langs)))
      }
      data <- merge(data, wikis, by.x=entity, by.y="entity", all.x=TRUE)
      data$wiki <- ifelse(is.na(data$pages) | data$pages=="", NA, data$pages)
      data$info <- ifelse(is.na(data$info), "", data$info)
      data$pages <- wikis <- names <- NULL
    }
  }

  links <- renderLinks(data, c("wikidata", "wiki"), NULL, "mainframe", sites=sites)
  data$links <- ifelse(is.na(data$wiki) & is.na(data$wikidata), data$info,
                       paste0(data$info, '</p><h3>ENLACES:</h3>', links))
  data$pop_up <- get_template2(data, title=title, title2=title2, text="links")
  data[, c("links", "info", "names", "wiki", "wikidata")] <- NULL
  return(data)
}
