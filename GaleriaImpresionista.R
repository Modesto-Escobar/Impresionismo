# Mon Feb 17 12:30:34 2025 ------------------------------
# Galería de impresionistas
# Prácticas de grado
# José Antonio Robles

library(netCoin)
library(wikiTools)
names <- readLines("nombres")

baseQ <- getWikiInf(names)

base <- w_EntityInfo(baseQ$Q, langsorder="es", wikilangs="es")

# input <- data.frame(name=base$entity, url=base$pic)
# getFiles(input, path="imgs", ext="jpg")

base$image <- paste0("imgs/",base$entity,".jpg")
base$ventana <- get_template2(base, title="label", text="description")

exhibit(base, name="label", ntext="ventana", image="image") |> plot(dir="~/temp")
