library(readxl)
library(tidyverse)
library(writexl)
library(wikiTools)

obras <- read_excel("obrasQ.xlsx")

obras <- obras %>%
  mutate(
    image = ifelse(!is.na(image),
                   paste0("https://commons.wikimedia.org/wiki/Special:FilePath/", gsub(" ", "_", image)),
                   image),
    nombre_reconciliado = coalesce(nombre_reconciliado, Nombre_esp),
    Nombre_esp = coalesce(Nombre_esp, nombre_reconciliado)) %>%
    drop_na(Autor, Fecha)  

if (!dir.exists("img_recon")) {
  dir.create("img_recon")
} else {
  cat("\033[33mLa carpeta 'img_recon' ya existe.\033[0m\n")
}

# input <- data.frame(name=obras$obrasQ, url=obras$image)
# getFiles(input, path="img_recon", ext="jpg")
# git rm --cached .Rhistory
# git commit -m "Ignore .Rhistory"


obras_sin_Q <- obras %>% 
  filter(is.na(obrasQ) & is.na(image)) %>% 
  group_by(Autor) %>% 
  mutate(obrasQ = paste0(autorQ, "-",sprintf("%02d", row_number())),
         image = "img_recon/impresionista.jpg") %>%
  ungroup() %>% 
  arrange(nombre_VO)

obras <- obras |>
  mutate(image=if_else(is.na(image), "img_recon/impresionista.jpg",
                       paste0("img_recon/", obrasQ, ".jpg"))) |>
  bind_rows(obras_sin_Q) |>
  mutate(fecha_inicio = str_extract(Fecha, "^[0-9]{4}"),
    Fecha= str_extract(sub("\\.0","",Fecha), "[0-9]{4}$"),
    Titulo = paste0(nombre_reconciliado, " (", Fecha,")")) |> 
  select(Titulo, Autor, Fecha, Formato, Museo, Lugar, image) |>
  distinct(Titulo, .keep_all = TRUE) |> 
  arrange(Fecha)
