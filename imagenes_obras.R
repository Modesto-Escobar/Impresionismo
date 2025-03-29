library(readxl)
library(tidyverse)
library(writexl)
library(wikiTools)

source("pop_up.R")
source("nolangs.R")
obrasT <- read_excel("obrasQ.xlsx")

obrasQ <- obrasT %>%
  mutate(
    image = ifelse(!is.na(image),
                   paste0("https://commons.wikimedia.org/wiki/Special:FilePath/", gsub(" ", "_", image)),
                   image),
    nombre_reconciliado = coalesce(nombre_reconciliado, Nombre_esp),
    Nombre_esp = coalesce(Nombre_esp, nombre_reconciliado),
    nombre_VO = coalesce(nombre_VO, Nombre_esp)) %>%
    drop_na(Autor, Fecha) |> 
    filter(!is.na(obrasQ))

obrasQ <- w_Wikipedias(obrasQ$obrasQ, wikilangs="es|ca|eu|gl|ast|en|fr|pt|it|de") |>
  mutate(pages=sub("\\|.*","", pages)) |> 
  right_join(obrasQ, join_by(entity==obrasQ)) |> 
  mutate(sin_Wiki=nolangs(langs, "es|ca|eu|gl|ast|en|fr|pt|it|de"))

if (!dir.exists("img_recon")) {
  dir.create("img_recon")
} else {
  # cat("\033[33mLa carpeta 'img_recon' ya existe.\033[0m\n")
}

# input <- data.frame(name=obras$obrasQ, url=obras$image)
# getFiles(input, path="img_recon", ext="jpg")
# git rm --cached .Rhistory
# git commit -m "Ignore .Rhistory"


obras_sin_Q <- obrasT %>% 
  filter(is.na(obrasQ) & is.na(image)) %>% 
  group_by(Autor) %>% 
  mutate(obrasQ = paste0(autorQ, "-",sprintf("%02d", row_number())),
         image = paste0("img_recon/", obrasQ, ".jpg")) %>%
  ungroup() %>% 
  arrange(nombre_VO) |> 
  rename(entity=obrasQ)

obras <- obrasQ |>
  mutate(image=if_else(is.na(image), "img_recon/impresionista.jpg",
                       paste0("img_recon/", entity, ".jpg"))) |>
  bind_rows(obras_sin_Q) |>
  mutate(fecha_inicio = str_extract(Fecha, "^[0-9]{4}"),
    Fecha= str_extract(sub("\\.0","",Fecha), "[0-9]{4}$"),
    Titulo = paste0(nombre_VO, " (", Fecha,")"),
    obrasR = sub(".*-\\d+", "", entity),
    Formato =  paste0(toupper(substr(Formato, 1, 1)), substr(Formato, 2, nchar(Formato))),
    Museo   =  paste0(toupper(substr(Museo, 1, 1)), substr(Museo, 2, nchar(Museo)))) |> 
  select(Titulo, Autor, Fecha, Formato, Museo, Lugar, image, obrasR, langs, sin_Wiki) |>
  rename(Wikipedias=langs, `Sin Wikipedia`=sin_Wiki) |> 
  distinct(Titulo, .keep_all = TRUE) |> 
  pop_up(title="Titulo", entity="obrasR", wikilangs = "es|en|fr|pt|it|de") |>
  arrange(Fecha) |>
  select(-obrasR)

