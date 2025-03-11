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
  filter(!is.na(Autor))  

if (!dir.exists("img_recon")) {
  dir.create("img_recon")
} else {
  cat("\033[33mLa carpeta 'img_recon' ya existe.\033[0m\n")
}

# input <- data.frame(name=obras$obrasQ, url=obras$image)
# getFiles(input, path="img_recon", ext="jpg")


obras_sin_Q <- obras %>% 
  filter(is.na(obrasQ) & is.na(image)) %>% 
  group_by(Autor) %>% 
  mutate(obrasQ = paste0(autorQ, "-",sprintf("%02d", row_number()))) %>%  
  ungroup() %>% 
  arrange(nombre_VO)

