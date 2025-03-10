library(readxl)
library(tidyverse)
library(writexl)
library(wikiTools)

obras <- read_excel("obrasQ.xlsx")

obras$image <- gsub(" ","_", obras$image)
obras$image <- paste0("https://commons.wikimedia.org/wiki/Special:FilePath/", obras$image)

obras <- obras %>% 
  filter(!obrasQ == "NA") %>% 
  select(nombre_VO, image)

input <- data.frame(name=obras$obrasQ, url=obras$image)
getFiles(input, path="img_recon", ext="jpg")

duplicados <- obras %>% 
  filter(Autor != "NA")

obras_sin_reconciliar <- duplicados %>% 
  filter(obrasQ != "NA" & image=="https://commons.wikimedia.org/wiki/Special:FilePath/NA")

obras_sin_Q <- duplicados %>%
  filter(is.na(obrasQ)) %>%
  select(Autor, autorQ, obrasQ, nombre_VO) %>%
  group_by(Autor) %>%  # Agrupar por autor
  mutate(obrasQ = paste0(autorQ, "-",sprintf("%02d", row_number()))) %>%  # Asignar número secuencial con 2 dígitos
  ungroup() %>% 
  arrange(nombre_VO)

write_xlsx(obras_sep, "obras_separadas.xlsx")



