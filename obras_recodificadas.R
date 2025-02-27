library(readxl)
library(tidyverse)

obras <- read_excel("obras.xlsx")
autores <- read_excel("prueba.xlsx")

obras_mod <- obras %>%
  mutate(
    fecha_inicio = if_else(str_detect(Fecha, "-"), str_extract(Fecha, "^[0-9]{4}"), NA_character_),  
    fecha_final = str_extract(Fecha, "[0-9]{4}$") 
  ) %>% 
  separate(Dimensiones, into = c("Alto", "Ancho"), sep = " x ", convert = TRUE)

autores  <- autores %>% 
  rename(Autor = label, Q = entity) %>% 
  select(Autor, Q)

union <- autores %>% 
  left_join(obras_mod, by = "Autor")

tabla <- union %>% 
  group_by(Q) %>% 
  summarise(Titulos = paste(Nombre_VO, collapse = "|")) %>% 
  mutate(Titulos = na_if(Titulos, "NA")) %>%
  drop_na(Titulos) 
  

