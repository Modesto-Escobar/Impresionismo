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

obras_mod$Nombre_VO <- ifelse(is.na(obras$Nombre_VO), obras$Nombre_esp, obras$Nombre_VO)
obras_mod$Nombre_esp <- ifelse(is.na(obras$Nombre_esp), obras$Nombre_VO, obras$Nombre_esp)
  

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


duplicados <- obras_mod %>%
  add_count(Nombre_VO) %>% 
  mutate(
    Nombre_VO = ifelse(n > 1, paste0(Nombre_VO, " (", fecha_final, ")"), Nombre_VO)  
  )

duplicados <- duplicados %>% 
  add_count(Nombre_VO, name = "n") %>%  
  group_by(Nombre_VO) %>% 
  mutate(suffix = if_else(n > 1, letters[row_number()], ""),
         Nombre_VO = paste0(Nombre_VO, suffix)) %>% 
  ungroup()

