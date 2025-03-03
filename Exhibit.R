library(readxl)
library(tidyverse)
library(netCoin)

load("Autores.RData")
obras <- read_excel("obras.xlsx")
autores <- read_excel("prueba.xlsx")
campos <- c("Nombre", "Descripción", "Sexo", "Nace", "País nace", "Muere", "País fallece", "Ocupación", "image", "ventana")

obras_mod <- obras %>%
  mutate(
    fecha_inicio = if_else(str_detect(Fecha, "-"), str_extract(Fecha, "^[0-9]{4}"), NA_character_),  
    fecha_final = str_extract(Fecha, "[0-9]{4}$"),
    Titulo = paste0(Nombre_VO, " (", fecha_final,")")
  )  
  # %>%  separate(Dimensiones, into = c("Alto", "Ancho"), sep = " x ", convert = TRUE) 

autores  <- autores %>% 
  rename(Autor = label, Q = entity) %>% 
  select(Autor, Q)

union <- autores %>% 
  left_join(obras_mod, by = "Autor")

tabla <- union %>% 
  group_by(Autor) %>% 
  summarise(Titulos = paste(Titulo, collapse = "|")) %>% 
  mutate(Titulos = na_if(Titulos, "NA")) %>%
  drop_na(Titulos) 

autor <- base |> 
  select(campos)

obra <- obras_mod |> 
  arrange(fecha_final) |> 
  select(Titulo, Autor, Fecha, Formato, Museo, Lugar) |> 
  distinct(Titulo, .keep_all = TRUE) 


netExhibit(tabla, tableformat=TRUE, initialType = "Autor", nodes=list(Autor=autor, Titulos=obra), 
           image="image", ntext="ventana", tableButton=TRUE,
           main="Impresionismo", colorScheme=1) |> plot("~/tmp")

