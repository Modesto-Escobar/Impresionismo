library(readxl)
library(tidyverse)
library(netCoin)

load("Autores.RData")
source("imagenes_obras.R")
autores <- read_excel("prueba.xlsx")
campos <- c("Nombre", "Descripción", "Sexo", "Nace", "País nace", "Muere", "País fallece", "Ocupación", "image", "ventana")

autores  <- autores %>% 
  rename(Autor = label, Q = entity) %>% 
  select(Autor, Q)

union <- autores %>% 
  left_join(obras, by = "Autor")

tabla <- union %>% 
  group_by(Autor) %>% 
  summarise(Titulos = paste(Titulo, collapse = "|")) %>% 
  mutate(Titulos = na_if(Titulos, "NA")) %>%
  drop_na(Titulos) 

autor <- base |> 
  select(campos)

names(autor)[1] <- names(obras)[1] <- "Elemento"
netExhibit(tabla, name="Elemento", tableformat=TRUE, initialType = "Autor", nodes=list(Autor=autor, Titulos=obras), 
           image="image", ntext="ventana", tableButton=TRUE, language="es",

