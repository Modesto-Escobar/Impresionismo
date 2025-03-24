library(readxl)
library(tidyverse)
library(netCoin)

if(FALSE){

# Carga de logos del pie
imageLink <- function(image, px=30, py=px) {
  link <- paste0('<img style="height:', py, 'px;width:', px,';vertical-align:bottom;margin-right:5px;" src="https://sociocav.usal.es/me/pics/', image, '.png"/>')
  return(link)
}

# Tratamiento de autores
load("Autores.RData")

autores <- read_excel("prueba.xlsx")
campos <- c("Nombre", "Descripción", "Sexo", "Nace", "País nace", "Muere", "País fallece", "Ocupación", "image", "ventana")

autores  <- autores %>% 
  rename(Autor = label, Q = entity) %>% 
  select(Autor, Q)

# Conjunción con obras. elaboración de tablas
source("imagenes_obras.R") # Prepara la base de obras
union <- autores %>% 
  left_join(obras, by = "Autor")

tabla <- union %>% 
  group_by(Autor) %>% 
  summarise(Titulos = paste(Titulo, collapse = "|")) %>% 
  mutate(Titulos = na_if(Titulos, "NA")) %>%
  drop_na(Titulos) 

# Preparativos finales
autor <- base |>
  left_join(Wikis, join_by(Q == entity)) |> 
  select(campos, langs) |> 
  rename(pop_up=ventana)

names(tabla) <- c("Pintores", "Pinturas")
} else {
  load("impresionismo.RData")
}

# Ejecución de la página
netExhibit(tabla, tableformat=TRUE, initialType = "Pintores", nodes=list(Pintores=autor, Pinturas=obras), 
           image="image", ntext="pop_up", tableButton=TRUE, language="es", export=TRUE,
           main="Impresionismo", colorScheme=1) |> 
  plot("~/Galerias/Impresionismo")

