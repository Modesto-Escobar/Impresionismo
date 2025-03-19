library(readxl)
library(tidyverse)
library(netCoin)

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
  select(campos) |> 
  rename(pop_up=ventana)

names(autor)[1] <- names(obras)[1] <- "Elemento"

# Ejecución de la página
netExhibit(tabla, name="Elemento", tableformat=TRUE, initialType = "Autor", nodes=list(Autor=autor, Titulos=obras), 
           image="image", ntext="pop_up", tableButton=TRUE, language="es",
           main="Impresionismo", colorScheme=1) |> plot("~/temp")
  # plot("~/Galerias/Impresionismo")


