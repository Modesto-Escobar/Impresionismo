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
source("nolangs.R")
Wikis <- w_Wikipedias(base$Q, wikilangs="es|ca|eu|gl|ast|en|fr|pt|it|de")
autor <- base |>
  select(-wiki) |> 
  left_join(Wikis, join_by(Q == entity)) |>
  mutate(`Sin Wikipedia`=nolangs(langs, "es|ca|eu|gl|ast|en|fr|pt|it|de")) |> 
  select(all_of(campos), langs, `Sin Wikipedia`) |>
  rename(pop_up=ventana, Wikipedias=langs)

names(tabla) <- c("Pintores", "Pinturas")
} else {
  load("impresionismo.RData")
}

# Ejecución de la página
netExhibit(tabla, tableformat=TRUE, initialType = "Pintores", nodes=list(Pintores=autor, Pinturas=obras), 
           image="image", ntext="pop_up", tableButton=TRUE, language="es", export=TRUE,
           main="Impresionismo", colorScheme=0,
           note =paste0("<p style=font-size:20px; bottom:0; position:absolute><center>", imageLink("MicinEurPRb"), 
                        "Fuente: Walter, Ingo F. (2022) Impresionismo. 1860-1920. Köln: TASCHEN ", imageLink("LogosXFs"),"</center></p>")) |> 
  plot("~/Galerias/Impresionismo")
