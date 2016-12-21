# Pacotes -----------------------------------------------------------------

library(folhar)
library(purrr)
library(dplyr)
library(readr)

# Busca -------------------------------------------------------------------

busca <- folha_buscar("sao paulo", "01/10/2016", "01/12/2016", max_r = 2000)

# Obtenção do texto -------------------------------------------------------

safe_noticia <- safely(folha_noticias, otherwise = NULL)
noticias <- map(busca$link, safe_noticia)

noticias_data <- map_df(noticias, ~.x$result)

# Salvar dados ------------------------------------------------------------

saveRDS(noticias_data, file = "data-raw/noticias.rds")
write_lines(noticias_data$texto, path = "data-raw/noticias_texto.txt")
