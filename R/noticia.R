#' Faz parse das notícias com url começando em:
#' http://www1.folha.uol.com.br/
#'
#' @param url url da noticia
#'
#' @examples
#' url <- "http://www1.folha.uol.com.br/mundo/2016/11/1830974-americanos-insatisfeitos-protestam-contra-vitoria-de-trump-em-eleicoes.shtml"
#' noticia <- parse_folha(url)
#'
#' @export
#'
parse_folha <- function(url){

  noticia <- httr::GET(url) %>%
    httr::content(encoding = "windows-1252")

  titulo <- noticia %>%
    rvest::html_nodes(".news h1") %>%
    rvest::html_text()

  autor <- noticia %>%
    rvest::html_node(".author p b") %>%
    rvest::html_text()

  datahora <- noticia %>%
    rvest::html_nodes(".news time") %>%
    rvest::html_text() %>%
    lubridate::dmy_hm()

  texto <- noticia %>%
    rvest::html_nodes(".news .content p") %>%
    rvest::html_text() %>%
    paste(collapse = " ")

  dplyr::data_frame(
    datahora = datahora,
    titulo = titulo,
    autor = autor,
    texto = texto
  )
}

