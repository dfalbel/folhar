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
    autor = ifelse(length(autor) != 1, NA_character_, autor),
    texto = texto
  )
}

#' Faz parse das notícias com URL começando em:
#' http://f5.folha.uol.com.br/
#'
#' @param url
#'
#' @examples
#' url <- "http://f5.folha.uol.com.br/colunistas/tonygoes/2016/12/sao-paulo-precisa-descobrir-sua-propria-maneira-de-realizar-a-virada-cultural.shtml"
#' noticia <- parse_f5(url)
#'
#' url <- "http://f5.folha.uol.com.br/celebridades/2016/12/por-que-a-atriz-mais-bem-paga-da-tv-americana-esta-sendo-processada-pelos-proprios-embrioes.shtml"
#' noticia <- parse_f5(url)
#'
#' @export
parse_f5 <- function(url){

  noticia <- httr::GET(url) %>%
    httr::content(encoding = "UTF-8")

  datahora <- noticia %>%
    rvest::html_nodes(".news .news__date-cover") %>%
    rvest::html_text() %>%
    lubridate::dmy_hms()

  autor <- noticia %>%
    rvest::html_nodes(".author__name") %>%
    rvest::html_text() %>%
    stringr::str_trim()

  titulo <- noticia %>%
    rvest::html_nodes(".news .news__title") %>%
    rvest::html_text()

  texto <- noticia %>%
    rvest::html_nodes(".news .news__content p") %>%
    rvest::html_text() %>%
    paste(collapse = " ")

  dplyr::data_frame(
    datahora = datahora,
    titulo = titulo,
    autor = ifelse(length(autor) != 1, NA_character_, autor),
    texto = texto
  )
}

