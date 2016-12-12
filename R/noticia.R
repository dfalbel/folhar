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
    url = url,
    datahora = datahora,
    titulo = titulo,
    autor = ifelse(length(autor) != 1, NA_character_, autor),
    texto = texto
  )
}

#' Faz parse das notícias com URL começando em:
#' http://f5.folha.uol.com.br/
#'
#' @param url url da noticia
#'
#' @examples
#' url <- "http://f5.folha.uol.com.br/colunistas/tonygoes/2016/12/sao-paulo-precisa-descobrir-sua-propria-maneira-de-realizar-a-virada-cultural.shtml"
#' noticia <- parse_f5(url)
#'
#' url <- "http://f5.folha.uol.com.br/celebridades/2016/12/por-que-a-atriz-mais-bem-paga-da-tv-americana-esta-sendo-processada-pelos-proprios-embrioes.shtml"
#' noticia <- parse_f5(url)
#'
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
    url = url,
    datahora = datahora,
    titulo = titulo,
    autor = ifelse(length(autor) != 1, NA_character_, autor),
    texto = texto
  )
}

#' Faz o parse para noticias com URL começando em:
#' http://www.agora.uol.com.br/
#'
#' @param url url da noticia
#'
#' @examples
#' url <- "http://www.agora.uol.com.br/saopaulo/2016/12/1839811-mortes-em-marginais-ocorrem-com-motos-nas-pistas-locais.shtml"
#' noticia <- parse_agora(url)
#'
#' @export
parse_agora <- function(url){

  noticia <- httr::GET(url) %>%
    httr::content(encoding = "windows-1252")

  datahora <- noticia %>%
    rvest::html_node("meta[name='date']") %>%
    rvest::html_attr("content") %>%
    readr::parse_datetime(format = "%Hh%M %d/%m/%Y")

  titulo <- noticia %>%
    rvest::html_node("meta[name='title']") %>%
    rvest::html_attr("content")

  autor <- noticia %>%
    rvest::html_nodes('#articleBy') %>%
    rvest::html_text() %>%
    stringr::str_replace_all("do Agora", "") %>%
    stringr::str_trim()

  texto <- noticia %>%
    rvest::html_nodes('#articleNew > p') %>%
    rvest::html_text() %>%
    paste(collapse = " ")

  dplyr::data_frame(
    url = url,
    datahora = datahora,
    titulo = titulo,
    autor = ifelse(length(autor) != 1, NA_character_, autor),
    texto = texto
  )
}

#' Parse Noticia
#'
#' Faz parse de uma notícia. Escolhe a função adequada, dependendo da URL.
#'
#' @param url url da notícia
#'
#' @examples
#' url <- "http://www.agora.uol.com.br/saopaulo/2016/12/1839811-mortes-em-marginais-ocorrem-com-motos-nas-pistas-locais.shtml"
#' parse_noticia(url)
#'
#' url <- "http://www1.folha.uol.com.br/mundo/2016/11/1830974-americanos-insatisfeitos-protestam-contra-vitoria-de-trump-em-eleicoes.shtml"
#' parse_noticia(url)
#'
#' url <- "http://f5.folha.uol.com.br/colunistas/tonygoes/2016/12/sao-paulo-precisa-descobrir-sua-propria-maneira-de-realizar-a-virada-cultural.shtml"
#' noticia <- parse_noticia(url)
#'
#' url <- "http://f5.folha.uol.com.br/celebridades/2016/12/por-que-a-atriz-mais-bem-paga-da-tv-americana-esta-sendo-processada-pelos-proprios-embrioes.shtml"
#' noticia <- parse_noticia(url)
#'
#' @export
parse_noticia <- function(url){

  fun_parse <-  dplyr::case_when(
    stringr::str_detect(url, "http://f5.folha.uol.com.br/") ~ "parse_f5",
    stringr::str_detect(url, "http://www.agora.uol.com.br/") ~ "parse_agora",
    stringr::str_detect(url, "http://www1.folha.uol.com.br/") ~  "parse_folha"
  )

  if(is.na(fun_parse)){
    message("Parse indefinido para esta noticia. Retornando vazio (NULL)")
    return(NULL)
  }

  fun_parse <- switch (fun_parse,
    parse_agora = parse_agora,
    parse_f5 = parse_f5,
    parse_folha = parse_folha
  )

  fun_parse(url)
}

#' Obter mais infos das notícias.
#'
#' @param urls vetor de urls para os quais deseja-se obter mais informações.
#'
#' @examples
#' urls <- c(
#'  "http://www1.folha.uol.com.br/mundo/2016/11/1830974-americanos-insatisfeitos-protestam-contra-vitoria-de-trump-em-eleicoes.shtml",
#'  "http://f5.folha.uol.com.br/colunistas/tonygoes/2016/12/sao-paulo-precisa-descobrir-sua-propria-maneira-de-realizar-a-virada-cultural.shtml",
#'  "http://f5.folha.uol.com.br/celebridades/2016/12/por-que-a-atriz-mais-bem-paga-da-tv-americana-esta-sendo-processada-pelos-proprios-embrioes.shtml",
#'  "http://www.agora.uol.com.br/saopaulo/2016/12/1839811-mortes-em-marginais-ocorrem-com-motos-nas-pistas-locais.shtml"
#'  )
#' noticias <- folha_noticias(urls)
#'
#' @export
folha_noticias <- function(urls){
  purrr::map_df(urls, parse_noticia)
}
