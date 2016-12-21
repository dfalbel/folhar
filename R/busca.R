#' Buscar_
#'
#' Função auxiliar da busca.
#'
#' @param q query
#' @param sd data de inicio no formado dd/mm/aaaa
#' @param ed data de fim no formato dd/mm/aaaa
#' @param sr primeiro item da consulta (a consulta sempre retorna 25 itens)
#'
buscar_ <- function(q, sd, ed, sr = 1){

  consulta <- httr::GET(
    "http://search.folha.uol.com.br/search",
    query = list(
      q = q,
      sd = sd,
      ed = ed,
      sr = sr
    )
  )

  return(consulta)
}

#' Buscar
#'
#' Função auxiliar que baixa todas as páginas de uma busca
#' dependendo de um número máximo de registros.
#'
#' @param q query
#' @param sd data de inicio no formado dd/mm/aaaa
#' @param ed data de fim no formato dd/mm/aaaa
#' @param max_r número máximo de registros
#' @param wt tempo de espera entre as consultas (importante p/ não sobrecarregar
#' o site/ ser bloqueado)
#'
buscar <- function(q, sd, ed, max_r = 100, wt = 1){

  consulta <- buscar_(q, sd, ed)

  itens <- max_itens(consulta)

  if(length(itens) == 0){
    message("A busca nao retornou resultados. Usou palavras com acentos? Retornando lista vazia")
    return(list())
  }

  if(itens > max_r){
    message(sprintf("A busca retornou %d registros, mas vamos retornar %d", itens, max_r))
  }


  max_r <- min(max_r, itens)
  iter <- max_r %/% 25
  if(max_r %% 25 != 0) iter <- iter + 1

  res <- vector("list", iter)
  res[[1]] <- consulta
  for(i in 2:iter){
    Sys.sleep(wt)
    res[[i]] <- buscar_(q, sd, ed, sr = (i-1)*25 + 1)
  }

  return(res)
}

#' Parse Busca
#'
#' Transforma o objeto da busca em um data_frame
#'
#' @param x objeto httr da busca
#'
parse_busca <- function(x){

  lista <- httr::content(x) %>%
    rvest::html_nodes(".search-results-list") %>%
    rvest::html_nodes("li")

  link <- lista %>%
    rvest::html_nodes(".search-results-title > a") %>%
    rvest::html_attr("href")

  trecho <- lista %>%
    rvest::html_nodes(".content") %>%
    rvest::html_text()

  titulo <- lista %>%
    rvest::html_nodes("h3 a") %>%
    rvest::html_text()

  data <- titulo %>%
    stringr::str_extract("[0-9]{2}/[0-9]{2}/[0-9]{4}") %>%
    lubridate::dmy()

  dplyr::data_frame(
    data = data,
    titulo = titulo,
    link = link,
    trecho = trecho
  )
}

#' Max Itens
#'
#' Devolve o máximo de itens retornados pela busca.
#'
#' @param x objeto httr da consulta
#'
max_itens <- function(x){
  httr::content(x) %>%
    rvest::html_nodes(".search-title span") %>%
    rvest::html_text() %>%
    stringr::str_extract("[0-9]{1,}\\)") %>%
    readr::parse_number()
}

#' Buscar Folha
#'
#' Função para buscar notícias no site da Folha de São Paulo.
#'
#' @param q query
#' @param sd data de inicio no formado dd/mm/aaaa
#' @param ed data de fim no formato dd/mm/aaaa
#' @param max_r número máximo de registros
#'
#' @examples
#' x <- folha_buscar("estatistica", "01/11/2016", "05/12/2016")
#'
#' @export
folha_buscar <- function(q, sd, ed, max_r = 100){
  busca <- buscar(q, sd, ed, max_r)
  busca %>%
    purrr::map_df(parse_busca)
}
