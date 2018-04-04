#' @title ELT - Bolsa Família
#' 
#' @description Função para leitura dos dados de pagamento do Programa Bolsa Família que estão 
#'              disponíveis no site do \href{http://www.portaltransparencia.gov.br/}{Portal da Transparência}.
#' 
#' @param data_path caminho completo do arquivo a ser carregado pela função.
#' @param ano ano de referência dos dados com 4 dígitos (\code{AAAA}).
#' @param mes mês de referência dos dados com 2 dígitos (\code{MM}).
#' @param cols colunas a serem retornadas. O padrão retorna todas as colunas.
#' 
#' @example 
#' \donrun{
#' file_path <- "~/Desktop/Bolsa-Familia/Pagamento/201701_BolsaFamiliaFolhaPagamento.csv"
#' data <- tidyBolsaFamilia(data_path = file_path, ano = "2017", mes = "01")
#' }
#' 
#' @import data.table bit64

tidyBolsaFamilia <- function(data_path, ano, mes, cols = NULL) {
  library(data.table)
  library(bit64)
  
  # loading the data 
  data <- suppressWarnings(fread(input = data_path, encoding = "Latin-1"))
  names(data) <- iconv(names(data), from = "latin1")

  nome_coluna <- names(data)
  n_linhas  <- nrow(data)
  
  colunas_completa <- c('UF', 'Código SIAFI Município', 'Nome Município', 'Código Função', 'Código Subfunção', 'Código Programa', 'Código Ação', 'NIS Favorecido', 'Nome Favorecido', 'Fonte-Finalidade', 'Valor Parcela', 'Mês Competência')
  
  # testa se o arquivo tem linhas
  if (n_linhas < 1) {
    stop("Arquivo com número de linhas inferior a 1")
  }
  
  # testa se o arquivo tem todas as colunas
  if (!all(nome_coluna %in% colunas_completa)) {
    col_missing <- colunas_completa[!(colunas_completa %in% nome_coluna)]
    msg <- sprintf("Variáveis faltantes: %s", col_missing)
    stop(msg) 
  }
  
  # coluna NA
  coluna_na <- data[, lapply(.SD, FUN = function(x) all(is.na(x)))]
  coluna_na <- apply(coluna_na, 2, isTRUE)
  
  # Coluna `Mês Competência` sem valor
  if (coluna_na["Mês Competência"]) {
    data[, `Mês Competência` := as.character(`Mês Competência`)]
    data[, `Mês Competência` := paste(mes, ano, sep = "/")]
  }
  
  # Coluna `Valor Parcela`
  if (data[, !is.numeric(`Valor Parcela`)]) {
    data[, `Valor Parcela` := as.numeric(gsub(pattern = ",", replacement = "", x = `Valor Parcela`))]
    data <- data[order(`Valor Parcela`, decreasing = T)]
  }

  if (is.null(cols)) {
    cols <- c('UF', 'Código SIAFI Município', 'Nome Município', 'NIS Favorecido', 'Nome Favorecido', 'Fonte-Finalidade', 'Valor Parcela', 'Mês Competência')
  }
  
  data <- data[, cols, with = F] 
  
  return(data)
}



