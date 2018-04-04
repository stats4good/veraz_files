#' @title ELT - Seguro Defeso
#' 
#' @description Função para leitura dos dados do Programa Seguro Defeso que estão 
#'              disponíveis no site do \href{http://www.portaltransparencia.gov.br/}{Portal da Transparência}.
#' 
#' @param data_path caminho completo do arquivo a ser carregado pela função.
#' @param ano ano de referência dos dados com 4 dígitos (\code{AAAA}).
#' @param mes mês de referência dos dados com 2 dígitos (\code{MM}).
#' 
#' @example 
#' \donrun{
#' file_path <- "/home/guerrero/S4G/201501_SeguroDefeso.csv"
#' data <- tidyBolsaFamilia(data_path = file_path, ano = "2015", mes = "01")
#' }
#' 
#' @import data.table dplyr bit64 lubridate

tidySeguroDefeso <- function(data_path, ano, mes) {
  library("data.table")
  library("dplyr")
  library("bit64")
  library("lubridate")
  
  # loading the data 
  data <- suppressWarnings(fread(input = data_path, header = TRUE, encoding = "Latin-1"))
  names(data) <- iconv(names(data), from = "latin1")
  
  nome_coluna <- names(data)
  n_linhas  <- nrow(data)
  
  # testa se o arquivo tem linhas
  if (n_linhas < 1) {
    stop("Arquivo com número de linhas inferior a 1")
  }
  
  # retira a coluna '0', caso ela esteja na base
  if('0' %in% nome_coluna) {
    data <- data[, -'0']
  }
  
  # atualiza nome_coluna
  nome_coluna <- names(data)
  
  colunas_completa <- c('Data Referência', 'Número Requerimento',
                        'Data Requerimento',	'PIS Pescador', 'CPF Pescador',
                        'Número RGP',	'Nome Pescador', 'UF Pescador', 'Código IBGE Município Pescador',
                        'Nome Município Pescador', 'Portaria Defeso IBAMA', 'Data Início Defeso',
                        'Data Fim Defeso', 'Data Emissão Parcela', 'Número Parcela',
                        'Data Saque Parcela', 'Valor Parcela', 'Data Restituição',
                        'Valor Restituição', 'Código Situação Parcela', 'Descrição Situação Parcela',
                        'CodigoFuncao', 'CodigoSubfuncao', 'CodigoPrograma', 'CodigoAcao')
  
  # testa se o arquivo tem todas as colunas
  if (!all(nome_coluna %in% colunas_completa)) {
    col_missing <- colunas_completa[!(colunas_completa %in% nome_coluna)]
    msg <- sprintf("Variáveis faltantes: %s", col_missing)
    stop(msg) 
  }
  
  # coluna NA
  coluna_na <- data[, lapply(.SD, FUN = function(x) all(is.na(x)))]
  coluna_na <- apply(coluna_na, 2, isTRUE)
  
  # Coluna `Data Referência` sem valor
  if (coluna_na["Data Referência"]) {
    data[, `Data Referência` := as.character(`Data Referência`)]
    data[, `Data Referência` := paste(mes, ano, sep = "/")]
  }
  
  # Coluna `Valor Parcela`
  if (data[, !is.numeric(`Valor Parcela`)]) {
    data[, `Valor Parcela` := as.numeric(gsub(pattern = ",", replacement = ".", x = `Valor Parcela`))]
    data <- data[order(`Valor Parcela`, decreasing = T)]
  }
  
  # Coluna `Valor Restituição`
  if (data[, !is.numeric(`Valor Restituição`)]) {
    data[, `Valor Restituição` := as.numeric(gsub(pattern = ",", replacement = ".", x = `Valor Restituição`))]
    data <- data[order(`Valor Restituição`, decreasing = T)]
  }
  
  # coersao de datas
  data$'Data Requerimento' <- dmy(data$'Data Requerimento')
  data$'Data Início Defeso' <- dmy(data$'Data Início Defeso')
  data$'Data Fim Defeso' <- dmy(data$'Data Fim Defeso')
  data$'Data Emissão Parcela' <- dmy(data$'Data Emissão Parcela')
  data$'Data Saque Parcela' <- dmy(data$'Data Saque Parcela')
  data$'Data Restituição' <- dmy(data$'Data Restituição')
  
  return(data)
}

arquivo <- "/home/guerrero/S4G/201501_SeguroDefeso.csv"
df <- tidySeguroDefeso(arquivo, 2015, 01)

arquivo2 <- "/home/guerrero/S4G/201610_SeguroDefeso.csv"
df2 <- tidySeguroDefeso(arquivo2, 2016, 10)


