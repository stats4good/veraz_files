#' @title ELT - Seguro Defeso
#' 
#' @description Função para leitura dos dados dos Outros Programas Sociais, a saber,
#'              o Programa Garantia Safra e o Prgrama de Erradição do Trabalho Infantil, 
#'              disponíveis no site do \href{http://www.portaltransparencia.gov.br/}{Portal da Transparência}.
#' 
#' @param data_path caminho completo do arquivo a ser carregado pela função.
#' @param ano ano de referência dos dados com 4 dígitos (\code{AAAA}).
#' @param mes mês de referência dos dados com 2 dígitos (\code{MM}).
#' 
#' @example 
#' \donrun{
#' file_path <- "/home/guerrero/S4G/201701_OutrasTransferenciasCidadao.csv"
#' data <- tidyOutrosProgramas(data_path = file_path, ano = "2017", mes = "01")
#' }
#' 
#' @import data.table dplyr bit64 stringr

tidyOutrosProgramas <- function(data_path, ano, mes) {
  library("data.table")
  library("dplyr")
  library("bit64")
  library("stringr")

  # Resolvendo problema: " embedded nul in string: '\0' " e de quebra de linhas
  aux <- readLines(con = data_path, skipNul = TRUE)
  
  verificacao <- stringr::str_count(string = aux, pattern = "\t")
  
  if(length(table(verificacao)) > 1)
  {
    verificacao <- stringr::str_count(string = aux, pattern = "\t")
    
    linha_corrigida <- paste0(aux[verificacao == 2], aux[verificacao == 9])
    
    aux_incompleto <- aux[verificacao == 11]
    aux <- c(aux_incompleto, linha_corrigida)
    
    write(x = aux, file = "Copy.csv")
  }
  
  else
  {
    write(x = aux, file = "Copy.csv") 
  }
  
  data <- fread("Copy.csv", header = TRUE, encoding = "Latin-1")
  names(data) <- iconv(names(data), from = "latin1")
  
  # Removendo arquivo temporaráio do diretório de trabalho
  unlink ("Copy.csv")
  
  nome_coluna <- names(data)
  n_linhas  <- nrow(data)
  
  # testa se o arquivo tem linhas
  if (n_linhas < 1) {
    stop("Arquivo com número de linhas inferior a 1")
  }
  
  colunas_completa <- c('UF', 'Código SIAFI Município', 'Nome Município', 'Código Função',
                        'Código Subfunção', 'Código Programa', 'Código Ação',
                        'Código Favorecido', 'Nome Favorecido', 'Fonte-Finalidade',
                        'Situação Parcela', 'Valor Parcela')
  
  # testa se o arquivo tem todas as colunas
  if (!all(nome_coluna %in% colunas_completa)) {
    col_missing <- colunas_completa[!(colunas_completa %in% nome_coluna)]
    msg <- sprintf("Variáveis faltantes: %s", col_missing)
    stop(msg) 
  }
  
  # coluna NA
  coluna_na <- data[, lapply(.SD, FUN = function(x) all(is.na(x)))]
  coluna_na <- apply(coluna_na, 2, isTRUE)
  
  # Coluna `Valor Parcela`
  if (data[, !is.numeric(`Valor Parcela`)]) {
    data[, `Valor Parcela` := as.numeric(gsub(pattern = ",", replacement = ".", x = `Valor Parcela`))]
    data <- data[order(`Valor Parcela`, decreasing = T)]
  }
  
  # Adicionando a coluna Data Referência
  data$`Data Referência` <- as.character( paste(mes, ano, sep = "/") )
  
  return(data)
}

# Testes realizados:
arquivo <- "/home/guerrero/S4G/201101_OutrasTransferenciasCidadao.csv"
df <- tidyOutrosProgramas(arquivo, 2011, 01)
glimpse(df)
nrow(df) - sum(is.na(df$`Situação Parcela`))

arquivo2 <- "/home/guerrero/S4G/201106_OutrasTransferenciasCidadao.csv"
df2 <- tidyOutrosProgramas(arquivo2, 2011, 06)
glimpse(df2)
nrow(df2) - sum(is.na(df2$`Situação Parcela`))
unique(df2$`Situação Parcela`)

arquivo3 <- "/home/guerrero/S4G/201112_OutrasTransferenciasCidadao.csv"
df3 <- tidyOutrosProgramas(arquivo3, 2011, 12)
glimpse(df3)
nrow(df3) - sum(is.na(df3$`Situação Parcela`))
unique(df3$`Situação Parcela`)

arquivo4 <- "/home/guerrero/S4G/201302_OutrasTransferenciasCidadao.csv"
df4 <- tidyOutrosProgramas(arquivo4, 2013, 02)
glimpse(df4)
nrow(df4) - sum(is.na(df4$`Situação Parcela`))
unique(df4$`Situação Parcela`)

arquivo5 <- "/home/guerrero/S4G/201311_OutrasTransferenciasCidadao.csv"
df5 <- tidyOutrosProgramas(arquivo5, 2013, 11)
glimpse(df5)
nrow(df5) - sum(is.na(df5$`Situação Parcela`))
unique(df5$`Situação Parcela`)

arquivo6 <- "/home/guerrero/S4G/201603_OutrasTransferenciasCidadao.csv"
df6 <- tidyOutrosProgramas(arquivo6, 2016, 03)
glimpse(df6)
nrow(df6) - sum(is.na(df6$`Situação Parcela`))
unique(df6$`Situação Parcela`)

arquivo7 <- "/home/guerrero/S4G/201610_OutrasTransferenciasCidadao.csv"
df7 <- tidyOutrosProgramas(arquivo7, 2016, 10)
glimpse(df7)
nrow(df7) - sum(is.na(df7$`Situação Parcela`))
unique(df7$`Situação Parcela`)

arquivo8 <- "/home/guerrero/S4G/201701_OutrasTransferenciasCidadao.csv"
df8 <- tidyOutrosProgramas(arquivo8, 2017, 01)
glimpse(df8)
nrow(df8) - sum(is.na(df8$`Situação Parcela`))
unique(df8$`Situação Parcela`)

arquivo9 <- "/home/guerrero/S4G/201703_OutrasTransferenciasCidadao.csv"
df9 <- tidyOutrosProgramas(arquivo9, 2017, 03)
glimpse(df9)
nrow(df9) - sum(is.na(df9$`Situação Parcela`))
unique(df9$`Situação Parcela`)







