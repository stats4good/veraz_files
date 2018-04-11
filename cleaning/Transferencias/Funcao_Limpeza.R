limpeza_Transf <- function(file)
{
  # setwd(path_wd)
  # file <- paste(path_wd, file, sep = "")
  
  a <- readLines(file, skipNul = TRUE)
  
  verificacao <- stringr::str_count(string = a, pattern = "\t")
  
  
  tempcsv <- tempfile(pattern = '', fileext = '.csv')
  
  
  if(length(table(verificacao)) > 1)
  {
    verificacao <- stringr::str_count(string = a, pattern = "\t")
    
    linha_corrigida <- paste0(a[verificacao == 2], a[verificacao == 15])
    
    a_incompleto <- a[verificacao == 17]
    a <- c(a_incompleto, linha_corrigida)
    
    write(x = a, file = tempcsv)
  }
  
  else
  {
    write(x = a, file = tempcsv) 
  }
  
  dados <- suppressWarnings(data.table::fread(tempcsv, dec = ",", encoding = "Latin-1"))
  
  names(dados)  <- iconv(names(dados), from = "latin1")
  
  dados2 <- dplyr::as_data_frame(dados)
  
  dados <- as.data.frame(dados2)
  
  
  ano_data <- strsplit(file,split = '_')
  ano_data <- as.numeric(strsplit(ano_data[[1]][1], "(?<=.{4})", perl = TRUE)[[1]])
  dados$Mes <- ano_data[2]
  dados$Ano <- ano_data[1]
  
  for(i in 1:ncol(dados))
  {
    if(names(dados)[i] != "Valor Parcela" | !is.numeric(dados[,i]))
    {
      dados[,i] <- as.factor(dados[,i])
    }
  }
  
  dados$`Valor Parcela` <- as.numeric(gsub(x = dados$`Valor Parcela`, ",", ""))
  
  unlink(tempcsv)
  
  return(dados)
  
}


limpeza_CPDC <- function(path_wd, file)
{
  setwd(path_wd)
  file <- paste(path_wd, file, sep = "")
  
  a <- readLines(file, skipNul = TRUE)
  
  write(x = a, file = "Copy.csv") 
  
  dados <- suppressWarnings(data.table::fread("Copy.csv", dec = ",", encoding = "Latin-1"))
  
  names(dados)  <- iconv(names(dados), from = "latin1")
  
  dados2 <- dplyr::as_data_frame(dados)
  
  dados <- as.data.frame(dados2)
  
  ano_data <- strsplit(file,split = '_')
  ano_data <- as.numeric(strsplit(ano_data[[1]][1], "(?<=.{4})", perl = TRUE)[[1]])
  dados$Mes <- ano_data[2]
  dados$Ano <- ano_data[1]
  
  for(i in 1:ncol(dados))
  {
    if(names(dados)[i] != "Valor Transação" & names(dados)[i] != "Ano Extrato" & 
       names(dados)[i] != "Mês Extrato" & 
       names(dados)[i] != "Data Transação" | !is.numeric(dados[,i]))
    {
      dados[,i] <- as.factor(dados[,i])
    }
  }
  
  dados$`Data Transação` <- as.Date(dados$`Data Transação`, format = "%d/%m/%Y")
  
  return(dados)
  
}

