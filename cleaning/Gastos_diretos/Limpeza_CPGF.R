read_gd_CPGF <- function (dir, file_name)
  
{ 
  require(data.table)
  require(dplyr)
  
  path <- paste(dir, file_name, sep="")
  
  table_data <- fread (input = path, header = T, na.string=c("", "NA"), encoding = "Latin-1", dec = ',')
  
  table_data <- dplyr::as_data_frame (table_data)
  
  table_data <- as.data.frame(table_data)
  
  
  # Verificando se existe algum NA na base
  
  if (any (is.na (table_data) == TRUE))
  {
    warning ('The dataset contains NA')
  }
  
  names_variables <- colnames(table_data)
  
  factor_variables <- names_variables [c (seq (from = 1, to = 8), 11)]
  
  table_data[,factor_variables] <- lapply (table_data [,factor_variables], as.factor)
  
  table_data$`Data Transação` <- as.Date (x = table_data$`Data Transação`, format =  '%d/%m/%Y')
  
  # Adicionando mês e ano
  name_date <- gsub('/','',file_name)
  ano_data <- strsplit(name_date,split = '_')
  ano_data <- as.numeric(strsplit(ano_data[[1]][1], "(?<=.{4})", perl = TRUE)[[1]])
  table_data$Mes <- ano_data[2]
  table_data$Ano <- ano_data[1]
  
  return(table_data)
}


dir <- 'C:/Users/Ana Julia/Documents/Stats4Good/ETL_CPGF/'

file_name <- '201703_CPGF.csv'

read_gd_CPGF(dir, file_name)