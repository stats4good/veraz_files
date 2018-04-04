read_gd_diarias <- function(dir, file_name) {
  library(data.table)
  library(dplyr)
  
  file_new_name = "tmp.csv"
  path1 = paste(dir, file_name, sep="")
  path2 = paste(dir, file_new_name, sep="")
  
  str_remove_null = paste("LC_ALL=C tr -d '\\00' <", path1, ">", path2, "| LC_ALL=C grep search-string", sep=" ")
  cmd_remove_null = try(system(str_remove_null, intern=T))
  
  if(attributes(cmd_remove_null)[[1]] > 1) #olhar direito a saida dessa funcao system
    return(NULL)
  
  file.remove(path1)
  file.rename(path2, path1)
  path2 = path1
  
  table_data <- suppressWarnings(data.table::fread(path2, dec = ",", encoding = "Latin-1"))
  names(table_data)  <- iconv(names(table_data), from = "latin1")
  table_data2 <- dplyr::as_data_frame(table_data)
  table_data <- as.data.frame(table_data2)
  
  
  for(i in 1:ncol(table_data)){
    if(names(table_data)[i] != "Nome Favorecido" & names(table_data)[i] != "Valor Parcela" & 
       !is.numeric(table_data[,i])){
      table_data[,i] <- as.factor(table_data[,i])
    }
  }
  
  table_data$`Data Pagamento` <- as.Date (table_data$`Data Pagamento`, format =  '%d/%m/%Y')
  
  # Adicionando mês e ano
  name_date <- gsub('/','',file_name)
  ano_data <- strsplit(name_date,split = '_')
  ano_data <- as.numeric(strsplit(ano_data[[1]][1], "(?<=.{4})", perl = TRUE)[[1]])
  table_data$Mes <- ano_data[2]
  table_data$Ano <- ano_data[1]
  
  return(table_data)
}

#######################################################################

file_name = '201703_Diarias.csv'
dir <- "~/Downloads/s4g/limpeza_diarias/"
dados = read_gd_diarias(dir, file_name)
