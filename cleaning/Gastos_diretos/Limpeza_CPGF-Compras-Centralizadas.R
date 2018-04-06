if (!require("data.table")) install.packages("data.table")
if (!require("dplyr")) install.packages("dplyr")

read_CPFG_Compras <- function (dir , file_name)
{
  
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
  
  # loading data
  
  path_name <- paste(dir, file_name, sep="")
  
  table_data <- suppressWarnings(data.table::fread (input = path_name, header = T, sep = '\t',
  																 encoding = "Latin-1", dec = ','))
  
 
  names(table_data)  <- iconv(names(table_data), from = "latin1")
  
  table_data <- dplyr::as_data_frame (table_data)
  table_data <- as.data.frame(table_data)
  
  
  # Verificando se existe algum NA na base
  if (any (is.na (table_data) == TRUE))
  {
    warning ('The dataset contains NA')
  }

  #Verificando as colunas do Banco de dados
  
  variable_names <- c("Código Órgão Superior", "Nome Órgão Superior", 
                      "Código Órgão Subordinado","Nome Órgão Subordinado", 
                      "Código Unidade Gestora", "Nome Unidade Gestora",
                      "Ano Extrato", "Mês Extrato", 
                      "Tipo de Aquisição", "Transação","Data Transação", 
                      "CNPJ ou CPF Favorecido","Nome Favorecido","Valor Transação")
  
  
  if (any (colnames (table_data) != variable_names))
  {
    index_dif <- which (variable_names != colnames(table_data))
    
    dif_variable <- variable_names[index_dif]
    
    warning (paste ('The variables :', dif_variable, 'doesnt exist or they are disorderly', sep = ""))
  }
  
  
  # Modificando o tipo das variáveis
  
  factor_variables <- names(table_data)[c (seq (from = 1, to = 6), 9,10,12,13)]
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

