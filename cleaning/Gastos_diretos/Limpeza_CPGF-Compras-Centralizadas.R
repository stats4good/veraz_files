limpeza_cpfg_compc <- function(file)
{
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
  
  
  # Verificando se existe algum NA na base
  if (any (is.na (dados) == TRUE))
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
  
  
  if (any (colnames (dados) != variable_names))
  {
    index_dif <- which (variable_names != colnames(dados))
    
    dif_variable <- variable_names[index_dif]
    
    warning (paste ('The variables :', dif_variable, 'doesnt exist or they are disorderly', sep = ""))
  }
  
  
  # Modificando o tipo das variáveis
  
  #factor_variables <- names(dados)[c (seq (from = 1, to = 6), 9,10,12,13)]
  #dados[,factor_variables] <- lapply (dados [,factor_variables], as.factor)
  

  # #Adicionando DATA e ano
  # ano_data <- strsplit(basename(file),split = '_')
  # ano_data <- as.numeric(strsplit(ano_data[[1]][1], "(?<=.{4})", perl = TRUE)[[1]])
  # dados$Mes <- ano_data[2]
  # dados$Ano <- ano_data[1]

  for(i in 1:ncol(dados))
  {
    if(names(dados)[i] != "Valor Transação" | !is.numeric(dados[,i]))
    {
      dados[,i] <- as.factor(dados[,i])
    }
  }
  
  #dados$`Data Transação` <- as.Date(x = dados$`Data Transação`, format =  '%d/%m/%Y')
   dados$`Data Transação` <- as.character(dados$`Data Transação`)
  
  dados$`Valor Transação` <- as.numeric(gsub(x = dados$`Valor Transação`, ",", ""))
  unlink(tempcsv)
  return(dados)
  
}
