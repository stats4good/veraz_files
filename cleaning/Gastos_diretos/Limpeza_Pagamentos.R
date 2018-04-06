if (!require("data.table")) install.packages("data.table")
if (!require("dplyr")) install.packages("dplyr")


read_gd_pag <- function (dir , file)
{
  # loading data
  path_name <- file.path(dir, file)
  
  aux <- readLines(con = path_name, skipNul = TRUE)
  
  path_tempfile <- file.path(dir,'temp_file.csv')
  
  write(x = aux, path_tempfile)
  
  dados <- suppressWarnings(data.table::fread (input = path_tempfile, header = T, sep = '\t',
                                               encoding = "Latin-1", dec = ','))
  
  unlink (path_tempfile)
  
  names(dados)  <- iconv(names(dados), from = "latin1")
  
  dados <- dplyr::as_data_frame (dados)
  
  dados <- as.data.frame(dados)
  
  # Verificando e adicionando mês e ano
  name_date <- gsub('/','',file)
  ano_data <- strsplit(name_date,split = '_')
  ano_data <- as.numeric(strsplit(ano_data[[1]][1], "(?<=.{4})", perl = TRUE)[[1]])
  dados$Mes <- ano_data[2]
  dados$Ano <- ano_data[1]
  
  # Verificando se existe algum NA na base
  if (any (is.na (dados) == TRUE))
  {
    warning ('The dataset contains NA')
  }
  
  variable_names <- c("Código Órgão Superior", "Nome Órgão Superior", "Código Órgão",
                      "Nome Órgao", "Código Unidade Gestora", "Nome Unidade Gestora",
                      "Código Grupo Despesa", "Nome Grupo Despesa", "Código Elemento Despesa", 
                      "Nome Elemento Despesa", "Código Função", "Nome Função", 
                      "Código Subfunção", "Nome Subfunção", "Código Programa", 
                      "Nome Programa", "Código Ação", "Nome Ação", "Linguagem Cidadã",
                      "Código Favorecido","Nome Favorecido", "Número Documento", 
                      "Gestão Pagamento", "Data Pagamento", "Valor", "Mes", "Ano")
  
  if (any (colnames (dados) != variable_names))
  {
    index_dif <- which (variable_names != colnames(dados))
    
    dif_variable <- variable_names[index_dif]
    
    warning (paste ('The variables :', dif_variable, 'doesnt exist or they are disorderly', sep = ""))
  }
  
  # Modificando o tipo das variáveis
  
  dados$`Data Pagamento` <- as.Date (x = dados$`Data Pagamento`, format =  '%d/%m/%Y')
  
  return(dados)
  
}

