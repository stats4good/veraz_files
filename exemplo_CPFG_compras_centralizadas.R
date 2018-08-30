library(magrittr)

#---- Portal Transf ----

# Source na funcao que faz download dos dados
source('download/download_data.R') # Importante: verificar o caminho dos arquivos

#---- CPFG Compras Centralizadas ----

#Descrição : Estão disponíveis informações dos gastos com Cartão de Pagamentos para aquisições de Passagens Aéreas

# Baixa os dados
download_portal_trans(dir        = NULL,
                      secs       = 'Despesas',
                      subsecs    = 'GastosDiretos',
                      subsubsecs = list('GastosDiretos'    = 'CPGFComprasCentralizadas',
                                        'Transferencias'   = NULL,
                                        'ProgramasSociais' = NULL),
                      years      = 2015,
                      months     = 1,
                      days       = NULL)
  
# Lendo os dados com funcao que faz a limpeza da base
  
# source na funcao
source('cleaning/Gastos_diretos/Limpeza_CPGF-Compras-Centralizadas.R' , encoding = "UTF-8")

dados <- limpeza_cpfg_compc(file = 'data_portal_trans/Despesas/GastosDiretos/CPGFComprasCentralizadas/2015/201501_CPGFComprasCentralizadas.csv')

# Example: Valor total de gastos com passagens aereas em Jan/2015
dados %>% 
  dplyr::select(`Nome Órgão Superior`,`Valor Transação`)%>%
  dplyr::group_by(`Nome Órgão Superior`) %>% 
  dplyr::summarise(Valor =  sum(`Valor Transação`))

# Example: Valor total recebido por favorecido de passagens aereas em Jan/2015 
dados %>% 
  dplyr::group_by(`Nome Favorecido`) %>% 
  dplyr::summarise(Valor = sum(`Valor Transação`)) %>% 
  dplyr::arrange(- Valor) 

rm(list = ls())
gc(reset = T)
