library(magrittr)

#---- Portal Transf ----

# Source na funcao que faz download dos dados
source('download/download_data.R') # Importante: verificar o caminho dos arquivos

#---- Transferencias ----

# Baixa os dados
download_portal_trans(dir        = NULL,
                      secs       = 'Despesas',
                      subsecs    = 'Transferencias',
                      subsubsecs = list('GastosDiretos'    = NULL,
                                        'Transferencias'   = 'Pagamentos_Transferencias',
                                        'ProgramasSociais' = NULL),
                      years      = 2018,
                      months     = 2,
                      days       = NULL)

# Lendo os dados com funcao que faz a limpeza da base

# source na funcao
source('cleaning/Transferencias/Funcao_Limpeza.R')

dados <- limpeza_Transf(file = 'data_portal_trans/Despesas/Transferencias/Transferencias/2018/201802_Transferencias.csv')

# Exemple: Valor medio das transferencias do governo federal em fev/2018
# por Funcao
dados %>% 
  dplyr::group_by(`Nome Funcao`) %>% 
  dplyr::summarise(`Valor Medio` = mean(`Valor Parcela`)) %>% 
  dplyr::arrange(-`Valor Medio`) %>% 
  dplyr::top_n(10)

rm(list = ls())
gc(reset = T)
