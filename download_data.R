download_perfil <- function (dir = NULL, sec, subsec, subsubsec, year, month, day = NULL)
{
  # Check inicial:
  today <- c(format(Sys.Date(), "%Y"), format(Sys.Date(), "%m"), format(Sys.Date(), "%d"))
  if (any(year < 2011 | year > as.numeric(today[1]))) stop("This Date is not available")
  if (any(month < 1 | month > 12)) stop("This Date is not available")
  if(is.null(dir)) dir <- getwd()

  # Transforma??o das vari?veis:
  year  <- as.character(year)
  month <- sprintf("%02d", month)
  if (subsec == 'GastosDiretos' && subsubsec == 'Pagamentos_GastosDiretos') subsubsec <- 'GastosDiretos'
  if (subsec == 'Transferencias' && subsubsec == 'Pagamentos_Transferencias') subsubsec <- 'Transferencias'

  # Cria??o do Link
  link <- "http://arquivos.portaldatransparencia.gov.br/downloads.asp?a=%s&m=%s&consulta=%s"
  link_down = sprintf(fmt = link, year, month, subsubsec)

  # Download e armazenamento dos dados:
  file_name = paste(year, month, subsec, subsubsec, sep = "_")
  file_direc = paste(dir, "/", file_name, ".zip", sep = "")

  link_test = httr::HEAD(link_down)$headers[[3]]

  dir_sec = paste(dir, "/", sec, sep = "")

  dir_subsec = paste(dir_sec, "/", subsec, sep = "")

  if ( (subsec == 'GastosDiretos' && subsubsec == 'Pagamentos_GastosDiretos') ||
       (subsec == 'Transferencias' && subsubsec == 'Pagamentos_Transferencias') )
  {
    dir_subsubsec = paste(dir_subsec, "/", 'Pagamentos', sep = "")
  }else{
    dir_subsubsec = paste(dir_subsec, "/", subsubsec, sep = "")
  }

  dir_year = paste(dir_subsubsec, "/", year, sep = "")

  if (link_test == "application/x-download")
  {
    download.file(url = link_down, destfile = file_direc, mode = "wb")
    if(!dir.exists(dir_sec))
    {
      if (!dir.exists(dir_subsec))
      {
        dir.create(dir_subsec)

        if (!dir.exists(dir_subsubsec))
        {
          dir.create(dir_subsubsec)
        }
        if (!dir.exists(dir_year))
        {
          dir.create(dir_year)
        }

      }
    }
    unzip(zipfile = file_direc, exdir = dir_year)
    unlink(file_direc)
    data.frame(dir = file_direc, link = link_down, link_error = 'F')
  }else{
    data.frame(dir = file_direc, link = link_down, link_error = 'T')
  }

}

perfil <- function (secs, subsecs, subsubsecs, years, months) {
  df <- vector(mode = 'list', length = length(subsecs))
  for (i in seq_along(subsecs)) {
    df[[i]] <- expand.grid(sec       = secs,
                           subsec    = subsecs[i],
                           subsubsec = subsubsecs[[subsecs[i]]],
                           year      = years,
                           month     = months)
  }
  df %>%
    dplyr::bind_rows() %>%
    dplyr::tbl_df()
}

# Fun??o para fazer downloads de forma recursiva:
download_portal_trans <- function (dir        = NULL,
                                   secs       = 'Despesas',
                                   subsecs    = c('GastosDiretos', 'Transferencias', 'ProgramasSociais'),
                                   subsubsecs = list('GastosDiretos'    = c('Pagamentos_GastosDiretos', 'Diarias', 'CPGF', 'CPGFComprasCentralizadas', 'FavorecidosGastosDiretos', 'ConsorcioGD'),
                                                     'Transferencias'    = c('Pagamentos_Transferencias', 'CPDC', 'FavorecidosTransferencias', 'ConsorcioTR'),
                                                     'ProgramasSociais' = c('BolsaFamiliaFolhaPagamento', 'BolsaFamiliaSacado', 'SeguroDefeso', 'OutrasTransferenciassCidadao')),
                                   years      = 2011:2017,
                                   months     = 1:12,
                                   days       = NULL)
{

  # Criando o diret?rio para salvar os arquivos:
  if (is.null(dir)) {
    dir <- paste(getwd(), '/data_portal_trans', sep = '')
    dir.create(dir)
  } else {
    dir <- paste(dir, '/data_portal_trans', sep = '')
    dir.create(dir)
  }

  # Fun??o para fazer os downloads de forma recursiva:
  f <- dplyr::failwith(dplyr::data_frame(), download_perfil)

  # Obtendo os perfis e fazendo os downloads:
  perfil(secs, subsecs, subsubsecs, years, months) %>%
    dplyr::filter(!(year >= as.numeric(format(Sys.Date(), "%Y")) & month >= as.numeric(format(Sys.Date(), "%m")))) %>%
    dplyr::group_by(sec,
                    subsec,
                    subsubsec,
                    year,
                    month) %>%
    dplyr::do(f(dir = dir,
                .$sec,
                .$subsec,
                .$subsubsec,
                .$year,
                .$month)) %>%
    dplyr::ungroup() %>%
    dplyr::tbl_df()
}
