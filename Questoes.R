#Importar arquivos de dados
library(readr)
#Importaçõa de domicílios
pdad_2018_domicilios <- read_delim("dados/PDAD_2018_dom2018_31ras.csv", 
                                      ";", escape_double = FALSE, trim_ws = TRUE)


#Importaçõa de moradores
pdad_2018_moradores <- read_delim("dados/PDAD_2018_mor2018_31ras.csv", 
                                   ";", escape_double = FALSE, trim_ws = TRUE)

#Unificar arquivos de dados a partir do identificador único da fixa
library(dplyr)
pdad <- pdad_2018_moradores %>%
  # Entrar com a função para left join
  dplyr::left_join(
    # Informar a base que iremos unir, filtrando para colunas repetidas
    pdad_2018_domicilios %>%
      dplyr::select(-c(A01ra,FATOR_PROJ)))







