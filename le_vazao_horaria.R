

library(RODBC)
library(tidyverse)

### LEITURA

  caminho <- dirname(rstudioapi::getSourceEditorContext()$path)
  
  setwd(caminho)
  
  caminho_dados <- paste0(caminho, '/Vazoes_ANA.mdb')
  
# comunicação com o banco de dados Vazoes_ANA.mdb
  canal <- odbcDriverConnect(paste0('Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=', caminho_dados))
  
# tabelas do schema
  sqlTables(canal)

# seleciona as colunas da tabela 'Vazoes' sem definir os tipos das colunas
  tb_vazao_horaria <- sqlFetch(canal, "Vazoes", as.is = TRUE)

# tipos da tabela 'Vazoes' no banco de dados
  sqlColumns(canal, "Vazoes")

# tipos da tabela 'Vazoes' importados no R
  str(tb_vazao_horaria)
  

  # seleciona as variáveis de interess
  df_vazao_horaria <- tb_vazao_horaria %>% 
    select(RegistroID, Data, EstacaoCodigo, NivelConsistencia,
           Vazao01, Vazao02, Vazao03, Vazao04, Vazao05, Vazao06,
           Vazao07, Vazao08, Vazao09, Vazao10, Vazao11, Vazao12,
           Vazao13, Vazao14, Vazao15, Vazao16, Vazao17, Vazao18,
           Vazao19, Vazao20, Vazao21,Vazao22, Vazao23, Vazao24)

  head(df_vazao_horaria)
