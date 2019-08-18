

library(RODBC)
library(tidyverse)
require(lubridate)

### LEITURA

  caminho <- dirname(rstudioapi::getSourceEditorContext()$path)
  
  setwd(caminho)
  
  caminho_dados <- paste0(caminho, '/Vazoes_ANA.mdb')
  
# comunicação com o banco de dados Vazoes_ANA.mdb
  canal <- odbcDriverConnect(paste0('Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=', caminho_dados))
  
# tabelas do schema
  sqlTables(canal)

# seleciona as colunas da tabela 'Vazoes' sem definir os tipos das colunas
  tb_vazao_diaria <- sqlFetch(canal, "Vazoes", as.is = TRUE)

# tipos da tabela 'Vazoes' no banco de dados
  sqlColumns(canal, "Vazoes")

# tipos da tabela 'Vazoes' importados no R
  str(tb_vazao_diaria)
  
# seleciona as variáveis de interesse tabela 'Vazoes' 
  df_vazao_diaria <- tb_vazao_diaria %>% 
    select(RegistroID, Data, EstacaoCodigo, NivelConsistencia,
           Vazao01, Vazao02, Vazao03, Vazao04, Vazao05, Vazao06,
           Vazao07, Vazao08, Vazao09, Vazao10, Vazao11, Vazao12,
           Vazao13, Vazao14, Vazao15, Vazao16, Vazao17, Vazao18,
           Vazao19, Vazao20, Vazao21, Vazao22, Vazao23, Vazao24,
           Vazao25, Vazao26, Vazao27, Vazao28, Vazao29, Vazao30,
           Vazao31)

# transforma Data em tipo date    
  df_vazao_diaria$Data <- format(as.Date(df_vazao_diaria$Data, origin="1970-01-01"),
                                '%Y-%m-%d %H:%M:%S')


  
# seleciona as colunas da tabela 'Estacao' sem definir os tipos das colunas
  tb_estacao_hidro <- sqlFetch(canal, "Estacao", as.is = TRUE)  

# tipos da tabela 'Estacao' importados no R  
  str(tb_estacao_hidro)
  
# seleciona as variáveis de interesse tabela 'Estacao'   
  df_estacao <- tb_estacao_hidro %>%
    select(RegistroID, RioCodigo, EstadoCodigo, MunicipioCodigo, 
           Codigo, Nome, PeriodoEscalaInicio, UltimaAtualizacao)
  
# seleciona registros pchs na tabela 'Estacao'
  df_estacao_pch <- df_estacao[grep('PCH', df_estacao$Nome), ]
  

# seleciona vazoes das pchs na tabela 'Vazoes'
  df_vazao_diaria_pch <- data.frame()
  
  for (codigo in df_estacao_pch$Codigo){
    
    vazao_estacao <-
      df_vazao_diaria %>%
        subset(EstacaoCodigo == codigo)
    
    estacao <- 
      df_estacao_pch %>%
        filter(Codigo == codigo)
    
    vazao_estacao <- 
      vazao_estacao %>% 
        mutate(nome_pch = estacao$Nome)
    
    df_vazao_diaria_pch <- rbind(df_vazao_diaria_pch, 
                                 vazao_estacao)
  }
        

# filtra vazão diárias de pchs com mais de 30 anos de registros horários
  df_vazao_diaria_pch30 <- 
    df_vazao_diaria_pch %>% 
      filter(year(Data) <= '1989')
  
  
# ordena pela data de início dos registros
  df_estacao_pch <- df_vazao_diaria_pch30[ order(df_vazao_diaria_pch30$Data), ]
  
  head(df_vazao_diaria_pch30,30)
  tail(df_vazao_diaria_pch30$Data)