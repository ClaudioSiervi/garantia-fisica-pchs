

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
  q_diaria <- tb_vazao_diaria %>% 
    select(RegistroID, Data, EstacaoCodigo,
           Vazao01, Vazao02, Vazao03, Vazao04, Vazao05, Vazao06,
           Vazao07, Vazao08, Vazao09, Vazao10, Vazao11, Vazao12,
           Vazao13, Vazao14, Vazao15, Vazao16, Vazao17, Vazao18,
           Vazao19, Vazao20, Vazao21, Vazao22, Vazao23, Vazao24,
           Vazao25, Vazao26, Vazao27, Vazao28, Vazao29, Vazao30,
           Vazao31)

  names(q_diaria)[1:3] <- c('id', 'mes_ano', 'estacao_id')
    
  
# transforma mesAno em tipo date    
  q_diaria$mes_ano <- format(as.Date(q_diaria$mes_ano, origin="1970-01-01"),
                                '%Y-%m-%d %H:%M:%S')

  
# filtra pchs com mais de 30 anos de registros diários
  q_diaria_30anos <- 
    q_diaria %>% 
      filter(year(mes_ano) <= '1987')
  
  q_diaria_30anos <- q_diaria[which(q_diaria_30anos$estacao_id %in% q_diaria$estacao_id),]
  
# ordena pela data de início dos registros
  q_diaria_30anos <- q_diaria_30anos[ order(q_diaria_30anos$mes_ano), ]
  
  tail(q_diaria_30anos,30)


# transforma dados colunares em linha
  #https://uc-r.github.io/tidyr
  q_diaria_30anos_long <- 
    q_diaria_30anos %>% 
      gather(dia, vazao, Vazao01:Vazao31)
  
  head(q_diaria_30anos_long)

# extrai os números referentes aos dias de cada registro  
  q_diaria_30anos_long$dia <- str_sub(q_diaria_30anos_long$dia, start= -2)
  
# converte em inteiro
  q_diaria_30anos_long$dia <- strtoi(q_diaria_30anos_long$dia)

# campo dia_mes_ano  
  q_diaria_30anos_long <- 
    q_diaria_30anos_long %>%
      mutate(data = paste0(dia, '/', month(mes_ano), '/', year(mes_ano)))
  
  
# transforma data em tipo date    
  q_diaria_30anos_long$data <- as.Date(q_diaria_30anos_long$data,format='%d/%m/%Y')

  str(q_diaria_30anos_long)
  
  
  #este <- q_diaria_30anos_long %>% filter(estacao_id==64715001
    
  ggplot(data=na.omit(q_diaria_30anos_long)) +
    geom_line(aes(x=data, y=vazao, colour =as.character(estacao_id)))
  
  ggsave("estacoes_com_historico_maior_que_30_anos.png")
 