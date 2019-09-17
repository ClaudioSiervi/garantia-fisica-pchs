

library(RODBC)
library(tidyverse)
require(lubridate)
require(Amelia)   # missing 

library(skimr) # show key descriptive stats for each column.
# https://www.machinelearningplus.com/machine-learning/caret-package/

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

# Fecha o canal de comunicação  
  odbcClose(canal)
  
  
  
  
### Análise das séries de vazão
  
  
  
  
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
  
# postos fluviométricos = 44923 postos
  total_postos <-
    q_diaria_30anos %>%
    as.tbl() %>%
    group_by(estacao_id) %>%
    dim(.)
  
  total_postos[1]
  
  
# transforma string mesAno em tipo date ano-mes   
  q_diaria$mes_ano <- format(as.Date(q_diaria$mes_ano, origin="1970-01-01"),
                                '%Y-%m-%d %H:%M:%S')

  tail(q_diaria)
  
  
# pchs com 30 anos ou mais registros diários= 14824 postos
  q_diaria_30anos <- 
    q_diaria %>% 
      as.tbl() %>%
        group_by(estacao_id) %>%
          filter(year(mes_ano) <= '1987')
  
    # q_diaria_30anos %>%
    #   summarise(
    #     min = min(year(mes_ano)),
    #     max = max(year(mes_ano))
    #     ) %>%
    #   tail(., 30)
    
    
# seleciona as pchs com pelo menos 30 anos de histórico
  q_diaria_30anos <- q_diaria[which(q_diaria_30anos$estacao_id %in% q_diaria$estacao_id),]
  

# ordena pelo data de início dos registros
  q_diaria_30anos <- q_diaria_30anos[ order(q_diaria_30anos$estacao_id, q_diaria_30anos$mes_ano, decreasing = F), ]
  

  
      
## ----- TRANSFORMAÇÃO DE DADOS


# transforma dados colunares em linha e cria um meta dado (dia da vazão)
  #https://uc-r.github.io/tidyr
  q_diaria_30anos_long <- 
    q_diaria_30anos %>% 
      gather(metadado_dia, vazao, Vazao01:Vazao31) %>%
        as_tibble(.)
  
  head(q_diaria_30anos_long)

  
  
# extrai os dias correspondentes a cada registro de vazão A PARTIR do meta dado nome das colunas de vazão
  q_diaria_30anos_long$dia <- 
    q_diaria_30anos_long$metadado_dia %>%
    str_sub(start= -2)
  
  head(q_diaria_30anos_long)
  tail(q_diaria_30anos_long)
  
  
  
# cria campo com a data completa DIA-MES-ANO (formato char)
  q_diaria_30anos_long <-
    q_diaria_30anos_long %>%
      mutate(data = paste0(dia, '-', month(mes_ano), '-', year(mes_ano)))
  
  # tipos
  str(q_diaria_30anos_long)
    
  
# transforma data em tipo date
# erro de conversão quando a coluna dos valores numéricos é NA
  # https://github.com/tidyverse/lubridate/issues/669
  
  # q_diaria_30anos_long$data2 <- as.Date(nq_diaria_30anos_long$data,format='%d-%m-%Y')
  q_diaria_30anos_long$data3 <- dmy(q_diaria_30anos_long$data)

  
# remove as observações do tipo NA.
  vazao_diaria <- na.omit(q_diaria_30anos_long)
  
  str(vazao_diaria)
  
  head(vazao_diaria)
  tail(vazao_diaria)

  
# número de estações com histórico de vazão de 30 anos ou maior = 69
  q_diaria_30anos_long %>%
      group_by(estacao_id) %>% 
        summarise() %>%
          count()
  



##--------------- Análise de falhas nas séries de vazão

### AMELIA
  # 
  # ??Amelia
  # 
  # 
  # 
  # a79<-amelia(dt79,m=5,idvars=c("id","caseid"))
  # missmap(d22[,5:39], 35)
  # 
  # data(africa)
  # a.out <- amelia(x = africa, cs = "country", ts = "year", logs = "gdp_pc")
  # summary(a.out)
  # plot(a.out)
  # 
  # 
  

  
  ###  Gráfico de todas as séries pré-selecionadas
  
  # for (id in estacoes_id$estacao_id[1:69]) {
  # 
  #  print(id)
  #   
  #  dados <-
  #    q_diaria_30anos_long[which(q_diaria_30anos_long$estacao_id %in% id), ]
  # 
  #  print(summary(dados))
  # 
  #  g <- ggplot(data=na.omit(dados))+
  #    geom_bar(aes(x=data,
  #                 y=vazao,
  #                 colour = estacao_id),
  #             stat="identity") +
  #    labs(colour = "estação")
  #  
  #  ggsave(g, file = paste0("estacao_", id,"vazao_diaria.png"))
  #  
  # }
 