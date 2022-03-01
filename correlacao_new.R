library(data.table)
library(ggplot2)
library(scales)
library(stringr)
library(lubridate)
library(dplyr)
library(plotly)
library(fBasics)

indicadores <- fread('/home/pedro/Documentos/Inep/Novos_Indicadores/Permanencia_Docente/Dados/INDICADORES_ENADE(2).txt')
dados <- fread('dados.txt')
dados_cursos <- dados[, list(N_DOCENTE = .N, MEDIA_PERMANENCIA = mean(TEMPO_PERMANENCIA),
                             MEDIANA_ENADE = median(TEMPO_PERMANENCIA)),
                      by = list(NO_CINE_ROTULO, CO_CURSO, DS_MODALIDADE, DS_ORG_ACADEMICA,
                                DS_CATEGORIA_ADM_PP, TIPO, QT_MATRICULA_TOTAL, DT_INICIO_FUNCIONAMENTO)]



temp <- merge(dados_cursos, indicadores, by = 'CO_CURSO')
temp <- temp[order(NU_ANO_EDICAO, decreasing = T)]
temp <- temp[!duplicated(temp[,CO_CURSO])]
temp$DT_INICIO_FUNCIONAMENTO <- dmy_hms(temp$DT_INICIO_FUNCIONAMENTO)
temp <- temp[, IDADE_CURSO := 2020 - year(DT_INICIO_FUNCIONAMENTO)]
temp <- temp[, FAIXA_IDADE := cut(temp2$IDADE_CURSO, breaks = c(2, 6, 10, 14, Inf), include.lowest = T)]
temp[, NIVEL_ACADEMICO := ifelse(str_detect(NO_AREA, 'LICENCIATURA'), yes = 'Licenciatura',
                                 no = ifelse(str_detect(NO_AREA, 'TECNOLOGIA'), yes = 'Tecnologico',
                                             no = 'Bacharelado'))]



# Geral
areas <- unique(temp$NO_AREA)

NO_AREA <- vector()
GRAUS_LIBERDADE <- vector()
LIMITE_INF <- vector()
LIMITE_SUP <- vector()
CORRELACAO <- vector()
P_VALOR <- vector()
resultado <- data.frame(NO_AREA, GRAUS_LIBERDADE, LIMITE_INF, LIMITE_SUP, CORRELACAO, P_VALOR)

for (x in 1:length(areas)) {
  temp3 <- temp[NO_AREA == areas[x]]
  teste_cor <- cor.test(temp3$MEDIA_PERMANENCIA, temp3$VL_NOTA_BRUTA)
  resultado[x, 'NO_AREA'] <- areas[x]   #Nome da area
  resultado[x, 'GRAUS_LIBERDADE'] <- teste_cor$parameter    #graus de liberdade
  resultado[x, 'LIMITE_INF'] <- teste_cor$conf.int[1]  #limite inferior
  resultado[x, 'LIMITE_SUP'] <- teste_cor$conf.int[2]  #limete superior
  resultado[x, 'CORRELACAO'] <- teste_cor$estimate     #correlação
  resultado[x, 'P_VALOR'] <- teste_cor$p.value      #p-valor
}



##### Modalidade de Ensino: #####
## Presencial
areas_presencial <- unique(temp[DS_MODALIDADE.x == 'Educação Presencial', NO_AREA])

NO_AREA <- vector()
N <- vector()
CORRELACAO <- vector()
P_VALOR <- vector()
resultado_presencial <- data.frame(NO_AREA, N, CORRELACAO, P_VALOR)

for (x in 1:length(areas_presencial)) {
  temp3 <- temp[NO_AREA == areas_presencial[x] & DS_MODALIDADE.x == 'Educação Presencial']
  teste_cor <- correlationTest(temp3$MEDIA_PERMANENCIA, temp3$VL_NOTA_BRUTA, method = 'spearman')
  resultado_presencial[x, 'NO_AREA'] <- areas[x]   #Nome da area
  resultado_presencial[x, 'N'] <- nrow(temp3)
  resultado_presencial[x, 'CORRELACAO'] <- round(teste_cor@test$estimate, digits = 3)     #correlação
  resultado_presencial[x, 'P_VALOR'] <- pvalue(teste_cor@test$p.value[3], accuracy = 0.01)     #p-valor greater
}


## EaD
areas_ead <- unique(temp[DS_MODALIDADE.x == 'Educação a Distância', NO_AREA])

NO_AREA <- vector()
N <- vector()
CORRELACAO <- vector()
P_VALOR <- vector()
resultado_ead <- data.frame(NO_AREA, N, CORRELACAO, P_VALOR)

for (x in 1:length(areas_ead)) {
  temp3 <- temp[NO_AREA == areas_ead[x] & DS_MODALIDADE.x == 'Educação a Distância']
  if(dim(temp3)[1] <= 3){
      resultado_ead[x,] <- c(areas[x], nrow(temp3), NA, NA)
  } else{
      teste_cor <- correlationTest(temp3$MEDIA_PERMANENCIA, temp3$VL_NOTA_BRUTA, method = 'spearman')
      resultado_ead[x, 'NO_AREA'] <- areas[x]   #Nome da area
      resultado_ead[x, 'N'] <- nrow(temp3)   #quantidade de cursos
      resultado_ead[x, 'CORRELACAO'] <- round(teste_cor@test$estimate, digits = 3)     #correlação
      resultado_ead[x, 'P_VALOR'] <- pvalue(teste_cor@test$p.value[3], accuracy = 0.01)      #p-valor greater
  }
    
}


#### NIVEL ACADEMICO ####
## Bacharelado
areas_bacharel <- unique(temp[NIVEL_ACADEMICO == 'Bacharelado', NO_AREA])

NO_AREA <- vector()
N <- vector()
CORRELACAO <- vector()
P_VALOR <- vector()
resultado_bacharelado <- data.frame(NO_AREA, N, CORRELACAO, P_VALOR)

for (x in 1:length(areas_bacharel)) {
  temp3 <- temp[NO_AREA == areas_bacharel[x] & NIVEL_ACADEMICO == 'Bacharelado']
  if(dim(temp3)[1] <= 3){
    resultado_bacharelado[x,] <- c(areas_bacharel[x], nrow(temp3), NA, NA)
  }
  else{
    teste_cor <- correlationTest(temp3$MEDIA_PERMANENCIA, temp3$VL_NOTA_BRUTA, method = 'spearman')
    resultado_bacharelado[x, 'NO_AREA'] <- areas_bacharel[x]   #Nome da area
    resultado_bacharelado[x, 'N'] <- nrow(temp3)   #Nome da area
    resultado_bacharelado[x, 'CORRELACAO'] <- round(teste_cor@test$estimate, digits = 3)     #correlação
    resultado_bacharelado[x, 'P_VALOR'] <- pvalue(teste_cor@test$p.value[3], accuracy = 0.01)  #p-valor greater
  }
  
}


## Licenciatura
areas_licenciatura <- unique(temp[NIVEL_ACADEMICO == 'Licenciatura', NO_AREA])

NO_AREA <- vector()
N <- vector()
CORRELACAO <- vector()
P_VALOR <- vector()
resultado_licenciatura <- data.frame(NO_AREA, N, CORRELACAO, P_VALOR)

for (x in 1:length(areas_licenciatura)) {
  temp3 <- temp[NO_AREA == areas_licenciatura[x] & NIVEL_ACADEMICO == 'Licenciatura']
  if(dim(temp3)[1] <= 3){
    resultado_licenciatura[x,] <- c(areas_licenciatura[x], nrow(temp3), NA, NA)
  }
  else{
    teste_cor <- correlationTest(temp3$MEDIA_PERMANENCIA, temp3$VL_NOTA_BRUTA, method = 'spearman')
    resultado_licenciatura[x, 'NO_AREA'] <- areas_licenciatura[x]   #Nome da area
    resultado_licenciatura[x, 'N'] <- nrow(temp3)   # número de cursos da area
    resultado_licenciatura[x, 'CORRELACAO'] <- round(teste_cor@test$estimate, digits = 3)     #correlação
    resultado_licenciatura[x, 'P_VALOR'] <- pvalue(teste_cor@test$p.value[3], accuracy = 0.01)      #p-valor
  }
  
}


## Tecnologico
areas_tecnologico <- unique(temp[NIVEL_ACADEMICO == 'Tecnologico', NO_AREA])

NO_AREA <- vector()
N <- vector()
CORRELACAO <- vector()
P_VALOR <- vector()
resultado_tecnologico <- data.frame(NO_AREA, N, CORRELACAO, P_VALOR)

for (x in 1:length(areas_tecnologico)) {
  temp3 <- temp[NO_AREA == areas_tecnologico[x] & NIVEL_ACADEMICO == 'Tecnologico']
  if(dim(temp3)[1] <= 3){
    resultado_tecnologico[x,] <- c(areas_tecnologico[x], nrow(temp3), NA, NA)
  }
  else{
    teste_cor <- correlationTest(temp3$MEDIA_PERMANENCIA, temp3$VL_NOTA_BRUTA, method = 'spearman')
    resultado_tecnologico[x, 'NO_AREA'] <- areas_tecnologico[x]   #Nome da area
    resultado_tecnologico[x, 'N'] <- nrow(temp3)   # número de curso da area
    resultado_tecnologico[x, 'CORRELACAO'] <- round(teste_cor@test$estimate, digits = 3)     #correlação
    resultado_tecnologico[x, 'P_VALOR'] <- pvalue(teste_cor@test$p.value[3], accuracy = 0.01)      #p-valor greater
  }
}


#### Organizacao Academica ####

## Universidade
areas_universidade <- unique(temp[DS_ORG_ACADEMICA == 'Universidade', NO_AREA])

NO_AREA <- vector()
N <- vector()
CORRELACAO <- vector()
P_VALOR <- vector()
resultado_universidade <- data.frame(NO_AREA, N, CORRELACAO, P_VALOR)

for (x in 1:length(areas_universidade)) {
  temp3 <- temp[NO_AREA == areas_universidade[x] & DS_ORG_ACADEMICA == 'Universidade']
  if(dim(temp3)[1] <= 3){
    resultado_universidade[x,] <- c(areas_universidade[x], nrow(temp3), NA, NA)
  }
  else{
    teste_cor <- correlationTest(temp3$MEDIA_PERMANENCIA, temp3$VL_NOTA_BRUTA, method = 'spearman')
    resultado_universidade[x, 'NO_AREA'] <- areas_universidade[x]   #Nome da area
    resultado_universidade[x, 'N'] <- nrow(temp3)   # número de cursos da area
    resultado_universidade[x, 'CORRELACAO'] <- round(teste_cor@test$estimate, digits = 3)     #correlação
    resultado_universidade[x, 'P_VALOR'] <- pvalue(teste_cor@test$p.value[3], accuracy = 0.01)     #p-valor
  }
}


## Faculdade
areas_faculdade <- unique(temp[DS_ORG_ACADEMICA == 'Faculdade', NO_AREA])

NO_AREA <- vector()
N <- vector()
CORRELACAO <- vector()
P_VALOR <- vector()
resultado_faculdade <- data.frame(NO_AREA, N, CORRELACAO, P_VALOR)

for (x in 1:length(areas_faculdade)) {
  temp3 <- temp[NO_AREA == areas_faculdade[x] & DS_ORG_ACADEMICA == 'Faculdade']
  if(dim(temp3)[1] <= 3){
    resultado_faculdade[x,] <- c(areas_faculdade[x], nrow(temp3), NA, NA)
  }
  else{
    teste_cor <- correlationTest(temp3$MEDIA_PERMANENCIA, temp3$VL_NOTA_BRUTA, method = 'spearman')
    resultado_faculdade[x, 'NO_AREA'] <- areas_faculdade[x]   #Nome da area
    resultado_faculdade[x, 'N'] <- nrow(temp3)   # número de cursos da area
    resultado_faculdade[x, 'CORRELACAO'] <- round(teste_cor@test$estimate, digits = 3)    #correlação
    resultado_faculdade[x, 'P_VALOR'] <- pvalue(teste_cor@test$p.value[3], accuracy = 0.01)     #p-valor
  }
}


## Centro Universitario
areas_cuniversitario <- unique(temp[DS_ORG_ACADEMICA == 'Centro Universitário', NO_AREA])

NO_AREA <- vector()
N <- vector()
CORRELACAO <- vector()
P_VALOR <- vector()
resultado_cuniversitario <- data.frame(NO_AREA, N, CORRELACAO, P_VALOR)

for (x in 1:length(areas_cuniversitario)) {
  temp3 <- temp[NO_AREA == areas_cuniversitario[x] & DS_ORG_ACADEMICA == 'Centro Universitário']
  if(dim(temp3)[1] <= 3){
    resultado_cuniversitario[x,] <- c(areas_cuniversitario[x], nrow(temp3), NA, NA)
  }
  else{
    teste_cor <- correlationTest(temp3$MEDIA_PERMANENCIA, temp3$VL_NOTA_BRUTA, method = 'spearman')
    resultado_cuniversitario[x, 'NO_AREA'] <- areas_cuniversitario[x]   #Nome da area
    resultado_cuniversitario[x, 'N'] <- nrow(temp3)   #número de cursos da area
    resultado_cuniversitario[x, 'CORRELACAO'] <- round(teste_cor@test$estimate, digits = 3)     #correlação
    resultado_cuniversitario[x, 'P_VALOR'] <- pvalue(teste_cor@test$p.value[3], accuracy = 0.01)      #p-valor greater
  }
}


## IF/CEFET
areas_ifcefet <- unique(temp[DS_ORG_ACADEMICA == 'IF/CEFET', NO_AREA])

NO_AREA <- vector()
N <- vector()
CORRELACAO <- vector()
P_VALOR <- vector()
resultado_ifcefet <- data.frame(NO_AREA, N, CORRELACAO, P_VALOR)

for (x in 1:length(areas_ifcefet)) {
  temp3 <- temp[NO_AREA == areas_ifcefet[x] & DS_ORG_ACADEMICA == 'IF/CEFET']
  if(dim(temp3)[1] <= 3){
    resultado_ifcefet[x,] <- c(areas_ifcefet[x], nrow(temp3), NA, NA)
  }
  else{
    teste_cor <- correlationTest(temp3$MEDIA_PERMANENCIA, temp3$VL_NOTA_BRUTA, method = 'spearman')
    resultado_ifcefet[x, 'NO_AREA'] <- areas_ifcefet[x]   #Nome da area
    resultado_ifcefet[x, 'N'] <- nrow(temp3)   #número de cursos da area
    resultado_ifcefet[x, 'CORRELACAO'] <- round(teste_cor@test$estimate, digits = 3)     #correlação
    resultado_ifcefet[x, 'P_VALOR'] <- pvalue(teste_cor@test$p.value[3], accuracy = 0.01)      #p-valor Greater
  }
}



#### Categoria Administrativa ####
## Publico
areas_publica <- unique(temp[DS_CATEGORIA_ADM_PP == 'Pública', NO_AREA])

NO_AREA <- vector()
N <- vector()
CORRELACAO <- vector()
P_VALOR <- vector()
resultado_publica <- data.frame(NO_AREA, N, CORRELACAO, P_VALOR)

for (x in 1:length(areas_publica)) {
  temp3 <- temp[NO_AREA == areas_publica[x] & DS_CATEGORIA_ADM_PP == 'Pública']
  if(dim(temp3)[1] <= 3){
    resultado_publica[x,] <- c(areas_publica[x], nrow(temp3), NA, NA)
  }
  else{
    teste_cor <- correlationTest(temp3$MEDIA_PERMANENCIA, temp3$VL_NOTA_BRUTA, method = 'spearman')
    resultado_publica[x, 'NO_AREA'] <- areas_publica[x]   #Nome da area
    resultado_publica[x, 'N'] <- nrow(temp3)   #número de cursos da area
    resultado_publica[x, 'CORRELACAO'] <- round(teste_cor@test$estimate, digits = 3)     #correlação
    resultado_publica[x, 'P_VALOR'] <- pvalue(teste_cor@test$p.value[3], accuracy = 0.01)     #p-valor greater
  }
}


## Privada
areas_privada <- unique(temp[DS_CATEGORIA_ADM_PP == 'Privada', NO_AREA])

NO_AREA <- vector()
N <- vector()
CORRELACAO <- vector()
P_VALOR <- vector()
resultado_privada <- data.frame(NO_AREA, N, CORRELACAO, P_VALOR)

for (x in 1:length(areas_privada)) {
  temp3 <- temp[NO_AREA == areas_privada[x] & DS_CATEGORIA_ADM_PP == 'Privada']
  if(dim(temp3)[1] <= 3){
    resultado_privada[x,] <- c(areas_privada[x], nrow(temp3), NA, NA)
  }
  else{
    teste_cor <- correlationTest(temp3$MEDIA_PERMANENCIA, temp3$VL_NOTA_BRUTA, method = 'spearman')
    resultado_privada[x, 'NO_AREA'] <- areas_privada[x]   #Nome da area
    resultado_privada[x, 'N'] <- nrow(temp3)   #número de cursos da area
    resultado_privada[x, 'CORRELACAO'] <- round(teste_cor@test$estimate, digits = 3)     #correlação
    resultado_privada[x, 'P_VALOR'] <- pvalue(teste_cor@test$p.value[3], accuracy = 0.01)      #p-valor greater
  }
}

write.csv2(resultado_presencial, 'correlacao_presencial.csv')
write.csv2(resultado_ead, 'correlacao_ead.csv')
write.csv2(resultado_bacharelado, 'correlacao_bacharelado.csv')
write.csv2(resultado_licenciatura, 'correlacao_licenciatura.csv')
write.csv2(resultado_tecnologico, 'correlacao_tecnologico.csv')
write.csv2(resultado_universidade, 'correlacao_universidade.csv')
write.csv2(resultado_faculdade, 'correlacao_faculdade.csv')
write.csv2(resultado_cuniversitario, 'correlacao_cuniversitario.csv')
write.csv2(resultado_ifcefet, 'correlacao_ifcefet.csv')
write.csv2(resultado_publica, 'correlacao_publica.csv')
write.csv2(resultado_privada, 'correlacao_privada.csv')