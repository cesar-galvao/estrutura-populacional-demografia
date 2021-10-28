#Instalando o DemoTools
library(usethis)
library(devtools)
install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
install_github("timriffe/DemoTools")

#Carregando o DemoTools
library(Rcpp)
library(DemoTools)

#Carregando demais pacotes

pacman::p_load(dplyr, stringr,foreign,tidyverse,ggplot2,factoextra,readxl,readODS,reshape2,XML,plyr,plotrix)

#Carregando os bancos

pop2000 <- read_excel("pop2000.xltx")
pop2010 <- read_excel("pop2010.xltx")
pop2015 <- read_excel("pop2015.xltx")
pop2020 <- read_excel("pop2020.xltx")
pop2030 <- read_excel("pop2030.xltx")
#Idade declarada
declarada <- read_ods("total.ods")
#Idade presumida
presumida <- read_ods("totalpres.ods")
#Idade pela data de nascimento
datanasc  <- read_ods("totalnas.ods")

#Preparando os bancos

pop2015 <- na.omit(pop2015)
pop2020 <- na.omit(pop2020)
pop2030 <- na.omit(pop2030)

#

rownames(declarada) <- declarada$declarada
declarada$declarada <- NULL

rownames(presumida) <- presumida$presumida
presumida$presumida <- NULL

rownames(datanasc) <- datanasc$Dtnasc
datanasc$Dtnasc <- NULL


#Exercícios:
#1) 
#b) Para todos os anos acima mencionados[2000, 2010, 2015, 2020 e 2030], calcule os indicadores de estrutura por idade (proporção de idosos 
#   (60 anos e mais),proporção de crianças (0 a 4 anos), proporção de jovens (0 a 14 anos), razão de dependência e índice de envelhecimento). 
#   Calcule a idade média e a idade mediana. Calcule e grafique a razão de sexo por grupos de idade para 2010 e 2030. Comente os resultados.

#Ordem:  2000, 2015,  2020, 2010, 2030 (Fiz nessa ordem pois pede-se mais coisas para 2010 e 2030)

##################################################### 2000 ####################################################
idosos2000 <- pop2000[13,4] + pop2000[14,4] + pop2000[15,4] + pop2000[16,4] + pop2000[17,4]
propidosos2000 <- idosos2000/pop2000[18,4]
#Proporção de idosos: 0.05453378

propcri2000 <- pop2000[1,4]/pop2000[18,4]
#Proporção de crianças: 0.1382662

propjovem2000 <- (pop2000[1,4] + pop2000[2,4] + pop2000[3,4])/pop2000[18,4]
#Proporção de jovens: 0.3882348

inativo2000 <- pop2000[1,4] + pop2000[2,4] + pop2000[3,4] + pop2000[14,4] + pop2000[15,4] + pop2000[16,4] + pop2000[17,4]
ativo2000 <- pop2000[4,4] + pop2000[5,4] + pop2000[6,4] + pop2000[7,4] + pop2000[8,4] + pop2000[9,4] + pop2000[10,4] + pop2000[11,4] + pop2000[12,4] +
  pop2000[13,4]

RDT2000 <- inativo2000/ativo2000
#Razão de dependência: 0.7404956

IE2000 <- propidosos2000/propjovem2000
#Índice de envelhecimento: 0.140466

teste2000 <- pop2000$Total
teste2000  <- teste2000[1:17]
teste2000 <- teste2000/pop2000[18,4]
teste2000

teste2000[1] + teste2000[2] + teste2000[3] + teste2000[4]
#0.5081539
#Idade mediana: 15-19 anos

#Idade média: 
pop2000
idademedia <- c(2,7,12,17,22,27,32,37,42,47,52,57,62,67,72,77,80,1)
pop2000$media <- idademedia
pop2000$cmedia <- pop2000$Total * pop2000$media
pop2000
sum(pop2000$cmedia[1:17])
media2000 <- sum(pop2000$cmedia[1:17]) / 557526
media2000
#Idade média: 23.44234
#
#
#
#
#
##################################################### 2015 ####################################################
idosos2015 <- pop2015[13,4] + pop2015[14,4] + pop2015[15,4] + pop2015[16,4] + pop2015[17,4]
propidosos2015 <- idosos2015/pop2015[18,4]
propidosos2015
#Proporção de idosos: 0.06498713

propcri2015 <- pop2015[1,4]/pop2015[18,4]
propcri2015
#Proporção de crianças: 0.1068421

propjovem2015 <- (pop2015[1,4] + pop2015[2,4] + pop2015[3,4])/pop2015[18,4]
propjovem2015
#Proporção de jovens: 0.3279312

inativo2015 <- pop2015[1,4] + pop2015[2,4] + pop2015[3,4] + pop2015[14,4] + pop2015[15,4] + pop2015[16,4] + pop2015[17,4]
ativo2015 <- pop2015[4,4] + pop2015[5,4] + pop2015[6,4] + pop2015[7,4] + pop2015[8,4] + pop2015[9,4] + pop2015[10,4] + pop2015[11,4] + pop2015[12,4] +
  pop2015[13,4]

RDT2015 <- inativo2015/ativo2015
RDT2015
#Razão de dependência: 0.5880959

IE2015 <- propidosos2015/propjovem2015
IE2015
#Índice de envelhecimento: 0.198173

teste2015 <- pop2015$Total
teste2015  <- teste2015[1:17]
teste2015 <- teste2015/pop2015[18,4]
teste2015

teste2015[1] + teste2015[2] + teste2015[3] + teste2015[4]+ teste2015[5]
#0.5257177
#Idade mediana: 20-24 anos

#Idade média: 
pop2015
idademedia
pop2015$media <- idademedia
pop2015$cmedia <- pop2015$Total * pop2015$media
pop2015
media2015 <- sum(pop2015$cmedia[1:17]) / pop2015$Total[18]
media2015
#Idade média: 26.42038

#
#
#
#
#
#################################################### 2020 #####################################################
idosos2020 <- pop2020[13,4] + pop2020[14,4] + pop2020[15,4] + pop2020[16,4] + pop2020[17,4]
propidosos2020 <- idosos2020/pop2020[18,4]
propidosos2020
#Proporção de idosos: 0.07609963

propcri2020 <- pop2020[1,4]/pop2020[18,4]
propcri2020
#Proporção de crianças: 0.09396051

propjovem2020 <- (pop2020[1,4] + pop2020[2,4] + pop2020[3,4])/pop2020[18,4]
propjovem2020
#Proporção de jovens: 0.2943225

inativo2020 <- pop2020[1,4] + pop2020[2,4] + pop2020[3,4] + pop2020[14,4] + pop2020[15,4] + pop2020[16,4] + pop2020[17,4]
ativo2020 <- pop2020[4,4] + pop2020[5,4] + pop2020[6,4] + pop2020[7,4] + pop2020[8,4] + pop2020[9,4] + pop2020[10,4] + pop2020[11,4] + pop2020[12,4] +
  pop2020[13,4]

RDT2020 <- inativo2020/ativo2020
RDT2020
#Razão de dependência: 0.5251386

IE2020 <- propidosos2020/propjovem2020
IE2020
#Índice de envelhecimento: 0.2585587

teste2020 <- pop2020$Total
teste2020  <- teste2020[1:17]
teste2020 <- teste2020/pop2020[18,4]
teste2020

teste2020[1] + teste2020[2] + teste2020[3] + teste2020[4]+ teste2020[5]+ teste2020[6]
#0.5257177
#Idade mediana: 25-29 anos

#Idade média: 
pop2020
idademedia
pop2020$media <- idademedia
pop2020$cmedia <- pop2020$Total * pop2020$media
pop2020
media2020 <- sum(pop2020$cmedia[1:17]) / pop2020$Total[18]
media2020
#Idade média: 28.03636

#
#
#
#
#
#################################################### 2010 #####################################################
idosos2010 <- pop2010[13,4] + pop2010[14,4] + pop2010[15,4] + pop2010[16,4] + pop2010[17,4]
propidosos2010 <- idosos2010/pop2010[18,4]
propidosos2010
#Proporção de idosos: 0.06397031

propcri2010 <- pop2010[1,4]/pop2010[18,4]
propcri2010
#Proporção de crianças: 0.1054653

propjovem2010 <- (pop2010[1,4] + pop2010[2,4] + pop2010[3,4])/pop2010[18,4]
propjovem2010
#Proporção de jovens: 0.3370281

inativo2010 <- pop2010[1,4] + pop2010[2,4] + pop2010[3,4] + pop2010[14,4] + pop2010[15,4] + pop2010[16,4] + pop2010[17,4]
ativo2010 <- pop2010[4,4] + pop2010[5,4] + pop2010[6,4] + pop2010[7,4] + pop2010[8,4] + pop2010[9,4] + pop2010[10,4] + pop2010[11,4] + pop2010[12,4] +
  pop2010[13,4]

RDT2010 <- inativo2010/ativo2010
RDT2010
#Razão de dependência: 0.6135545

IE2010 <- propidosos2010/propjovem2010
IE2010
#Índice de envelhecimento: 0.1898071

teste2010 <- pop2010$Total
teste2010  <- teste2010[1:17]
teste2010 <- teste2010/pop2010[18,4]
teste2010

teste2010[1] + teste2010[2] + teste2010[3] + teste2010[4]+ teste2010[5]
#0.5378545
#Idade mediana: 20-24 anos

#Idade média: 
pop2010
idademedia
pop2010$media <- idademedia
pop2010$cmedia <- pop2010$Total * pop2010$media
pop2010
media2010 <- sum(pop2010$cmedia[1:17]) / pop2010$Total[18]
media2010
#Idade média: 25.95028

#Calcule e grafique a razão de sexo por grupos de idade para 2010 e 2030. Comente os resultados.

razao2010  <- pop2010
razao2010[,4] <- razao2010[,4]/pop2010[18,4]
razao2010[,3] <- razao2010[,3]/pop2010[18,3]
razao2010[,2] <- razao2010[,2]/pop2010[18,2]
razao2010

razao2010$razaomasc <- razao2010$Masculino/razao2010$Total
razao2010$razaofem <- razao2010$Feminino/razao2010$Total
razao2010

#gráfico de série
#adicionar ano, bindar com 2030, graficar com série, tirar razaofem

# A razão de população por sexo segue uma homogeneidade,  em nenhum momento a população total de homens ou de mulheres chega a  2%. Para as idades
# entre 0 a 4; 5 a  9;10 a 14; e depois 60 a 64; 70 a 74 e 75 a 79, a proporção de homens é ligeiramente maior. No restante, a de mulheres é ligeira-
# mente  superior.
#
#Não pensei em um bom gráfico para esta tabela, mas está pronta.
#
#
#
#
#
#################################################### 2030 #####################################################
idosos2030 <- pop2030[13,4] + pop2030[14,4] + pop2030[15,4] + pop2030[16,4] + pop2030[17,4]
propidosos2030 <- idosos2030/pop2030[18,4]
propidosos2030
#Proporção de idosos: 0.105681

propcri2030 <- pop2030[1,4]/pop2030[18,4]
propcri2030
#Proporção de crianças: 0.07418269

propjovem2030 <- (pop2030[1,4] + pop2030[2,4] + pop2030[3,4])/pop2030[18,4]
propjovem2030
#Proporção de jovens: 0.2356786

inativo2030 <- pop2030[1,4] + pop2030[2,4] + pop2030[3,4] + pop2030[14,4] + pop2030[15,4] + pop2030[16,4] + pop2030[17,4]
ativo2030 <- pop2030[4,4] + pop2030[5,4] + pop2030[6,4] + pop2030[7,4] + pop2030[8,4] + pop2030[9,4] + pop2030[10,4] + pop2030[11,4] + pop2030[12,4] +
  pop2030[13,4]

RDT2030 <- inativo2030/ativo2030
RDT2030
#Razão de dependência: 0.4432296

IE2030 <- propidosos2030/propjovem2030
IE2030
#Índice de envelhecimento: 0.4484116

teste2030 <- pop2030$Total
teste2030  <- teste2030[1:17]
teste2030 <- teste2030/pop2030[18,4]
teste2030

teste2030[1] + teste2030[2] + teste2030[3] + teste2030[4]+ teste2030[5]+ teste2030[6]
#0.5378545
#Idade mediana: 25-29 anos

#Idade média: 
pop2030
idademedia
pop2030$media <- idademedia
pop2030$cmedia <- pop2030$Total * pop2030$media
pop2030
media2030 <- sum(pop2030$cmedia[1:17]) / pop2030$Total[18]
media2030
#Idade média: 31.65193

#Calcule e grafique a razão de sexo por grupos de idade para 2010 e 2030. Comente os resultados.

razao2030  <- pop2030
razao2030[,4] <- razao2030[,4]/pop2030[18,4]
razao2030[,3] <- razao2030[,3]/pop2030[18,3]
razao2030[,2] <- razao2030[,2]/pop2030[18,2]
razao2030

razao2030$razaomasc <- razao2030$Masculino/razao2030$Total
razao2030$razaofem <- razao2030$Feminino/razao2030$Total
razao2030
# A razão de população por sexo segue uma homogeneidade, exceto para a idade de 80 anos ou mais em quem a população  feminina dispara 
# em  relação à masculina, no restante da tabela a proporção entre homens e mulheres não chega a 2%. 
# De 0 a 44 anos, a  proporção de homens é ligeiramente maior que a de mulheres. De 45  em  diante, a proporção de mulheres é constantemente
# e ligeiramente superior, exceto para as idades de 80 anos ou mais, quando a proporção de mulheres dispara em relação a população masculina,
# sendo quase 20% maior.
#
#Não pensei em um bom gráfico para esta tabela, mas está pronta.
#
#
#
#
#

#c) Avalie a qualidade da declaração de idade no Censo 2000 segundo forma de declaração (data de nascimento e idade presumida). 
#   Calcule os índices de Whipple, Myers e Bachi. Construa a pirâmide por idade simples. Comente os resultados. 
#   (utilize a planilha SINGAGE do PAS - disponível na plataforma)
#
##### Não vou utilizar a tabela  SINGAGE por empatia ao Cesar. Ao invés, utilizarei o pacote  DemoTools.
#
# Bancos: declarada,datanasc,presumida.
# Dados extraídos do censo de 2000 para o acre, para uma mesma população, segundo, na ordem; Idade declarada, Data de nascimento e Idade presumida.
#
declarada
#Total: declarada[,1]
presumida
#Total: presumida[,1]
datanasc
#Total: datanasc[,1]

idadedeclarada <- 0:(length(declarada[,1])-1)

plot(idadedeclarada, declarada[,1], type = 'o')  

# Whipple Index
# Using this assumption over a 5-year range, Whipple’s index measures heaping usually in numbers ending in zero or five. 
# This index is a summary measure that determines variability in the quality of age reporting between regions or countries and its evolution over time.
# Usually this index is calculated for digits 0 and 5 in adult ages 25 to 60. The uniformity assumption is less useful outside of this range.

#Declarada:
Wideclarada <-  check_heaping_whipple(declarada[,1], idadedeclarada, ageMin = 25, ageMax = 60, digit = c(0, 5))
Wideclarada
#  1.079055
#
#Presumida:
Wipresumida <-  check_heaping_whipple(presumida[,1], idadedeclarada, ageMin = 25, ageMax = 60, digit = c(0, 5))
Wipresumida
# 1.427128
#
#Data de nascimento:
Widatanasc <-  check_heaping_whipple(datanasc[,1], idadedeclarada, ageMin = 25, ageMax = 60, digit = c(0, 5))
Widatanasc
#1.063955

# The result varies from 1, which means no concentration around numbers ending in zero or five, to a maximum of 5 (or 10 if only a single digit is tested). 
# The selection of calculating the index for ages 25-60 is arbitrary but it has been found to be suitable for practical purposes (United Nations 1955)
#[Análise]





# Myers
# The method determines the proportion of the population whose age ends in each terminal digit (0-9), also varying the particular starting age for 
# any 10-year age group. It is based on the principle that in the absence of age heaping, the population aggregated on each terminal digits 0-9 
# should represent roughly 10 percent of the total population.

# Declarada:
Mideclarada <- check_heaping_myers(declarada[,1], idadedeclarada, ageMin = 20, ageMax = 90) 
Mideclarada
# 2.059559
#
# Presumida:
Mipresumida <- check_heaping_myers(presumida[,1], idadedeclarada, ageMin = 20, ageMax = 90) 
Mipresumida
# 10.88078
#
#Data de nascimento:
Midatanasc <- check_heaping_myers(datanasc[,1], idadedeclarada, ageMin = 20, ageMax = 90) 
Midatanasc
# 1.767307

# Myers index, 40.55 in this case, expresses the extent of concentration on or avoidance of a particular digit (Myers 1954). 
# The theoretical range of Myers index is 0, representing no heaping (perfect uniformity over digits), to 90, which would result if 
# all ages were reported at a single terminal digit (Siegel Jacob and Swanson David 2004).
# [Análise]





#Bachi
# With the previous formulas, Bachi’s index involves applying the Whipple method repeatedly to determine the extent of preference for each final digit.
# Similarly to Myers, it equals the sum of the positive deviations from 10 percent (Bachi 1951). It has a theoretical range from 0 to 90, 
# AND 10% is the expected value for each digit. Therefore, the results of Bachi’s method is similar to those obtained by Myers’ index.

# Declarada:
Bideclarada <- check_heaping_bachi(declarada[,1], idadedeclarada, ageMin = 20, ageMax = 90)
Bideclarada
# 1.966324
#
# Presumida:
Bipresumida <- check_heaping_bachi(presumida[,1], idadedeclarada, ageMin = 20, ageMax = 90)
Bipresumida
# 10.28615
#
#Data de nascimento:
Bidatanasc <- check_heaping_bachi(datanasc[,1], idadedeclarada, ageMin = 20, ageMax = 90)
Bidatanasc
#1.697535
#

#[Análise]






#Pirâmide;
#Modelo:
#por idade simples:
xy.pop2000<- declarada$Homens
xx.pop2000<- declarada$Mulheres
agelabels<-c("0","1","2","3","4","5","6",
             "7","8","9","10","11","12","13","14",
             "15","16","17","18","19","20","21","22",
             "23","24","25","26","27" , "28" , "29" , "30" ,
             "31" , "32" , "33",  "34"  ,"35" , "36" , "37" ,
             "38",  "39",  "40"  ,"41" , "42" , "43" , "44" ,
             "45" , "46" , "47",  "48"  ,"49" , "50" ,"51",  
             "52" , "53" , "54" , "55"  ,"56" , "57" , "58" ,
             "59" , "60" , "61" , "62",  "63",  "64" , "65",  "66",
             "67" , "68" , "69",  "70"  ,"71" , "72" , "73" , "74" , "75" ,
             "76" , "77" , "78" , "79" ,"80" , "81" , "82" , "83",  "84" ,
             "85" , "86"  ,"87" , "88",  "89",  "90" , "91", "92",  "93" , "94" ,
             "95",  "96" , "97" , "98" , "99"  ,"100+")
mcol<-color.gradient(c(0,0,0.5,1),c(0,0,0.5,1),c(1,1,0.5,1),18)
fcol<-color.gradient(c(1,1,0.5,1),c(0.5,0.5,0.5,1),c(0.5,0.5,0.5,1),18)

par(mar=pyramid.plot(xy.pop2000,xx.pop2000,labels=agelabels,
                     main="2000",lxcol=mcol,rxcol=fcol,
                     gap=0.5,show.values=TRUE,
                     top.labels =c("","","")))
# ???
#####################
#Bindando a tabua de 2010 com a de 2030 para graficar
razao2010
razao2030

bind1 <- razao2010
summary(bind1)
colnames(bind1) <- c("idade","masc","fem","total","razaomasc","razaofem")
bind1$ano <- "2010"
bind1$ano <- as.numeric(bind1$ano)

bind3 <- razao2030
summary(bind3)
colnames(bind3) <- c("idade","masc","fem","total","razaomasc","razaofem")
bind3$ano <- "2030"
bind3$ano <- as.numeric(bind3$ano)


bind <- bind_rows(bind1,bind3)
bind
bind$razaofem <- NULL
###############################################################################################################################################################
###############################################################################################################################################################
###############################################################################################################################################################
###############################################################################################################################################################
###############################################################################################################################################################

#Respostas:

#b) Para todos os anos acima mencionados, calcule os indicadores de estrutura por idade (proporção de idosos (60 anos e mais),
#proporção de crianças (0 a 4 anos), proporção de jovens (0 a 14 anos), razão de dependência e índice de envelhecimento).
#Calcule a idade média e a idade mediana. Calcule e grafique a razão de sexo por grupos de idade para 2010 e 2030. 
#Comente os resultados.

#2000:

#Proporção de idosos: 0.05453378
#Proporção de crianças: 0.1382662
#Proporção de jovens: 0.3882348
#Razão de dependência: 0.7404956
#Índice de envelhecimento: 0.140466
#Idade mediana: 17 anos
#Idade média: 23.44234


#2010:

#Proporção de idosos: 0.06397031
#Proporção de crianças: 0.1054653
#Proporção de jovens: 0.3370281
#Razão de dependência: 0.6135545
#Índice de envelhecimento: 0.1898071
#Idade mediana: 22 anos
#Idade média: 25.95028

#2015:

#Proporção de idosos: 0.06498713
#Proporção de crianças: 0.1068421
#Proporção de jovens: 0.3279312
#Razão de dependência: 0.5880959
#Índice de envelhecimento: 0.198173
#Idade mediana: 22 anos
#Idade média: 26.42038

#2020:

#Proporção de idosos: 0.07609963
#Proporção de crianças: 0.09396051
#Proporção de jovens: 0.2943225
#Razão de dependência: 0.5251386
#Índice de envelhecimento: 0.2585587
#Idade mediana: 27 anos
#Idade média: 28.03636

#2030:

#Proporção de idosos: 0.105681
#Proporção de crianças: 0.07418269
#Proporção de jovens: 0.2356786
#Razão de dependência: 0.4432296
#Índice de envelhecimento: 0.4484116
#Idade mediana: 27 anos
#Idade média: 31.65193

#Calcule e grafique a razão de sexo por grupos de idade para 2010 e 2030. :
#USAR A SEGUINTE TÁBUA[P/GRAFICAR]:

bind

# QUESTÃO C
# c) Avalie a qualidade da declaração de idade no Censo 2000 segundo forma de declaração (data de nascimento e idade presumida). 
# Calcule os índices de Whipple, Myers e Bachi. 
# Construa a pirâmide por idade simples. Comente os resultados. (utilize a planilha SINGAGE do PAS - disponível na plataforma)

#ÍNDICES SEGUNDO FORMA DE DECLARAÇÃO:
# Data de nascimento:

# índices de Myers: 1.767307
# índices de Whipple: 1.063955
# índices de Bachi: 1.697535

# Idade presumida

# índices de Myers: 10.88078
# índices de Whipple: 1.427128
# índices de Bachi: 10.28615

# Pirâmide (idade simples):

xy.pop2000<- declarada$Homens
xx.pop2000<- declarada$Mulheres
agelabels<-c("0","1","2","3","4","5","6",
             "7","8","9","10","11","12","13","14",
             "15","16","17","18","19","20","21","22",
             "23","24","25","26","27" , "28" , "29" , "30" ,
             "31" , "32" , "33",  "34"  ,"35" , "36" , "37" ,
             "38",  "39",  "40"  ,"41" , "42" , "43" , "44" ,
             "45" , "46" , "47",  "48"  ,"49" , "50" ,"51",  
             "52" , "53" , "54" , "55"  ,"56" , "57" , "58" ,
             "59" , "60" , "61" , "62",  "63",  "64" , "65",  "66",
             "67" , "68" , "69",  "70"  ,"71" , "72" , "73" , "74" , "75" ,
             "76" , "77" , "78" , "79" ,"80" , "81" , "82" , "83",  "84" ,
             "85" , "86"  ,"87" , "88",  "89",  "90" , "91", "92",  "93" , "94" ,
             "95",  "96" , "97" , "98" , "99"  ,"100+")
mcol<-color.gradient(c(0,0,0.5,1),c(0,0,0.5,1),c(1,1,0.5,1),18)
fcol<-color.gradient(c(1,1,0.5,1),c(0.5,0.5,0.5,1),c(0.5,0.5,0.5,1),18)

par(mar=pyramid.plot(xy.pop2000,xx.pop2000,labels=agelabels,
                     main="2000",lxcol=mcol,rxcol=fcol,
                     gap=0.5,show.values=TRUE,
                     top.labels =c("","","")))