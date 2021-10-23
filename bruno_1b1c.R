#Instalando o DemoTools
library(usethis)
library(devtools)
install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
install_github("timriffe/DemoTools")

#Carregando o DemoTools
library(Rcpp)
library(DemoTools)

#Carregando demais pacotes

pacman::p_load(dplyr, stringr,foreign,tidyverse,ggplot2,factoextra,readxl,readODS)

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
#
#c) Avalie a qualidade da declaração de idade no Censo 2000 segundo forma de declaração (data de nascimento e idade presumida). 
#   Calcule os índices de Whipple, Myers e Bachi. Construa a pirâmide por idade simples. Comente os resultados. 
#   (utilize a planilha SINGAGE do PAS - disponível na plataforma)
#
#

#(...)