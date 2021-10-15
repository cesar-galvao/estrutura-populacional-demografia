#Instalando o DemoTools
library(usethis)
library(devtools)
install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
install_github("timriffe/DemoTools")

#Carregando o DemoTools
library(Rcpp)
library(DemoTools)

#Carregando demais pacotes

pacman::p_load(dplyr, stringr,foreign,tidyverse,ggplot2,factoextra)

#Carregando os bancos

#(...)

#Preparando os bancos

#(...)

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