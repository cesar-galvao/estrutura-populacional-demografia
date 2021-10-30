pacman::p_load(dplyr, magrittr, ggpol, ggplot2, readxl, stringr, DT, htmltools)

tabua_fem <- read.csv("./data/tabua_fem.csv")
tabua_masc <- read.csv("./data/tabua_masc.csv")

calcula_npx <- function(nlx, cont = 1){

  if (cont == 18){
    return(c((nlx[cont]+nlx[cont+1]+nlx[cont+2])/(nlx[cont-1]+nlx[cont]+nlx[cont+1]+nlx[cont+2]), NA, NA))
  }
  else if (cont == 1){
    return(c(NA,
           calcula_npx(nlx, cont = cont+1)))
  }
  else if (cont == 2){
    return(c(1,
           calcula_npx(nlx, cont = cont+1)))
  }
  else if (cont == 3){
    return(c(nlx[cont]/(nlx[1]+nlx[2]),
           calcula_npx(nlx, cont = cont+1)))
  }
  else {
    return(c(nlx[cont]/nlx[cont-1],
           calcula_npx(nlx, cont = cont+1)))
  }
  }
tabua_fem$nPx <- calcula_npx(tabua_fem$nLx)
tabua_masc$nPx <- calcula_npx(tabua_masc$nLx)

fecundidade <- data.frame(grupo = c("15-19", "20-24", "25-29",
                                    "30-34", "35-39", "40-44",
                                    "45-49"),
                          nfx = c(0.07661, 0.06105, 0.04105,
                                    0.01197, 0.00108, 0.00021,
                                    0))

pop2010 <- read_excel("./data/pop2010.xltx")
pop2010 <- pop2010[c(1:nrow(pop2010)-1),]


projeta_pop <- function(tabua_fem, tabua_masc, pop_entrada){
  projeta <- function(pop0, tabua, sex, cont = 1, projecao = 1, pop_entrada = pop_entrada){
  #___________________________________ Projetando população 80+
  
    if(cont == nrow(pop0)){
      if (sex == "Feminino"){
        return((round(pop_entrada$Feminino[16]+pop_entrada$Feminino[17] * tabua$nPx[18])))
      }
      else if (sex == "Masculino"){
        return((round(pop_entrada$Masculino[16]+pop_entrada$Masculino[17] * tabua$nPx[18])))
      }
    }
  #___________________________________ Projetando nascidos vivos
  
    else if (cont == 1){
      if (projecao == 1){
        temp1 <- pop2010 %>% filter(pop2010$`Faixa Etária detalhada` %in% c("15 a 19 anos", "20 a 24 anos", "25 a 29 anos", "30 a 34 anos",
                                                                 "35 a 39 anos", "40 a 44 anos", "45 a 49 anos"))
        temp2 <- pop0 %>% filter(pop0$`Faixa.Etária.detalhada` %in% c("15 a 19 anos", "20 a 24 anos", "25 a 29 anos", "30 a 34 anos",
                                                                 "35 a 39 anos", "40 a 44 anos", "45 a 49 anos"))
        nasc <- (((temp1$Feminino + temp2$Feminino)/2) * fecundidade$nfx) %>% sum()
        if (sex == "Feminino"){
          return(c(round(5*nasc*0.4878), projeta(pop0, tabua, sex, cont = cont + 1, pop_entrada = pop_entrada)))
        }
        else if (sex == "Masculino"){
          return(c(round(5*nasc*0.5122), projeta(pop0, tabua, sex, cont = cont + 1, pop_entrada = pop_entrada)))
        }
      }
      else if (projecao == 2){
        temp1 <- pop2010 %>% filter(pop2010$`Faixa Etária detalhada` %in% c("15 a 19 anos", "20 a 24 anos", "25 a 29 anos", "30 a 34 anos",
                                                                    "35 a 39 anos", "40 a 44 anos", "45 a 49 anos"))
        temp2 <- pop_entrada %>% filter(pop2015$`Faixa.Etária.detalhada` %in% c("15 a 19 anos", "20 a 24 anos", "25 a 29 anos", "30 a 34 anos",
                                                                    "35 a 39 anos", "40 a 44 anos", "45 a 49 anos"))
        temp3 <- pop0 %>% filter(pop0$`Faixa.Etária.detalhada` %in% c("15 a 19 anos", "20 a 24 anos", "25 a 29 anos", "30 a 34 anos",
                                                                 "35 a 39 anos", "40 a 44 anos", "45 a 49 anos"))
        nasc <- (((temp1$Feminino + temp2$Feminino + temp2$Feminino)/3) * fecundidade$nfx) %>% sum()
        if (sex == "Feminino"){
          return(c(round(5*nasc*0.4878), projeta(pop0, tabua, sex, cont = cont + 1, pop_entrada = pop_entrada)))
        }
        else if (sex == "Masculino"){
          return(c(round(5*nasc*0.5122), projeta(pop0, tabua, sex, cont = cont + 1, pop_entrada = pop_entrada)))
        }
      }
    }
  #___________________________________ Projetando resto da população
      
      else {
        if (sex == "Feminino"){
          return(c(round(pop_entrada$Feminino[cont] * tabua_fem$nPx[!is.na(tabua_fem$nPx)][cont]),
                   projeta(pop0, tabua, sex, cont = cont + 1, pop_entrada = pop_entrada)))
        }
        if (sex == "Masculino"){
          return(c(round(pop_entrada$Masculino[cont] * tabua_masc$nPx[!is.na(tabua_masc$nPx)][cont]),
                   projeta(pop0, tabua, sex, cont = cont + 1, pop_entrada = pop_entrada)))
        }
        }
        
  }

    pop_saida <- data.frame(`Faixa Etária detalhada` = pop2010$`Faixa Etária detalhada`[1:nrow(pop2010)])

    pop_saida$Feminino <- projeta(pop0 = pop_saida, tabua = tabua_fem, sex = "Feminino", cont = 1, projecao = projec, pop_entrada = pop_entrada)
    pop_saida$Feminino <- projeta(pop0 = pop_saida, tabua = tabua_fem, sex = "Feminino", cont = 1, projecao = projec, pop_entrada = pop_entrada)
    
    pop_saida$Masculino <- projeta(pop0 = pop_saida, tabua = tabua_masc, sex = "Masculino", cont = 1, projecao = projec, pop_entrada = pop_entrada)
    pop_saida$Masculino <- projeta(pop0 = pop_saida, tabua = tabua_masc, sex = "Masculino", cont = 1, projecao = projec, pop_entrada = pop_entrada)

    pop_saida$Total <- pop_saida$Masculino + pop_saida$Feminino 
    
    return(pop_saida)
}

#__________________________________ a

projec = 1
pop2015 <- projeta_pop(tabua_fem, tabua_masc, pop2010)

projec = 2
pop2020 <- projeta_pop(tabua_fem, tabua_masc, pop2015)

pop2015[nrow(pop2015)+1,] <- c("Total", sum(pop2015$Feminino), sum(pop2015$Masculino),sum(pop2015$Total))
pop2020[nrow(pop2020)+1,] <- c("Total", sum(pop2020$Feminino), sum(pop2020$Masculino),sum(pop2020$Total))

#__________________________________ b

#projecoes IBGE

#____________ Piramides etárias

le_dados_cria_piramide <- function(ano, acao){
  proj_ibge <- read_excel("./data/projecoes-IBGE.xlsx", sheet = ano)
  proj_ibge <- proj_ibge[2:nrow(proj_ibge),]
  tabela <- proj_ibge
  proj_ibge <- data.frame(idade = factor(c(proj_ibge$`FX Etária`, proj_ibge$`FX Etária`),
                                              levels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44",
                                                         "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", 
                                                         "85-89", "90+")), 
                               populacao = c(proj_ibge$Feminino, proj_ibge$Masculino * -1),
                               sexo = c("Feminino","Feminino","Feminino","Feminino",
                                        "Feminino","Feminino","Feminino","Feminino","Feminino",
                                        "Feminino","Feminino","Feminino","Feminino","Feminino",
                                        "Feminino","Feminino","Feminino","Feminino","Feminino",
                                        "Masculino", "Masculino", "Masculino", "Masculino", 
                                        "Masculino", "Masculino", "Masculino", "Masculino", "Masculino", 
                                        "Masculino", "Masculino", "Masculino", "Masculino", "Masculino", 
                                        "Masculino", "Masculino", "Masculino", "Masculino", "Masculino"))

  if (acao == "G"){
   grafico <-  ggplot(proj_ibge, aes(x = idade, y = populacao, fill = sexo)) + 
                  geom_bar(data = subset(proj_ibge, sexo == "Feminino"), stat = "identity") + 
                  geom_bar(data = subset(proj_ibge, sexo == "Masculino"), stat = "identity") + 
                  scale_y_continuous(labels = paste0(as.character(c(seq(5000, 0, -2500), seq(2500, 5000, 2500))), "")) + 
                  labs(title = str_c("Pirâmide Etária Projeção IBGE - Acre ", ano),
                  x = "Faixa Etária",
                  y = "População")+
                  coord_flip()+ theme_classic()
  
    return(grafico)
  }
  if (acao == "T"){
    return(tabela)
  }
}

# 2010
ibge_proj_2010_tabela <- le_dados_cria_piramide("2010", "T")
ibge_proj_2010_grafico <- le_dados_cria_piramide("2010", "G")

# 2015
ibge_proj_2020_tabela <- le_dados_cria_piramide("2020", "T")
ibge_proj_2020_grafico <- le_dados_cria_piramide("2020", "G")

# 2060
ibge_proj_2060_tabela <- le_dados_cria_piramide("2060", "T")
ibge_proj_2060_grafico <- le_dados_cria_piramide("2060", "G")

#_________ Comparacao item b com ibge

ibge_proj_2015_tabela <- le_dados_cria_piramide("2015", "T")

compara_tabela <- function(banco1, banco2, pesq1 = "Demografia 1/21", pesq2 = "IBGE"){
banco1 <- banco1[,c(1,2,3)]
ajusta_banco <- function(banco){
  if (nrow(banco) > 17){
    vetor1 <- c()
    vetor2 <- c()
    vetor3 <- c()
    for (i in 1:nrow(banco)){
      if (i < 17){
        vetor1 <- c(vetor1, str_c(str_split(banco$`FX Etária`[i], "-")[[1]][1], " a ", str_split(banco$`FX Etária`[i], "-")[[1]][2], " anos"))
        vetor2 <- c(vetor2, banco$feminino[i])
        vetor3 <- c(vetor3, banco$masculino[i])
      }
      else if (i == 17){
        vetor1 <- c(vetor1, "80 anos e mais")
        vetor2 <- c(vetor2, sum(banco$feminino[i:nrow(banco)]))
        vetor3 <- c(vetor3, sum(banco$masculino[i:nrow(banco)]))
      }
    }
    banco <- banco[1:17,]
    banco$`FX Etária` <- vetor1
    banco$feminino <- vetor2
    banco$masculino <- vetor3
  }
  return(banco)
}  

colnames(banco2) <- c("FX Etária", "feminino", "masculino")
banco2 <- ajusta_banco(banco2)
colnames(banco1)[1] <- "FX Etária"
tabela <- left_join(banco1, banco2)
sketch <- htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(rowspan = 1, 'Pesquisa'),
        th(colspan = 2, pesq1),
        th(colspan = 2, pesq2)
      ),
      tr(
        lapply(names(tabela)[-1], th)
      )
    )
  ))
  DT::datatable(tabela, container = sketch, rownames = FALSE)
}

comparacao_2010 <- compara_tabela( pop2010, ibge_proj_2010_tabela)
comparacao_2015 <- compara_tabela( pop2015, ibge_proj_2015_tabela)
comparacao_2020 <- compara_tabela( pop2020, ibge_proj_2020_tabela)

rm(fecundidade, tabua_fem, tabua_masc, projec, calcula_npx, projeta_pop, 
   le_dados_cria_piramide, compara_tabela)

   