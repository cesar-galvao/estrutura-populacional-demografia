pacman::p_load(dplyr)

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

projeta <- function(pop0, tabua, sex, pop_entrada = pop2010, cont = 1, projecao = 1){
#___________________________________ Projetando população 80+

  if(cont == nrow(pop0)){
    if (sex == "Feminino"){
      return((pop_entrada$Feminino[16]+pop_entrada$Feminino[17]) * tabua$nPx[18])
    }
    else if (sex == "Masculino")
      return((pop_entrada$Masculino[16]+pop_entrada$Masculino[17]) * tabua$nPx[18])
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
        return(c(5*nasc*0.4878, projeta(pop0, tabua, sex, cont = cont + 1)))
      }
      else if (sex == "Masculino"){
        return(c(5*nasc*0.5122, projeta(pop0, tabua, sex, cont = cont + 1)))
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
        return(c(5*nasc*0.4878, projeta(pop0, tabua, sex, cont = cont + 1)))
      }
      else if (sex == "Masculino"){
        return(c(5*nasc*0.5122, projeta(pop0, tabua, sex, cont = cont + 1)))
      }
    }
  }
#___________________________________ Projetando resto da população
    
    else {
      if (sex == "Feminino"){
        return(c(pop_entrada$Feminino[cont] * tabua_fem$nPx[!is.na(tabua_fem$nPx)][cont],
                 projeta(pop0, tabua, sex, cont = cont + 1)))
      }
      if (sex == "Masculino"){
        return(c(pop_entrada$Masculino[cont] * tabua_masc$nPx[!is.na(tabua_masc$nPx)][cont],
                 projeta(pop0, tabua, sex, cont = cont + 1)))
      }
      }
      
}

projeta_pop <- function(tabua_fem, tabua_masc, pop_entrada){

    pop_saida <- data.frame(`Faixa Etária detalhada` = pop2010$`Faixa Etária detalhada`[1:nrow(pop2010)])

    pop_saida$Feminino <- projeta(pop0 = pop_saida, tabua = tabua_fem, sex = "Feminino", cont = 1, projecao = projec)
    pop_saida$Feminino <- projeta(pop0 = pop_saida, tabua = tabua_fem, sex = "Feminino", cont = 1, projecao = projec)
    
    pop_saida$Masculino <- projeta(pop0 = pop_saida, tabua = tabua_masc, sex = "Masculino", cont = 1, projecao = projec)
    pop_saida$Masculino <- projeta(pop0 = pop_saida, tabua = tabua_masc, sex = "Masculino", cont = 1, projecao = projec)

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

