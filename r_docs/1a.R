library(dplyr)
library(ggplot2)
library(readxl)

pop2000 <- read_excel("data/pop2000.xltx")
pop2010 <- read_excel("data/pop2010.xltx")
pop2015 <- read_excel("data/pop2015.xltx")
pop2020 <- read_excel("data/pop2020.xltx")
pop2030 <- read_excel("data/pop2030.xltx")

pop2015 <- na.omit(pop2015)
pop2020 <- na.omit(pop2020)
pop2030 <- na.omit(pop2030)


total2000 <- 557526
total2010 <- 733559
total2015 <- 803513
total2020 <- 866811
total2030 <- 972464

pop2000['Porcentagem F'] <- (pop2000$Feminino/total2000)*100
pop2000['Porcentagem M'] <- (pop2000$Masculino/total2000)*100

pop2010['Porcentagem F'] <- (pop2010$Feminino/total2010)*100
pop2010['Porcentagem M'] <- (pop2010$Masculino/total2010)*100

pop2015['Porcentagem F'] <- (pop2015$Feminino/total2015)*100
pop2015['Porcentagem M'] <- (pop2015$Masculino/total2015)*100

pop2020['Porcentagem F'] <- (pop2020$Feminino/total2020)*100
pop2020['Porcentagem M'] <- (pop2020$Masculino/total2020)*100

pop2030['Porcentagem F'] <- (pop2030$Feminino/total2030)*100
pop2030['Porcentagem M'] <- (pop2030$Masculino/total2030)*100

pop2000 <- pop2000[-18,]
pop2010 <- pop2010[-18,]
pop2015 <- pop2015[-18,]
pop2020 <- pop2020[-18,]
pop2030 <- pop2030[-18,]

library(plotrix)

#2000
xy.pop2000<- pop2000$`Porcentagem M`
xx.pop2000<- pop2000$`Porcentagem F`
agelabels<-c("0-4","5-9","10-14","15-19","20-24","25-29","30-34",
             "35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74",
             "75-79","80+")
mcol<-color.gradient(c(0,0,0.5,1),c(0,0,0.5,1),c(1,1,0.5,1),18)
fcol<-color.gradient(c(1,1,0.5,1),c(0.5,0.5,0.5,1),c(0.5,0.5,0.5,1),18)

par(mar=pyramid.plot(xy.pop2000,xx.pop2000,labels=agelabels,
                    main="Pirâmide etária da população do Acre em 2000",lxcol=mcol,rxcol=fcol,
                    gap=0.5,show.values=TRUE))


#2010
xy.pop2010<- pop2010$`Porcentagem M`
xx.pop2010<- pop2010$`Porcentagem F`
agelabels<-c("0-4","5-9","10-14","15-19","20-24","25-29","30-34",
             "35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74",
             "75-79","80+")
mcol<-color.gradient(c(0,0,0.5,1),c(0,0,0.5,1),c(1,1,0.5,1),18)
fcol<-color.gradient(c(1,1,0.5,1),c(0.5,0.5,0.5,1),c(0.5,0.5,0.5,1),18)

par(mar=pyramid.plot(xy.pop2010,xx.pop2010,labels=agelabels,
                     main="Pirâmide etária da população do Acre em 2010",lxcol=mcol,rxcol=fcol,
                     gap=0.5,show.values=TRUE))

#2015
xy.pop2015<- pop2015$`Porcentagem M`
xx.pop2015<- pop2015$`Porcentagem F`
agelabels<-c("0-4","5-9","10-14","15-19","20-24","25-29","30-34",
             "35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74",
             "75-79","80+")
mcol<-color.gradient(c(0,0,0.5,1),c(0,0,0.5,1),c(1,1,0.5,1),18)
fcol<-color.gradient(c(1,1,0.5,1),c(0.5,0.5,0.5,1),c(0.5,0.5,0.5,1),18)

par(mar=pyramid.plot(xy.pop2015,xx.pop2015,labels=agelabels,
                     main="Pirâmide etária da população do Acre em 2015",lxcol=mcol,rxcol=fcol,
                     gap=0.5,show.values=TRUE))


#2020
xy.pop2020<- pop2020$`Porcentagem M`
xx.pop2020<- pop2020$`Porcentagem F`
agelabels<-c("0-4","5-9","10-14","15-19","20-24","25-29","30-34",
             "35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74",
             "75-79","80+")
mcol<-color.gradient(c(0,0,0.5,1),c(0,0,0.5,1),c(1,1,0.5,1),18)
fcol<-color.gradient(c(1,1,0.5,1),c(0.5,0.5,0.5,1),c(0.5,0.5,0.5,1),18)

par(mar=pyramid.plot(xy.pop2020,xx.pop2020,labels=agelabels,
                     main="Pirâmide etária da população do Acre em 2020",lxcol=mcol,rxcol=fcol,
                     gap=0.5,show.values=TRUE))

#2030
xy.pop2030<- pop2030$`Porcentagem M`
xx.pop2030<- pop2030$`Porcentagem F`
agelabels<-c("0-4","5-9","10-14","15-19","20-24","25-29","30-34",
             "35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74",
             "75-79","80+")
mcol<-color.gradient(c(0,0,0.5,1),c(0,0,0.5,1),c(1,1,0.5,1),18)
fcol<-color.gradient(c(1,1,0.5,1),c(0.5,0.5,0.5,1),c(0.5,0.5,0.5,1),18)

par(mar=pyramid.plot(xy.pop2030,xx.pop2030,labels=agelabels,
                     main="Pirâmide etária da população do Acre",lxcol=mcol,rxcol=fcol,
                     gap=0.5,show.values=TRUE))




