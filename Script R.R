#Etape1: conception/definition de la problematique et des donnees

#Etape2:data import



if(!require(readxl)) install.packages('readxl')
library(readxl)
d<- read_excel("étude statistique.xlsx")
View(d)

#variable explicative:age,genre,année d'étude
#variable à expliqué:
#Etape3:pretreatment
#3.1:Conversion
d$age[32]=22
d$age= as.integer(d$age)
d$Genre = as.factor(d$Genre)
d$A_E=as.factor(d$A_E)
d$MT_item1=as.factor(d$MT_item1)
d$MT_item2=as.factor(d$MT_item2)
d$MT_item3=as.factor(d$MT_item3)
d$For_item=as.factor(d$For_item)
d$item1=as.factor(d$item1)
d$item2=as.factor(d$item2)
d$item3=as.factor(d$item3)
d$item4=as.factor(d$item4)
d$item5=as.factor(d$item5)
#Data Cleaning
#1-Detecting abherant values through Boxlot
#---------valeur aberante?-----------------
boxplot(d$age,main="Boxplot", ylim = range(18,24),col = c("yellow"),ylab = "Quantiles")
quantile(d$age,probs=c(0,0.25,0.5,0.75,1))
#Dans ce plot on trouve que des valeurs qui n'appartient pas aux valeurs normaux
c=boxplot.stats(d$age)$out
c

#Algo ci-dessous pour detecter les VAs et les covertir aprÃ©s en NA

for(i in 1:length(d$age))
{
  for(j in 1:length(c))
  {
    if(!is.na(d$age[i])){
      if(d$age[i]==c[j]){
        d$age[i]=NA
      }
    }
  }
}
#3.2.2 traitement des valeurs manquantes
#Dans notre cas, on pe ut pas les remplacer

L=sum(is.na(d$age))
L
p=L/length(d$age)
p
if (p>0.05){
  print('On doit estimer:')
  for (i in 1:length(d$age))
  {
    if (is.na(d$age[i]))
    {d$age[i]=mean(d$age,na.rm = T)}
  }}
if (p<0.05)
{
  print('On supprime les samples avec des NA :la ligne toute entiere')
  if(!require(dplyr)) {install.packages('dplyr')}
  library(dplyr)
  d = d %>%
    na.omit()
  View(d)
}

#3.3 Test de normalite
options(scipen=999)
shapiro.test(d$age)
#histograme
hist(d$age,col = c("orange"),main = paste("Histogramme pour la variable age"),ylab = "Effectifs",xlab = "age")
#P-value<<5% -->Hl est majoritaire
#-->Notre distribution ne suit pas la distribution theo
#-->ne suit pas la loi normale
#-->pourquoi: Je pense parce que notre population est tres centree sur un a
#test de quasi nomrmalite avec le test de l'inclinaison
if(!require(moments)) {install.packages('moments')}
library(moments)
skewness(d$age)
#a little bit skewed (skeweness>0.5)
kurtosis(d$age)
#--> distribution quasi-normale
#qui est inclinee vers la gauche (skeweness>0) et kurtosis est proche de 3
#Etape 4:Analyse de donnees

#4.1.Statistique univariee

summary(d$Genre)
table(d$Genre)
#-test de representativite
  chisq.test(table(d$Genre),p=c(0.46,0.54))
#P-value>5% --> il n'y a pas de différence Significative
#entre le nombre des hommes et des femmes
#notre echnatillon est representative

summary(table(d$A_E))
chisq.test(table(d$A_E))
#P-value<5% --> il y a une difference Significative
#entre 1a distribution theo et notre distribution
#-->on a pas le meme nombre d'invidus de chaque coté: CI1,CI2

summary(d$age)
#age min 19 le max= 22
#le premier quartile (25%)=20
#le deuxieme quartile (50%)<->la mediane=20
#Ie tros1eme quartile (75%)<->3rd Qu=21
round(prop.table(table(d$age)),3)


table(d$MT_item1)
table(d$MT_item2)
table(d$MT_item3)
table(d$For_item)
table(d$item1)
table(d$item2)
table(d$item3)
table(d$item4)
table(d$item5)
summary(d$For_item)
#representation graphique
barplot(table(d$MT_item1))
barplot(table(d$MT_item2))
barplot(table(d$MT_item3))
barplot(table(d$For_item))
barplot(table(d$item1))
barplot(table(d$item2))
barplot(table(d$item3))
barplot(table(d$item4))
barplot(table(d$item5))


#4.2 Statistique bivariee

#´´´´´´´´´´´´´´´´´´´´´´´´´´´´´´´´

#elimer A_E comme variable explicative´<--------


#Relation de Genre
#le genre influence la perception des etudiant

  fisher.test (table (d$Genre,d$For_item))
#HO est rejeté=> il y a association entre le choix et le genre

ggplot (data=d, aes(x=d$Genre,fill=d$For_item))+
  geom_bar (stat="count", position = position_dodge())+
  facet_grid(d$For_item)
#on peut dire les F ont une vision plus positive que les H Sur 1'autoformation.
 
  fisher.test (table(d$Genre,d$MT_item1))
#HO est accepteevil n y a pas d'association

  fisher.test (table(d$Genre,d$MT_item2))
#HO est acceptee -> il n y a pas d'association

  fisher.test (table (d$Genre,d$MT_item3) )
#HO est acceptee->1l n y a pas d'association

  fisher.test (table (d$Genre,d$item1))
#HO est acceptee => il n y a pas d'associarion

  fisher.test (table (d$Genre,d$item2))
#HO est acceptee -> il n y a pas d'association

  fisher.test (table (d$Genre,d$item3))
#HO est acceptee->il n y a pas d'association
  
  fisher.test (table (d$Genre,d$item4))
#HO est acceptee->il n y a pas d'association
  
  fisher.test (table (d$Genre,d$item5))
  #HO est acceptee->il n y a pas d'association
  
#Relation de age------------------------------
  
  wilcox.test(d$age~d$A_E,conf.int=T,exact=F)
  #on ajoute exact=RF car sans lui on va avoir une erreur declarer par R car on
  #peut pascalculer les valeur tres précise
  
  ##h1 est accepte -> il ya une diffrence significative entre lage de qui sont en premire anner et ceux qui 
  #sont en deuxieme anne ce qui conferme notre etude
  wilcox.test(d$age~d$Genre,conf.int=T,exact=F)
  ##h0est accepte -> il n'y pas de difference sign de lage entre homme et femme
  
  kruskal.test(d$age~d$For_item)
 #il ny pas de correspondance entre les deux
  #age n'explice pas le choix
  #Relation de MT_item1--------------------------------
  
  fisher.test (table (d$MT_item1,d$For_item))
#H1 est acceptee->il y a une associati on entre ces deux variables
  
  if(!require(questionr)) {install.packages('questionr')}
  library(questionr)
  LL= table (d$MT_item1,d$For_item)
  ##cramer.v(table (d$MT_item1,d$For_item))
  ###v=0.41 relation moyenne
  
  library(DescTools)
  #ce sont des coeff qui etudie la nature de 1a relation entre 2vars ordinales
 
  
  SomersDelta(LL ,direction ="column", conf.level=0.95)
  KendallTauB (LL ,conf.level = 0.95)
  
  #tau_b>0.10 ->relation fort
 
  require(ggplot2)
  require(ggthemes)
  ggplot (d, aes (x=d$MT_item1 , fill=d$For_item)) +
    geom_bar (stat="count",position =position_dodge ())+
    facet_grid (d$For_item)
  #c'est un graphe qui explique la relation  qui 1ie les deux
  
  #chisq.test (table (d$MT_item1,d$MT_item2))
  fisher.test (table (d$MT_item1,d$MT_item2))
  #H1 est acceptee->il y a une associati on entre ces deux variables
  LL= table (d$MT_item1,d$MT_item2)
  cramer.v(table (d$MT_item1,d$MT_item2))
  
  #v=0.41 relation moyenne
  
    library(DescTools)
    #ce sont des coeff qui etudie la nature de 1a relation entre 2vars ordinales
    SomersDelta(LL ,direction ="column", conf.level=0.95)
    KendallTauB (LL ,conf.level = 0.95)
    ggplot (d, aes (x=d$MT_item1 , fill=d$MT_item2)) +
      geom_bar (stat="count",position =position_dodge ())+
      facet_grid (d$MT_item2)
  
  
  fisher.test (table (d$MT_item1,d$MT_item3))
  #H1 est acceptee->il y a une associati on entre ces deux variables
  
  LL2= table (d$MT_item1,d$MT_item3)
  cramer.v(table (d$MT_item1,d$MT_item3))
  #v=0.55 relation moyenne
  library(DescTools)
  #ce sont des coeff qui etudie la nature de 1a relation entre 2vars ordinales
  SomersDelta(LL2 ,direction ="column", conf.level=0.95)
  KendallTauB (LL2 ,conf.level = 0.95)
  #tau_b= 0.6695549 le lien est fort
  ggplot (d, aes (x=d$MT_item1 , fill=d$MT_item3)) +
    geom_bar (stat="count",position =position_dodge ())+
    facet_grid (d$MT_item3)
  
  fisher.test (table (d$MT_item1,d$item1))
  #H1 est acceptee->il y a une associati on entre ces deux variables
  
  fisher.test (table (d$MT_item1,d$item2))
  #H0 est acceptee->il n'y a pas d'associati on entre ces deux variables
  
  fisher.test (table (d$MT_item1,d$item3))
  #H0 est acceptee->il n'y a pas d'associati on entre ces deux variables
  
  fisher.test (table (d$MT_item1,d$item4))
  #H0 est acceptee->il n'y a pas d'associati on entre ces deux variables

  fisher.test (table (d$MT_item1,d$item5))
  
  #H0 est acceptee->il n'y a pas d'associati on entre ces deux variables

#Relation de MT_item2------------------------

fisher.test (table (d$MT_item2,d$For_item))
#H1 est acceptee->il y a une associati on entre ces deux variables
  if(!require(questionr)) {install.packages('questionr')}
  library(questionr)
  LL= table (d$MT_item2,d$For_item)
  cramer.v(table (d$MT_item1,d$For_item))
  #v=0.38
  library(DescTools)
  #ce sont des coeff qui etudie la nature de 1a relation entre 2vars ordinales
  SomersDelta(LL ,direction ="column", conf.level=0.95)
  KendallTauB (LL ,conf.level = 0.95)
  #tau_b= -0.44
require(ggplot2)
require(ggthemes)
ggplot (d, aes (x=d$MT_item2 , fill=d$For_item)) +
  geom_bar (stat="count",position =position_dodge ())+
  facet_grid (d$For_item)
#C'est un graphe qui explique la relation qui 1ie les deux

#chisq.test (table (d$MT_item2,d$MT_item3))
fisher.test (table (d$MT_item2,d$MT_item3))

#H1 est acceptee->il y a une associati on entre ces deux variables

LL= table (d$MT_item2,d$MT_item3)
cramer.v(table (d$MT_item2,d$MT_item3))
#v=0.41 relation moyenne
library(DescTools)
#ce sont des coeff qui etudie la nature de 1a relation entre 2vars ordinales
SomersDelta(LL ,direction ="column", conf.level=0.95)
KendallTauB (LL ,conf.level = 0.95)
ggplot (d, aes (x=d$MT_item2, fill=d$MT_item3)) +
  geom_bar (stat="count",position =position_dodge ())+
  facet_grid (d$MT_item3)


fisher.test (table (d$MT_item2,d$item1))
#H1 est accepte->il y a  d'associati on entre ces deux variables
LL= table (d$MT_item2,d$item1)
cramer.v(table (d$MT_item2,d$item1))


fisher.test (table (d$MT_item2,d$item2))
#H0 est accepte->il n'y a pas d'associati on entre ces deux variables


fisher.test (table (d$MT_item2,d$item3))
#H0 est accepte->il n'y a pas  d'associati on entre ces deux variables

fisher.test (table (d$MT_item2,d$item4))
#H0 est accepte->il n'y a pas  d'association on entre ces deux variables

fisher.test (table (d$MT_item2,d$item5))
#H0 est accepte->il n'y a pas  d'association on entre ces deux variables

#Relation de MT_item3------------------------

    fisher.test (table (d$MT_item3,d$For_item))
    #H0 est acceptee->il n'y a pas une association on entre ces deux variables

    
    fisher.test (table (d$MT_item3,d$item1))
    #H0 est accepte->il n'y a pas  d'associati on entre ces deux variables

    fisher.test (table (d$MT_item3,d$item2))
    #H0 est accepte->il n'y a pas d'associati on entre ces deux variables
    
    
    fisher.test (table (d$MT_item3,d$item3))
    #H1 est accepte->il y a   d'associati on entre ces deux variables
    LL= table (d$MT_item3,d$item3)
    cramer.v(table (d$MT_item3,d$item3))
    #moyen
    
    fisher.test (table (d$MT_item3,d$item4))
    #H0 est accepte->il n'y a pas  d'association on entre ces deux variables

    fisher.test (table (d$MT_item3,d$item5))
    #H0 est accepte->il n'y a pas  d'association on entre ces deux variables
    
#Relation de item1------------------------
    
    fisher.test (table (d$item1,d$For_item))
    #H1 est acceptee->il y a  une association on entre ces deux variables
    LL= table (d$item1,d$For_item)
    cramer.v(table (d$item1,d$For_item))#0,6
    
    fisher.test (table (d$item1,d$item2))
    #H0 est accepte->il n'y a pas  d'associati on entre ces deux variables
    
    fisher.test (table (d$item1,d$item3))
    #H1 est accepte->il y a  d'associati on entre ces deux variables
    LL= table (d$item1,d$item3)
    cramer.v(table (d$item1,d$item3))#0,43
    
    fisher.test (table (d$item1,d$item4))
    #H1 est accepte->il y a   d'associati on entre ces deux variables
    LL= table (d$item1,d$item4)
    cramer.v(LL)
    #0.47
    fisher.test (table (d$item1,d$item5))
    
#Relation de item2------------------------
    
    fisher.test (table (d$item2,d$For_item))
    #H0 est acceptee->il n'y a pas une association on entre ces deux variables
    
    fisher.test (table (d$item2,d$item3))
    #H1 est accepte->il y a  d'associati on entre ces deux variables
    LL= table (d$item2,d$item3)
    cramer.v(table (d$item2,d$item3))#0,42
    
    fisher.test (table (d$item2,d$item4))
    #H0 est acceptee->il n'y a pas une association on entre ces deux variables
    
    fisher.test (table (d$item2,d$item5))
    #H1est acceptee->il y a  une association on entre ces deux variables
    LL= table (d$item2,d$item5)
    cramer.v(table (d$item2,d$item5))#0,52
    
#Relation de item3------------------------
    
    fisher.test (table (d$item3,d$For_item))
    #H1 est acceptee->il y a  une association on entre ces deux variables
    LL= table (d$item3,d$For_item)
    cramer.v(LL)#0,42
    

    
    fisher.test (table (d$item3,d$item4))
    #H0 est acceptee->il n'y a pas une association on entre ces deux variables
    
    fisher.test (table (d$item3,d$item5))
    #H0 est acceptee->il n'y a pas une association on entre ces deux variables
    
#Relation de item4------------------------
    
    fisher.test (table (d$item4,d$For_item))
    #H0 est acceptee->il n'y a pas une association on entre ces deux variables

    
    fisher.test (table (d$item4,d$item5))
    #H0 est acceptee->il n'y a pas une association on entre ces deux variables
#Relation de item5------------------------
    
    fisher.test (table (d$item5,d$For_item))
    #H0 est acceptee->il n'y a pas une association on entre ces deux variables
    
# 4.3:Analyse multivar1ee
#commencons d' abord par la conversion
if(is.factor(d$MT_item1))
  d$MT_item1=as.character(d$MT_item1)

for (i in 1:length(d$MT_item1))
{
  if (d$MT_item1[i]=="Tout a fait d'accord")
  {
    d$MT_item1[i]=1 
  }
  if (d$MT_item1 [i]=="D'accord")
  {
    d$MT_item1[i]=2 
  }
  if (d$MT_item1 [i] =="Neutre")
  {
    d$MT_item1[i]=3 
  }
  if (d$MT_item1 [i] =="Pas d'accord")
  {
    d$MT_item1[i]=4
  }
  if (d$MT_item1 [i] =="Pas du tout d'accord")
  {
    d$MT_item1[i]=5
  }
}

if(is.character(d$MT_item1))
  d$MT_item1=as.integer(d$MT_item1)
if(is.factor(d$MT_item2))
  d$MT_item2=as.character(d$MT_item2)

for (i in 1:length(d$MT_item2))
{
  if (d$MT_item2[i]=="Tout a fait d'accord")
  {
    d$MT_item2[i]=1 
  }
  if (d$MT_item2 [i]=="D'accord")
  {
    d$MT_item2[i]=2 
  }
  if (d$MT_item2 [i] =="Neutre")
  {
    d$MT_item2[i]=3 
  }
  if (d$MT_item2 [i] =="Pas d'accord")
  {
    d$MT_item2[i]=4
  }
  if (d$MT_item2[i] =="Pas du tout d'accord")
  {
    d$MT_item2[i]=5
  }
}

if(is.character(d$MT_item2))
  d$MT_item2=as.integer(d$MT_item2)

if(is.factor(d$MT_item3))
  d$MT_item3=as.character(d$MT_item3)

for (i in 1:length(d$MT_item3))
{
  if (d$MT_item3[i]=="Tout a fait d'accord")
  {
    d$MT_item3[i]=1 
  }
  if (d$MT_item3 [i]=="D'accord")
  {
    d$MT_item3[i]=2 
  }
  if (d$MT_item3 [i] =="Neutre")
  {
    d$MT_item3[i]=3 
  }
  if (d$MT_item3 [i] =="Pas d'accord")
  {
    d$MT_item3[i]=4
  }
  if (d$MT_item3[i] =="Pas du tout d'accord")
  {
    d$MT_item3[i]=5
  }
}
#d$item1[1]="d'accord"
#d$item1[8]="d'accord"

if(is.character(d$MT_item3))
  d$MT_item3=as.integer(d$MT_item3)

if(is.factor(d$item1))
  d$item1=as.character(d$item1)

for (i in 1:length(d$item1))
{
  if (d$item1[i]=="Tout a fait d'accord")
  {
    d$item1[i]=1 
  }
  if (d$item1[i]=="d'accord")
  {
    d$item1[i]=2 
  }
  if (d$item1[i] =="neutre")
  {
    d$item1[i]=3 
  }
  if (d$item1[i] =="pas d'accord")
  {
    d$item1[i]=4
  }
  if (d$item1[i] =="pas du tout d'accord")
  {
    d$item1[i]=5
  }
}

if(is.character(d$item1))
  d$item1=as.integer(d$item1)

if(is.factor(d$item2))
  d$item2=as.character(d$item2)

for (i in 1:length(d$item2))
{
  if (d$item2[i]=="Tout a fait d'accord")
  {
    d$item2[i]=1 
  }
  if (d$item2[i]=="D'accord")
  {
    d$item2[i]=2 
  }
  if (d$item2[i] =="Neutre")
  {
    d$item2[i]=3 
  }
  if (d$item2[i] =="Pas d'accord")
  {
    d$item2[i]=4
  }
  if (d$item2[i] =="Pas du tout d'accord")
  {
    d$item2[i]=5
  }
}

if(is.character(d$item2))
  d$item2=as.integer(d$item2)

if(is.factor(d$item3))
  d$item3=as.character(d$item3)

for (i in 1:length(d$item3))
{
  if (d$item3[i]=="Tout a fait d'accord")
  {
    d$item3[i]=1 
  }
  if (d$item3[i]=="d'accord")
  {
    d$item3[i]=2 
  }
  if (d$item3[i] =="neutre")
  {
    d$item3[i]=3 
  }
  if (d$item3[i] =="pas d'accord")
  {
    d$item3[i]=4
  }
  if (d$item3[i] =="pas du tout d'accord")
  {
    d$item3[i]=5
  }
}

if(is.character(d$item3))
  d$item3=as.integer(d$item3)

if(is.factor(d$item4))
  d$item4=as.character(d$item4)

for (i in 1:length(d$item4))
{
  if (d$item4[i]=="Tout a fait d'accord")
  {
    d$item4[i]=1 
  }
  if (d$item4[i]=="d'accord")
  {
    d$item4[i]=2 
  }
  if (d$item4[i] =="neutre")
  {
    d$item4[i]=3 
  }
  if (d$item4[i] =="pas d'accord")
  {
    d$item4[i]=4
  }
  if (d$item4[i] =="pas du tout d'accord")
  {
    d$item4[i]=5
  }
}

if(is.character(d$item4))
  d$item4=as.integer(d$item4)
    if(is.factor(d$item5))
      d$item5=as.character(d$item5)
    
    for (i in 1:length(d$item5))
    {
      if (d$item5[i]=="Tout a fait d'accord")
      {
        d$item5[i]=1 
      }
      if (d$item5[i]=="d'accord")
      {
        d$item5[i]=2 
      }
      if (d$item5[i] =="neutre")
      {
        d$item5[i]=3 
      }
      if (d$item5[i] =="pas d'accord")
      {
        d$item5[i]=4
      }
      if (d$item5[i] =="pas du tout d'accord")
      {
        d$item5[i]=5
      }
    }
    
    if(is.character(d$item5))
      d$item5=as.integer(d$item5)



    
install.packages("ltm")
library(ltm)
items=data.frame(d$MT_item1,d$MT_item2,d$MT_item3)
bartlett.test(items)
# p-value = 0.04234 H1 acceptee. il ya une association significative entre les items
items2=data.frame(d$item1,d$item2,d$item3,d$item4,d$item5)
items3=data.frame(d$item1,d$item2,d$item3,d$item4,d$item5,d$MT_item1,d$MT_item2,d$MT_item3)
bartlett.test(items2)
bartlett.test(items3)
# p-value = 0.04234 H1 acceptee. il ya une association significative entre les items
cronbach.alpha(items2)
cronbach.alpha(items)
cronbach.alpha(items3)
alpha(items2)



#Afin d'augmenter la valeur de alpha on ete obligé d'enlever item1
#Ainsi on aura comme valeur >0.6 ce qui est tres bien
alpha(items) #ca nous donne alpha: 0.476
#Afin d'augmenter la valeur de alpha on ete obligé d'enlever MT_item3
#Ainsi on aura comme valeur >0.6 ce qui est tres bien


if(!require(Rcmdr)) {install.packages("Rcmdr")}
library("Rcmdr")
#test de normaliter
shapiro.test(d$F1)

shapiro.test(d$F2)
#les deux suit la loi normal


  aov(d$F1~d$For_item)
  aov(d$F2~d$For_item)
# Compute the analysis of variance
  res.aov <- aov(d$F1~d$For_item)
# Summary of the analysis
  summary(res.aov)
#Pr(>F) >5% ->il n'ya pas une diffrence significatife entre le factore et le for_item
  res.aov1 <- aov(d$F2~d$For_item)
# Summary of the analysis
  summary(res.aov1)
#Pr(>F) >5% ->il n'ya pas une diffrence significatife entre le factore et le for_item
  
  #80% confidence level erreur marginal est 10% et le population est 158 de premier et deuxiemme année
  #et ca va donner un echantillon de 33 personne ce qui est approximative a note echantillon de 31 personnes
  
#conclution
 #notre h.r les etudiants pensent le autoforamtion influence de maniere positive le marcher de travaill est confirmer par notre
  #etude et cella peut etre 
  #explice par les factore principal
  #le genre influence la perception
  #l'age n'influence pas
  
  