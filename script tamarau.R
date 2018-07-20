library(reshape2)
library(summarytools)

setwd("C:/Users/Manon GHISLAIN/Documents/tamarau/datas pour analyses")

load("data.Rdata")
##### BILAN DES DONNESS ------------------
head(data)
view(dfSummary(data))
str(data)
hist(data$Number)
#différences d'échantillonnage entre années?
table(data$year)

table(data$year,data$Age)#2004->2008 : les calfs ne sont pas différenciés
table(data$year,data$Site) #2004->2006 : sites 17 et 18 non comptés
                           #2007->site 17 différent?? : c'est le même!! #à modifier

#sélection datas pour un premier bilan : sites 1 à 16
dataselec<-data[which(data$Site!="17.  Tangle" & data$Site!="17.  Saligi east" & data$Site!="18.  Malibayong"),]
dataselec<-droplevels(dataselec)
#constance de l'abondance par site dans le temps
zfac <-as.factor( dataselec$year[dataselec$Age=="Ad" & dataselec$count=="AN" & dataselec$Sexe=="M"] )
plot(dataselec$Number[dataselec$Age=="Ad" & dataselec$count=="AN" & dataselec$Sexe=="M"], 
     dataselec$Site[dataselec$Age=="Ad" & dataselec$count=="AN" & dataselec$Sexe=="M"] 
     , col = mescouleurs[zfac], pch = rank(levels(zfac)),
     xlab="Nombre d'individus (M Ad)",
     ylab="Sites")
legend("topright", inset = 0.02, pch = rank(levels(zfac)), legend = levels(zfac), col = mescouleurs)

boxplot(dataselec$Number[dataselec$Age=="Ad" & dataselec$count=="AN" & dataselec$Sexe=="M"]
        ~dataselec$Site[dataselec$Age=="Ad" & dataselec$count=="AN" & dataselec$Sexe=="M"],
        las=2)

#relation Total Number TN et Actual Number AN (=doublons pris en compte)
      #suppr 2005 car juste TN
dataselec2<-dataselec[which(dataselec$year!=2005),]
actual_number<-dataselec2$Number[dataselec2$Age=="Ad" & dataselec2$count=="AN" & dataselec2$Sexe=="M"]
total_number<-dataselec2$Number[dataselec2$Age=="Ad" & dataselec2$count=="TN" & dataselec2$Sexe=="M"]
plot(total_number,actual_number)
reg<-lm(actual_number~total_number)
summary(reg) #significativemet corréllé ***
abline(reg, col="blue")
    # Equation de la droite de regression : 
coeff=coefficients(reg)
eq = paste0("y = ", round(coeff[2],1), "*x ", round(coeff[1],1))

#évolution du nombre d'individus par catégorie d'âge
#somme tous sites confondus sur actualnumber
dataselec3<-dataselec[which(dataselec$count=="AN" &  dataselec$year>=2006),]
tableau<-aggregate(dataselec3$Number, by=list(Age=dataselec3$Age, year= dataselec3$year), FUN=sum)
plot( x = (2006:2018),  y=as.numeric(tableau$x[tableau$Age=="Ad"])  , type="b", xlab="",ylab="",
      pch=19 ,cex=0.5, ylim=c(0,300), bty="n",  axes = FALSE, col="blue") #adultes
annees<-c(2006:2018)
axis(1, at=seq(2006,2018,1), labels=annees,las=2)
axis(2, at=seq(0,300,50),labels=seq(0,300,50), las=1)

points(x=(2006:2018), y=as.numeric(tableau$x[tableau$Age=="J"]),type="b",
       pch=19, cex=0.5, col="red")  #juv/subadult
points(x=(2006:2018), y=as.numeric(tableau$x[tableau$Age=="Y"]),type="b",
       pch=19, cex=0.5, col="black")  #yearling
points(x=(2006:2018), y=c(rep(NA,3),as.numeric(tableau$x[tableau$Age=="C"])),type="b",
       pch=19, cex=0.5, col="orange")  #calf

#lien entre yearling t et calf t-1??
yearling<-tableau$x[tableau$year>=2010 & tableau$Age=="Y"]
calf<-tableau$x[tableau$year>=2009 & tableau$year!=2018 & tableau$Age=="C"]
plot(yearling,calf)
summary(lm(yearling~calf)) ##pas de corrélation (sur 9 années)
