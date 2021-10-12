#install.packages("Hmisc")
#install.packages("tidyverse")
#install.packages("corrplot")
#install.packages("plyr")
library("Hmisc")
library(tidyverse)
library(corrplot)
library(readXL)
#https://www.displayr.com/how-to-create-a-correlation-matrix-in-r/
library(readxl)
library(car)
library(plyr)
library(pastecs)

# Chargement des données
SIO_6003_Mid_session_project <- read_excel("C:/Users/jpierre/Desktop/SIO_6003/données/SIO 6003 Mid session project.xlsx", 
                                           col_types = c("text", "numeric", "numeric", 
                                                         "numeric", "numeric", "numeric", 
                                                         "numeric", "numeric", "numeric", 
                                                         "numeric", "text"))
#Préparation des données

## renomer des vriables

SIO_6003_Mid_session_project= dplyr::rename(SIO_6003_Mid_session_project, Sexe=Gender, SalaireMensuel=`Income per month`, 
                                     InvestBourse         =`Monthly Investment in stock market`,
                                     InvestBitcoin        =`Investment in Bitcoin`,
                                     NbPersonne           =`Family size`,
                                     PrixPetrole          =`Global Oil prices in dollar per gallon`,
                                     LoyeMensuel          =`Monthly Rental`,
                                     AchatMoisPrecedent   =`Previous Purchase Value`,
                                     AchatMoisEnCours     =`Purchase Value`,
                                     PaysRes              = `Country of Residence`)

# Recodage de variables
SIO_6003_Mid_session_project$PaysRes <- plyr::revalue(SIO_6003_Mid_session_project$PaysRes, 
                                                      c("Canada"="1", "US"="0")) 
# convertir en nombre
SIO_6003_Mid_session_project$PaysRes<- as.numeric(SIO_6003_Mid_session_project$PaysRes)
# Suppression de la variable Sexe 
SIO_6003_Mid_session_project = SIO_6003_Mid_session_project %>% dplyr::select(-c(Sexe))





StaDescUsCan=stat.desc(SIO_6003_Mid_session_project)

tes1=StaDescUsCan %>%
  rownames_to_column %>% 
  gather(var, value, -rowname) %>% 
  spread(rowname, value) 

qqPlot(SIO_6003_Mid_session_project$AchatMoisEnCours)

#shapiro.test(UsData$AchatMoisEnCours)


CanUsdataCorr = rcorr(as.matrix(SIO_6003_Mid_session_project))



CaUsdata_coeff =as.data.frame(round(CanUsdataCorr$r,2))
CanUsdata_p = CanUsdataCorr$P



corrplot(cor(UsData, method = c("spearman")))

#palette = colorRampPalette(c("DarkRed", "FireBrick",  "Red",  "LightSalmon", "DarkSalmon", "Salmon")) (20)
#heatmap(x = cor(UsData, method = c("spearman")), col = palette, symm = TRUE)


UsCorr = rcorr(as.matrix(UsData))

USData_coeff = CandataCorr$r
UsData_p = CandataCorr$P



corrplot(cor(CanadaData, method = c("spearman")))


library(pastecs)
StaDesc=stat.desc(UsData)
UsData$AchatMoisEnCours<-log(UsData$AchatMoisEnCours)
