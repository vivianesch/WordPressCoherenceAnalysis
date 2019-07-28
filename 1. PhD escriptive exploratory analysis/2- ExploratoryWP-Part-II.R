library(readr)
install.packages("dplyr")
library(dplyr)
library(plotly)
library(markdown)
library(ggplot2)
library(lattice)


TicketW <- read_csv('~/PhD Analysis/1. PhD escriptive exploratory analysis/TicketW.csv')
View(TicketW) # Read 
glimpse(TicketW)



#Bivariable analysis

#1. Analyse Reporters and Keywords data

#1.1 Analyse Keywords used for Active reporters
Reporter_Keywords<- TicketW %>% select(Reporter,Keywords) #Table of Reporters and Keywords
summary(Reporter_Keywords)
dim(Reporter_Keywords)
glimpse(Reporter_Keywords)
rownames(Reporter_Keywords)
colnames(Reporter_Keywords)

nrow(ActiveReporters)
nrow #Quantity of Active Reporters
AR<-ActiveReporters[1:34,1:1] #Select array of active reporters names to filter 
AR<-as.character(AR) #Transform filter into caracter

ActiveR_Key = subset(Reporter_Keywords, Reporter_Keywords$Reporter %in% AR) 
ActiveR_Key<-table(ActiveR_Key$Keywords,ActiveR_Key$Reporter)
dim(ActiveR_Key)
glimpse(ActiveR_Key)
rownames(ActiveR_Key)
colnames(ActiveR_Key)






pairs(CompType, diag.panel = panel.hist, upper.panel = panel.cor)
CompType<-table(TicketW$Component)

pairs(CompType, diag.panel = panel.hist, upper.panel = panel.cor)
CompStatus<-table(TicketW$Component,TicketW$Status)


        
        
        
        
        
    
   



