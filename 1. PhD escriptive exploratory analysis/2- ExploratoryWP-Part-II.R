library(readr)
install.packages("dplyr")
library(dplyr)
library(plotly)
library(markdown)
library(ggplot2)
library(lattice)
library(tidyr)


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

nrow(ActiveReporters)#Quantity of Active Reporters
AR<-ActiveReporters[1:34,1:1] #Select array of active reporters names to filter 
AR<-as.character(AR) #Transform filter into caracter

ActiveR_Key = subset(Reporter_Keywords, Reporter_Keywords$Reporter %in% AR) 
ActiveR_Key<-table(ActiveR_Key$Keywords,ActiveR_Key$Reporter)
dim(ActiveR_Key)# Find dimensions
ActiveR_Sum_Keys<-rowSums(ActiveR_Key[1:229,1:34])# Sum of Keywords
ActiveR_Sum_Keys<-as.data.frame(as.table(ActiveR_Sum_Keys)) # Transform to a data frame
ActiveR_Sum_Keys<- ActiveR_Sum_Keys[order(ActiveR_Sum_Keys$Freq, decreasing = TRUE),] #Ranking Keywords

Most_Key_AR = filter(ActiveR_Sum_Keys,ActiveR_Sum_Keys$Freq>10)
Most_Key_AR# Filter the keywords with more than 10 frequency

Most_Key_MR

#1.2 Analyse Keywords used for Median reporters
nr<-nrow(MedianReporters)#Quantity of Median Reporters
MR<-MedianReporters[1:nr,1:1] #Select array of median reporters names to filter 
MR<-as.character(MR) #Transform filter into caracter

MedianR_Key = subset(Reporter_Keywords, Reporter_Keywords$Reporter %in% MR) 
MedianR_Key<-table(MedianR_Key$Keywords,MedianR_Key$Reporter)
dim(MedianR_Key)# Find dimensions
MedianR_Sum_Keys<-rowSums(MedianR_Key[1:76,1:36])# Sum of Keywords
MedianR_Sum_Keys<-as.data.frame(as.table(MedianR_Sum_Keys)) # Transform to a data frame
MedianR_Sum_Keys<- MedianR_Sum_Keys[order(MedianR_Sum_Keys$Freq, decreasing = TRUE),] #Ranking Keywords

Most_Key_MR = filter(MedianR_Sum_Keys,MedianR_Sum_Keys$Freq>2)
Most_Key_MR# Filter the keywords with more than 10 frequency

spread(Most_Key_MR, Var1, Freq)

par(mfrow=c(1,2))
barplot(Most_Key_AR$Freq, las = 2, names.arg = Most_Key_AR$Var1,
        col ="lightblue", main ="Freq. words of Active Reporters",
        ylab = "Word frequencies")
barplot(Most_Key_MR$Freq, las = 2, names.arg = Most_Key_MR$Var1,
        col ="lightgreen", main ="Freq. words of Median Reporters",
        ylab = "Word frequencies")









pairs(CompType, diag.panel = panel.hist, upper.panel = panel.cor)
CompType<-table(TicketW$Component)

pairs(CompType, diag.panel = panel.hist, upper.panel = panel.cor)
CompStatus<-table(TicketW$Component,TicketW$Status)


        
        
        
        
        
    
   



