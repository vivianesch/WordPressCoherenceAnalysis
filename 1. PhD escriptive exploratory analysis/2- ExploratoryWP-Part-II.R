#Install
install.packages("dplyr")
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
#Load
library(readr)
library(dplyr)
library(plotly)
library(markdown)
library(ggplot2)
library(lattice)
library(tidyr)
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library(data.table)


TicketW <- read_csv('~/PhD Analysis/1. PhD escriptive exploratory analysis/TicketW.csv')
View(TicketW) # Read 
glimpse(TicketW)


#Bivariable analysis
#Functions
    #Function .List.f.filter() make variable set that will be filtered
    .List.f.filter = function(x){
        nr<-nrow(x)#Quantity of members
        y<-x[1:nr,1:1] #Select array of members names to filter 
        z<-as.character(y) #Transform names into caracter array
        return(z)
    }
    
     #Function .List.f.filter() make variable set that will be filtered
    .List.w.Group = function(x,y,f){
        z = subset(x, y %in% f) 
        w<-table(z[ , 1:1],z[ ,2:2])
        w<-rowSums(w)
        w<-as.data.frame(as.table(w)) # Transform to a data frame
        w<- w[order(w[ ,2:2], decreasing = TRUE),] #Ranking Keywords
          return(w)
    } 
    
    LessR_Key = subset(Reporter_Keywords, Reporter_Keywords$Reporter %in% LR) 
    LessR_Key<-table(LessR_Key$Keywords,LessR_Key$Reporter)
    dim(MedianR_Key)# Find dimensions
    LessR_Sum_Keys<-rowSums(LessR_Key[1:76,1:36])# Sum of Keywords
    LessR_Sum_Keys<-as.data.frame(as.table(LessR_Sum_Keys)) # Transform to a data frame
    LessR_Sum_Keys<- LessR_Sum_Keys[order(LessR_Sum_Keys$Freq, decreasing = TRUE),] #Ranking Keywords
    
    Most_Key_LR = filter(LessR_Sum_Keys,LessR_Sum_Keys$Freq>=1)
    Most_Key_LR# Filter the keywords with more than 10 frequency
    
    
    
    .List.w.Group(Reporter_Keywords, Reporter_Keywords$Reporter,MR) 
    
    #1.1 Analyse Keywords used for Active reporters
    MR<-.List.f.filter(ActiveReporters)
    ActiveR_Key = subset(Reporter_Keywords, Reporter_Keywords$Reporter %in% MR) 
    ActiveR_Sum_Key<-table(ActiveR_Key$Keywords,ActiveR_Key$Reporter)
    ActiveR_Sum_Key<-as.data.frame(as.table(ActiveR_Sum_Key)) # Transform to a data frame
    ActiveR_Sum_Key<- ActiveR_Sum_Key[order(ActiveR_Sum_Key$Freq, decreasing = TRUE),] #Ranking Keywords
    Most_Key_AR = filter(ActiveR_Sum_Key,ActiveR_Sum_Key$Freq > 4)
   
    
  #1. Analyse Reporters and Keywords data
    Reporter_Keywords<- TicketW %>% select(Reporter,Keywords) #Table of Reporters and Keywords
    summary(Reporter_Keywords)
    dim(Reporter_Keywords)
    glimpse(Reporter_Keywords)
    rownames(Reporter_Keywords)
    colnames(Reporter_Keywords)

#1.1 Analyse Keywords used for Active reporters
MR<-.List.f.filter(ActiveReporters)
ActiveR_Key = subset(Reporter_Keywords, Reporter_Keywords$Reporter %in% MR) 
ActiveR_Sum_Key<-table(ActiveR_Key$Keywords,ActiveR_Key$Reporter)
ActiveR_Sum_Key<-as.data.frame(as.table(ActiveR_Sum_Key)) # Transform to a data frame
ActiveR_Sum_Key<- ActiveR_Sum_Key[order(ActiveR_Sum_Key$Freq, decreasing = TRUE),] #Ranking Keywords
Most_Key_AR = filter(ActiveR_Sum_Key,ActiveR_Sum_Key$Freq>4)
Most_Key_AR<-ActiveR_Sum_Key[Freq > 4, .N]# Filter the keywords with more than 10 frequency
   
    
#1.2 Analyse Keywords used for Median reporters
MR<-.List.f.filter(MedianReporters)
MedianR_Key = subset(Reporter_Keywords, Reporter_Keywords$Reporter %in% MR) 
MedianR_Key<-table(MedianR_Key$Keywords,MedianR_Key$Reporter)
MedianR_Sum_Key<-as.data.frame(as.table(MedianR_Key)) # Transform to a data frame
MedianR_Sum_Key<- MedianR_Sum_Key[order(MedianR_Sum_Key$Freq, decreasing = TRUE),] #Ranking Keywords
Most_Key_MR = filter(MedianR_Sum_Key,MedianR_Sum_Key$Freq>2)# Filter the keywords with more than 10 frequency
Most_Key_MR#Table


#1.3 Analyse Keywords used for Median reporters
MR<-.List.f.filter(LessReporters)
LessR_Key = subset(Reporter_Keywords, Reporter_Keywords$Reporter %in% MR) 
LessR_Key<-table(LessR_Key$Keywords,LessR_Key$Reporter)
LessR_Sum_Keys<-as.data.frame(as.table(LessR_Key)) # Transform to a data frame
LessR_Sum_Keys<- LessR_Sum_Keys[order(LessR_Sum_Keys$Freq, decreasing = TRUE),] #Ranking Keywords
Most_Key_LR = filter(LessR_Sum_Keys,LessR_Sum_Keys$Freq>1)# Filter the keywords with more than 10 frequency
Most_Key_LR#Table

par(mfrow=c(2,3))
    #Graphics of most frequent group words 
    barplot(Most_Key_AR$Freq, las = 2, names.arg = Most_Key_AR$Var1,
            col ="lightblue", main ="Freq. words of Active Reporters",
            ylab = "Word frequencies")
    barplot(Most_Key_MR$Freq, las = 2, names.arg = Most_Key_AR$Var1,
            col ="lightgreen", main ="Freq. words of Median Reporters",
            ylab = "Word frequencies")
    barplot(Most_Key_MR$Freq, las = 2, names.arg = Most_Key_MR$Var1,
            col ="lightgreen", main ="Freq. words of Median Reporters",
            ylab = "Word frequencies")
    #Graphics of most frequent group words 
    set.seed(12)
    wordcloud(words = Most_Key_MR$Var1, freq = Most_Key_LR$Freq, min.freq = 1,
              max.words=200, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"))
    set.seed(4)
    wordcloud(words = Most_Key_MR$Var1, freq = Most_Key_MR$Freq, min.freq = 1,
              max.words=200, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"))
    set.seed(12)
    wordcloud(words = Most_Key_LR$Var1, freq = Most_Key_LR$Freq, min.freq = 1,
              max.words=200, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"))






pairs(CompType, diag.panel = panel.hist, upper.panel = panel.cor)
CompType<-table(TicketW$Component)

pairs(CompType, diag.panel = panel.hist, upper.panel = panel.cor)
CompStatus<-table(TicketW$Component,TicketW$Status)





        
        
        
ActiveR_Key<-table(ActiveR_Key$Keywords,ActiveR_Key$Reporter)
dim(ActiveR_Key)# Find dimensions

ActiveR_Sum_Keys<-rowSums(ActiveR_Key[1:229,1:34])# Sum of Keywords
ActiveR_Sum_Keys<-as.data.frame(as.table(ActiveR_Sum_Keys)) # Transform to a data frame
ActiveR_Sum_Keys<- ActiveR_Sum_Keys[order(ActiveR_Sum_Keys$Freq, decreasing = TRUE),] #Ranking Keywords

Most_Key_AR = filter(ActiveR_Sum_Keys,ActiveR_Sum_Keys$Freq>10)
Most_Key_AR# Filter the keywords with more than 10 frequency

        
    
   



