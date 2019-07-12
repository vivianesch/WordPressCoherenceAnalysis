# Exploratory analyses for understand how Core WordPress Community coproduce code
# General Question: Wich are the atributes of coproduction (colunms of dataframe or variables)?
# What are the levels of whith atribute?
# Whith data are more important to figure out the dinamic of the community?

# Data come from https://core.trac.wordpress.org/query?status=accepted&status=assigned&status=new&status=reopened&status=reviewing&col=id&col=summary&col=status&col=owner&col=type&col=priority&col=milestone&col=component&col=version&col=severity&col=resolution&col=time&col=changetime&col=focuses&col=reporter&col=keywords&order=priority
# Collected 07/04/2019.

#1. Read data
library(readr)
  TicketW <- read_csv("PhD Analysis/1. PhD escriptive exploratory analysis/TicketW.csv")
  View(TicketW)

#2. Short analyses of data
  dim(TicketW) #dimension
  TicketW[1:5,]  #5 fist lines

 #Variable Status
  
  # Transform Variable into Table
  Status<-table(TicketW$Status) 
  Status<-as.data.frame(as.table(Status))
  
  # Summary of variable
  max(Status) #max value
  min(Status) #min value
  mean(Status)#mean value
  median(Status) #median value
  sd(Status) #standard deviation
  
  # Grafic of Variable
   pie(Status, col = c("purple","violetred1","green3","cornsilk","cyan"), main = "Tickets Status")
      
  #Variable Version
      
    # Transform Variable into Table
      Version<-table(TicketW$Version) 
      Version<-as.data.frame(as.table(Version))
      
     # Summary of variable
      max(Version) #max value
      min(Version) #min value
      mean(Version)#mean value
      median(Version) #median value
      sd(Version) #standard deviation
      
    #Variable Owner
      
      # Transform Variable into Table
      Owner<-table(TicketW$Owner) 
      Owner<-as.data.frame(as.table(Owner))
      
     T<- sum(Owner[,2])
     Owner["Percent"]<-Owner/T
      
      # Summary of variable
      max(Owner) #max value
      min(Owner) #min value
      mean(Owner)#mean value
      median(Owner) #median value
      sd(Owner) #standard deviation
      
      # Grafic of active owners
      ActiveOwner<-which(Owner>=10)
      pie(ActiveOwner, main = "Active Owners")
      as.data.frame(as.table(ActiveOwner))
      
      # Grafic of median owners
      MedianOwners<-which(Owner<10 & Owner>5)
      pie(MedianOwners, main = "Median Owners")
      as.data.frame(as.table(MedianOwners))
      
      
      # Variable Reporter
      
      # Transform Variable into Table
      Reporter<-table(TicketW$Reporter) 
      Reporter<-as.data.frame(as.table(Reporter))
      
      #Select only reporters have more than 9 Tickets
      ActiveReporters<-Reporter[which(Reporter$Freq>=10),]
     
      
      
      
      
      
  

  
  cbind("f" = table(TicketW$Owner))
  cbind("f" = table(TicketW$Type))
  cbind("f" = table(TicketW$Priority))
  cbind("f" = table(TicketW$Milestone))
  cbind("f" = table(TicketW$Component))
  cbind("f" = table(TicketW$Severity))
  cbind("f" = table(TicketW$Resolution))
  cbind("f" = table(TicketW$Created))
  cbind("f" = table(TicketW$Modified))
  cbind("f" = table(TicketW$Focuses))
  cbind("f" = table(TicketW$Reporter))
  cbind("f" = table(TicketW$Keywords))  
  
  
