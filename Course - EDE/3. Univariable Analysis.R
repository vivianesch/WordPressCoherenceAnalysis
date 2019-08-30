library(readr)
library(dplyr)
library(plotly)


#READ DATA
library(readr)
TicketW <- read_csv('~/PhD Analysis/1. PhD escriptive exploratory analysis/TicketW.csv')
View(TicketW)

# EXPLORATORY ANALYSIS - UNIAVARIABLE ANALYSIS
# Goal - Find which variables have hight variability and a line of cut, in order to use into Bivariate Analysis
dim(TicketW) #dimension
TicketW[1:5,]  #5 fist lines
summary(TicketW)
glimpse(TicketW)

#1.1 Variables related with members Analysis**
#**Goal:** Find which variables have hight variability, and find a line of cut, in order to use into Bivariate Analysis.

# Transform vector into a data frame with frequency of levels and proportion
.Unianalysis = function (x) {
        y<-table(x)
        y<-as.data.frame(as.table(y))
        mutate(y, proportion = prop.table(y$Freq) * 100)
        return(y)
}

# Var1
Status<-.Unianalysis(TicketW$Status)

#Var2
TType<-.Unianalysis(TicketW$Type)

#Var3
Priority<-.Unianalysis(TicketW$Priority)

#Var4
Milestone<-.Unianalysis(TicketW$Milestone)

#Var5
Component<-.Unianalysis(TicketW$Component) 
Component = filter(Component, Freq>100) # Filter components with more than 100 tickets
sum(Component$Freq) #Total Tickets into most frequent components (more than 100 tickets)

#Var6
Severity<-.Unianalysis(TicketW$Severity)

#Var7
Focuses<-.Unianalysis(TicketW$Focuses)
Focuses = filter(Focuses, Freq>10) # Filter Focuses with more than 10 tickets
Focuses<-Focuses[order(Focuses$Freq, decreasing = TRUE),]
sum(Focuses$Freq) #Total Tickets into most frequent components (more than 100 tickets)

#Var8
Keywords<-.Unianalysis(TicketW$Keywords)
Keywords = filter(Keywords, Freq>8)
Keywords<-Keywords[order(Keywords$Freq, decreasing = TRUE),]
sum(Keywords$Freq)

#**Functions for establish groups of active (GroupActive()), median (GroupMedian()), or less active (GroupAlien()) members: **
#1. Filter the most active group
.GroupActive = function(x) {
        y = filter(x, Freq>10)
        x <- y[order(y$Freq, decreasing = TRUE),]
        return(x)
}
#2. Filter the median active group
.GroupMedian = function(x) {
        y = filter(x, Freq<10 & Freq>4)
        x <- y[order(y$Freq, decreasing = TRUE),]
        return(x)
}  
#3. Filter the less active group 
.GroupAlien = function(x) {
        y = filter(x, Freq<4)
        x <- y[order(y$Freq, decreasing = TRUE),]
        return(x)
}


#**1.2 Variable Members Analysis - Reporters:** Reporters are WordPress Community members who find and report a problem from WP Platform, into a Ticket.

#**Goal:** Find type of report member groups (actives, medians, and aliens), and find a line of cut, in order to use into Bivariate Analysis.

#Find Groups and quantity of people for each group

Reporter<-.Unianalysis(TicketW$Reporter) 
totalReporter<-nrow(Reporter)

ActiveReporters<-.GroupActive(Reporter)
totalAR<-nrow(ActiveReporters)

MedianReporters<-.GroupMedian(Reporter)
totalMR<-nrow(MedianReporters)

LessReporters<-.GroupAlien(Reporter)
totalLR<-nrow(LessReporters)

#**1.3 Variable Member Analysis - Owners:** Owners are WordPress Community members who pick up a ticket from WP Platform (sended by a reporter) in order to solve it.


#**Goal:** Find type of owner member groups (actives, medians, and aliens), and find a line of cut, in order to use into Bivariate Analysis.

Owner<-.Unianalysis(TicketW$Owner) 
totalOwner<-nrow(Owner)

#Find Groups and quantity of people for each group

ActiveOwner<-.GroupActive(Owner)
totalAO<-nrow(ActiveOwner)

MedianOwner<-.GroupMedian(Owner)
totalMO<-nrow(MedianOwner)

LessOwner<-.GroupAlien(Owner)
totalLO<-nrow(LessOwner)


#**1.4 Sum of groups by Reporters and Owners:** 
        
#Total members per Reporter
MembersTotalR<-c(totalReporter,totalAR,totalMR,totalLR)
CoreGroupR<-c("Reporters","Active R","Median R","Alien R")
PropMR<-prop.table(MembersTotalR)# Proportion
WPCGroupR<-data.frame(CoreGroupR,MembersTotalR,PropMR)

#Total members per Owner
MembersTotalO<-c(totalOwner,totalAO,totalMO,totalLO)
CoreGroupO<-c("Owners","Active O","Median O","Alien O")
PropMO<-prop.table(MembersTotalO) #Proportion
WPCGroupO<-data.frame(CoreGroupO,MembersTotalO,PropMO)


#**2. DESCRIPTION ANALYSIS REPORT :** 
        
 #**2.1  Variables related with members Report:** Variables selected to bivariate analysis are Component(+100 tickets per level), Focuses (+1 Ticket per level), Keywords(+9 tickets per level), Type, Status.

Status#Var1
#Graphics
pie(Status$Freq, main="Frequency of Tickets Status", label=Status$x, col = rainbow(7))

TType# Var2
#Graphics
pie(TType$Freq, main="Frequency of Tickets Type", label=TType$x, col = rainbow(7))

Priority# Var3
#Graphics
barplot(Priority$Freq, las = 2, names.arg = Priority$x,
        col =rainbow(7), main ="Frequency of Tickets Priority",
        ylab = "Priority frequency")

Milestone#Var4
#Graphics
barplot(Milestone$Freq, las = 2, names.arg = Milestone$x,
        col =rainbow(7), main ="Frequency of Tickets Milestone",
        ylab = "Milestone frequency")

Component#Var5
#Graphics
barplot(Component$Freq, las = 2, names.arg = Component$x,
        col = "aquamarine", main ="Most frequent Components",
        ylab = "Component frequencies")

Severity #Var6
#Graphics
barplot(Severity$Freq, las = 2, names.arg = Severity$x,
        col = rainbow(7), main ="Frequency of Tickets Severity",
        ylab = "Severity frequencies")

Focuses#Var7
#Graphics
barplot(Focuses$Freq, las = 2, names.arg = Focuses$x,
        col ="lightblue", main ="Frequency of Tickets Focuses",
        ylab = "Focuses frequencies")

Keywords#Var8
#Graphics
barplot(Keywords$Freq, las = 2, names.arg = Keywords$x,
        col ="orangered", main ="Frequency of Tickets Keywords",
        ylab = "Keywords frequencies")

#2.2 Reporter Members Analysis Report** The groups of Reporters:
        
ActiveReporters #Ranking the most active reporters
MedianReporters #Ranking the Median active reporters
LessReporters[c(1:30),c(1:2)] #Ranking the 30 less active Reporters

#Grafic of Top 5 active Reporters
par(mfrow=c(1,3))
barplot(ActiveReporters$Freq[1:5], las = 2, names.arg = ActiveReporters$x[1:5],
        col ="navyblue", main ="Top 5 active Reporters",
        ylab = "Tickets per Reporter")

#Top 5 Median Reporters
barplot(MedianReporters$Freq[1:5], las = 2, names.arg = MedianReporters$x[1:5], 
        col ="orange4", main ="Top 5 median Reporters",
        ylab = "Tickets per Reporter")

#Top 5 Less acctive Reporters
barplot(LessReporters$Freq[1:5], las = 2, names.arg = LessReporters$x[1:5],
        col ="magenta4", main ="Top 5 less active Reporters",
        ylab = "Tickets per Reporter")

#2.3 Owner Members Analysis Report:** The groups of Owners:
        
      
ActiveOwner#Ranking the most active Owners
MedianOwner#Ranking the Median Owners
LessOwner[c(1:30),c(1:2)]#Ranking the 30 less active Owners

#Grafic of Top 5 active Owners
par(mfrow=c(1,3))
barplot(ActiveOwner$Freq[1:5], las = 2, names.arg = ActiveOwner$x[1:5],
        col ="navyblue", main ="Top 5 active Owners",
        ylab = "Tickets per Owner")

#Top 5 Median Owners
barplot(MedianOwner$Freq[1:5], las = 2, names.arg = MedianOwner$x[1:5], 
        col ="orange4", main ="Top 5 median Owners",
        ylab = "Tickets per Ownerr")

#Top 5 Less acctive Owners
barplot(LessOwner$Freq[1:5], las = 2, names.arg = LessOwner$x[1:5],
        col ="magenta4", main ="Top 5 less active Owners",
        ylab = "Tickets per Owner")



#2.4 Comparation of groups by Reporters and Owners:**
        
WPCGroupR #Group Types of Reporters
WPCGroupO #Group Types of Owners

par(mfrow=c(1,2))
barplot(WPCGroupR$MembersTotal,  las = 2,
        names.arg=WPCGroupR$CoreGroupR,
        ylab="Total Members",
        col=rainbow(4),
        main="Reporters per Groups",border="red")

barplot(WPCGroupO$MembersTotal,  las = 2,
        names.arg=WPCGroupO$CoreGroupO,
        ylab="Total Members",
        col=rainbow(15),
        main="Owners per Groups",border="red")


#**3. POTENTIAL INDICATORS:** 

similarityOR<-ifelse(PropMO>PropMR,1-(PropMO-PropMR),1-(PropMR-PropMO))
similarityOR<-mean(similarityOR)
similarityOR #Means: percentual of tickets into any subgroups into Owner Groups are similar thand percentual of tickets into any subgroups into Reporter Groups
#Where: 
#PropMo is the proportion of tickets per active, median, and alien groups of Owners, and
#PropMR is the proportion of tickets per active, median, and alien groups of Reporters.


## Notes
# Comparing two ways to make univariable tables

Component_t <-
  TicketW %>%
  group_by(Component) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  mutate(proportion = prop.table(count) * 100) 

View(Component_t) 

.Unianalysis = function (x) {
  y<-table(x)
  y<-as.data.frame(as.table(y))
  y <- mutate(y, proportion = prop.table(y$Freq) * 100)
  y <- y[order(y$Freq, decreasing = TRUE),]
  return(y)
}

component_t2 <- .Unianalysis(TicketW$Component)
View(component_t2)
