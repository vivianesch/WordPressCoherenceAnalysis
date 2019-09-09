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

# Var1
Status<-table(TicketW$Status)
# Transform into table
Status<-as.data.frame(as.table(Status))
PropStatus<-prop.table(Status$Freq) #Proportion
Status<-data.frame(Status,PropStatus)

#Var2
TType<-table(TicketW$Type)
# Transform into table
TType<-as.data.frame(as.table(TType))
PropTType<-prop.table(TType$Freq) #Proportion
TType<-data.frame(TType,PropTType)

#Var3
Priority<-table(TicketW$Priority)
# Transform into table
Priority<-as.data.frame(as.table(Priority))
PropPrio<-prop.table(Priority$Freq) #Proportion
Priority<-data.frame(Priority,PropPrio)

#Var4
Milestone<-table(TicketW$Milestone)
# Transform into table
Milestone<-as.data.frame(as.table(Milestone))
PropMiles<-prop.table(Milestone$Freq) #Proportion
Milestone<-data.frame(Milestone,PropMiles)


#Var5
Component<-table(TicketW$Component)
# Transform into table
Component<-as.data.frame(as.table(Component))
c1<-Component[order(Component$Freq, decreasing = TRUE),]
Component = filter(Component, Freq>100)
sum(Component$Freq)
PropComp<-prop.table(Component$Freq) #Proportion
Component<-data.frame(Component,PropComp)


#Var6
Severity<-table(TicketW$Severity)
# Transform into table
Severity<-as.data.frame(as.table(Severity))
PropSever<-prop.table(Severity$Freq) #Proportion
Severity<-data.frame(Severity,PropSever)

#Var7
Focuses<-table(TicketW$Focuses)
# Transform into table
Focuses<-as.data.frame(as.table(Focuses))
f1<-Focuses[order(Focuses$Freq, decreasing = TRUE),]
Focuses = filter(Focuses, Freq>10)
PropFocus<-prop.table(Focuses$Freq) #Proportion
Focuses<-data.frame(Focuses,PropFocus)
sum(Focuses$Freq)

Keywords<-table(TicketW$Keywords)#Var8
# Transform into table
Keywords<-as.data.frame(as.table(Keywords))
k1<-Keywords[order(Keywords$Freq, decreasing = TRUE),]
Keywords = filter(Keywords, Freq>9)
Keywords = filter(Keywords, Freq>40)
Prop.Keyw<-prop.table(Keywords$Freq) #Proportion
Keywords<-data.frame(Keywords,Prop.Keyw)
sum(Keywords$Freq)


#1.2 Variable Members Analysis - Reporters:** Reporters are WordPress Community members who find and report a problem from WP Platform, into a Ticket.

# Goal:** Find type of report member groups (actives, medians, and aliens), and find a line of cut, in order to use into Bivariate Analysis.

# Transform into table
Reporter<-table(TicketW$Reporter) 
Reporter<-as.data.frame(as.table(Reporter))
summary(Reporter)
totalReporter<-nrow(Reporter)

#1.2.1 Filter the most active group of reporters (Which reporter with more than 10 tickets)
ActiveReporters = filter(Reporter,Freq>10)
totalAR<-nrow(ActiveReporters) #Total members of active reporters group
RankAR <- ActiveReporters[order(ActiveReporters$Freq, decreasing = TRUE),]
summary(RankAR$Freq)

#1.2.2 Filter the median active group of reporters (Which reporter with less than 10 tickets and more than 4 tickets)
MedianReporters = filter(Reporter,Freq<10 & Freq>4)
totalMR<-nrow(MedianReporters)
#Ranking the Median active reporters
RankMR <- MedianReporters[order(MedianReporters$Freq, decreasing = TRUE),]

#1.2.3 Filter the less active group of Report (Which reporter with less than 10 tickets)
LessReporters = filter(Reporter,Freq<4)
totalLR<-nrow(LessReporters)
#Ranking the less active Reporters
Ranklr <- LessReporters[order(LessReporters$Freq, decreasing = TRUE),]

#1.3 Variable Member Analysis - Owners:** Owners are WordPress Community members who pick up a ticket from WP Platform (sended by a reporter) in order to solve it.
#Goal:** Find type of owner member groups (actives, medians, and aliens), and find a line of cut, in order to use into Bivariate Analysis.

# Transform into table
Owner<-table(TicketW$Owner) 
Owner<-as.data.frame(as.table(Owner))
summary(Owner)
totalOwner<-nrow(Owner)

# 1.3.1 Filter the most active group of Owners (Which Owner with more than 10 tickets)
ActiveOwner = filter(Owner,Freq>10)
totalAO<-nrow(ActiveOwner)
#Ranking the most active Owners
RankAO <- ActiveOwner[order(ActiveOwner$Freq, decreasing = TRUE),]

#1.3.2 Filter the median active group of Owners (Which Owner with less than 10 tickets and more than 4 tickets)
MedianOwner = filter(Owner,Freq<10 & Freq>4)
totalMO<-nrow(MedianOwner)
#Ranking the Median Owners
RankMO <- MedianOwner[order(MedianOwner$Freq, decreasing = TRUE),]

#1.3.3 Filter the less active group of Owners (Which Owner with less than 10 tickets)
LessOwner = filter(Owner,Freq<4)
totalLO<-nrow(LessOwner)
#Ranking the less active Owners
RanklO <- LessOwner[order(LessOwner$Freq, decreasing = TRUE),]

#1.4 Sum of groups by Reporters and Owners:** 
        
#Total members per Reporter
MembersTotalR<-c(totalReporter,totalAR,totalMR,totalLR)
CoreGroupR<-c("Reporters","Active Reporters","Median Reporters","Alien Reporters")
PropMR<-prop.table(MembersTotalR)# Proportion
WPCGroupR<-data.frame(CoreGroupR,MembersTotalR,PropMR)

#Total members per Owner
MembersTotalO<-c(totalOwner,totalAO,totalMO,totalLO)
CoreGroupO<-c("Owners","Active Owners","Median Owners","Alien Owners")
PropMO<-prop.table(MembersTotalO) #Proportion
WPCGroupO<-data.frame(CoreGroupO,MembersTotalO,PropMO)

#2. DESCRIPTION ANALYSIS REPORT :** 
# 2.1  Variables related with members Report:** Variables selected to bivariate analysis are Component(+100 tickets per level), Focuses (+1 Ticket per level), Keywords(+9 tickets per level), Type, Status.

Status#Var1
#Graphics
pie(Status$Freq, main="Frequency of Tickets Status", label=Status$Var1, col = rainbow(7))

TType# Var2
#Graphics
pie(TType$Freq, main="Frequency of Tickets Type", label=TType$Var1, col = rainbow(7))

Priority# Var3
#Graphics
pie(Priority$Freq, main="Frequency of Tickets Priority", label=Priority$Var1, col = rainbow(7))

Milestone#Var4
#Graphics
pie(Milestone$Freq, main="Frequency of Tickets Milestone", label=Milestone$Var1, col = rainbow(7))

Component#Var5
#Graphics
barplot(Component$Freq, 
        xlab = "Tickets", 
        ylab = "Components", 
        main="Frequency of Tickets Component", 
        col = rainbow(5),
        legend.text = Component$Var1,
        horiz=TRUE,
        args.legend = list("bottom", bty="n", cex = 1))

Severity #Var6
#Graphics
pie(Severity$Freq, main="Frequency of Tickets Severity", label=Severity$Var1, col = rainbow(7))

Focuses#Var7
#Graphics
barplot(Focuses$Freq, 
        xlab = "Tickets", 
        ylab = "Focuses", 
        main="Frequency of Tickets Focuses", 
        col = rainbow(13),
        legend.text = Focuses$Var1,
        horiz=TRUE,
        args.legend = list("bottom", bty="n", cex = 1))

Keywords#Var8
#Graphics
barplot(Keywords$Freq,
        xlab = "Tickets", 
        ylab = "Keywords", 
        main="Frequency of Tickets Keywords", 
        col = rainbow(13),
        legend.text = Keywords$Var1,
        horiz=TRUE,
        args.legend = list("bottom", bty="n", cex = 1))

#2.2 Reporter Members Analysis Report** The groups of Reporters:
        
RankAR #Ranking the most active reporters
RankMR #Ranking the Median active reporters
Ranklr[c(1:30),c(1:2)] #Ranking the 30 less active Reporters

#Grafic of Top 5 active Reporters
par(mfrow=c(1,3))
barplot(RankAR$Freq[1:5],
        names.arg=RankAR$Var1[1:5],
        horiz=TRUE,
        xlab="Tickets per Reporter",
        ylab="Reporters",
        col=rainbow(5),
        main="Top 5 active Reporters",
        border="blue",
        legend.text = RankAR$Var1[1:5],
        args.legend = list("bottom", bty="n", cex = 0.8))#Top 5 Active Reporters

barplot(RankMR$Freq[1:5],
        names.arg=RankMR$Var1[1:5],
        horiz=TRUE,
        xlab="Tickets per Reporter",
        ylab="Reporters",
        col=rainbow(5),
        main="Top 5 Median Reporters",
        border="blue",
        legend.text = RankMR $Var1[1:5],
        args.legend = list("bottom", bty="n", cex = 0.7))#Top 5 Median Reporters

barplot(Ranklr$Freq[1:5],
        names.arg=Ranklr$Var1[1:5],
        horiz=TRUE,
        xlab="Tickets per Reporter",
        ylab="Reporters",
        col=rainbow(5),
        main="Top 5 Alien Reporters",
        border="blue",
        legend.text = Ranklr$Var1[1:5],
        args.legend = list("bottom", bty="n", cex = 0.7))#Top 5 Less acctive Reporters

#2.3 Owner Members Analysis Report:** The groups of Owners:

RankAO#Ranking the most active Owners
RankMO#Ranking the Median Owners
RanklO[c(1:30),c(1:2)]#Ranking the 30 less active Owners

#Grafic of Owners
par(mfrow=c(1,3))
barplot(RankAO$Freq[1:5],
        names.arg=RankAO$Var1[1:5],
        horiz=TRUE,
        xlab="Tickets per owner",
        ylab="Owners",
        col=rainbow(5),
        main="Top 5 active Owners",
        border="blue",
        legend.text = RankAO$Var1[1:5],
        args.legend = list("bottom", bty="n", cex = 0.7))#Top 5 Active Owners

barplot(RankMO$Freq[1:5],
        names.arg=RankMO$Var1[1:5],
        horiz=TRUE,
        xlab="Tickets per owner",
        ylab="Owners",
        col=rainbow(5),
        main="Top 5 median Owners",
        border="blue",
        legend.text = RankMO$Var1[1:5],
        args.legend = list("bottom", bty="n", cex = 0.7))#Top 5 Median Owners

barplot(RanklO$Freq[1:5],
        names.arg=RanklO$Var1[1:5],
        horiz=TRUE,
        xlab="Tickets per owner",
        ylab="Owners",
        col=rainbow(5),
        main="The 5 less active Owners",
        border="blue",
        legend.text = RanklO$Var1[1:5],
        args.legend = list("bottom", bty="n", cex = 0.7))#Top 5 Less Active Owners
#2.4 Comparation of groups by Reporters and Owners:**
 
WPCGroupR #Group Types of Reporters
WPCGroupO #Group Types of Owners

par(mfrow=c(1,2))
barplot(WPCGroupR$MembersTotal,
        names.arg=WPCGroupR$MembersTotal,
        xlab="Reporter Groups",
        ylab="Total Members",
        legend=WPCGroupR$CoreGroupR,
        col=rainbow(8),
        main="Reporters per Groups",border="red")

barplot(WPCGroupO$MembersTotal,
        names.arg=WPCGroupO$MembersTotal,
        xlab="Owners Groups",
        ylab="Total Members",
        legend=WPCGroupO$CoreGroupO,
        col=rainbow(8),
        main="Owners per Groups",border="red")

similarityOR<-ifelse(PropMO>PropMR,1-(PropMO-PropMR),1-(PropMR-PropMO))
similarityOR<-mean(similarityOR)
similarityOR #Means: percentual of tickets into any subgroups into Owner Groups are similar thand percentual of tickets into any subgroups into Reporter Groups
#Where: 
#PropMo is the proportion of tickets per active, median, and alien groups of Owners, and
#PropMR is the proportion of tickets per active, median, and alien groups of Reporters.

