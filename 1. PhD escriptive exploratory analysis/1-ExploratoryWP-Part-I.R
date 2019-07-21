library(dplyr)
library(plotly)
library(googleVis)

#READ DATA
library(readr)
TicketW <- read_csv('~/PhD Analysis/1. PhD escriptive exploratory analysis/TicketW.csv')
View(TicketW)

#1. EXPLORATORY ANALYSIS - UNIAVARIABLE ANALYSIS
# Goal - Find which variables have hight variability and a line of cut, in order to use into Bivariate Analysis
dim(TicketW) #dimension
TicketW[1:5,]  #5 fist lines
summary(TicketW)
glimpse(TicketW)

#1.1 Variables related with members
# Var1
Status<-table(TicketW$Status)
# Transform into table
Status<-as.data.frame(as.table(Status))
Status
prop.table(Status$Freq)
#Graphics
pie(Status$Freq, main="Frequency of Tickets Status", label=Status$Var1, col = rainbow(7))

#Var2
TType<-table(TicketW$Type)
# Transform into table
TType<-as.data.frame(as.table(TType))
TType
#Graphics
pie(TType$Freq, main="Frequency of Tickets Type", label=TType$Var1, col = rainbow(7))

#Var3
Priority<-table(TicketW$Priority)
# Transform into table
Priority<-as.data.frame(as.table(Priority))
Priority
#Graphics
pie(Priority$Freq, main="Frequency of Tickets Priority", label=Priority$Var1, col = rainbow(7))

#Var4
Milestone<-table(TicketW$Milestone)
# Transform into table
Milestone<-as.data.frame(as.table(Milestone))
Milestone
#Graphics
pie(Milestone$Freq, main="Frequency of Tickets Milestone", label=Milestone$Var1, col = rainbow(7))

#Var5
Component<-table(TicketW$Component)
# Transform into table
Component<-as.data.frame(as.table(Component))
c1<-Component[order(Component$Freq, decreasing = TRUE),]
Component = filter(Component, Freq>100)
sum(Component$Freq)
#Graphics
barplot(Component$Freq, 
        xlab = "Tickets", 
        ylab = "Components", 
        main="Frequency of Tickets Component", 
        col = rainbow(5),
        legend.text = Component$Var1,
        horiz=TRUE,
        args.legend = list("bottom", bty="n", cex = 1))

#Var6
Severity<-table(TicketW$Severity)
# Transform into table
Severity<-as.data.frame(as.table(Severity))
Severity
#Graphics
pie(Severity$Freq, main="Frequency of Tickets Severity", label=Severity$Var1, col = rainbow(7))

#Var6
Focuses<-table(TicketW$Focuses)
# Transform into table
Focuses<-as.data.frame(as.table(Focuses))
f1<-Focuses[order(Focuses$Freq, decreasing = TRUE),]
Focuses = filter(Focuses, Freq>10)
sum(Focuses$Freq)
#Graphics
barplot(Focuses$Freq, 
        xlab = "Tickets", 
        ylab = "Focuses", 
        main="Frequency of Tickets Focuses", 
        col = rainbow(13),
        legend.text = Focuses$Var1,
        horiz=TRUE,
        args.legend = list("bottom", bty="n", cex = 1))
#Var7
Keywords<-table(TicketW$Keywords)
# Transform into table
Keywords<-as.data.frame(as.table(Keywords))
k1<-Keywords[order(Keywords$Freq, decreasing = TRUE),]
Keywords = filter(Keywords, Freq>9)
Keywords = filter(Keywords, Freq>40)
sum(Keywords$Freq)
#Graphics
barplot(Keywords$Freq,
        xlab = "Tickets", 
        ylab = "Keywords", 
        main="Frequency of Tickets Keywords", 
        col = rainbow(13),
        legend.text = Keywords$Var1,
        horiz=TRUE,
        args.legend = list("bottom", bty="n", cex = 1))

#1.2 Var Reporter
# Reporters are WordPress Community members who find and report a problem from WP Platform, into a Ticket.
# Transform into table
Reporter<-table(TicketW$Reporter) 
Reporter<-as.data.frame(as.table(Reporter))
summary(Reporter)
totalReporter<-nrow(Reporter)
totalReporter
Reporter[c(1:30),c(1:2)]

#1.2.1 Filter the most active group of reporters.  
ActiveReporters = filter(Reporter,Freq>10)
totalAR<-nrow(ActiveReporters)
totalAR #Total members of active reporters group

RankAR <- ActiveReporters[order(ActiveReporters$Freq, decreasing = TRUE),]
RankAR #Ranking the most active reporters
label<-names(summary(RankAR$Freq))
summary(RankAR$Freq)

#1.2.2 Filter the median active group of reporters
MedianReporters = filter(Reporter,Freq<10 & Freq>4)
totalMR<-nrow(MedianReporters)
MedianReporters
totalMR

#Ranking the Median active reporters
RankMR <- MedianReporters[order(MedianReporters$Freq, decreasing = TRUE),]
RankMR

#Filter the less active group of Report
LessReporters = filter(Reporter,Freq<4)
totalLR<-nrow(LessReporters)

#Ranking the less active Reporters
Ranklr <- LessReporters[order(LessReporters$Freq, decreasing = TRUE),]
Ranklr[c(1:30),c(1:2)]

#Grafic of Reporters
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
        args.legend = list("bottom", bty="n", cex = 0.7))#Top 5 Active Reporters

barplot(RankMR$Freq[1:5],
        names.arg=RankMR$Var1[1:5],
        horiz=TRUE,
        xlab="Tickets per Reporter",
        ylab="Reporters",
        col=rainbow(5),
        main="Top 5 Median Reporters",
        border="blue",
        legend.text = RankMR$Var1[1:5],
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


#1.3 Var Owner
# Owners are WordPress Community members who pick up a ticket from WP Platform (sended by a reporter) in order to solve it.
# Transform into table
Owner<-table(TicketW$Owner) 
Owner<-as.data.frame(as.table(Owner))
Owner[c(1:30),c(1:2)]
summary(Owner)
totalOwner<-nrow(Owner)

# Filter the most active group of Owners
ActiveOwner = filter(Owner,Freq>10)
totalAO<-nrow(ActiveOwner)

#Ranking the most active Owners
RankAO <- ActiveOwner[order(ActiveOwner$Freq, decreasing = TRUE),]
RankAO

#Filter the median active group of Owners
MedianOwner = filter(Owner,Freq<10 & Freq>4)
totalMO<-nrow(MedianOwner)

#Ranking the Median Owners
RankMO <- MedianOwner[order(MedianOwner$Freq, decreasing = TRUE),]
RankMO

#Filter the less active group of Owners
LessOwner = filter(Owner,Freq<4)
totalLO<-nrow(LessOwner)


#Ranking the less active Owners
RanklO <- LessOwner[order(LessOwner$Freq, decreasing = TRUE),]
RanklO[c(1:30),c(1:2)]

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

#1.4 Sum of groups by Reporters and Owners
#Total members per Reporter
MembersTotalR<-c(totalReporter,totalAR,totalMR,totalLR)
CoreGroupR<-c("Reporters","Active Reporters","Median Reporters","Alien Reporters")
PropMR<-prop.table(MembersTotalR)
WPCGroupR<-data.frame(CoreGroupR,MembersTotalR,PropMR)
print('Group Types of Reporters')
WPCGroupR

#Total members per Owner
MembersTotalO<-c(totalOwner,totalAO,totalMO,totalLO)
CoreGroupO<-c("Owners","Active Owners","Median Owners","Alien Owners")
PropMO<-prop.table(MembersTotalO)
WPCGroupO<-data.frame(CoreGroupO,MembersTotalO,PropMO)
print('Group Types of Owners')
WPCGroupO

par(mfrow=c(2,2))
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
        col=rainbow(4),
        main="Owners per Groups",border="red")

#1.5 Potential indicators
# Kendal
cor(WPCGroupO$MembersTotal, WPCGroupR$MembersTotal, method = "kendall")
cor.test(WPCGroupO$MembersTotal, WPCGroupR$MembersTotal, method = "kendall")