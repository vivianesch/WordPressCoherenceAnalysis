# Data manipulation tasks: select(), filter(), arrange(), mutate(), and summarize().

#Read
library(readr)
TicketW <- read_csv("TicketW.csv")

#Look the data
library(dplyr)
packageVersion("dplyr")
dim(TicketW)
head(TicketW)

# Rum As a data frame
WPTicket <- tbl_df(TicketW)

#Remove duplicate dataframe
rm("TicketW")

#Function select colunms you want - Select()
?select

select(WPTicket, Owners, Reporter)
select(WPTicket, id:Owner)
select(WPTicket, Prioeity:Reporter)
select(WPTicket, -Created)
select(WPTicket, -(Type:Created))

#Function select rows you want - Filter()
filter(WPTicket, Reporter == "Johnbillion")
filter(WPTicket, Type == "defect (bug)", Milestone == "Awaiting Review")
filter(WPTicket, Component == "Media", Milestone == "5.3")
filter(WPTicket, Focuses =="ui, accessibility" | Milestone == "5.3")
filter(WPTicket, id > 36441, Type == "defect (bug)")
filter(WPTicket, !is.na(Owner))

#Function order colunms you want - arrange()
T_id_Comp <- select(WPTicket, id:Component, -Summary)
arrange(T_id_Comp, id)
arrange(T_id_Comp, desc(id))
arrange(T_id_Comp, Component, id)
arrange(T_id_Comp, Milestone, desc(Type), id)

# Function include colunms with new data you want to calculet - mutate()
WPTicket1 <- select(WPTicket, id, Component, Milestone)
mutate(cran3, size_mb = size / 2^20, size_gb = size_mb / 2^10)
mutate(cran3, correct_size = size + 1000)


# Function collapses the dataset to a single row - summarise()
sumarise(cran, avg_bytes = mean(size))










