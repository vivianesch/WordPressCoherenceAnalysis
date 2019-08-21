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

select(WPTicket, ip_id, Reporter)
select(WPTicket, country:r_arch)
select(WPTicket, r_arch:country)
select(WPTicket, -time)
select(WPTicket, -(X:time))

#Function select rows you want - Filter()
filter(WPTicket, package == "swirl")
filter(cran, r_version == "3.1.1", country == "US")
filter(cran, country == "IN", r_version <= "3.0.2")
filter(cran, country =="US" | country == "IN")
filter(cran, size > 100500, r_os == "linux-gnu")
filter(cran, !is.na(r_version))

#Function order colunms you want - arrange()
cran2 <- select(cran, size:ip_id)
arrange(cran2, ip_id)
arrange(cran2, desc(ip_id))
arrange(cran2, package, ip_id)
arrange(cran2, country, desc(r_version), ip_id)

# Function include colunms with new data you want to calculet - mutate()
cran3 <- select(cran, ip_id, package, size)
mutate(cran3, size_mb = size / 2^20, size_gb = size_mb / 2^10)
mutate(cran3, correct_size = size + 1000)


# Function collapses the dataset to a single row - summarise()
sumarise(cran, avg_bytes = mean(size))










