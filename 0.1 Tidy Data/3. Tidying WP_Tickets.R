#Tidyng WP Data Tickets
#Read
library(readr)
TicketW <- read_csv("TicketW.csv")
library(tidyr)
library(dplyr)

by_component <-
  TicketW %>%
  group_by(Component) %>%
  summarize(count = n(),
            Id = n_distinct(id),
            Status = n_distinct(Status),
            Version = n_distinct(Version),
            Owner = n_distinct(Owner),
            Reporter = n_distinct(Reporter),
            Type = n_distinct(Type),
            Priority = n_distinct(Priority),
            Mileston = n_distinct(Milestone),
            Severity = n_distinct(Severity),
            Resolution = n_distinct(Resolution),
            Focuses = n_distinct(Focuses),
            Keywords = n_distinct(Keywords)
             ) %>%
  arrange(desc(count))
  View(by_component)

by_reporter <-
  TicketW %>%
  group_by(Reporter) %>%
  summarize(count = n(),
            Id = n_distinct(id),
            Role_Owner = n_distinct(Owner),
            Role_Reporter = count - Role_Owner,
            Status = n_distinct(Status),
            Version = n_distinct(Version),
            Type = n_distinct(Type),
            Priority = n_distinct(Priority),
            Mileston = n_distinct(Milestone),
            Component = n_distinct(Component),
            Severity = n_distinct(Severity),
            Resolution = n_distinct(Resolution),
            Focuses = n_distinct(Focuses),
            Keywords = n_distinct(Keywords)
  )  %>%
  arrange(desc(count))
  View(by_reporter)
  
by_owner <-
  TicketW %>%
  group_by(Owner) %>%
  summarize(count = n(),
            Id = n_distinct(id),
            Role_Reporter = n_distinct(Reporter),
            Role_Owner = count - Role_Reporter,
            Status = n_distinct(Status),
            Version = n_distinct(Version),
            Type = n_distinct(Type),
            Priority = n_distinct(Priority),
            Mileston = n_distinct(Milestone),
            Component = n_distinct(Component),
            Severity = n_distinct(Severity),
            Resolution = n_distinct(Resolution),
            Focuses = n_distinct(Focuses),
            Keywords = n_distinct(Keywords)
  ) %>%
arrange(desc(count))
View(by_owner)
 

