#Tidyng WP Data Tickets
#Read
library(readr)
TicketW <- read_csv("TicketW.csv")
library(tidyr)
library(dplyr)
library(ggplot2)
library(GGally)
library(corrplot)
library(corrgram)
library(ppcor)

# Function for graphs
#função retirada do help(pairs) by Melina de Souza Leite 
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}

# by Melina de Souza Leite 
panel.lm <- function (x, y, col = par("col"), bg = NA, pch = par("pch"), 
                      cex = 1, col.line="red") {
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)) {
    abline(lm(y[ok]~x[ok]), col = col.line)
  }
}

#função retirada do help(pairs) by Melina de Souza Leite 
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

#Multivariable Analysis
#1. Reporter
by_reporter <-
  TicketW %>%
  group_by(Reporter) %>%
  summarize(count = n(),
            Role_Owner = n_distinct(Owner),
            Role_Reporter = count - Role_Owner,
            Component = n_distinct(Component),
            Focuses = n_distinct(Focuses),
            Keywords = n_distinct(Keywords),
            Status = n_distinct(Status),
            Type = n_distinct(Type),
            Priority = n_distinct(Priority),
            Mileston = n_distinct(Milestone),
            Severity = n_distinct(Severity),
            
        ) %>%
  arrange(desc(Role_Reporter))
  View(by_reporter)
  pairs(by_reporter[, 2:6], col = by_reporter$Role_Reporter)
  

  pairs(by_reporter [, 2:6],
        diag.panel = panel.hist,
        upper.panel = panel.cor,
        lower.panel = panel.lm)
  
  ggcorr(by_reporter [, 2:12], label=T)
  
  
  M <- cor(by_reporter [, 2:11])
  corrplot(M, method = "circle")
  
  #pair with hcluster
  corrplot(M, order = "hclust", addrect = 3, tl.pos="d")
  
  corrgram(by_reporter [, 2:7], 
           lower.panel = panel.pts, 
           upper.panel= panel.conf,
           diag.panel = panel.density)
  
  corrgram(by_reporter [, 2:12], order=TRUE, lower.panel=panel.shade,
           upper.panel=panel.pie, text.panel=panel.txt,
           main="Ticket")
  
  corrgram(by_reporter [, 2:12], order=TRUE, lower.panel=panel.ellipse,
           upper.panel=panel.pts, text.panel=panel.txt,
           diag.panel=panel.minmax, 
           main="Ticket")
  
  with(by_reporter [, 2:12], pcor.test(x=Role_Owner, y=Role_Reporter, z=Component))
  
  pcor(by_reporter [, 3:5])
  
  ggcorr(by_reporter [, 2:12], geom = "circle", nbreaks = 5)
   
#Conclusion - Need to realize a bivariable analysis with 2 groups: Components and Reporters
# and Components and Owners. Complement with Focuses and Keywords
  
#2. Owner  
by_owner <-
  TicketW %>%
  group_by(Owner) %>%
  summarize(count = n(),
            Role_Reporter = n_distinct(Reporter),
            Role_Owner = count - Role_Reporter,
            Component = n_distinct(Component),
            Focuses = n_distinct(Focuses),
            Keywords = n_distinct(Keywords),
            Status = n_distinct(Status),
            Type = n_distinct(Type),
            Priority = n_distinct(Priority),
            Mileston = n_distinct(Milestone),
            Severity = n_distinct(Severity),
  ) %>%
arrange(desc(Role_Owner))
View(by_owner)

M <- cor(by_owner [, 2:11])

corrplot(M, order = "hclust", addrect = 3, tl.pos="d")
 

#3. Component
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
            Focuses = n_distinct(Focuses),
            Keywords = n_distinct(Keywords)
  ) %>%
  arrange(desc(count))
View(by_component)

M <- cor(by_component [, 2:12])

corrplot(M, order = "hclust", addrect = 3, tl.pos="d")



