---
#  title: "IDS investigation worksheet"
#author: "by Team-Name: User1, User2, User3, User4 & User5"
#date: "`r Sys.Date()`"
#output: html_document
---
  
#  **Note:** You can use this file as you 'working document' where you can try out various investigation ideas and keep notes about your findings. How you use and structure this file is up to you. It is recommended that you keep notes about what you are investigating and what you find as this will make the process of creating your presentation and report easier. Please note that you _do not_ need to submit this file as part of your group project.
install.packages("tidyverse")

#```{r load-lib, message = FALSE}
library(tidyverse)
library(stringr)
library(readxl)
library(readr)
library(dplyr)

# Add any other libraries here



#```


#```{r load-data}
# load your data
AdjudicationsQ12023 <- read_csv("data/AdjudicationsQ12023.csv")
spec(AdjudicationsQ12023)

caseloadQ12023 <- read_csv("data/caseloadQ12023.csv")
spec(caseloadQ12023)

PunishmentsQ12023 <- read_excel("data/PunishmentsQ12023.xlsx")
spec(PunishmentsQ12023)

startsQ12023 <- read_csv("data/startsQ12023.csv")
spec(startsQ12023)

terminationsQ12023 <- read_csv("data/terminationsQ12023.csv")
spec(terminationsQ12023)
#```

#```{r cleaning data}
# remove N/As and sort cell values

#1 AdjudicationsQ12023

adj <- AdjudicationsQ12023 %>%
  rename(Security = `Predominant function of establishment`) %>%
  filter(Outcomes != "Other",
         Adjudicator != "Missing",
         Ethnicity != "d Other ethnic group",
         Religion != "g Other religious groups")%>%
  mutate(Ethnicity = str_sub(Ethnicity, 3), 
         Religion = str_sub(Religion, 3))

#2 CaseloadQ12023

case <- caseloadQ12023 %>%
  filter(`ORDER TYPE` != "Other sentences")

#3 PunishmentsQ12023
puni <-PunishmentsQ12023 %>%
  rename(Security = `Predominant function of establishment`) %>%
  filter(Adjudicator != "Missing",
         Ethnicity != "d Other ethnic group",
         Religion != "g Other religious groups", 
         Offence != "Other offences", 
         Punishment != "Others")%>%
  rename('No_of_days' = 'No of days') %>%
  mutate(Ethnicity = str_sub(Ethnicity, 3), 
         Religion = str_sub(Religion, 3), 
         No_of_days = case_when(
           No_of_days== "." ~ "0",
           TRUE ~ No_of_days))

#4 StartsQ12023

start <- startsQ12023 %>%
  filter(`ORDER TYPE` != "Other sentences",
         REGION != "Other")

#5 TerminationsQ12023

term <- terminationsQ12023 %>% 
  filter(`REASON` != "Terminated early for other reasons",
         COUNT != "c", 
         REGION != "Other")
#```

#```{r adj_analysis}
# Group "Establishment" by country (England & Wales)

adj <- adj %>%
  mutate(country= case_when(
    Establishment %in% c("Berwyn", "Cardiff", "Parc", "Proscoed", "Usk", "Swansea") ~ "Wales",
    TRUE ~ "England"))

#```

#```{r adj_analysis2}
# Security categories with the total number of cases in each Establishments
# There are 122 prisons in Englans & Wales, and the security level of cases allocated into each prison is unified. (one prison registers only one security level)

Security <- adj %>%
  group_by(Security) %>%
  count(Security, Establishment)

print(Security)

#```
#```{r adj analysis5}
# bar graph of Count VS Security

#```

#```{r adj analysis6}
# Grid bar plots by age groups on Count VS Offence

ggplot(adj, aes(y = Count,
                fill = Offence)) +
  geom_bar() +
  facet_grid(~`Age group`)+
  ylim(0, 42)

#```

#```{r adj_analysis3}
# Proportion of case outcome by country
ggplot(adj, aes(
  y = Establishment,  
  fill = Outcomes
)) +
  geom_bar(position = "fill") +
  facet_grid(~country) +
  labs(
    x = "Outcomes of the offence cases commited in prison",
    y = "List of establishments",
    title = "Proportion of outcomes of the offence cases commited in prison per establishment",
    subtitle = "(2023 January - March)")
#```

#```{r adj_analysis4}
ggplot(adj, aes(
  y = Establishment,
  fill = Offence
)) +
  geom_bar() +
  labs(
    x = "Outcomes of the offence cases commited in prison",
    y = "List of establishments",
    title = "Proportion of outcomes of the offence cases commited in prison per establishment",
    subtitle = "(2023 January - March)")
#```

#```{r puni}
# plot graph of 

ggplot(puni,
       mapping = aes(x = `Age group`,
                     y = Sum,
                     colour = `Security`))+
  geom_jitter(width = 0, size = 1, alpha = 0.5) + 
  theme_bw() +
  geom_smooth(method = lm, se = FALSE) +
  labs(title = "Scatter Plot of Sentenced punishment dates Against Age",
       subtitle = "(Category A being the highest security to Category D the lowest)", 
       x = "Age of prisoners",
       y = "Punishment duration in days",
       colour = "Security") 

#```


