#################
#
# Importing, reshaping & joining data
#
#################

# setup -------------------------------------------------------------------
library(tidyverse)

# Excel-Dateien importieren
library(readxl)
#install.packages("readxl")

#https://de.wikipedia.org/wiki/Bundestagswahl_2017
btw2017 <- read_excel("../_src/btw2017.xlsx", sheet = "btw2017", skip = 1) %>%
  # Nur Zweitstimme ist von Interesse - folgende Spalten haben Relevanz: 
  select(land = 1, cdu = 4, spd = 6, afd = 8, fdp = 10, pds = 12, gru = 14) %>% 
  separate(land, into = c("land", "land_short"), sep = " ") %>% 
  mutate(land_short = str_remove_all(land_short, "\\(|\\)"))

class(btw2017$cdu)

# pivoting between long and wide to get tidy data
btw2017 <- btw2017 %>% 
  pivot_longer(
    cols = cdu:gru,
    names_to = "party",
    values_to = "voteshare"
  ) %>% 
  mutate(
    voteshare = parse_number(voteshare, locale = locale(decimal_mark = ","))
  ) 

btw2017 %>% 
  pivot_wider(
    names_from = "party",
    values_from = "voteshare"
  )

btw2017 %>% 
  filter(party == "afd") %>% 
  ggplot(aes(x = reorder(land, voteshare),  y = voteshare)) +
  geom_col() +
  coord_flip()

btw2017 %>% 
  ggplot(aes(x = reorder (party, voteshare), y = voteshare)) +
  geom_col() +
  facet_wrap(~land) +
  coord_flip()

# Wahlberechtigte nach Bundesländern
wahlberechtigte <- tribble(
  ~land, ~n_wahlb,
  "Baden-Württemberg", 8.3,
  "Bayern",	10.1,
  "Berlin",	2.6,
  "Brandenburg", 2.1,
  "Bremen",	0.5,
  "Hamburg", 1.4,
  "Hessen", 4.7,
  "Mecklenburg-Vorpommern",	1.4,
  "Niedersachsen", 6.3,
  "Nordrhein-Westfalen", 13.8,
  "Rheinland-Pfalz", 3.2,
  "Saarland",	0.8,
  "Sachsen", 3.4,
  "Sachsen-Anhalt", 2.0,
  "Schleswig-Holstein",	2.3,
  "Thüringen", 1.9
) %>%
  # case_when, Generalisierung von ifelse
  mutate(
    region = case_when(
      land %in% c("Mecklenburg-Vorpommern", "Sachsen", 
                  "Sachsen-Anhalt", "Thüringen") ~ "Ostdeutschland",
      TRUE ~ "Westdeutschland"
    )
  )

wahlberechtigte %>% 
  arrange(desc(n_wahlb))



# Joining data frames -----------------------------------------------------

# something_join(x, y)
# left_join() : all rows from x
# right_join() : all rows from y
# full_join() : all rows from both x and y
# semi_join() : all rows from x where there are matching values in y, keeping just columns from x
# inner_join() : all rows from x where there are matching values in y, return all combination of multiple
# matches in the case of multiple matches
# anti_join() : return all rows from x where there are not matching values in y, never duplicate rows of x

btw_wahlb <- btw2017 %>% 
  left_join(wahlberechtigte) 


# writing data
btw_wahlb %>% 
  write.csv(file = "output/btw2017.csv")

read_csv("output/btw2017.csv", locale = readr::locale(encoding = "latin1"))

#library(openxlsx)
#https://cran.r-project.org/web/packages/openxlsx/index.html

# rename
btw_wahlb %>% 
  rename(Bundesland = land, Partei = party)

# Stata u.a. Dateien importieren ------------------------------------------
library(haven)
#https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/QSFXLQ

read_dta(file = "../_src/mp_characteristics.dta") %>% 
  View()

# read_sav() to read data from SPSS

# ÜBUNG: Importieren Sie die Wahlbeteiligungsdaten aus den Bundesländern
# und mergen Sie dies mit btw2017
# https://de.statista.com/statistik/daten/studie/36658/umfrage/wahlbeteiligung-bei-den-bundestagswahlen/

wahlbeteiligung <- read_excel("src/statistic_id36658_wahlbeteiligung-bei-den-bundestagswahlen-nach-bundeslaendern-bis-2021.xlsx", sheet = "Daten", skip = 4) %>%
  select(land = 1, wahlb_prozent = 8) %>% 
  mutate(wahlb_prozent = parse_number(wahlb_prozent)) %>% 
  mutate(region = case_when
         (land %in% c
           ("Mecklenburg-Vorpommern", "Sachsen", "Sachsen-Anhalt", "Thüringen") 
           ~ "Ostdeutschland", TRUE ~ "Westdeutschland"
          )
        ) 

btw2017_wahlb <- btw2017 %>% 
  left_join(wahlbeteiligung, by = "land")

# Berechnen Sie den durchschnittlichen Stimmenanteil der AfD in Ost und West

btw2017_wahlb %>% 
  filter(party == "afd") %>% 
  group_by(region) %>% 
  summarise(afd_mean = mean(voteshare))
  


