##################################
#
# Practicing the tidyverse
# Analyse der Bundesgesetzgebung
#
##################################

# Setup -------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(forcats)

# Daten einlesen ----------------------------------------------------------

# Daten zur Bundesgesetzgebung
gesta <- readRDS("../_src/gesta_kurs.rds")

# Daten zu Mehrheitsverhältnissen im Bundesrat
load("../_src/brvotes_party.Rda")
load("../_src/brmaj_daily.Rda")

# Schlaglichter auf die Bundesgesetzgebung --------------------------------
# Datensatz zur Bundesgesetzgebung
# https://github.com/benjaminguinaudeau/tidybundestag
glimpse(gesta)

gesta %>% 
  count(init_short, sort = TRUE) 


# Factors -----------------------------------------------------------------
gesta %>% 
  count(beratungsstand, sort = TRUE)  %>% 
  print(n = 50)

gesta <- gesta %>% 
  mutate(beratungsstand = as_factor(beratungsstand),
         beratungsstand = 
           fct_collapse(beratungsstand, 
                        verkündet = c("Verkündet"),
                        abgelehnt = c("Abgelehnt"),
                        erledigt = c("Für erledigt erklärt", "Erledigt durch Ablauf der Wahlperiode"),
                        other_level = "Sonstiges")
         ) %>% 
  drop_na(beratungsstand)

gesta %>% 
  mutate(beratungsstand = fct_recode(
    beratungsstand, 
    "verkuendet" = "verkündet",
    "nein" = "abgelehnt")
    ) %>% 
  count(beratungsstand)
   
gesta %>% 
  count(beratungsstand) %>% 
  ggplot(aes(x = beratungsstand, y = n)) +
  geom_col()

gesta %>% 
  count(beratungsstand) %>% 
  ggplot(aes(x = fct_rev(beratungsstand), y = n)) +
  geom_col()

### ÜBUNG ###
# Stellen Sie die Einbringungsaktivität unterschiedlicher Akteure (init_cat) graphisch dar!
# Wie häufig ist die Opposition mit ihren Gesetzen erfolgreich? (beratungsstand == "Verkündet")
gesta %>% 
  count(init_cat) %>% 
  mutate(perc = n())
  ggplot(aes(x = init_cat, y = n)) +
  geom_col()

gesta %>% 
  group_by(init_gov) %>% 
  count(beratungsstand) %>% 
  ggplot(aes(x = init_gov, y = n, fill = beratungsstand)) +
  geom_col(position = "dodge")





### ÜBUNG ###
# Wie lange dauert die Gesetzgebung? Dauert es bei Zustimmungsgesetzen länger?
gesta %>% 
  mutate(x= verk_date-init_date) %>% 
  select(x, verk_date, init_date) 

gesta %>% 
  mutate(bill_duration = verk_date - init_date) %>% 
  ggplot(aes(x=bill_duration)) +
  geom_histogram(bins = 30) 
# Fehler im Datensatz. Negative Werte wurden durch das Histrogram deutlich und starke Ausreiser - filter schafft abhilfe 

gesta %>%  
  mutate(start = as.numeric(init_date), end = as.numeric(verk_date)) %>% #as.numeric für den filter
  mutate(time = end - start) %>%
  filter(time > 0, time < 500) %>% 
  group_by(bill_type) %>%
  drop_na() %>% 
  ggplot(aes(x = time, fill = bill_type)) +
  geom_density(alpha = 0.5)

gesta %>%  
  mutate(start = as.numeric(init_date), end = as.numeric(verk_date)) %>% #as.numeric für den filter
  mutate(time = end - start) %>%
  filter(time > 0, time < 500) %>% 
  group_by(bill_type) %>%
  drop_na() %>% 
  summarise(sum = sum(time)) %>% 
  ggplot(aes(x = bill_type, y = sum)) +
  geom_col()

gesta %>%  
  mutate(start = as.numeric(init_date), end = as.numeric(verk_date)) %>% #as.numeric für den filter
  mutate(time = end - start) %>%
  filter(time > 0, time < 500) %>% 
  group_by(bill_type) %>% 
  summarise(mean_time = mean(time, na.rm = T)) 


### ÜBUNG ###
# Wie hat sich die Zustimmungsquote entwickelt? ---------------------------
# Schauen Sie sich jahresweise und legislaturperiodenweise Verteilungen an



# Blick auf die Bundesratsinitiativen der Länder
# Welche Länder sind besonders einbringungsfreudig?
# Werden eher einzelne Länder oder Ländergruppen aktiv?
# Inwieweit strukturiert der ROM-Status eines Landes die Einbringungsaktivität?

br_initiatives <- gesta %>% 
  filter(init_cat == "Länderinitiative") 

br_initiatives_long <- br_initiatives %>% 
  separate_rows(init_short, sep = ";")

br_initiatives_long %>% 
  count(init_short, sort = TRUE) %>% 
  print(n = 25)


br_initiatives %>% 
  mutate(n_initstates = str_count(init_short, "\\w+")) %>% 
  ggplot(aes(x = n_initstates)) +
  geom_histogram(binwidth = 1)


land_rom <- brmaj_daily %>% 
  pivot_longer(cols = c(R, O, M),
               names_to = "rom_status",
               values_to = "land"
  ) %>% 
  separate_rows(land, sep = ";") %>% 
  select(date, land, rom_status) %>% 
  group_by(land, rom_status) %>% 
  mutate(status_years = n()/365)


br_initiatives_long %>% 
  left_join(land_rom, by = c("init_date" = "date")) %>% 
  select(wahlperiode, beratungsstand, initiative, init_short, init_date, land,
         rom_status) %>% 
  group_by(land) %>% 
  count(rom_status) %>% 
  mutate(perc = n/sum(n)*100) %>% 
  ggplot(aes(x = rom_status, y = perc)) +
  geom_boxplot()


br_initiatives_long %>% 
  left_join(land_rom, by = c("init_date" = "date")) %>% 
  select(wahlperiode, beratungsstand, initiative, init_short, init_date, land,
         rom_status, status_years) %>% 
  group_by(land, status_years) %>% 
  count(rom_status) %>% 
  mutate(inits = n/status_years) %>% 
  ggplot(aes(x = rom_status, y = inits)) +
  geom_boxplot()
  

# Parteienstimmen im Bundesrat
# http://chrstecker.de/wp-content/uploads/2021/10/bundesrat-2021-10-21.html
brvotes_party %>% 
  filter(br_party %in% c("cdu", "fdp", "gru", "pds", "spd")) %>% 
  ggplot(aes(x = date, y = br_ppgvotes, col = br_party)) +
  geom_line() +
  geom_line(aes(x = date, y = maj_hurdle), color = "blue", linetype = "dashed") +
  scale_y_continuous(limits = c(0, 65),
                     breaks = seq(0, 65, 5),
                     labels = seq(0, 65, 5)) +
  scale_color_manual(name = "Parteien",
                     values = c("black", "yellow", "green", "pink", "red"), 
                     labels = c("CDU/CSU", "FDP", "Grüne", "Linke", "SPD")) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y",
               minor_breaks = NULL) +
  labs(title = "Mehrheitsverhältnisse im BR",
       x = "", y = "Stimmenzahl im Bundesrat") +
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 45),
    legend.position = "bottom"
  )




# String-Functions --------------------------------------------------------

# a string
c("spd")

# a string vector
parties <- c("spd", "cdu", "gru")

# length of a string
length(parties)
str_length(parties)

# combine strings
str_c("cdu", "csu", sep = "/")

# str_replace
str_replace(parties, pattern = "cdu", replacement = "cdu/csu")

# subset strings
str_sub(parties, 1, 2)

# str_to_upper
str_to_upper(parties)


# string matching
abstimmung <- "SPD stimmt dagegen, Grüne stimmen dafür, CDU/CSU enthält sich, auch ein SPD-Abgeordneter, 221 Abgeordente fehlten."
parties <- c("SPD", "CDU/CSU", "Grüne")



# \d: matches any digit.
# \s: matches any whitespace (e.g. space, tab, newline).
# [abc]: matches a, b, or c.
# [^abc]: matches anything except a, b, or c.


# Number of matches  
# {n}: exactly n
# {n,}: n or more
# {,m}: at most m
# {n,m}: between n and m


str_view(abstimmung, "stimmt")
str_view(abstimmung, "\\d")
str_view(abstimmung, "\\d{2}")
str_view(abstimmung, "\\d{1,}")
str_view_all(abstimmung, "SPD")
str_view_all(abstimmung, "^SPD")
str_view_all(abstimmung, parties)

str_extract(abstimmung, "[[:digit:]]{1,}")

str_remove(abstimmung, pattern = "dagegen")

str_replace(abstimmung, pattern = "dagegen", replacement = "DAGEGEN")

str_detect(parties, "SPD")
parties[str_detect(parties, "SPD")]

gesta %>% 
  mutate(
    gesetzart = case_when(
      str_detect(titel, "Gesetz zur Änderung") ~ "Änderungsgesetz",
      TRUE ~ "Gesetz"
    )
  ) %>% 
  select(titel, gesetzart) %>% 
  count(gesetzart)

gesta %>% 
  mutate(
    compositum = str_extract(titel, "\\(.+?\\)")
  ) %>% 
  drop_na(compositum) %>% 
  select(titel, compositum) %>% 
  mutate(length = str_length(compositum)) %>% 
  arrange(desc(length))

### ÜBUNG ###
# Wie häufig thematisierte die AfD in ihren Gesetzen Immigration und Integration?


