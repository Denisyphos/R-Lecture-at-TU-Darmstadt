#################
#
# Practicing the tidyverse
#
#################

# setup -------------------------------------------------------------------
# general options appearance
# https://notepad-plus-plus.org/downloads/


library(tidyverse)
# https://www.rstudio.com/resources/cheatsheets/

# import data -------------------------------------------------------------
list.files("../_src")

btw2021 <- readRDS("../_src/btw2021.rds") %>% 
  as_tibble()

# tidy data
btw2021 %>% View()
glimpse(btw2021)

# selecting (with helpers)
select(btw2021, land, party, candidate)

btw2021 %>% 
  select(land, party, candidate)

btw2021 %>% 
  select(land, party, candidate, contains("2021"))

btw2021 %>% 
  select(land, party, candidate, starts_with("v2"))


# welche Ausprägungen gibt es?
unique(btw2021$party) 

btw2021 %>% 
  count(party)

btw2021 %>% 
  count(party, sort = TRUE)

# filter 
btw2021 %>% 
  filter(party == "CDU")

btw2021 %>% 
  filter(party != "CDU")

btw2021 <- btw2021 %>%  # Reduktion auf Bundestagsparteien
  filter(party %in% c("CDU", "CSU", "AfD", "SPD", "FDP", "DIE LINKE", "GRÜNE"))

includeparties <- c("CDU", "CSU", "AfD", "SPD", "FDP", "DIE LINKE", "GRÜNE")

btw2021 <- btw2021 %>% 
  filter(party %in% includeparties)

btw2021 %>% 
  filter(v2_2021 >= 40)

btw2021 %>% 
  filter(v2_2017 >= 40)

btw2021 %>% 
  filter(v2_2021 >= 30, v2_2017 >= 30)

btw2021 %>% 
  distinct(wkr_nr)

btw2021 %>% 
  distinct(wkr_nr, .keep_all = TRUE)

# mutate & arrange & group_by
ranks <- btw2021 %>% 
  group_by(wkr_nr) %>% 
  arrange(desc(v1_2021)) %>% 
  mutate(rank = row_number()) %>% 
  filter(rank == 1) %>% 
  select(wkr_name, party, candidate, v1_2021, rank) %>% 
  ungroup()

ranks %>% 
  count(party) %>% 
  mutate(perc = n/sum(n)*100)
  

# ÜBUNG: Wer ist beliebter als die eigene Partei?
# Lassen Sie sich für jede Partei die drei beliebtesten (relativ zur Partei)
# Kandidierenden anzeigen

  ranks %>% 
    group_by(party) %>% 
    arrange(desc(v1_2021), .by_group = T) %>% 
    mutate(candidate_rank = row_number()) %>% 
    filter(candidate_rank <=3)

# Inspektion der Daten ----------------------------------------------------
# Wo hat die CDU am meisten Zweitstimmen verloren? 
btw2021 %>% 
  filter(party == "CDU") %>% 
  arrange(v2_diff) %>% 
  view()

  ## Antwort: Mecklenburg-Vorpommern

# Wo hat die CSU am meisten Erststimmen verloren?
btw2021 %>% 
  filter(party == "CSU") %>% 
  arrange(v1_diff) %>% 
  view()

  #Antwort: Oberallgäu, Passau, Traunstein

# summarise
btw2021 %>% 
  summarise(
    mean = mean(v2_2021, na.rm = TRUE),
    min = min(v2_2021, na.rm = TRUE),
    max = max(v2_2021, na.rm = TRUE)
  )

# summarise & group_by
btw2021 %>% 
  group_by(party) %>% 
  summarise(
    mean = mean(v2_2021, na.rm = TRUE),
    min = min(v2_2021, na.rm = TRUE),
    max = max(v2_2021, na.rm = TRUE)
  )


# Wieviele Promovierte haben kandidiert?
btw2021$candidate %>% str_count(., "Dr.") %>% sum(., na.rm = TRUE)

# https://katapult-magazin.de/en/article/mehr-thomasse-als-buergermeisterinnen
btw2021 %>% 
  mutate(thomas = str_detect(candidate, "Thomas")) %>% 
  count(thomas)

# graphische Inspektion der Daten -----------------------------------------
btw2021 %>% 
  filter(party %in% c("SPD", "CDU")) %>% 
  ggplot(aes(x = v2_2017, y = v2_2021)) +
  geom_point()

btw2021 %>% 
  filter(party %in% c("SPD", "CDU")) %>% 
  ggplot(aes(x = v2_2017, y = v2_2021, col = party)) +
  geom_point(alpha = .5) +
  scale_color_manual(values = c("black", "red")) +
  labs(title = "Zusammenhang Wahlergebnisse 2017 und 2021",
       x = "Wahlergebnis 2017",
       y = "Wahlergebnis 2021") +
  theme_minimal()

# facetting
btw2021 %>% 
  filter(party %in% c("SPD", "CDU")) %>% 
  ggplot(aes(x = v2_2017, y = v2_2021)) +
  geom_point(alpha = .5) +
  scale_color_manual(values = c("black", "red")) +
  facet_wrap(~party) +
  labs(title = "Zusammenhang Wahlergebnisse 2017 und 2021",
       x = "Wahlergebnis 2017",
       y = "Wahlergebnis 2021") +
  theme_minimal()
  
# Wie ist das durchschnittliche Erststimmenergebnis der AfD nach Bundesländern?
btw2021 %>% 
  filter(party == "AfD") %>% 
  group_by(land) %>% 
  summarise(mean_vote = mean(v2_2021)) %>% 
  ggplot(aes(x = reorder(land, mean_vote), y = mean_vote)) +
  geom_col() +
  coord_flip()

btw2021 %>% 
  filter(party == "AfD") %>% 
  ggplot(aes(x = reorder(land, v2_2021), y = v2_2021)) +
  geom_boxplot() +
  geom_jitter(width = .3, alpha = .4) +
  coord_flip()

ggsave(filename = "graph/AfD-results.jpg")

# ÜBUNG: Wie sind die Verteilungen bei der Beliebtheit Kandidat vs. Partei?
# Kandidat:innen verstanden als Erststimme und Partei als Zweitstimme 

  btw2021 %>% 
    group_by(party) %>% 
    summarise_at(c("v1_2021", "v2_2021"),mean, na.rm = T) %>% 
    pivot_longer(cols = contains('2021'), names_to = 'vote', values_to = 'mean') %>% 
    ggplot(aes(x = party, y = mean, fill = vote)) +
    geom_col(position="dodge") +
    scale_fill_manual(labels = c("Erststimme", "Zweitstimme"), values = c("#7C7C7C", "#404040"))
  
  ggsave(filename = "graph/beliebtheit.jpg")


