getwd()
list.files(".")
list.files("../_src")
load("../_src/kurs_survey2020-04.Rda")

library(tidyverse)
library(ggforce)
library(ggdist)
library(gghalves)
library(ggbeeswarm)
library(ggtext)

#---Erstellen einer Farbpalette---
party_cols <- c("#FF00AE", "#FF0000", "#50D400", "#000000", "#FFFB00", "#00AAFF", "#8A8A8A", "#9700FF") #von links nach rechts + sonstige

#---Sortierung des Datensatzes nach Sitzordung in Bundestag---
survey$vote <- factor(survey$vote, levels = c("Die Linke","SPD","Die Grünen","CDU","FDP","AfD", "DIE PARTEI", "Sonstige"))

#---Vorbereitung zur Beschriftung des Plotts: N der einzelnen Parteien aus dem Datensatz wird aufgenommen--- 
vote_count <- survey %>% 
  count(vote)

linke_n <- vote_count %>% 
  filter(vote == "Die Linke") %>% 
  pull(n)

SPD_n <- vote_count %>% 
  filter(vote == "SPD") %>% 
  pull(n)

gruene_n <- vote_count %>% 
  filter(vote == "Die Grünen") %>% 
  pull(n)

CDU_n <- vote_count %>% 
  filter(vote == "CDU") %>% 
  pull(n)

FDP_n <- vote_count %>% 
  filter(vote == "FDP") %>% 
  pull(n)

AfD_n <- vote_count %>% 
  filter(vote == "AfD") %>% 
  pull(n)

partei_n <- vote_count %>% 
  filter(vote == "DIE PARTEI") %>% 
  pull(n)

sonstige_n <- vote_count %>% 
  filter(vote == "Sonstige") %>% 
  pull(n)

##---Erstellen des Plotts: Darstellung der Parteipräferenz nach Selbsteinschätzung politischer Position (links vs. rechts)
survey %>% 
  ggplot(aes(x=vote, y=as.numeric(lire_self), fill=vote)) +
  ggdist::stat_halfeye(
    ## custom bandwidth - the default standard deviation of the smoothing kernel is not optimal
    adjust = .5, 
    ## adjust height
    width = .6, 
    ## move geom to the right
    justification = -.5, 
    ## remove slab interval
    .width = 0, 
    alpha = 0.7,
    point_colour = NA, 
    show.legend = FALSE
  ) + 
  geom_boxplot(
    alpha = .7,
    width = .2,
    show.legend = FALSE,
    ## remove outliers
    outlier.color = NA ## `outlier.shape = NA` works as well
  ) +
  ## add justified jitter from the {gghalves} package
  gghalves::geom_half_point(
    ## draw jitter on the left
    side = "l", 
    ## control range of jitter
    range_scale = .2, 
    ## add some transparency
    alpha = .5, 
    show.legend = FALSE
  ) +
  coord_cartesian(xlim = c(1.2, NA), clip = "off") +
  ## Hinzufügen der erstellten Fabpalette 
  scale_fill_manual(values = party_cols) +
  ## Beschriftungen
  labs(title = "Korrelation der politischen Selbsteinschätung und der Parteipräferenz", 
       subtitle = "Darstellung in Box- Violinen- und Scatterplott",
       x = NULL,
       y = "Politische Selbsteinschätzung von 0 Links bis 10 Rechts") +
  ## 95% Konfidenzintervall über stat_summary
  stat_summary(fun.data = median_hilow, geom="errorbar", width = .1, position = position_nudge(x = 0.18), show.legend = FALSE) + 
  theme_classic()
