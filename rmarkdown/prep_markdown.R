library(tidyverse)
library(readxl)
library(kableExtra)
library(DT)

btw2017 <- read_excel("../_src/btw2017.xlsx", sheet = "btw2017", skip = 1) %>% 
  select(land = 1, cdu = 4, spd = 6, afd = 8, fdp = 10, pds = 12, gru = 14) %>% 
  separate(land, into = c("land", "land_short"), sep = " ") %>% 
  mutate(land_short = str_remove_all(land_short, "\\(|\\)")) %>% 
  pivot_longer(
    cols = cdu:gru,
    names_to = "party",
    values_to = "voteshare"
  ) %>% 
  mutate(
    voteshare = parse_number(voteshare, locale = locale(decimal_mark = ","))
  ) 

btw2017_ordered <- btw2017 %>% 
  filter(party == "afd") %>% 
  arrange(desc(voteshare))


plot_btw2017 <- btw2017 %>% 
  filter(party == "afd") %>% 
  ggplot(aes(x = reorder(land, voteshare),  y = voteshare)) +
  geom_col() +
  coord_flip() +
  theme_minimal()



# Run Markdown ------------------------------------------------------------
rmarkdown::render(
  input = "rmarkdown/report_slim.Rmd"
)

