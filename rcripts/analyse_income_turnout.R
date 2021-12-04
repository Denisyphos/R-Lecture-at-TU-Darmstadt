###############################
#
# Einführung in die Regressionsanalyse
# (Daten zum Kumulieren/Panaschieren &
# zum Zusammenhang von Einkommen und Wahlbeteiligung)
#
###############################


# Setup -------------------------------------------------------------------
# packages
library(tidyverse)
library(readxl)
library(broom)
library(kableExtra)

# definitionen und styles
source("../_src/_definitions.R", encoding = "UTF-8")

# Datensätze ---
# Sozialstrukturdaten Hessen
load("../_src/he_gemeindedaten.Rda")

# vorläufiges und endgültiges Wahlergebnis Hessen
load("../_src/he2021_gmd_trend.Rda")
load("../_src/he2021_gmd_end.Rda")

# Wahl- und Strukturdaten FFM
load("../_src/Frankfurt_06412000_stadtteil_income.Rda")
load("../_src/ffm2021_ortsteile.Rda")


# some wrangling
ffm_tb <- ffm2021_ortsteile %>% 
  left_join(ffm) %>% 
  drop_na(median_income) %>% 
  mutate(median_income = median_income/1000)

he_tb <- he_gemeindedaten %>% 
  left_join(he2021_gmd_trend) 

trend_long <- he2021_gmd_trend %>% 
  select(gmd_key, gmd_name, cdu:fwg) %>% 
  pivot_longer(
    cols = cdu:fwg,
    values_to = "share_list"
  )

end_long <- he2021_gmd_end %>% 
  select(gmd_key, gmd_name, cdu:fwg) %>% 
  pivot_longer(
    cols = cdu:fwg,
    values_to = "share_panakum"
  )

he_kum <- he_gemeindedaten %>% 
  left_join(trend_long) %>% 
  left_join(end_long) %>%
  mutate(gain_panakum = share_panakum - share_list) 

# Zusammenhang Einkommen und Wahlbeteiligung ------------------------------
library(broom)

ffm_tb %>% 
  select(gebiet, median_income, turnout)


ffm_tb %>% 
  ggplot(aes(x = median_income, y = turnout)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(limits = c(3, 7),
                     breaks = seq(3, 7, 1)) +
  scale_y_continuous(limits = c(25, 65)) +
  labs(title = "Median-Einkommen und Wahlbeteiligung",
       x = "Medianeinkommen (Tausend €)", y = "Wahlbeteiligung") +
  theme_minimal()

ffm_tb %>% 
  ggplot(aes(x = median_income, y = turnout, size = share_pankum)) +
  geom_point(shape = 1) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(limits = c(3, 7),
                     breaks = seq(3, 7, 1)) +
  scale_y_continuous(limits = c(25, 65)) +
  guides(size = "none") +
  labs(title = "Median-Einkommen und Wahlbeteiligung",
       x = "Medianeinkommen (Tausend €)", y = "Wahlbeteiligung") +
  theme_minimal()

ffm_tb %>% 
  ggplot(aes(x = median_income, y = turnout, label = gebiet)) +
  geom_point(shape = 1) +
  geom_text(size = 2.4) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(limits = c(3, 7),
                     breaks = seq(3, 7, 1)) +
  scale_y_continuous(limits = c(25, 65)) +
  guides(size = "none") +
  labs(title = "Median-Einkommen und Wahlbeteiligung",
       x = "Medianeinkommen (Tausend €)", y = "Wahlbeteiligung") +
  theme_minimal()


ffm_tb %>% 
  ggplot(aes(x = median_income, y = turnout, label = gebiet)) +
  geom_point(shape = 1) +
  geom_text(size = 2.4) +
  geom_smooth(method = "lm", se = FALSE, fullrange = TRUE) +
  scale_x_continuous(limits = c(0, 7),
                     breaks = seq(0, 7, 1)) +
  scale_y_continuous(limits = c(0, 65),
                     breaks = c(seq(0, 70, 10))) +
  guides(size = "none") +
  labs(title = "Median-Einkommen und Wahlbeteiligung",
       x = "Medianeinkommen (Tausend €)", y = "Wahlbeteiligung") +
  theme_minimal()


model_kum <- lm(turnout ~ median_income, data = ffm_tb)
summary(model_kum)

model_kum$coefficients
coefficients(model_kum)
summary(model_kum)

tidy(model_kum) 
augment(model_kum)
glance(model_kum) 

ffm_tb %>% 
  summarise(corr = cor(median_income, turnout)^2)

augment(model_kum) %>% 
  ggplot() +
  geom_point(aes(x = median_income, y = turnout), color = "black") +
  geom_point(aes(x = median_income, y = .fitted), color = "red") +
  theme_minimal()

augment(model_kum) %>% 
  ggplot() +
  geom_point(aes(x = .fitted, y = .resid), color = "black") +
  geom_hline(yintercept = 0, color = "red") +
  theme_minimal()

### ÜBUNG ###
# Sagen Sie die Wahlbeteiligung für einen Stadtteil mit einem Medianeinkommen 
# von 7000€ voraus



# Total Sum of Squares
(tss <- ffm_tb %>% 
  mutate(
    mean_turnout = mean(turnout, na.rm = TRUE),
    residuals_sq = (mean_turnout-turnout)^2
  ) %>% 
  summarise(
    resid_sum_sq = sum(residuals_sq)) %>% 
    pull(resid_sum_sq))

# Residual Sum of Squares
(rss <- ffm_tb %>% 
  mutate(
    residuals_sq = residuals(model_kum)^2
  ) %>% 
    summarise(
      resid_sum_sq = sum(residuals_sq)) %>% 
    pull(resid_sum_sq))

# Model Sum of Squares
(mss <- ffm_tb %>% 
  mutate(
    residuals_sq = (predict(model_kum)-mean(ffm_tb$turnout))^2
  ) %>% 
    summarise(
      resid_sum_sq = sum(residuals_sq)) %>% 
    pull(resid_sum_sq))

tss-mss

mss/tss
glance(model_kum) %>% pull(r.squared)

# Mittelwert als good guess
ffm_tb %>% 
  ggplot(aes(x = 1, y = turnout)) + 
  geom_point() +
  stat_summary(
    geom = "point",
    fun = "mean",
    color = "red",
    size = 4
  ) +
  labs(title = paste("MW = ", round(mean(ffm_tb$turnout), 2),
       "; TSS = ", round(tss, 2))) + 
  theme_minimal()

# Regressionsgleichung als better guess
ffm_tb %>% 
  ggplot(aes(x = median_income, y = turnout)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_hline(yintercept = mean(ffm_tb$turnout)) +
  geom_vline(xintercept = mean(ffm_tb$median_income)) +
  scale_x_continuous(limits = c(3, 7),
                     breaks = seq(3, 7, 1)) +
  scale_y_continuous(limits = c(25, 65)) +
  labs(title = paste("Median-Einkommen und Wahlbeteiligung; MSS = ", round(mss, 2)), 
       x = "Medianeinkommen (Tausend €)", y = "Wahlbeteiligung") +
  theme_minimal()


# Ausreißer und Influence
augment(model_kum) %>% 
  arrange(desc(.cooksd))
  

augment(model_kum) %>% 
  ggplot(aes(x = median_income, y = turnout, size = .cooksd)) +
  geom_point(shape = 1) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(limits = c(3, 7),
                     breaks = seq(3, 7, 1)) +
  scale_y_continuous(limits = c(25, 65)) +
  labs(title = "Median-Einkommen und Wahlbeteiligung", 
       x = "Medianeinkommen (Tausend €)", y = "Wahlbeteiligung") +
  guides(size = "none") +
  theme_minimal()



# Wer gewinnt durch Kumulieren und Panaschieren? --------------------------
he_kum %>% 
  ggplot(aes(x = name, y = gain_panakum, color = name, fill = name)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.2) +
  geom_jitter(alpha = .2) +
  facet_wrap(vars(gmd_type)) +
  labs(title = "veränderter Stimmenanteil nach Kumulieren/Panaschieren",
       x = "Partei", y = "veränderter Stimmenanteil (%)") +
  scale_color_manual(name = "Parteien",
                     values = party_colors) +
  scale_fill_manual(name = "Parteien",
                    values = party_colors) +
  scale_x_discrete(labels = party_names) +
  guides(color = FALSE, fill = FALSE) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45)
  )



# Was erklärt Kumulieren/Panaschieren? ------------------------------------
he_gemeindedaten %>% 
  left_join(he2021_gmd_trend) %>% 
  drop_na(share_pankum) %>% 
  select(gmd_name, gmd_type, pop, share_pankum) %>% 
  group_by(gmd_type) %>% 
  arrange(gmd_type, desc(share_pankum)) %>% 
  filter(row_number() <=3 | row_number() >= n()-3) 

he_tb %>% 
  drop_na(share_pankum) %>% 
  group_by(gmd_type) %>% 
  summarise(mean_pankum = mean(share_pankum))

levels(he_tb$gmd_type)

he_tb %>% 
  ggplot(aes(x = gmd_type, y = share_pankum)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(color = "grey", alpha = 0.5) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(title = "Anteil der Stimmzettel mit kumulieren/panaschieren",
       x = "", y = "Anteil Stimmzettel (%)") +
  theme_minimal()


### ÜBUNG ###
# Berechnen Sie eine Regression der Gemeindegröße auf share_pankum und beschäftigen Sie
# sich mit den Modellen (mss, tss, residuen, Influence etc.)
