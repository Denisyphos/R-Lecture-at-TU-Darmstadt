###############################
#
# Einführung in die multiple Regression
#
#
###############################


# Setup -------------------------------------------------------------------
# packages
library(tidyverse)
library(haven)
library(broom)
library(kableExtra)
library(corrplot)
library(huxtable)
library(emmeans)

list.files("../_src")


# Datensatz laden ---------------------------------------------------------
crime <- read_dta("../_src/florida_crime.dta") %>% 
  mutate(county = str_to_title(county))

str(crime)
glimpse(crime)


# Zusammenhang zwischen Bildung und Kriminalität
crime_plot <- crime %>% 
  ggplot(aes(x = education , y = crime_rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Positiver Effekt von Bildung auf Kriminalität?",
       x = "Bildung", y = "Verbrechensrate") +
  theme_minimal()

crime_plot

model1 <- lm(crime_rate ~ education, data = crime)

model1$coefficients
coefficients(model1)
summary(model1)

tidy(model1) 
augment(model1)
glance(model1)

augment(model1) %>% 
  ggplot(aes(x = education, y = crime_rate, size = .cooksd)) +
  geom_point(shape = 1) +
  geom_smooth(method = "lm", se = FALSE) +
  labs( x = "Bildung", y = "Verbrechensrate") +
  guides(size = "none") +
  theme_minimal()

# Welche Fälle sind besonders einflussreich? 
# leverage: außergewöhnliche x-Werte
# diskrepanz: außergewöhnliche y-Werte

crime %>% 
  bind_cols(augment(model1) %>% select(.cooksd)) %>% 
  arrange(desc(.cooksd)) %>% 
  ggplot(aes(x = education, y = crime_rate, label = county)) +
  geom_point(aes(size = .cooksd), shape = 1) +
  geom_text()

# ÜBUNG: Berechnen Sie eine Regression ohne die counties Dade Leon und Alachua
  
lm(crime_rate ~ education, data = crime %>% filter(county !="Dade"))
lm(crime_rate ~ education, data = filter(crime, county !="Dade"))

lm(crime_rate ~ education, data = filter(crime, county %in% c("Leon", "Alachua", "Lafayette", "Okaloosa")))


# Was könnte hinter dem statistischen Zusammenhang von Kriminalität und 
# höherer Bildung stecken?

# multiple Regression auf Kriminalität
cor(crime[, 2:5]) #Korrelations Matritze aller Variablen in crime 
                  #String Variable "county" wurde entfernt (2:5)

corrplot(cor(crime[, 2:5]), method = 'number')
corrplot(cor(crime[, 2:5]), method = 'color')
corrplot(cor(crime[, 2:5]), method = 'ellipse')


# Regression von Bildung und Urbanisierung auf Kriminalität
summary(model1)
0.4669119^2 # for simple regression R = correlation^2

model2 <- lm(crime_rate ~ education + urbanization, data = crime)

model2$coefficients
coefficients(model2)
summary(model2)



# Regressionstabllen / Vergleich von Modellen
# https://cran.r-project.org/web/packages/huxtable/vignettes/huxtable.html
huxreg(model1, model2)

regtable <- huxreg("Modell 1" = model1, "Modell 2" = model2,
                   coefs = c("Konstante" = "(Intercept)", "Bildung" = "education", 
                             "Urbanisierung" = "urbanization")) 

quick_pdf(regtable, file = "graph/regtable.pdf")
quick_docx(regtable, file = "graph/regtable.docx")

# Koeffizientenplot
tidy(model2) %>% 
  filter(term != "(Intercept)") %>% 
  mutate(lower = estimate - 1.96*std.error,
         upper = estimate + 1.96*std.error) %>% 
  ggplot(aes(x = term, y = estimate)) +
  geom_point() +
  geom_linerange(aes(ymin = lower, ymax = upper),
                 width = .5) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  theme_minimal()

tidy(model1) %>% 
  mutate(model = "model 1") %>% 
  bind_rows(tidy(model2) %>% mutate(model = "model 2")) %>% 
  filter(term != "(Intercept)") %>% 
  mutate(lower = estimate - 1.96*std.error,
         upper = estimate + 1.96*std.error) %>% 
  ggplot(aes(x = term, y = estimate, col = model)) +
  geom_point(position = position_dodge(width = .3)) +
  geom_linerange(aes(ymin = lower, ymax = upper),
                 width = .5,
                 position = position_dodge(width = .3)) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  theme_minimal()


# Koeffizientenplot

crime %>% 
  ggplot(aes(x = urbanization)) +
  geom_histogram()

crime <- crime %>% 
  mutate(urban_cat = cut(urbanization, breaks = 4,
                         labels = c("low urbanization", "medium urbanization", 
                                    "high urbanization", "very high urbanization")))

crime %>% 
  ggplot(aes(x = education, y = crime_rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(vars(urban_cat))

crime %>% 
  ggplot(aes(x = education, y = crime_rate)) +
  geom_point(aes(col = urban_cat)) +
  geom_smooth(aes(x = education, y = crime_rate), method = "lm", se = FALSE,
              linetype = "dashed") +
  geom_smooth(aes(col = urban_cat), method = "lm", se = FALSE) +
  theme_minimal()


predict(model2)
augment(model2)


# Was bedeutet eigentlich unter Kontrolle? (vgl. Kohler/Kreutter)
crime_urb <- lm(crime_rate ~ urbanization, data = crime) %>% augment() 

edu_urb <- lm(education ~ urbanization, data = crime) %>% augment() 

lm(crime_urb$.resid ~ edu_urb$.resid) %>% .$coefficients %>% round(4)
summary(model2)


# ÜBUNG/Hausaufgabe: Berechenen Sie eine Regression von Urbanisierung, Einkommen und Bildung auf
# Kriminalität und produzieren Sie einen Koeffizientenplot!

model3 <- lm(crime_rate ~ median_income + urbanization  +education, data = crime) 
crime_reg <- lm(crime_rate ~ urbanization + median_income +education, data = crime) %>% augment() 

huxreg(model1, model2, model3)

tidy(model3) %>% 
  filter(term != "(Intercept)") %>% 
  mutate(lower = estimate - 1.96*std.error,
         upper = estimate + 1.96*std.error) %>% 
  ggplot(aes(x = term, y = estimate)) +
  geom_point() +
  geom_linerange(aes(ymin = lower, ymax = upper),
                 width = .5) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  theme_minimal()


### Voraussetzungen ###

# Residuenplot: Visueller Test auf Homoskedastizität 
# Für multiple-lineare-Regression wird Vorausgesetzt, 
# dass die Residuen unabhängig sind und eine konstante 
# Varianz aufweisen (V(ϵi)=σ2," Homoskedastizität"). 
# Dies kann grafisch überprüft werden, indem die geschätzten 
# Werte der abhängigen Variablen in einem Streudiagramm gegen 
# die geschätzten Residuen des Modells abgetragen werden. 
# (https://wikis.fu-berlin.de/display/fustat/Residuenplots)

crime_reg %>% 
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title = "Homoskelastizität",
       x = "Vorhersagewert ŷ", y = "Residuen")

# Histogramm der Residuen: Visueller Test auf Normalverteilung
# Damit den F-Test und die t-Tests für die Parameter sinnvoll 
# interpretiert weden können, müssen die Residuen normalverteilt sein. 
# (https://wikis.fu-berlin.de/display/fustat/Residuenplots)

crime_reg %>% 
  ggplot(aes(x = .resid)) +
  geom_histogram()

