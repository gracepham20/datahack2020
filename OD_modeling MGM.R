library(tidyverse)

OD_info <- read_csv("Downloads/Overdose_Information_Network_Data_CY_January_2018_-_Current_Monthly_County_State_Police.csv")

OD_unique <- OD_info %>%
  dplyr::select(
    `Incident ID`:`Victim County`, `Incident County FIPS Code`:`Victim County Latitude and Longitude`, `Naloxone Administered`, `Response Time Desc`,
    Survive
  ) %>%
  distinct()

OD_drug <- OD_info %>%
  dplyr::select(`Accidental Exposure`:`Susp OD Drug Desc`, `Incident ID`, `Victim ID`) %>%
  distinct() %>%
  mutate(tmp = 1) %>%
  pivot_wider(id_cols = c(`Incident ID`, `Victim ID`), values_from = tmp, values_fill = list(tmp = 0), names_from = `Susp OD Drug Desc`)

OD_all <- inner_join(OD_unique, OD_drug)

names(OD_all) <- make.names(names(OD_all)) # for convenience

OD_all$Died <- ifelse(OD_all$Survive == "N", 1, 0)
OD_all$Lived <- ifelse(OD_all$Survive == "Y", 1, 0)

OD_all$Age2 <- ifelse(OD_all$Age.Range %in% c("0 - 9", "10 - 14"), "15 - 19", OD_all$Age.Range)
OD_all$Age2 <- ifelse(OD_all$Age2 %in% c("70 - 79", "80 - *"), "60 - 69", OD_all$Age2)
OD_all$Race <- relevel(as.factor(OD_all$Race), "White")
OD_all$Race2 <- OD_all$Race
levels(OD_all$Race2) <- c("White", "Other or Unknown", "Other or Unknown", "Black", "Other or Unknown")
OD_all$Ethnicity.Desc <- relevel(as.factor(OD_all$Ethnicity.Desc), "Not Hispanic")
OD_all$HEROIN <- as.factor(OD_all$HEROIN)
OD_all$FENTANYL <- as.factor(OD_all$FENTANYL)

library(rms)
frm_glm <- Died ~ Naloxone.Administered * (HEROIN * FENTANYL + Age2) + Race2 + UNKNOWN
OD_lrm <- lrm(frm_glm, OD_all, x = TRUE, y = TRUE)

glm1 <- glm(frm_glm, data = OD_all, family = binomial)
glm2 <- glmer(update(frm_glm, . ~ . + (1 | Incident.County.FIPS.Code)), data = OD_all, family = binomial)
glm3 <- glmer(update(frm_glm, . ~ . + (Naloxone.Administered | Incident.County.FIPS.Code)), data = OD_all, family = binomial)

anova(glm3, glm2, glm1)

library(ggeffects)
library(sjPlot)
ggeffect(glm3, terms = c("Naloxone.Administered", "HEROIN", "FENTANYL"))

glm4 <- glmer(update(frm_glm, Lived ~ . + (Naloxone.Administered | Incident.County.FIPS.Code)), data = OD_all, family = binomial)

ggeffect(glm4, terms = c("Naloxone.Administered", "HEROIN", "FENTANYL"))


OD_all %>%
  group_by(HEROIN, FENTANYL, Naloxone.Administered) %>%
  summarise(n = n(), Died = sum(Died), rate = mean(Died)) %>%
  arrange(Naloxone.Administered, HEROIN, FENTANYL)

OD_all %>%
  group_by(HEROIN, FENTANYL, Naloxone.Administered) %>%
  summarise(n = n(), Lived = sum(Lived), rate = mean(Lived)) %>%
  arrange(Naloxone.Administered, HEROIN, FENTANYL)

OD_all %>%
  group_by(Naloxone.Administered,Age2) %>%
  summarise(n = n(), Died = sum(Died),Lived=sum(Lived)) %>%
  arrange(Naloxone.Administered,Age2)

ggeffect(glm3, terms = c("Naloxone.Administered", "Age2"))

plot_model(glm3)
plot_model(glm4)

plot_model(glm3,"re",axis.lim = c(0.1,10))
plot_model(glm4,"re",axis.lim = c(0.1,10))
