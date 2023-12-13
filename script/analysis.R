library(tidyverse)
library(lme4)
library(ggbeeswarm)

# outlier removal:
# 1. Hard limits for too long/too short
# 1.5 Need to log transform the RT
# 2. Standardization removal
# 3. Participant removal

df.subj <- read_csv("~/Downloads/lbq_selectedcolumns_aug2023.csv")
df.subj %>%
  group_by(sonaID) %>%
  count() %>%
  arrange(desc(n)) %>%
  print(n = 200)

df.subj %>%
  pull(Gender) %>%
  unique()

df.dur <- read_csv("../data/audio_duration_CVenvironment.csv") %>%
  mutate(Filename = str_sub(Filename, end = -5))

df.subj.aug <- read_csv("../data/participants_aug2023.csv")
df.subj.dec <- read_csv("../data/participants_dec2023.csv")
df.subj <- bind_rows(df.subj.aug, df.subj.dec)
df.subj <- df.subj %>%
  group_by(sonaID) %>%
  summarize(across(everything(), last))

df.raw <- read_csv("../data/fric.categorization.df.csv")

df.subj %>% group_by(sonaID) %>% count() %>% arrange(desc(n)) %>% print(n = 200)

df.raw <- df.raw %>%
  left_join(x = ., y = df.subj, by = c("id" = "sonaID"))



df.raw <- read_csv("../data/fric.categorization.df.csv") %>%
  left_join(x = ., y = df.subj, by = c("id" = "sonaID")) %>%
  filter(SLH == 'No', EnglishNative == 'Yes', EN_AOA <= 5) %>%
  dplyr::select(-word, -CategorizeWhat) %>%
  mutate(filename = str_sub(filename, end = -5)) %>%
  left_join(x = ., y = df.dur, by = c("filename" = "Filename")) %>%
  mutate(
    TYPE = if_else(CategoryLabel %in% c("S", "SH"), "C", "CV"),
    CONS = str_extract(CategoryLabel, "(SH|S)"),
    CORRECT = Response == CONS,
    KEPT = if_else(is.na(Response), 0, 1),
    Duration = Duration * 1000,
    KEPT = if_else(KEPT == 1, if_else(Duration <= RT, 1, 0), 0),
    LOG_RT = if_else(KEPT == 1, log(RT), NA)
  ) %>%
  dplyr::select(-CategoryLabel) %>%
  group_by(id, talker) %>%
  mutate(
    SE = abs((LOG_RT - mean(LOG_RT, na.rm = TRUE)) / sd(LOG_RT, na.rm = TRUE)),
    KEPT = if_else(KEPT == 1, if_else(SE >= 2, 0, 1), 0),
    KEPT = if_else(id %in% c(38371, 38689, 38677, 38644, 38038, 37966, 35083, 37948, 38686, 36430), 0, KEPT)
  )

df.raw %>%
  write_csv(file = "../data/raw.csv")
  
df.full <- df.raw %>%
  filter(KEPT == 1)

df.full %>%
  group_by(talker, TYPE) %>%
  summarize(mean(CORRECT))

df.full %>%
  mutate(talker = as.factor(talker),
         talker = fct_relevel(talker, "159", "2470", "115", "2471", "2453", "2436")) %>%
  group_by(talker, TYPE, id, CONS) %>%
  summarize(M = mean(CORRECT)) %>%
  ggplot(aes(x = as.factor(talker), y = M)) +
  theme_bw() +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.5) +
  facet_grid(CONS ~ TYPE)

df.full %>%
  mutate(talker = as.factor(talker),
         talker = fct_relevel(talker, "159", "2470", "115", "2471", "2453", "2436")) %>%
  ggplot(aes(x = as.factor(talker), y = LOG_RT)) +
  theme_bw() +
  geom_violin() +
  #geom_jitter(width = 0.1) +
  facet_grid(CONS ~ TYPE)

df.full <- df.full %>%
  mutate(talker = as.factor(talker))

m <- glmer(formula = CORRECT ~ TYPE * CONS * talker + (1 | id) + (1 | word), data = df.full,
           family = binomial(link = "logit"))

summary(m)

# Q: coding
# SAA1 = socket
# SAE1 = salmon, satellite, saddle
# SAH0 = cement
# SAH1 = sunroof, submarine
# SAH2 = submarine
# SAO1 = sauna
# SAY1 = sidewalk
# SEH = ceremony
# SEH1 = seminar, settlement, cemetery, sector, ceremony
# SER1 = surfboard, syrup
# SEY1 = sane, sailboat
# SHAA1 = shoplifter, charlatan
# SHAE0 = shampoo, chandelier
# SHAE1 = shadow, chaperone
# SHAH1 = shuffle, shudder, shovel
# SHAO1 = shortcut
# SHAY1 = shiny
# SHEH1 = shelter, shareholder
# SHER0 = charade
# SHEY1 = shady, Shane
# SHIH1 = shipment, shindig, chivalry
# SHIY1 = sheep
# SHOW1 = show, shoulder
# SHUH1 = sugar
# SIH1 = syrup, silver
# SIY1 = seem, seedling, ceiling
# SOW1 = soak

df.raw %>%
  filter(CategoryLabel == "SH") %>%
  pull(word) %>%
  unique()

# Q: why do some people have 275 and some have 274 trials?
# "chaperone" from 2436 seems to be missing from some people
df.raw %>%
  group_by(id) %>%
  count() %>%
  print(n = 200)

df1 <- df.raw %>%
  filter(id == 36430) %>%
  dplyr::select(talker, word) %>%
  unique()

df2 <- df.raw %>%
  filter(id == 25522) %>%
  dplyr::select(talker, word) %>%
  unique()

setdiff(df2, df1)

df.raw %>%
  group_by(id) %>%
  filter(n() == 274) %>%
  dplyr::select(id) %>%
  unique() %>%
  nrow()

df.raw %>%
  group_by(id) %>%
  filter(n() == 274) %>%
  filter(word == "chaperone", talker == 2436)

df.raw %>%
  filter(word == "chaperone", talker == 2436) %>%
  dplyr::select(id) %>%
  unique() %>%
  print(n = 200)

# Q: How do we filter out participants based on trials without response?
df.raw %>%
  filter(is.na(Response)) %>%
  group_by(id) %>%
  count() %>%
  arrange(desc(n)) %>%
  print(n = 200)

df.raw %>%
  group_by(id) %>%
  summarize(
    IS_NA = mean(is.na(Response))
  ) %>%
  arrange(desc(IS_NA)) %>%
  print(n = 200)

# Q: filerting based on RT (too short or too long)
# Q: how is RT measured
# Q: the duration of each stimulus
set.seed(11)
df.raw %>%
  filter(id %in% sample(unique(.$id), 25)) %>%
  ggplot(aes(x = RT, group = CategoryLabel, fill = CategoryLabel)) +
  geom_histogram() +
  facet_wrap(. ~ id, nrow = 5)

df.raw %>%
  filter(!is.na(Response)) %>%
  group_by(id) %>%
  summarize(
    CORR_PROP = mean(CORRECT)
  ) %>%
  arrange(desc(CORR_PROP)) %>%
  print(n = 200)

df.raw %>%
  filter(!is.na(Response)) %>%
  group_by(CategoryLabel) %>%
  summarize(
    CORR_PROP = mean(CORRECT)
  ) %>%
  arrange(desc(CORR_PROP)) %>%
  print(n = 200)




