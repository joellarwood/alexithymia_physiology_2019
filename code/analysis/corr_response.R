
#################################################################
##                        Model corr                        ##
#################################################################

# Load packages -----------------------------------------------------------

library(emmeans) # Estimated Marginal Means, aka Least-Squares Means
library(tidyverse) # Easily Install and Load the 'Tidyverse'
library(lmerTest) # Linear Mixed-Effects Models using 'Eigen' and S4
library(AICcmodavg) # Model Selection and Multimodel Inference Based on (Q)AIC(c)
source("code/apa_f.r")

# Import Data -------------------------------------------------------------

corr_all <-here::here(
  "data",
  "all_data.Rds"
) %>%
  read_rds() %>%
  mutate(
    liking = liking - 3
  ) 

corr_log <- here::here(
  "data",
  "log.csv"
) %>%
  read_csv() %>%
  rename(
    pid = P
  ) %>% 
  filter(
    grepl("bad", Corr)
  ) %>%
  select(pid)

corr <- corr_all %>% 
  anti_join(corr_log) %>% 
  filter(abs(corr_z) < 3) %>% 
  drop_na(liking, tas_z)


length(unique(corr$pid))
# Model Responses ---------------------------------------------------------

# Fit data to null model 

corr_null <- lmerTest::lmer(
  corr_pct ~ 1 + (1 | pid),
  data = corr,
  REML = FALSE
)

corr_rand <- lmerTest::lmer(
  corr_pct ~ 1 + (1 | pid) + (1 | song),
  data = corr,
  REML = FALSE
)

anova(corr_null, corr_rand) # null model is prefered

# Random structure is best 
# Model 1: Liking ------------------------------------------


corr_1 <- update(
  corr_null, 
  . ~ . + liking
)
# Model 2: Add factor -----------------------------------------------------
corr_2 <- update(
  corr_1,
  . ~ . + factor
)

# Model 4 Add interaction ------------------------------------------------

corr_3 <- update(
  corr_2,
  . ~ . + liking:factor
)
# Model 5 add TAS ---------------------------------------------------------

corr_4 <- update(
  corr_3,
  . ~ . + tas_z
)


corr_5 <- update(
  corr_4,
  . ~ . + tas_z:factor + tas_z:liking
)
# Model 6 add depression --------------------------------------------------

corr_5 <- update(
  corr_4,
  . ~ . + depression_z 
)

corr_6 <- update(
  corr_5,
  . ~ . + depression_z:factor + depression_z:liking
)


# LRT of model fit ------------------------------------------------------

anova(corr_1, corr_2, corr_3, corr_4, corr_5, corr_6)

# ANOVA of Model 3 --------------------------------------------------------

apa_f(corr_1)


# Simple slopes analysis --------------------------------------------------

# Plot --------------------------------------------------------------------

corr_plot <- emmeans::emmip(
  corr_1,
  ~ liking,
  CIs = TRUE,
  at = list(liking = c(-2, 0, 2))
) +
  scale_color_discrete(name = "") +
  ylab("Predicted corr") +
  xlab("Song liking") +
  ggplot2::theme_classic() +
  ggtitle("A")


