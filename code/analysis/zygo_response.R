
#################################################################
##                        Model zygo                        ##
#################################################################

# Load packages -----------------------------------------------------------

library(emmeans) # Estimated Marginal Means, aka Least-Squares Means
library(tidyverse) # Easily Install and Load the 'Tidyverse'
library(lmerTest) # Linear Mixed-Effects Models using 'Eigen' and S4
library(AICcmodavg) # Model Selection and Multimodel Inference Based on (Q)AIC(c)
source("code/apa_f.r")

# Import Data -------------------------------------------------------------

zygo_all <-here::here(
  "data",
  "all_data.Rds"
) %>%
  read_rds() %>%
  mutate(
    liking = liking - 3
  ) 

zygo_log <- here::here(
  "data",
  "log.csv"
) %>%
  read_csv() %>%
  rename(
    pid = P
  ) %>% 
  filter(
    grepl("bad", Zygo)
  ) %>%
  select(pid)

zygo <- zygo_all %>% 
  anti_join(zygo_log) %>% 
  filter(abs(zygo_z) < 3) %>% 
  drop_na(liking, tas_z)


length(unique(zygo$pid))
# Model Responses ---------------------------------------------------------

# Fit data to null model 

zygo_null <- lmerTest::lmer(
  zygo_pct ~ 1 + (1 | pid),
  data = zygo,
  REML = FALSE
)

zygo_rand <- lmerTest::lmer(
  zygo_pct ~ 1 + (1 | pid) + (1 | song),
  data = zygo,
  REML = FALSE
)

anova(zygo_null, zygo_rand)

# Random structure is best 
# Model 1: Liking ------------------------------------------


zygo_1 <- update(
  zygo_null, 
  . ~ . + liking
)
# Model 2: Add factor -----------------------------------------------------
zygo_2 <- update(
  zygo_1,
  . ~ . + factor
)

# Model 4 Add interaction ------------------------------------------------

zygo_3 <- update(
  zygo_2,
  . ~ . + liking:factor
)
# Model 5 add TAS ---------------------------------------------------------

zygo_4 <- update(
  zygo_3,
  . ~ . + tas_z
)


zygo_5 <- update(
  zygo_4,
  . ~ . + tas_z:factor + tas_z:liking
)
# Model 6 add depression --------------------------------------------------

zygo_5 <- update(
  zygo_4,
  . ~ . + depression_z 
)

zygo_6 <- update(
  zygo_5,
  . ~ . + depression_z:factor + depression_z:liking
)


# LRT of model fit ------------------------------------------------------

anova(zygo_1, zygo_2, zygo_3, zygo_4, zygo_5, zygo_6)

# ANOVA of Model 3 --------------------------------------------------------

apa_f(zygo_3)

# Marginal Means Analysis -------------------------------------------------

emmeans::emmeans(
  zygo_3,
  pairwise ~ factor,
  infer = TRUE)
# Simple slopes analysis --------------------------------------------------

emmeans::emtrends(
  zygo_3,
  pairwise ~ factor,
  var = "liking",
  infer = TRUE
)


# Plot --------------------------------------------------------------------

zygo_plot <- emmeans::emmip(
  zygo_3,
  factor ~ liking,
  CIs = TRUE,
  at = list(liking = c(-2, 0, 2))
) +
  scale_color_discrete(name = "") +
  ylab("Predicted Zygo") +
  xlab("Song liking") +
  ggplot2::theme_classic() +
  ggtitle("A")


