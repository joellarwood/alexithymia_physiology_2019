
#################################################################
##                        Model Valence                        ##
#################################################################

# Load packages -----------------------------------------------------------

library(emmeans) # Estimated Marginal Means, aka Least-Squares Means
library(tidyverse) # Easily Install and Load the 'Tidyverse'
library(lmerTest) # Linear Mixed-Effects Models using 'Eigen' and S4
library(AICcmodavg) # Model Selection and Multimodel Inference Based on (Q)AIC(c)
source("code/apa_f.r")

# Import Data -------------------------------------------------------------

data <- here::here(
  "data",
  "all_data.Rds"
) %>%
  read_rds() %>%
  mutate(
    liking = liking - 3,
    pid = as.factor(pid)
  ) %>% 
  drop_na(tas_z, liking)

# Model Responses ---------------------------------------------------------



# Model 1: Liking ------------------------------------------


valence_1 <- update(
  lmerTest::lmer(
    valence_rating ~ liking + (1 + factor | pid) + (1 | song),
    data = data,
    contrasts = list(factor = contr.sum),
  ),
  REML = FALSE
)

# Model 2: Add factor -----------------------------------------------------
valence_2 <- update(
  valence_1,
  . ~ . + factor
)

# Model 4 Add interaction ------------------------------------------------

valence_3 <- update(
  valence_1,
  . ~ . + liking:factor
)
# Model 5 add TAS ---------------------------------------------------------

valence_4 <- update(
  valence_3,
  . ~ . + tas_z + tas_z:factor 
) 


# Model 6 add depression --------------------------------------------------

valence_5 <- update(
  valence_4,
  . ~ . + depression_z + depression_z:factor 
) 


# Get conditional AIC -----------------------------------------------------

v_tab <- AICcmodavg::aictab(
  list(valence_1, valence_2, valence_3, valence_4, valence_5),
  second.ord = TRUE,
  sort = FALSE
) 

data.frame(v_tab) %>% 
  mutate(
    delta = round(Delta_AICc, 2)
  ) %>% 
  select(delta)

AICcmodavg::evidence(v_tab, model.low = "Mod4") #evidence ratio for selected over TAS


# ANOVA of Model 3 --------------------------------------------------------

apa_f(valence_3)


# Marginal Means Analysis -------------------------------------------------

emmeans::emmeans(
  valence_3,
  pairwise ~ factor, 
  infer = TRUE
)
# Simple slopes analysis --------------------------------------------------

emmeans::emtrends(
  valence_3,
  pairwise ~ factor,
  var = "liking",
  infer = TRUE
)


# Plot --------------------------------------------------------------------

v_plot <- emmeans::emmip(
  valence_3,
  factor ~ liking,
  CIs = TRUE,
  at = list(liking = c(-2, 2))
) +
  scale_color_discrete(name = "") +
  ylab("Predicted Valence") + 
  xlab("Song liking") +
  ggplot2::theme_classic() + 
  ggtitle("A")

