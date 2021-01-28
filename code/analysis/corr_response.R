
#################################################################
##                      Model Corrugator                       ##
#################################################################


# Load Packages -----------------------------------------------------------

library(tidyverse) # Easily Install and Load the 'Tidyverse'
library(lmerTest) # Tests in Linear Mixed Effects Models
library(emmeans) # Estimated Marginal Means, aka Least-Squares Means
source("code/apa_f.R") # Get APA summary of fixed effects
# Import Data -------------------------------------------------------------

corr_data <- here::here(
  "data",
  "all_data.Rds"
) %>%
  read_rds() %>%
  mutate(
    liking = liking - 3
  ) %>%
  filter(abs(corr_z) < 3) %>% # remove scores above/below 3SDs
  drop_na(liking, tas_z)

length(unique(corr_data$pid))

log <- here::here(
  "data",
  "log.csv"
) %>%
  read_csv() %>%
  rename(
    pid = P
  )

questionable <- here::here(
  "data",
  "improbable_emg.Rds"
) %>% 
  read_rds()

# Get bad recordings ------------------------------------------------------

bad_corr <- log %>%
  filter(!is.na(Corr)) %>%
  select(pid) 

nrow(bad_corr)

remove_corr <- bad_corr %>% 
  full_join(questionable)

# Create corr data --------------------------------------------------------

corr <- corr_data %>%
  anti_join(remove_corr,
            by = "pid"
  )

length(unique(corr_data$pid)) - length(unique(corr$pid)) # 53 cases removed

# Model Responses ---------------------------------------------------------

# Model 1: Main Effect of Factor ------------------------------------------


corr_1 <- lmerTest::lmer(
  corr_z ~ factor + (1 | pid) + (1|song),
  data = corr,
  contrasts = list(
    factor = contr.sum
  ),
  REML = FALSE
) # singulatrity warning but this is owing to person as the standarised scores are being used. The random slope and ranfom intercept of song allow a better fit acording to the AIC


AIC(corr_1)
# Model 2: Add liking -----------------------------------------------------
corr_2 <- update(
  corr_1,
  . ~ . + liking
)

# Model 3 Add Interactions ------------------------------------------------

corr_3 <- update(
  corr_2,
  . ~ . + liking:factor
)
# Model 4 add TAS ---------------------------------------------------------

corr_4 <- update(
  corr_3,
  . ~ . + tas_z + tas_z:factor
)


# Model 5 add depression --------------------------------------------------

corr_5 <- update(
  corr_4,
  . ~ . + depression_z + depression_z:factor
) # model failed to converge



# ANOVA of models ---------------------------------------------------------

anova(corr_1, corr_2, corr_3, corr_4, corr_5)
# Compare Model Fits ------------------------------------------------------

sapply(
  paste0(
    "corr_",
    c(1:5)
  ),
  function(x) {
    AIC(get(x))
  }
) %>%
  data.frame() %>%
  rename(
    AIC = "."
  ) %>%
  mutate(
    AICdiff = AIC - AIC(corr_5)
  ) # considewring parsimony model 1 is the best model

# Get anova table ---------------------------------------------------------

apa_f(corr_1)



# Plot effect -------------------------------------------------------------

sjPlot::plot_model(
  corr_1,
  type = "pred",
  terms = "factor"
)

