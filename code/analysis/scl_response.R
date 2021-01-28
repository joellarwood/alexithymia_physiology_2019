
#################################################################
##                          Model SCL                          ##
#################################################################


# Load Packages -----------------------------------------------------------

library(tidyverse) # Easily Install and Load the 'Tidyverse'
library(lmerTest) # Tests in Linear Mixed Effects Models
library(emmeans) # Estimated Marginal Means, aka Least-Squares Means
source("code/apa_f.R") # Get APA summary of fixed effects
# Import Data -------------------------------------------------------------

scl_data <- here::here(
  "data",
  "all_data.Rds"
) %>%
  read_rds() %>%
  mutate(
    liking = liking - 3
  ) %>%
  filter(abs(tonic_z) < 3) %>% # remove scores above/below 3SDs
  drop_na(liking, tas_z)

length(unique(scl_data$pid))

log <- here::here(
  "data",
  "log.csv"
) %>%
  read_csv() %>%
  rename(
    pid = P
  )


# Get bad recordings ------------------------------------------------------

bad_tonic <- log %>%
  filter(!is.na(Eda)) %>%
  select(pid)

# Create tonic data --------------------------------------------------------

tonic <- scl_data %>%
  anti_join(bad_tonic,
    by = "pid"
  )

length(unique(scl_data$pid)) - length(unique(tonic$pid)) # 14 cases removed

# Model Responses ---------------------------------------------------------

# Model 1: Main Effect of Factor ------------------------------------------


tonic_1 <- lmerTest::lmer(
  tonic_z ~ factor + (1 + factor | pid) + (1 | song),
  data = tonic,
  contrasts = list(
    factor = contr.sum
  ),
  REML = FALSE
) # singulatrity warning but this is owing to person as the standarised scores are being used. The random slope and ranfom intercept of song allow a better fit acording to the AIC


# Model 2: Add liking -----------------------------------------------------
tonic_2 <- update(
  tonic_1,
  . ~ . + liking
)

# Model 3 Add Interactions ------------------------------------------------

tonic_3 <- update(
  tonic_2,
  . ~ . + liking:factor
)
# Model 4 add TAS ---------------------------------------------------------

tonic_4 <- update(
  tonic_3,
  . ~ . + tas_z + tas_z:factor
)


# Model 5 add depression --------------------------------------------------

tonic_5 <- update(
  tonic_4,
  . ~ . + depression_z + depression_z:factor
) # model failed to converge



# ANOVA of models ---------------------------------------------------------

anova(tonic_1, tonic_2, tonic_3, tonic_4, tonic_5)
# Compare Model Fits ------------------------------------------------------

sapply(
  paste0(
    "tonic_",
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
    AICdiff = AIC - AIC(tonic_5)
  ) # considewring parsimony model 1 is the best model

# Get anova table ---------------------------------------------------------

apa_f(tonic_1)



# Plot effect -------------------------------------------------------------

sjPlot::plot_model(
  tonic_1,
  type = "pred",
  terms = "factor"
)


