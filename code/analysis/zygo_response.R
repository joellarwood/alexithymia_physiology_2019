
#################################################################
##                      Model Zygomaticus                      ##
#################################################################

# Load Packages -----------------------------------------------------------

library(tidyverse) # Easily Install and Load the 'Tidyverse'
library(lmerTest) # Tests in Linear Mixed Effects Models
library(emmeans) # Estimated Marginal Means, aka Least-Squares Means
source("code/apa_f.R") # Get APA summary of fixed effects
# Import Data -------------------------------------------------------------

zygo_data <- here::here(
  "data",
  "all_data.Rds"
) %>%
  read_rds() %>%
  mutate(
    liking = liking - 3
  ) %>%
  filter(abs(zygo_z) < 3) %>% # remove scores above/below 3SDs
  drop_na(liking, tas_z)

length(unique(zygo_data$pid))

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

bad_zygo <- log %>%
  filter(!is.na(Zygo)) %>%
  select(pid) 

nrow(bad_zygo)

remove_zygo <- bad_zygo %>% 
  full_join(questionable)

# Create zygo data --------------------------------------------------------

zygo <- zygo_data %>%
  anti_join(remove_zygo,
            by = "pid"
  )

length(unique(zygo_data$pid)) - length(unique(zygo$pid)) # 53 cases removed

# Model Responses ---------------------------------------------------------

# Model 1: Main Effect of Factor ------------------------------------------


zygo_1 <- lmerTest::lmer(
  zygo_z ~ factor + (1 | pid),
  data = zygo,
  contrasts = list(
    factor = contr.sum
  ),
  REML = FALSE
) # singulatrity warning but this is owing to person as the standarised scores are being used. The random slope and ranfom intercept of song allow a better fit acording to the AIC


AIC(zygo_1)
# Model 2: Add liking -----------------------------------------------------
zygo_2 <- update(
  zygo_1,
  . ~ . + liking
)

# Model 3 Add Interactions ------------------------------------------------

zygo_3 <- update(
  zygo_2,
  . ~ . + liking:factor
)
# Model 4 add TAS ---------------------------------------------------------

zygo_4 <- update(
  zygo_3,
  . ~ . + tas_z + tas_z:factor
)


# Model 5 add depression --------------------------------------------------

zygo_5 <- update(
  zygo_4,
  . ~ . + depression_z + depression_z:factor
) # model failed to converge



# ANOVA of models ---------------------------------------------------------

anova(zygo_1, zygo_2, zygo_3, zygo_4, zygo_5)
# Compare Model Fits ------------------------------------------------------

sapply(
  paste0(
    "zygo_",
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
    AICdiff = AIC - AIC(zygo_5)
  ) # considewring parsimony model 1 is the best model

# Get anova table ---------------------------------------------------------

apa_f(zygo_1)



# Plot effect -------------------------------------------------------------

sjPlot::plot_model(
  zygo_1,
  type = "pred",
  terms = "factor"
)




