
##################################################################
##              Music, Physiology, and Alexithymia              ##
##                        Model Building                        ##
##################################################################


###  Here I am fitting each of the models and exporting to .rds
###  This is so I can load them directly into the .Rmd document


# Load packages -----------------------------------------------------------
library(tidyverse) # Easily Install and Load the 'Tidyverse'
library(emmeans) # Estimated Marginal Means, aka Least-Squares Means
library(afex) # Analysis of Factorial Experiments

# Load in dataframes ------------------------------------------------------

responses <- here::here(
  "data",
  "psychopy_long.csv"
) %>% 
  read_csv() %>% 
  mutate(
    id = as.factor(str_remove(participant, "^0+")),
    song = as.factor(songmark),
    factor = fct_relevel(
      affect_label, 
      "Positive/Low",
      "Positive/High",
      "Negative/Low",
      "Negative/High"
    )
  )

glimpse(responses)

survey <- here::here(
  "data",
  "survey_responses.csv"
) %>%
  read_csv() %>%
  mutate(
    id = as.factor(id),
    tas_z = scale(tas),
    dep_z = scale(depression)
  )


### Add physiology files here


data <- responses %>%
  left_join(select
  (survey, tas_z, dep_z, id, age, gender),
  by = "id"
  )

glimpse(data)




# Valence -----------------------------------------------------------------


# Fit models --------------------------------------------------------------


valence_factor <- afex::mixed(
  valence_rating ~ factor + (1 + factor | id) + (1 | song),
  data = data
)


valence_factor_tas <- afex::mixed(
  valence_rating ~ factor * tas_z + (1 + factor | id) + (1 | song),
  data = data
)

valence_factor_liking <- afex::mixed(
  valence_rating ~ factor * liking_c + (1 + factor | id) + (1 | song),
  data = data
)

valence_factor_liking_tas <- afex::mixed(
  valence_rating ~ factor * liking_c + factor * tas_z + (1 + factor | id) + (1 | song),
  data = data
)

valence_factor_liking_tas_dep <- afex::mixed(
  valence_rating ~ factor * liking_c + factor * tas_z + factor * dep_z + (1 + factor | id) + (1 | song),
  data = data
)

beepr::beep(4)
beepr::beep(8)

# Write models ------------------------------------------------------------

write_rds(
  valence_factor,
  path = here::here(
    "analysis",
    "models",
    "valence",
    "valence_factor.rds"
  )
)

write_rds(
  valence_factor_tas,
  path = here::here(
    "analysis",
    "models",
    "valence",
    "valence_factor_tas.rds"
  )
)


write_rds(
  valence_factor_liking,
  path = here::here(
    "analysis",
    "models",
    "valence",
    "valence_factor_liking.rds"
  )
)

write_rds(
  valence_factor_liking_tas,
  path = here::here(
    "analysis",
    "models",
    "valence",
    "valence_factor_liking_tas.rds"
  )
)

write_rds(
  valence_factor_liking_tas_dep,
  path = here::here(
    "analysis",
    "models",
    "valence",
    "valence_factor_liking_tas_dep.rds"
  )
)

# Arousal -----------------------------------------------------------------

arousal_factor <- afex::mixed(
  arousal_rating ~ factor + (1 + factor | id) + (1 | song),
  data = data
)
beepr::beep(4)


arousal_factor_tas <- afex::mixed(
  arousal_rating ~ factor * tas_z + (1 + factor | id) + (1 | song),
  data = data
)
beepr::beep(4)

arousal_factor_liking <- afex::mixed(
  arousal_rating ~ factor * liking_c + (1 + factor | id) + (1 | song),
  data = data
)
beepr::beep(4)

arousal_factor_liking_tas <- afex::mixed(
  arousal_rating ~ factor * liking_c + factor * tas_z + (1 + factor | id) + (1 | song),
  data = data
)
beepr::beep(4)

arousal_factor_liking_tas_dep <- afex::mixed(
  arousal_rating ~ factor * liking_c + factor * tas_z + factor * dep_z + (1 + factor | id) + (1 | song),
  data = data
)

beepr::beep(4)
beepr::beep(8)

# Write models ------------------------------------------------------------

write_rds(
  arousal_factor,
  path = here::here(
    "analysis",
    "models",
    "arousal",
    "arousal_factor.rds"
  )
)

write_rds(
  arousal_factor_tas,
  path = here::here(
    "analysis",
    "models",
    "arousal",
    "arousal_factor_tas.rds"
  )
)


write_rds(
  arousal_factor_liking,
  path = here::here(
    "analysis",
    "models",
    "arousal",
    "arousal_factor_liking.rds"
  )
)

write_rds(
  arousal_factor_liking_tas,
  path = here::here(
    "analysis",
    "models",
    "arousal",
    "arousal_factor_liking_tas.rds"
  )
)

write_rds(
  arousal_factor_liking_tas_dep,
  path = here::here(
    "analysis",
    "models",
    "arousal",
    "arousal_factor_liking_tas_dep.rds"
  )
)