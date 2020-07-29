
##################################################################
##                   Join Psychopy and Survey                   ##
##################################################################

library(dplyr)
library(readr)
library(here)


# Load survey data --------------------------------------------------------
survey_processed <- here::here(
  "data",
  "survey_responses.csv"
) %>% 
  read_csv()

# Load psychopy -----------------------------------------------------------

psychopy_processed <- here::here(
  "data",
  "psychopy_long.csv"
) %>% 
  read_csv() %>% 
  mutate(
    id = as.numeric(participant),
    factor = dplyr::recode_factor(affectcat,
      "1" = "Positive/High",
      "2" = "Negative/High",
      "3" = "Negative/Low",
      "4" = "Positive/Low"
    )
  )


# Join files  -------------------------------------------------------------

self_report_join <- dplyr::inner_join(
  survey_processed,
  psychopy_processed,
  by = "id"
)

average_self_report <- self_report_join %>% 
  group_by(id, factor) %>% 
  select(
    tas,
    depression, 
    arousal_rating,
    valence_rating,
    liking
  ) %>% 
  summarise_all(mean, na.rm = TRUE)


# Export files ------------------------------------------------------------

write.csv(
  self_report_join,
  here::here(
    "data",
    "self_report_only.csv"
  )
)

write.csv(
  average_self_report,
  here::here(
    "data",
    "grouped_self_report.csv"
  )
)




