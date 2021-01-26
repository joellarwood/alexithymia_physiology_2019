
#################################################################
##                       Merge Data Sets                       ##
#################################################################


# Psychopy data -----------------------------------------------------------

reaction_data <- here::here(
  "data",
  "psychopy_long.csv"
) %>% 
  read_csv() %>% 
  mutate(
    participant = as.numeric(participant),
    id_trial = paste0(participant, "_", trial_number)
  )

# Qualtrics Data ----------------------------------------------------------

z_score <- function(x){
  (x-mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)
}

survey_data <- here::here(
  "data",
  "survey_responses.csv"
) %>% 
  read_csv() %>% 
  select(
    id:eot,
    gender:yearsplay
  ) %>% 
  mutate(
    across(
      c(tas:eot),
      z_score,
      .names = "{col}_z"
      )
  )


# Physiology Data ---------------------------------------------------------

physiology_data <- here::here(
  "data",
  "physiology_data_processed.Rds"
) %>% 
  read_rds() %>% 
  mutate(
    id_trial = paste0(pid, "_", trial)
  ) %>% 
  filter(
    all_trials == TRUE)

length(unique(physiology_data$pid))

# Merge Data --------------------------------------------------------------

merged <- reaction_data %>% 
  left_join(survey_data,
            by = c("participant" = "id")) %>% 
  left_join(physiology_data,
            by = "id_trial") %>% 
  select(-pid) %>% 
    rename(
      "pid" = "participant"
    )

length(unique(merged$pid))

# Export all data ---------------------------------------------------------

merged %>% 
  write_rds(
    here::here(
      "data",
      "all_data.Rds"
    )
  )
