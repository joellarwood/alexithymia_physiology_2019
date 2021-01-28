
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
    pid = as.numeric(participant)
  ) %>% 
  select(-X1) 

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
  ) %>% 
  rename(
    pid = id
  )


# Physiology Data ---------------------------------------------------------

physiology_data <- here::here(
  "data",
  "physiology_data_processed.Rds"
) %>% 
  read_rds() %>% 
  filter(
    all_trials == TRUE &
    trial != 0)

length(unique(physiology_data$pid))

# Merge Data --------------------------------------------------------------

merged <- reaction_data %>% 
  full_join(physiology_data, by = c("pid", "trial")) %>% 
  full_join(survey_data, by = "pid") %>% 
  mutate(
    factor = as.factor(affect_label)
  )


# Export all data ---------------------------------------------------------

merged %>% 
  write_rds(
    here::here(
      "data",
      "all_data.Rds"
    )
  )

