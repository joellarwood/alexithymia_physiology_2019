## Emotion differntiation 
library(tidyverse)
library(irr)
library(naniar)
library(psych)
library(car)


# Load data ---------------------------------------------------------------

differentiation_data <- here::here(
  "data",
  "self_report_only.csv"
) %>% 
  read_csv() 

# Define data file with negative trials
neg_trials <- differentiation_data %>% 
  filter(valence_target == "negative")

#Define data file with positive trials
pos_trials <- differentiation_data %>% 
  filter(valence_target == "positive")


# Negative Emotion Differntiation -----------------------------------------

participant <- unique(differentiation_data$id)

neg_diff <- NULL # init empty data list for loop 

for (i in participant){
  p_select = filter(neg_trials, id == i)
  word_select = select(p_select, 
                       anxious,
                       ashamed,
                       scared,
                       angry,
                       sad,
                       guilty,
                       frustrated)
  icc = irr::icc(word_select, model = "twoway", unit = "average")
  icc_temp = icc$value
  row_tmp = c(i, icc_temp)
  neg_diff = rbind(neg_diff, row_tmp)
  print(icc_temp)
}

colnames(neg_diff) = c("id", "neg_icc")

neg_diff <- neg_diff %>%
  as_tibble() %>%
  naniar::replace_with_na_at(
    .vars = c("neg_icc"),
    condition = ~(.x) < 0 # remove negative values as they cannot be interpreted
  ) %>%
  naniar::replace_with_na_at(
    .vars = c("neg_icc"),
    condition = ~(.x) > 1
  ) %>%
  mutate(neg_icc = 1 - neg_icc,
         neg_icc_fz = psych::fisherz(neg_icc)
  )


# Positive Emotion Differentiation  ---------------------------------------

pos_diff <- NULL # init empty data list for loop 

for (i in participant){
  p_select = filter(pos_trials, id == i)
  word_select = select(p_select, 
                       alert,
                       active,
                       happy,
                       excited,
                       tender
  )
  icc = irr::icc(word_select, model = "twoway", unit = "average")
  icc_temp = icc$value
  row_tmp = c(i, icc_temp)
  pos_diff = rbind(pos_diff, row_tmp)
  print(icc_temp)
}

colnames(pos_diff) = c("id", "pos_icc")

pos_diff <- pos_diff %>% 
  as_tibble() %>% 
  naniar::replace_with_na_at(
    .vars = c("pos_icc"), 
    condition = ~(.x) < 0 # remove negative values as they cannot be interpreted 
  ) %>% 
  naniar::replace_with_na_at(
    .vars = c("pos_icc"), 
    condition = ~(.x) > 1
  ) %>% 
  mutate(pos_icc = 1 - pos_icc,
         pos_icc_fz = psych::fisherz(pos_icc)
  )


# Merge data --------------------------------------------------------------

icc_data <- differentiation_data %>% 
  select(id, 
         depression,
         tas,
         eot,
         ddf,
         dif,
         gender,
         age) %>% 
  distinct(id, .keep_all = TRUE) %>% 
  inner_join(neg_diff, by = "id") %>% 
  inner_join(pos_diff, by = "id") 

## Write Data 

icc_data %>% 
  write_csv(here::here(
    "data",
    "icc_values.csv"
  ))
