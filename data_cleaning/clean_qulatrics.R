#################################################################
##                      Process qualtrics                      ##
#################################################################


# Load libraries ----------------------------------------------------------

library(magrittr) # get pipe 
library(readr) # to laod data
library(dplyr) # to manipulate data
library(tidyr) # to tidy data
library(visdat) # to visualise data
library(janitor) # clean names
library(qualtRics) # to read qualtrics
library(gendercodeR) # to code gender

# Load file ---------------------------------------------------------------


survey <- here::here(
  "data",
  "qualtrics_survey.csv"
) %>% 
  qualtRics::read_survey() %>% 
  janitor::clean_names() %>% 
  filter(grepl('[0-9][0-9][0-9]', id)) %>% 
  mutate(id = as.integer(id))


# Clean file --------------------------------------------------------------

survey <- survey %>% 
  mutate(  #Reverse code items
    tas_4r = 6 - .$tas_4,
    tas_5r = 6 - .$tas_5,
    tas_10r = 6 - .$tas_10,
    tas_18r = 6 - .$tas_18,
    tas_19r = 6 - .$tas_19
  )

eot_vector <- c( #externally oreinted thinking
  "tas_5r", 
  "tas_8",
  "tas_10r",
  "tas_15",
  "tas_16",
  "tas_18r",
  "tas_19r",
  "tas_20"
  )
ddf_vector <- c( #difficulty describing feelings
  "tas_2", 
  "tas_4r", 
  "tas_11", 
  "tas_12", 
  "tas_17"
  )

dif_vector <- c( #difficulty identifying feelings 
  "tas_1",
  "tas_3", 
  "tas_6",
  "tas_7", 
  "tas_9", 
  "tas_13", 
  "tas_14"
  )

depression_vector <- c(# depression items 
  "dass_3", 
  "dass_5", 
  "dass_10", 
  "dass_13", 
  "dass_16", 
  "dass_17", 
  "dass_21"
  )


survey_clean <- survey %>% 
  mutate( # create scores
    dif = survey %>% #difficulty identifying feelings
      select(dif_vector) %>% 
      rowSums(), 
    ddf = survey %>% #difficulty describing feelings
      select(ddf_vector) %>% 
      rowSums(),
    eot = survey %>% #externally oriented thinking
      select(eot_vector) %>% 
      rowSums(), 
    depression = survey %>% # depression
      select(depression_vector) %>% 
      rowSums(), 
    yearsplay = forcats::as_factor(
      recode_factor(
        yearsplay, 
        "0" = "No musical experience", 
        "1" = "1 year musical experience",
        "2" = "2 years musical experience",
        "3" = "3 years musical experience", 
        "4" = "4 years musical experience",
        "5" = "5 years musical experience",
        "6" = "More than 5 years"
      )
    ), 
    musichours = forcats::as_factor(
      recode_factor(
        musichours,
        "0" = "Less than 1 hour", 
        "1" = "1 hour",
        "2" = "2 hours",
        "3" = "3 hours", 
        "4" = "4 hours",
        "5" = "5 hours",
        "6" = "More than 5 hours"
      )
    ),
    gender = gendercodeR::recode_gender(
      gender = gender, 
      dictionary = broad),
    age = as.numeric(age)
  ) %>% 
  mutate(
    tas = eot + ddf + dif
  ) %>% 
  filter(id !=106)


# filter down -------------------------------------------------------------

survey_small <- survey_clean %>% 
  select(id,
         tas,
         depression,
         dif,
         ddf,
         eot, 
         contains("tas"), 
         contains(depression_vector),
         gender,
         age,
         musichours,
         yearsplay
         )

# export qualtrics --------------------------------------------------------

write_csv(
  survey_small,
  "data/survey_responses.csv"
)


