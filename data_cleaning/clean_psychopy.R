#################################################################
##                  Processing psychopy files                  ##
#################################################################

# Load required packages --------------------------------------------------
library(magrittr) # get pipe 
library(readr) # to laod data
library(dplyr) # to manipulate data
library(tidyr) # to tidy data
library(visdat) # to visualise data
library(janitor) # clean names


# Process single file -----------------------------------------------------

raw <- here::here(
  "data", 
  "individual_psychopy_data", 
  "001_MusicAffectPsychophysiology2019_2019_May_07_0907.csv"
) %>% 
  readr::read_csv() %>% 
  janitor::clean_names() %>% 
  tidyr::drop_na(
    song
  )
  

glimpse(raw)

raw_shrink <- raw %>% 
  select( #keep required variables
   participant, 
   song, 
   valence,
   arousal, 
   affectcat,
   songmark,
   word,
   songtrials_this_trial_n,
   songtrials_this_rep_n,
   songtrials_this_n,
   arousalkey_keys,
   valencekey_keys,
   likekey_keys,
   word_response_keys
  ) %>% 
  rename( # make names more clear
    valence_target = valence, 
    arousal_target = arousal, 
    rep_number = songtrials_this_rep_n,
    trial_number = songtrials_this_n,
    arousal_rating = arousalkey_keys,
    valence_rating = valencekey_keys,
    liking = likekey_keys,
    emotion_rating = word_response_keys
  ) %>% 
  mutate( # code factors
    valence_target = recode_factor(
      valence_target, 
      "1" = "negative", 
      "2" = "positive"
    ), 
    arousal_target = recode_factor(
      arousal_target, 
      "1" = "low", 
      "2" = "high"
    ), 
    emotion_target = recode_factor(
      affectcat, 
      "1" = "happy", 
      "2" = "fear", 
      "3" = "sad",
      "4" = "tender"
    ), 
    affect_label = recode_factor(
      affectcat, 
      "1" = "Positive/High", 
      "2" = "Negative/High", 
      "3" = "Negative/Low",
      "4" = "Positive/Low"
    )
  )


raw_spread <- raw_shrink %>% 
  tidyr::pivot_wider( # pivot wider so that each song has only one row with emotion rating as column
    id_cols = trial_number, 
    names_from = word,
    values_from = emotion_rating
  ) %>% 
  janitor::clean_names() 

raw_valence_arousal <- raw_shrink %>% 
  drop_na(valence_rating)


processed <- left_join(
  raw_valence_arousal, 
  raw_spread, 
  by = "trial_number"
)



# Create function ---------------------------------------------------------

psychopy_process <- function(x) {
  # clean file
  tmp_data <- x %>% 
    janitor::clean_names() %>% # make name snake case
    tidyr::drop_na( # drop practice trial
      song
    ) %>%
    select( # keep required variables
      participant,
      song,
      valence,
      arousal,
      affectcat,
      songmark,
      word,
      songtrials_this_trial_n,
      songtrials_this_rep_n,
      songtrials_this_n,
      arousalkey_keys,
      valencekey_keys,
      likekey_keys,
      word_response_keys
    ) %>%
    rename( # make names more clear
      valence_target = valence,
      arousal_target = arousal,
      rep_number = songtrials_this_rep_n,
      trial_number = songtrials_this_n,
      arousal_rating = arousalkey_keys,
      valence_rating = valencekey_keys,
      liking = likekey_keys,
      emotion_rating = word_response_keys
    ) %>%
    mutate( # code factors
      valence_target = recode_factor(
        valence_target,
        "1" = "negative",
        "2" = "positive"
      ),
      arousal_target = recode_factor(
        arousal_target,
        "1" = "low",
        "2" = "high"
      ),
      emotion_target = recode_factor(
        affectcat,
        "1" = "happy",
        "2" = "fear",
        "3" = "sad",
        "4" = "tender"
      ),
      affect_label = recode_factor(
        affectcat, 
        "1" = "Positive/High", 
        "2" = "Negative/High", 
        "3" = "Negative/Low",
        "4" = "Positive/Low"),
      # centre within cluster 
      arousal_rating_c = scale(arousal_rating, scale = FALSE),
      liking_c = scale(liking, scale = FALSE),
      valence_rating_c = scale(valence_rating, scale = FALSE)
    )

  tmp_spread <- tmp_data %>%
    tidyr::pivot_wider( # pivot wider so that each song has only one row with emotion rating as column
      id_cols = trial_number,
      names_from = word,
      values_from = emotion_rating
    ) %>%
    janitor::clean_names() # clean names of emotion words

  tmp_v_a <- tmp_data %>%
    drop_na(valence_rating) # get only row with affect ratings


  tmp_processed <- left_join(
    tmp_v_a,
    tmp_spread,
    by = "trial_number"
  )
}



# test_function -----------------------------------------------------------

test <- here::here(
  "data", 
  "individual_psychopy_data", 
  "001_MusicAffectPsychophysiology2019_2019_May_07_0907.csv"
) %>% 
  read_csv() %>% 
  psychopy_process()

identical(test, processed)

visdat::vis_dat(test)


# Process and Merge -------------------------------------------------------

## File directory 

psychopy_files <- here::here(
  "data",
  "individual_psychopy_data"
) %>% 
  list.files()


base_dir <- here::here(
  "data",
  "individual_psychopy_data"
)

psychopy_data <- data.frame()

for (file in 1:length(psychopy_files)){
  tmp_file <- psychopy_files[file] # select file
  tmp_full_path <- paste0(base_dir, "/", tmp_file) # create file path
  tmp_csv <- read_csv(tmp_full_path)
  tmp_processed <- psychopy_process(tmp_csv) #process file
  psychopy_data <- rbind(psychopy_data, tmp_processed) #add to long dataset
}


# wrtie file --------------------------------------------------------------

write.csv(
  psychopy_data, 
  "data/psychopy_long.csv"
)
