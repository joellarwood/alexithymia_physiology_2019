library(reticulate)

pd <- reticulate::import("pandas")


physiology_path <- here::here("data", "physiology_data")

physiology_files <- list.files(physiology_path, full.names = TRUE, pattern = ".xls")

pct_response <- function(baseline, response){
  (response / baseline) * 100
}

mad_out <- function(x, threshold = 3){ # print absolute MADs per row and mark outlier
  if_else(abs(x - median(x)) / mad(x)> threshold,
          TRUE,
          FALSE)
}


physiology <- data.frame()
participant_tmp = NULL
for (file in physiology_files){
  participant_tmp <- str_match(file, "(...?)\\.xls")[2]
  print(participant_tmp)
  data_tmp <- pd$read_excel(file)%>% 
    transmute(
      zygo = Mean,
      corr = Mean.1,
      tonic = Mean.2,
      events = `Evt_count...`
    ) %>%  
    filter(!grepl("cycles", zygo) 
           & !grepl("Mean", zygo)
           & !is.na(zygo)
    )
  trials_tmp <- nrow(data_tmp)/2
  data_tmp["trial_index"] <- rep(1:trials_tmp, times = 2)
  data_tmp["epoch_index"] <- rep(0:1, each = trials_tmp)
  reshape_tmp <- data_tmp %>% 
    pivot_wider(
    values_from = zygo:events,
    names_from = epoch_index
  ) %>%
    mutate_all(
      as.numeric
    ) %>% 
    mutate(
      zygo = zygo_1 - zygo_0,
      zygo_z = scale(zygo_1) - scale(zygo_0),
      zygo_pct = pct_response(zygo_0, zygo_1),
      zygo_out = mad_out(zygo),
      corr = corr_1 -corr_0,
      corr_z = scale(corr_1) - scale(corr_0),
      corr_z = scale(corr),
      corr_pct = pct_response(corr_0, corr_1),
      corr_out = mad_out(corr),
      tonic = tonic_1 - tonic_0,
      tonic_z = scale(tonic_1) - scale(tonic_0),
      tonic_pct = pct_response(tonic_0, tonic_1),
      tonic_out = mad_out(tonic),
      trial = case_when(nrow(.) == 33 ~ trial_index - 1,
                        TRUE ~ trial_index),
      all_trials = case_when(nrow(.) == 33 ~ "TRUE",
                             nrow(.) == 32 ~ "TRUE",
                             nrow(.) < 32 ~ "FALSE")
    ) 
  
  reshape_tmp["pid"] <- participant_tmp
  if(nrow(reshape_tmp) < 32)
    print("Incomplete Data")

  physiology <- rbind(physiology, reshape_tmp)
}


# remove annoying / in particpoant 90
physiology <- physiology %>% 
  mutate(
    pid = as.numeric(stringr::str_remove(pid, "/"))
  )

## Write data 

physiology %>% 
  write_rds(
    here::here(
    "data",
    "physiology_data_processed.Rds"
  )
  )



# Model Correlations Between Muscles --------------------------------------
# It is possible for emg to pick up genreal arousal via gloabl increases in muscle tension. 
#If this were the case then we would expect a positive correlation between zygomaticus and corrugator activity. 
#Generally however, the zygomaticus increases in activation during positive valence and does not change during negative while the corrugator increases in activity during negative experiences and decreases during positive. 
# The same arguement could be applied to skin conductance, although this evidence would be less strong. Skin conductance is a pure measure of arousal rather than with muscle activity which can index valence but also be confounded by arousal. 


physiology %>% 
  select(pid, corr_z, zygo_z, tonic_z) %>% # scores used as th
  group_by(pid) %>% 
  nest() %>% 
  mutate( # calculate correlation between zygo and corr for each particiapnt
    z_c_correlate = map(data, ~lm(zygo_z ~ corr_z, data = .x)),
    tidy = map(z_c_correlate, tidy)
  ) %>% 
  unnest(c(tidy)) %>% 
  select(pid, term, estimate, p.value) %>% 
  filter(term == "corr_z") %>% 
  filter(estimate > 0 & p.value < .05) %>% # if an arousal effect then the correlation will be positive
  select(pid) %>% 
  write_rds("data/improbable_emg.Rds")
