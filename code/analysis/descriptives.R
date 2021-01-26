
#################################################################
##                     Process Survey Data                     ##
#################################################################


# Load packages -----------------------------------------------------------

library(tidyverse)
library(NbClust)
library(psych)
library(car)


# Load Functions ----------------------------------------------------------

jl_alpha <- function(data, vars){
  psych::alpha(select(data,
                      vars),
               warnings = FALSE)$total$raw
}


# Load Data ---------------------------------------------------------------

survey_data <- here::here(
  "data",
  "survey_responses.csv"
) %>% 
  read_csv()


# Create vectors for each measure -----------------------------------------

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


# Get descriptives --------------------------------------------------------

descriptives <- survey_data %>% 
  select(dif,
         ddf,
         eot,
         depression,
         age
         ) %>% 
  psych::describe() %>% 
  select(n,
         mean,
         sd) %>% 
  rownames_to_column("variable") %>% 
  mutate(
    alpha = case_when(
      variable == "dif" ~ jl_alpha(survey_data, dif_vector),
      variable == "ddf" ~ jl_alpha(survey_data, ddf_vector),
      variable == "eot" ~ jl_alpha(survey_data, eot_vector),
      variable == "tas" ~ jl_alpha(survey_data, paste0("tas_", c(1:20))),
      variable == "depression" ~ jl_alpha(survey_data, depression_vector)
    )
  )


# Evaluate distributions -----------------------------------------------
hist(survey_data$ddf)
car::qqPlot(survey_data$ddf)
boxplot(survey_data$ddf)


hist(survey_data$dif)
car::qqPlot(survey_data$dif)
boxplot(survey_data$dif)


hist(survey_data$eot)
car::qqPlot(survey_data$eot)
boxplot(survey_data$eot)

hist(survey_data$tas, breaks = 5)
car::qqPlot(survey_data$tas)
boxplot(survey_data$tas)

hist(survey_data$depression, breaks = 5)
car::qqPlot(survey_data$depression)
boxplot(survey_data$depression)



# Correlations ------------------------------------------------------------
survey_data %>% 
  select(ddf,
         dif,
         eot,
         tas,
         depression,
         age) %>% 
  apaTables::apa.cor.table(
    filename = "output/correlations.rtf"
  )


