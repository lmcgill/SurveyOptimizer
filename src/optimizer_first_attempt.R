## Step 1 - Calculate the %Freq_{species, survey} * Value_{species}

# The Value_{species} is determined by default or user input values, in addition to objective criteria weights 

# Objective weights default to equal but can be changed by the user 
objective.weights = data.frame(values = c("commercial_value","recreational_value","ecosystem_value",
                                          "management_importantce",	"uniqueness"), 
                               weights = c(1,1,1,1,1))

# Read in a table of species value (or use default values provided by Joel) 
species.value = read.csv(here::here("data","species_value.csv"))
# Calculate the weighted average for each species 
species.value = species.value %>% 
  dplyr::mutate(commercial_value = commercial_value * objective.weights$weights[objective.weights$values == "commercial_value"], 
                recreational_value = recreational_value * objective.weights$weights[objective.weights$values == "recreational_value"], 
                ecosystem_value = ecosystem_value * objective.weights$weights[objective.weights$values == "ecosystem_value"], 
                management_importantce = management_importantce * objective.weights$weights[objective.weights$values == "management_importantce"], 
                uniqueness = uniqueness * objective.weights$weights[objective.weights$values == "uniqueness"]) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(total_value = (sum(commercial_value, recreational_value, ecosystem_value, management_importantce, uniqueness) * include)/sum(objective.weights$weights)) %>%  
  dplyr::ungroup()

# The %Freq_{species, survey} is determined by data that does not change 



calculate_survey_cv <- function(){
  annual.sample.size = XXXX 
  effort.cv.power = XXXX 
  
  survey.cv = annual.sample.size*exp(-1 * effort.cv.power)
}

effort.cv.power is a species X survey matrix


survey.value = read.csv(here::here("data","current_survey_size.csv"))

unique(survey.value$survey)


# survey.value = survey.value %>% 
#   dplyr::mutate(survey = case_when(survey == "Spring Trawl " ~ "spring_trawl", 
#                                    survey == "Fall Trawl" ~ "fall_trawl", 
#                                    survey == "SEAMAP  BLL" ~ "seamap_bll", 
#                                    survey == "NMFS BLL" ~ "nmfs_bll", 
#                                    survey == "Camera/Reef (ALL) " ~ "camera_reef", 
#                                    survey == "Sum_Plank_Bongo" ~ "sum_plank_bongo", 
#                                    survey == "Sum_Plank_Neuston" ~ "sum_plan_neuston", 
#                                    survey == "Fall_Plank_Bongo" ~ "fall_plank_bongo", 
#                                    survey == "Fall_Plak_Neust" ~ "fall_plank_neust", 
#                                    survey == "NMFS Small Pelagics" ~ "nmfs_small_pelagics"))
# 
