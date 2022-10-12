
################################################################################################################
############################# Preloading data #############################
################################################################################################################
 
# Objective weights default to equal but can be changed by the user 
objective.weights = data.frame(values = c("commercial_value","recreational_value","ecosystem_value",
                                          "management_importantce",	"uniqueness"), 
                               weights = c(0,0,1,0,0))

# Read in a table of species value (or use default values provided by Joel) 
species.value = read.csv(here::here("data","species_value.csv"))
# Calculate the weighted average for each species 
species.value = species.value %>% 
  dplyr::mutate(species = tolower(species)) %>% 
  dplyr::mutate(commercial_value = commercial_value * objective.weights$weights[objective.weights$values == "commercial_value"], 
                recreational_value = recreational_value * objective.weights$weights[objective.weights$values == "recreational_value"], 
                ecosystem_value = ecosystem_value * objective.weights$weights[objective.weights$values == "ecosystem_value"], 
                management_importantce = management_importantce * objective.weights$weights[objective.weights$values == "management_importantce"], 
                uniqueness = uniqueness * objective.weights$weights[objective.weights$values == "uniqueness"]) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(total_value = (sum(commercial_value, recreational_value, ecosystem_value, management_importantce, uniqueness) * include)/sum(objective.weights$weights)) %>%  
  dplyr::ungroup()

# The %Freq_{species, survey} is determined by data that does not change 
species.survey.freq = read.csv(here::here("data","species_pct_frequency.csv")) %>% 
  dplyr::mutate(species = tolower(species)) 
# Replace all NA values with 0 (NAs are the empty cells from the excel sheet)
species.survey.freq[is.na(species.survey.freq)] = 0

# Open power parameter folder 
species.survey.power = read.csv(here::here("data","species_power_parameter.csv")) %>% 
  dplyr::mutate(species = tolower(species)) %>% 
  gather("survey","power_param",-c(species, life_stage, Group))

# Now we want to calculate the %Freq_{species, survey} * Value_{species}
species.survey.freq.value = left_join(species.survey.freq, species.value[, c("species", "life_stage","total_value")], by=c("species", "life_stage")) %>% 
  gather("survey","pct_freq",-c(species, life_stage, Group, total_value)) %>% 
  left_join(., species.survey.power, by=c("species", "life_stage", "Group", "survey")) %>% 
  dplyr::mutate(freq_value = total_value * pct_freq) %>% 
  dplyr::select(survey, freq_value, power_param)

################################################################################################################
##################################### Optimization routine ###############################
################################################################################################################

## First thing we need is the vector of starting parameters and constraints for individual parameters. 
# current.n corresponds to the sample size of each survey found in survey names 
# lb corresponds to the lower bound of survey sample size 
# ub corresponds to the upper bound of survey sample size 

survey.names = c("spring_trawl", "fall_trawl", "seamap_bll", "nmfs_bll", "camera_reef", "sum_plank_bongo",
                 "sum_plan_neuston", "fall_plank_bongo", "fall_plank_neust", "nmfs_small_pelagics")

## More constraints - this time it's cost constraints. They will be inequality constraints.   
# NOTE - I'm not sure what the difference between "cost per" and "current cost" is on the sheet 

con <- function(survey.n) {
  cost.per.survey = c(2910, 3414, 2830, 3600, 1871, 1854, 1854, 2555, 2555, 4172)
  max.cost =  6866963 
  max.capacity = 2590
  f = NULL
  f = rbind(f, sum(survey.n * cost.per.survey) - max.cost)
  f = rbind(f, sum(survey.n) - max.capacity)
  return(list(ceq = NULL, c = f))
}

## Objective function 
# Objective function is %Freq_{survey, species}*Value_{species} * (1-survey.size^{-cv.param_{survey, species}})
# You need to multply objective function by 1000 to get a value that's not too sensitive

obj=function(survey.n){
  
  survey.names = c("spring_trawl", "fall_trawl", "seamap_bll", "nmfs_bll", "camera_reef", "sum_plank_bongo",
                   "sum_plan_neuston", "fall_plank_bongo", "fall_plank_neust", "nmfs_small_pelagics")
  
  # Spring trawl 
  spring.trawl.power = survey.n[1]^(-1 * species.survey.freq.value$power_param[species.survey.freq.value$survey == survey.names[1]])
  spring.trawl.sum = sum(species.survey.freq.value$freq_value[species.survey.freq.value$survey == survey.names[1]] * (1-spring.trawl.power))* 100
  
  # Fall trawl 
  fall.trawl.power = survey.n[2]^(-1 * species.survey.freq.value$power_param[species.survey.freq.value$survey == survey.names[2]])
  fall.trawl.sum = sum(species.survey.freq.value$freq_value[species.survey.freq.value$survey == survey.names[2]] * (1-fall.trawl.power))* 100
  
  # Seamap BLL 
  seamap.bll.power = survey.n[3]^(-1 * species.survey.freq.value$power_param[species.survey.freq.value$survey == survey.names[3]])
  seamap.bll.sum = sum(species.survey.freq.value$freq_value[species.survey.freq.value$survey == survey.names[3]] * (1-seamap.bll.power))* 100
  
  # NMFS BLL 
  nmfs.bll.power = survey.n[4]^(-1 * species.survey.freq.value$power_param[species.survey.freq.value$survey == survey.names[4]])
  nmfs.bll.sum = sum(species.survey.freq.value$freq_value[species.survey.freq.value$survey == survey.names[4]] * (1-nmfs.bll.power))* 100
  
  # Camera Reef 
  camera.reef.power = survey.n[5]^(-1 * species.survey.freq.value$power_param[species.survey.freq.value$survey == survey.names[5]])
  camera.reef.sum = sum(species.survey.freq.value$freq_value[species.survey.freq.value$survey == survey.names[5]] * (1-camera.reef.power))* 100
  
  # Sum Plank Bongo 
  sum.plank.bongo.power = survey.n[6]^(-1 * species.survey.freq.value$power_param[species.survey.freq.value$survey == survey.names[6]])
  sum.plank.bongo.sum = sum(species.survey.freq.value$freq_value[species.survey.freq.value$survey == survey.names[6]] * (1-sum.plank.bongo.power))* 100
  
  # Sum Plan Neuston 
  sum.plan.neuston.power = survey.n[7]^(-1 * species.survey.freq.value$power_param[species.survey.freq.value$survey == survey.names[7]])
  sum.plan.neuston.sum = sum(species.survey.freq.value$freq_value[species.survey.freq.value$survey == survey.names[7]] * (1-sum.plan.neuston.power))* 100
  
  # Fall Plank Bongo
  fall.plank.bongo.power = survey.n[8]^(-1 * species.survey.freq.value$power_param[species.survey.freq.value$survey == survey.names[8]])
  fall.plank.bongo.sum = sum(species.survey.freq.value$freq_value[species.survey.freq.value$survey == survey.names[8]] * (1-fall.plank.bongo.power))* 100
  
  # Fall Plank Neust
  fall.plank.neust.power = survey.n[9]^(-1 * species.survey.freq.value$power_param[species.survey.freq.value$survey == survey.names[9]])
  fall.plank.neust.sum = sum(species.survey.freq.value$freq_value[species.survey.freq.value$survey == survey.names[9]] * (1-fall.plank.neust.power))* 100
  
  # NMFS Small Pelagics 
  nmfs.small.pelagics.power = survey.n[10]^(-1 * species.survey.freq.value$power_param[species.survey.freq.value$survey == survey.names[10]])
  nmfs.small.pelagics.sum = sum(species.survey.freq.value$freq_value[species.survey.freq.value$survey == survey.names[10]] * (1-nmfs.small.pelagics.power)) * 100
  
  return(-sum(spring.trawl.sum, fall.trawl.sum, 
             seamap.bll.sum, nmfs.bll.sum, 
             camera.reef.sum, 
             sum.plank.bongo.sum, sum.plan.neuston.sum, 
             fall.plank.bongo.sum, fall.plank.neust.sum, 
             nmfs.small.pelagics.sum))
  
}

survey.n = rep(100, 10)
survey.n = c(315, 275, 146, 150, 1359, 51, 51, 66, 66, 111)

lower.bound = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
upper.bound = c(346, 302, 160, 165, 1494, 56, 56, 72, 72, 122)

upper.bound = c(500, 500, 160, 165, 1494, 56, 56, 72, 72, 122)


result <- solnl(X = survey.n, objfun = obj, confun = con, 
                     lb = lower.bound, ub = upper.bound, 
                     tolFun = 1e-08, tolCon = 1e-08, maxnFun = 1e+011, maxIter = 8000)

if(exists("result") == FALSE){
  result <- solnl(X = rep(200, 10), objfun = obj, confun = con, 
                  lb = lower.bound, ub = upper.bound, 
                  tolFun = 1e-08, tolCon = 1e-08, maxnFun = 1e+011, maxIter = 8000)
}






final.table = data.frame(Survey=c("Spring Trawl","Fall Trawl","Seamap BLL", "NMFS BLL",
                                  "Camera Reef","Summer Plankton Bongo","Summer Plankton Neust",
                                  "Fall Plankton Bongo","Fall Plankton Neust","NMFS Small Pelagics"), 
                         Optimized.N = round(result$par, digits=0), 
                         Current.N = round(survey.n, digits=0), 
                         Optimized.Cost = round(result$par * cost.per.survey, digits=0), 
                         Current.Cost = round(survey.size.current * cost.per.survey, digits=0))


final.table %>% 
  gather("Type","Survey Metric", -Survey) %>% 
  dplyr::mutate(Metric = case_when(Type %in% c("Optimized.N", "Current.N") ~ "Number",
                                   Type %in% c("Optimized.Cost", "Current.Cost") ~ "Cost ($)")) %>% 
  dplyr::mutate(Type = case_when(Type %in% c("Optimized.N", "Optimized.Cost") ~ "Optimized",
                                   Type %in% c("Current.N", "Current.Cost") ~ "Current")) %>% 
  ggplot(aes(y=`Survey Metric`, x=Survey, fill=Type)) + 
  geom_bar(stat="identity", position='dodge', color="black") + 
  theme_half_open(12) + 
  xlab("")+
  facet_wrap(~Metric, nrow=2, scales="free_y")+
  scale_fill_manual(values=c("gray","darkgreen"))+
  theme(axis.text.x = element_text(angle = 45,hjust=1))





