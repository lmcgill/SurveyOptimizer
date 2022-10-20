
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


species.include = species.survey.freq %>% mutate_if(is.numeric, ~1 * (. > 0)) %>% 
  gather("survey","include",-c(species, life_stage, Group)) 
  


# Open power parameter folder 
species.survey.power = read.csv(here::here("data","species_power_parameter.csv")) %>% 
  dplyr::mutate(species = tolower(species)) %>% 
  gather("survey","power_param",-c(species, life_stage, Group))

# Now we want to calculate the %Freq_{species, survey} * Value_{species}
species.survey.freq.value = left_join(species.survey.freq, species.value[, c("species", "life_stage","total_value")], by=c("species", "life_stage")) %>% 
  gather("survey","pct_freq",-c(species, life_stage, Group, total_value)) %>% 
  left_join(., species.survey.power, by=c("species", "life_stage", "Group", "survey")) %>% 
  left_join(., species.include, by=c("species", "life_stage", "Group", "survey")) %>% 
  dplyr::mutate(freq_value = total_value * pct_freq * include) 

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
  max.capacity = 1590
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
  
  spring.trawl.power = log(survey.n[1]^(-1 * species.survey.freq.value$power_param[species.survey.freq.value$survey == survey.names[1]]))
  spring.trawl.sum = sum(species.survey.freq.value$freq_value[species.survey.freq.value$survey == survey.names[1]] * (1-spring.trawl.power)* 100)
  
  # Fall trawl 
  fall.trawl.power = log(survey.n[2]^(-1 * species.survey.freq.value$power_param[species.survey.freq.value$survey == survey.names[2]]))
  fall.trawl.sum = sum(species.survey.freq.value$freq_value[species.survey.freq.value$survey == survey.names[2]] * (1-fall.trawl.power)* 100)
  
  # Seamap BLL 
  seamap.bll.power = log(survey.n[3]^(-1 * species.survey.freq.value$power_param[species.survey.freq.value$survey == survey.names[3]]))
  seamap.bll.sum = sum(species.survey.freq.value$freq_value[species.survey.freq.value$survey == survey.names[3]] * (1-seamap.bll.power)* 100)
  
  # NMFS BLL 
  nmfs.bll.power = log(survey.n[4]^(-1 * species.survey.freq.value$power_param[species.survey.freq.value$survey == survey.names[4]]))
  nmfs.bll.sum = sum(species.survey.freq.value$freq_value[species.survey.freq.value$survey == survey.names[4]] * (1-nmfs.bll.power)* 100)
  
  # Camera Reef 
  camera.reef.power = log(survey.n[5]^(-1 * species.survey.freq.value$power_param[species.survey.freq.value$survey == survey.names[5]]))
  camera.reef.sum = sum(species.survey.freq.value$freq_value[species.survey.freq.value$survey == survey.names[5]] * (1-camera.reef.power)* 100)
  
  # Sum Plank Bongo 
  sum.plank.bongo.power = log(survey.n[6]^(-1 * species.survey.freq.value$power_param[species.survey.freq.value$survey == survey.names[6]]))
  sum.plank.bongo.sum = sum(species.survey.freq.value$freq_value[species.survey.freq.value$survey == survey.names[6]] * (1-sum.plank.bongo.power)* 100)
  
  # Sum Plan Neuston 
  sum.plan.neuston.power = log(survey.n[7]^(-1 * species.survey.freq.value$power_param[species.survey.freq.value$survey == survey.names[7]]))
  sum.plan.neuston.sum = sum(species.survey.freq.value$freq_value[species.survey.freq.value$survey == survey.names[7]] * (1-sum.plan.neuston.power)* 100)
  
  # Fall Plank Bongo
  fall.plank.bongo.power = log(survey.n[8]^(-1 * species.survey.freq.value$power_param[species.survey.freq.value$survey == survey.names[8]]))
  fall.plank.bongo.sum = sum(species.survey.freq.value$freq_value[species.survey.freq.value$survey == survey.names[8]] * (1-fall.plank.bongo.power)* 100)
  
  # Fall Plank Neust
  fall.plank.neust.power = log(survey.n[9]^(-1 * species.survey.freq.value$power_param[species.survey.freq.value$survey == survey.names[9]])) 
  fall.plank.neust.sum = sum(species.survey.freq.value$freq_value[species.survey.freq.value$survey == survey.names[9]] * (1-fall.plank.neust.power)* 100)
  
  # NMFS Small Pelagics 
  nmfs.small.pelagics.power = log(survey.n[10]^(-1 * species.survey.freq.value$power_param[species.survey.freq.value$survey == survey.names[10]])) 
  nmfs.small.pelagics.sum = sum(species.survey.freq.value$freq_value[species.survey.freq.value$survey == survey.names[10]] * (1-nmfs.small.pelagics.power)* 100) 
  
  # # Spring trawl 
  # spring.trawl.power = log(survey.n[1]^(-1 * species.survey.freq.value$power_param[species.survey.freq.value$survey == survey.names[1]]))
  # spring.trawl.sum = sum(species.survey.freq.value$freq_value[species.survey.freq.value$survey == survey.names[1]] * (1-spring.trawl.power))*10000
  # 
  # # Fall trawl 
  # fall.trawl.power = log(survey.n[2]^(-1 * species.survey.freq.value$power_param[species.survey.freq.value$survey == survey.names[2]]))
  # fall.trawl.sum = sum(species.survey.freq.value$freq_value[species.survey.freq.value$survey == survey.names[2]] * (1-fall.trawl.power))*10000
  # 
  # # Seamap BLL 
  # seamap.bll.power = log(survey.n[3]^(-1 * species.survey.freq.value$power_param[species.survey.freq.value$survey == survey.names[3]]))
  # seamap.bll.sum = sum(species.survey.freq.value$freq_value[species.survey.freq.value$survey == survey.names[3]] * (1-seamap.bll.power))*10000
  # 
  # # NMFS BLL 
  # nmfs.bll.power = log(survey.n[4]^(-1 * species.survey.freq.value$power_param[species.survey.freq.value$survey == survey.names[4]]))
  # nmfs.bll.sum = sum(species.survey.freq.value$freq_value[species.survey.freq.value$survey == survey.names[4]] * (1-nmfs.bll.power))*10000
  # 
  # # Camera Reef 
  # camera.reef.power = log(survey.n[5]^(-1 * species.survey.freq.value$power_param[species.survey.freq.value$survey == survey.names[5]]))
  # camera.reef.sum = sum(species.survey.freq.value$freq_value[species.survey.freq.value$survey == survey.names[5]] * (1-camera.reef.power))*10000
  # 
  # # Sum Plank Bongo 
  # sum.plank.bongo.power = log(survey.n[6]^(-1 * species.survey.freq.value$power_param[species.survey.freq.value$survey == survey.names[6]]))
  # sum.plank.bongo.sum = sum(species.survey.freq.value$freq_value[species.survey.freq.value$survey == survey.names[6]] * (1-sum.plank.bongo.power))*10000
  # 
  # # Sum Plan Neuston 
  # sum.plan.neuston.power = log(survey.n[7]^(-1 * species.survey.freq.value$power_param[species.survey.freq.value$survey == survey.names[7]]))
  # sum.plan.neuston.sum = sum(species.survey.freq.value$freq_value[species.survey.freq.value$survey == survey.names[7]] * (1-sum.plan.neuston.power))*10000
  # 
  # # Fall Plank Bongo
  # fall.plank.bongo.power = log(survey.n[8]^(-1 * species.survey.freq.value$power_param[species.survey.freq.value$survey == survey.names[8]]))
  # fall.plank.bongo.sum = sum(species.survey.freq.value$freq_value[species.survey.freq.value$survey == survey.names[8]] * (1-fall.plank.bongo.power))*10000
  # 
  # # Fall Plank Neust
  # fall.plank.neust.power = log(survey.n[9]^(-1 * species.survey.freq.value$power_param[species.survey.freq.value$survey == survey.names[9]]))
  # fall.plank.neust.sum = sum(species.survey.freq.value$freq_value[species.survey.freq.value$survey == survey.names[9]] * (1-fall.plank.neust.power))*10000
  # 
  # # NMFS Small Pelagics 
  # nmfs.small.pelagics.power = log(survey.n[10]^(-1 * species.survey.freq.value$power_param[species.survey.freq.value$survey == survey.names[10]]))
  # nmfs.small.pelagics.sum = sum(species.survey.freq.value$freq_value[species.survey.freq.value$survey == survey.names[10]] * (1-nmfs.small.pelagics.power)) *10000
  # 
  return(-sum(spring.trawl.sum, fall.trawl.sum, 
              seamap.bll.sum, nmfs.bll.sum, 
              camera.reef.sum, 
              sum.plank.bongo.sum, sum.plan.neuston.sum, 
              fall.plank.bongo.sum, fall.plank.neust.sum, 
              nmfs.small.pelagics.sum))
  
}




survey.n = rep(10, 10)
#survey.n = c(315, 275, 146, 150, 1359, 51, 51, 66, 66, 111)
survey.size.current = c(315, 275, 146, 150, 1359, 51, 51, 66, 66, 111)
cost.per.survey = c(2910, 3414, 2830, 3600, 1871, 1854, 1854, 2555, 2555, 4172)

lower.bound = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
upper.bound = c(346, 302, 160, 165, 1494, 56, 56, 72, 72, 122)

survey.n = c(346, 302, 160, 165, 1494, 56, 56, 500, 500, 122)

survey.n = c(1000, 1000, 1000, 1000, 1500, 1000, 1000, 1000, 1000, 1000)-100
upper.bound = c(346, 302, 160, 165, 1494, 56, 56, 500, 500, 122) 

result <- solnl(X = survey.n, objfun = obj, confun = con, 
                     lb = lower.bound, ub = upper.bound, 
                     tolFun = 0.000001, tolCon = 0.000001, maxnFun = 1e+08, tolX = 0.000001, maxIter = 100000)






final.table = data.frame(Survey=c("Spring Trawl","Fall Trawl","Seamap BLL", "NMFS BLL",
                                  "Camera Reef","Summer Plankton Bongo","Summer Plankton Neust",
                                  "Fall Plankton Bongo","Fall Plankton Neust","NMFS Small Pelagics"), 
                         Optimized.N = round(result$par, digits=0), 
                         Current.N = round(survey.size.current, digits=0), 
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

final.table$survey = survey.names
new.values = left_join(species.survey.freq.value, final.table[, c("Survey","survey","Optimized.N")], by="survey") %>% 
  rowwise() %>% 
  dplyr::mutate(enterprise.score = Optimized.N^(-1*power_param)) %>% 
  dplyr::mutate(enterprise.score = freq_value * (1-enterprise.score) * 100) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(Group, Survey, enterprise.score) %>% 
  dplyr::group_by(Group, Survey) %>% 
  dplyr::summarize(enterprise.score = sum(enterprise.score)) %>% 
  dplyr::ungroup()

new.values = left_join(species.survey.freq.value, final.table[, c("Survey","survey","Optimized.N", "Current.N")], by="survey") %>% 
  rowwise() %>% 
  dplyr::mutate(Optimized.enterprise.score = Optimized.N^(-1*power_param)) %>% 
  dplyr::mutate(Optimized.enterprise.score = freq_value * (1-Optimized.enterprise.score) * 100) %>% 
  dplyr::mutate(Current.enterprise.score = Current.N^(-1*power_param)) %>% 
  dplyr::mutate(Current.enterprise.score = freq_value * (1-Current.enterprise.score) * 100) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(Group, Survey, Optimized.enterprise.score, Current.enterprise.score) %>% 
  dplyr::group_by(Group, Survey) %>% 
  dplyr::summarize(Optimized.enterprise.score = sum(Optimized.enterprise.score), 
                   Current.enterprise.score = sum(Current.enterprise.score)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(Optimized.enterprise.score = round(Optimized.enterprise.score)) %>%
  dplyr::mutate(Current.enterprise.score = round(Current.enterprise.score, 2)) 

new.values %>% 
  ggplot(aes(x=Survey, y=Optimized.enterprise.score, fill=Group)) + 
  geom_bar(position="stack", stat="identity", color="black")+
  theme_half_open(12) + 
  xlab("")+
  ylab("Enterprise Score")+
  #scale_fill_manual(values=c("gray","darkgreen"))+
  theme(axis.text.x = element_text(angle = 45,hjust=1)) + 
  theme(legend.position = "top", 
        legend.title = element_blank())


new.values %>% 
  dplyr::group_by(Group) %>% 
  dplyr::summarize(Optimized.enterprise.score = sum(Optimized.enterprise.score), 
                   Current.enterprise.score = sum(Current.enterprise.score)) %>% 
  dplyr::ungroup() %>% 
  gather("Type","Enterprise Score", -Group) %>% 
  dplyr::mutate(Type = case_when(Type %in% c("Optimized.enterprise.score") ~ "Optimized",
                                 Type %in% c("Current.enterprise.score") ~ "Current")) %>% 
  ggplot(aes(x=Group, y=`Enterprise Score`, fill=Type)) + 
  geom_bar(position="dodge", stat="identity", color="black")+
  theme_half_open(12) + 
  xlab("")+
  ylab("Enterprise Score")+
  scale_fill_manual(values=c("gray","darkgreen"))+
  theme(axis.text.x = element_text(angle = 45,hjust=1)) + 
  theme(legend.position = "top", 
        legend.title = element_blank())




