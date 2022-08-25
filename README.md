# SurveyOptimizer

## Data inputs 

### Species and ecosystem valuation (fixed for now, maybe later user can change) 
This is the `species_value.csv` file in the `data` folder. Define species to use in the analysis.  These can be single species, age classes, functional groups, or even environmental variables.  Assign a value to each species for each objective criteria. These can be total landings, total value, number of directed trips, etc. They can be scaled accordingly. Ecosystem valuation will need some thought, but generally should represent the importance of each species or functional group. 

### Current percent frequency (derived from empirical data) 
This is the `species_pct_frequency.csv` file in the `data` folder.Percent frequency of occurrence for each species in each survey.  This can remain constant, or vary depending on proposed changes to spatial/seasonal coverage (e.g. to target recruitment events or move into deeper depths) 

### Survey effort-CV power parameters (derived from empirical data?) 
This is the `effort_cv_power_parameters.csv` file in the `data` folder. Values are survey and species specific. We use this dataframe to get the value `current_survey_cv`. Survey CV is  calculated a function of CURRENT sample size (i.e., it appears that CV does not change if optimized values are very different) using a power function (e.g., survey.species.cv = survey.sample.size ^ -survey.species.effort.power.parameter). 

### Current survey sample size (derived from empirical data) 
This is the `current_survey_size.csv` file in the `data` folder. Values are survey specific. We use this dataframe to get the value `current_survey_cv`. 
