library(stringr)
library(dplyr)

kc_2020_df <- read.csv("King_County_Census_2020_blocks_with_PL_94-171_Redistricting__Data.csv") 
census_2010_df <- read.csv("2010_Census_Block_Seattle_-_Population_Statistics.csv") 

clean_kc_df <- filter(kc_2020_df, Place == "Seattle") 