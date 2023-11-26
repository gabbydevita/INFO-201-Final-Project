library(stringr)
library(dplyr)

kc_2020_df <- read.csv("King_County_Census_2020_blocks_with_PL_94-171_Redistricting__Data.csv") 
census_2010_df <- read.csv("2010_Census_Block_Seattle_-_Population_Statistics.csv") 

clean_kc_df <- filter(kc_2020_df, Place == "Seattle") 
# something crazy

# testing
clean_kc_df <- subset(clean_kc_df, select = -c(ACRES_WATER, ACRES_LAND, GEOID, POP_OVER_18, POP_UNDER_18, HU, HU_OCC,
                                               HU_VACANT, GQ_TOTAL_POP, GQI_CORRECT_ADULT, GQI_CORRECT_JUV, GQI_NURSING_FACIL,
                                               GQI_INST_OTHER, GQNI_COLLEGE_HOUSING, GQNI_MILITARY, GQNI_NONINST_OTHER))
census_2010_df <- subset(census_2010_df, select = -c(ACRES_WATER, ACRES_LAND, ACRES_TOTAL, GEO_TYPE, GEOID10))

testing_kc_df <- summarize(group_by(clean_kc_df, BLOCK), 
                        total_pop_2020 = sum(TOT_POP), 
                        poc_pop_2020 = sum(POC), 
                        white_pop_2020 = sum(WHITE_ALONE), 
                        black_pop_2020 = sum(BLACK_ALONE), 
                        asian_pop_2020 = sum(ASIAN_ALONE), 
                        am_native_pop_2020 = sum(AMI_ALONE))
testing_kc_df <- filter(testing_kc_df, total_pop_2020 > 0)

