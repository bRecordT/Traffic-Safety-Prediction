#crash severity dataset on a driver basis
#severity level is defined according to the most severly injured occupant in the crash
rm(list=ls())
gc()
library(dplyr)
setwd("G:/*****/Data")
path<-getwd()

#define function to clean yearly crash data
sev_data <- function(path, year, output = F) {
  #read yearly data
  temp_path <- paste0(path, "/Statewide_", as.character(year))
  files <- list.files(temp_path)
  crash <- read.csv(paste0(temp_path, "/", files[2]), header = T, stringsAsFactors = F)
  flag <- read.csv(paste0(temp_path, "/", files[4]), header = T, stringsAsFactors = F)
  person <- read.csv(paste0(temp_path, "/", files[5]), header = T, stringsAsFactors = F)
  roadway <- read.csv(paste0(temp_path, "/", files[7]), header = T, stringsAsFactors = F)
  vehicle <- read.csv(paste0(temp_path, "/", files[10]), header = T, stringsAsFactors = F)
  #filter data
  crash <- crash %>% select(-(DISTRICT:POLICE_AGCY), -TIME_OF_DAY, -(WORK_ZONE_TYPE:INTERSECT_TYPE), 
                            -LOCATION_TYPE, -(FATAL_COUNT:PED_MAJ_INJ_COUNT), -(COMM_VEH_COUNT:COUNTY_NAME))
  flag <- flag %>% select(CRN:TURNPIKE, WORK_ZONE, INTERSECTION, SCHOOL_ZONE, ALCOHOL_RELATED, UNLICENSED, 
                          DISTRACTED, CURVED_ROAD, SPEEDING_RELATED:FATIGUE_ASLEEP, UNBELTED:PEDESTRIAN, 
                          DRUG_RELATED, COUNTY)
  person <- person %>% filter(PERSON_TYPE==1) %>% select(-(PERSON_NUM:INJ_SEVERITY), -(AIRBAG_PADS:EJECTION_IND), 
                                                         -(EJECT_PATH_CD:DVR_LIC_STATE),-(PED_LOCATION:MUNICIPALITY))
  
  roadway <- roadway %>% select(CRN, LANE_COUNT, ROUTE:SPEED_LIMIT)
  vehicle <- vehicle %>% select(CRN:UNIT_NUM, VEH_TYPE)
  #merge data
  yrdata <- crash %>% inner_join(flag, by = "CRN") %>% inner_join(roadway, by = "CRN") %>% 
                    inner_join(vehicle, by = "CRN") %>% inner_join(person, by = c("CRN", "UNIT_NUM"))
  #return yearly data
  if (output == T) 
    write.csv(yrdata, file = paste0(path, "/severity data ", as.character(year), ".csv"), row.names = F)
  return(yrdata)
}

#assemble 5-year crash data
data <- data.frame()
for (year in 2013:2017) {
  yrdata <- sev_data(path, year)
  data <- rbind(data, yrdata)
}
summary(data)

#merge and clean final dataset
#select non-intersection crashes
data <- data %>% filter(STATE_ROAD == 1) %>%  
  mutate(Inj_level = ifelse(MAX_SEVERITY_LEVEL == 9, NA, 
                            ifelse(MAX_SEVERITY_LEVEL == 1, 1,
                                   ifelse(MAX_SEVERITY_LEVEL == 0, 3, 2))),
         Weekend = ifelse(DAY_OF_WEEK == 1 | DAY_OF_WEEK == 7, 1, 0),
         Quarter = ifelse(CRASH_MONTH <= 3, 1, ifelse(CRASH_MONTH <= 6, 2, ifelse(CRASH_MONTH <= 9, 3, 4))),
         Peak_hour = ifelse(HOUR_OF_DAY == 99 | is.na(HOUR_OF_DAY), NA,
                            ifelse((HOUR_OF_DAY >=7 & HOUR_OF_DAY <= 9) | 
                                     (HOUR_OF_DAY >= 16 & HOUR_OF_DAY <= 18), 1, 0)),
         Daylight = ifelse(ILLUMINATION == 1, 1, 0),
         Adv_weather = ifelse(WEATHER == 9 | is.na(WEATHER), NA, ifelse(WEATHER != 1, 1, 0)),
         Rdway_cond = ifelse(ROAD_CONDITION == 0, 1, ifelse(ROAD_CONDITION == 1, 2, 3)),
         Crash_loc = ifelse(RELATION_TO_ROAD == 9, NA, ifelse(RELATION_TO_ROAD == 1, 1, 0)),
         Urban = ifelse(URBAN_RURAL == 1, 0, 1),
         License = ifelse(UNLICENSED == 0, 1, 0),
         Belt = ifelse(UNBELTED == 0, 1, 0),
         Rdway_grade = ifelse(GRADE == 9 | is.na(GRADE), NA, ifelse(GRADE == 1, 1, 0)),
         Veh_type = ifelse(VEH_TYPE >= 98 | is.na(VEH_TYPE), NA,
                           ifelse(VEH_TYPE == 1 | VEH_TYPE == 6 | VEH_TYPE == 7, 1,
                                  ifelse(VEH_TYPE == 3, 2, 
                                         ifelse(VEH_TYPE == 4 | VEH_TYPE == 5, 3, 
                                                ifelse(VEH_TYPE == 2, 4,
                                                       ifelse(VEH_TYPE == 20 | VEH_TYPE == 21, 5, 6)))))),
         Age = ifelse(AGE == 99 | AGE <= 15, NA, AGE),
         Gender = ifelse(SEX == "U" | SEX == " ", NA, ifelse(SEX == "M", 0, 1)),
         Col_type = ifelse(COLLISION_TYPE ==9, NA, COLLISION_TYPE)
         ) %>% 
  filter(INTERSECTION == 0 & (!is.na(Inj_level))) %>%
  select(-(CRASH_YEAR:TURNPIKE), -INTERSECTION, -SCHOOL_ZONE, -UNLICENSED, -UNBELTED, -PEDESTRIAN, 
         -(COUNTY:DVR_PED_CONDITION)) %>%
  rename(Workzone = WORK_ZONE, Alcohol = ALCOHOL_RELATED, Distracted = DISTRACTED, Curve_rd = CURVED_ROAD, 
         Speeding = SPEEDING_RELATED, Aggresive = AGGRESSIVE_DRIVING, Fatigue = FATIGUE_ASLEEP, Drug = DRUG_RELATED)

summary(data)

#export full dataset
write.csv(data, file = "severity data.csv", row.names = F)

