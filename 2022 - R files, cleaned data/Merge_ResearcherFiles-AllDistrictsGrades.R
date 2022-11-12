library(tidyverse)
library(dplyr)
library(janitor) #for get_dupes funct

#######################################################################
## Creates CSV with: District data (schools removed), AND 
##                   cohorts: 1(all),11(SWD),15(econ disadvantaged),6(hispanic)
######################################################################
##       ANNUAL UPDATING - directions
##
## 1 - Get new year's data from: https://data.nysed.gov/downloads.php
##     (Assessment Database, get file called - 3-8_ELA_AND_MATH_RESEARCHER_FILE_YYYY)
##
## 2 - If the file is a .xlsx, open it and save it as a .csv (it's faster than doing it in R)
##
## 3 - Update this script to add the new years data.  UPdate the following sections:
##        '#Import NY State Data (no 2020 Data due to COVID):' section through
##          'Merge Data' Sections
##
## 4 - Edit input and output file names (to the dir your working in) (ie, line 24,28, etc below)
##
###################################################

setwd("C:/Users/srene/Documents/NY LITERACY REPORT CARD/NYSourceDATA/SOURCE_DATA/ELA_data/SourceData-ExceptNYC")
output_file <- "SOURCEDATA_ALLDistrictsGrades.csv"

###################################################

# Import NY State Data (no 2020 Data due to COVID):
# 2016 Data (csv)
inputfile <- "3-8_ELA_AND_MATH_RESEARCHER_FILE_2016.csv"
data_2016 <- read.csv(inputfile, blank.lines.skip = TRUE)[,c(1, 4:7, 9:12, 22)]
data_2016$BEDSCODE <- format(data_2016$BEDSCODE, scientific = FALSE)
data_2016$BEDSCODE <- str_replace_all(data_2016$BEDSCODE, ' ', '')   

# 2017 Data (csv)
inputfile <- "3-8_ELA_AND_MATH_RESEARCHER_FILE_2017.csv"
data_2017  <- read.csv(inputfile, blank.lines.skip = TRUE)[,c(1, 4:7, 9:12, 22)]
data_2017$BEDSCODE <- format(data_2017$BEDSCODE, scientific = FALSE)
data_2017$BEDSCODE <- str_replace_all(data_2017$BEDSCODE, ' ', '') 

# 2018 Data (csv)
inputfile <- "3-8_ELA_AND_MATH_RESEARCHER_FILE_2018.csv"
data_2018 <- read.csv(inputfile, blank.lines.skip = TRUE)[,c(1, 4:7, 9:12, 22)]
data_2018$BEDSCODE <- format(data_2018$BEDSCODE, scientific = FALSE)
data_2018$BEDSCODE <- str_replace_all(data_2018$BEDSCODE, ' ', '') 

# 2019 Data (xlsx)
inputfile <- "3-8_ELA_AND_MATH_RESEARCHER_FILE_2019.csv"
data_2019 <- read.csv(inputfile, blank.lines.skip = TRUE)[,c(1, 4:7, 9:12, 22)]
data_2019$BEDSCODE <- format(data_2019$BEDSCODE, scientific = FALSE)
data_2019$BEDSCODE <- str_replace_all(data_2019$BEDSCODE, ' ', '') 

# 2021 Data (xlsx) -- NOTE: This year is missing needed cols: COUNTY_CODE, COUNTY_DESC
#                           This year col name are also different
inputfile <- "3-8_ELA_AND_MATH_RESEARCHER_FILE_2021.csv"
data_2021 <- read.csv(inputfile, blank.lines.skip = TRUE)[,c(1:3,5:7,10,19)]
data_2021$BEDSCODE <- format(data_2021$BEDSCODE, scientific = FALSE)
data_2021$BEDSCODE <- str_replace_all(data_2021$BEDSCODE, ' ', '') 

# 2022 Data (xlsx) -- NOTEs:
inputfile <- "3-8_Annual EM ELA_2022.csv"
data_2022 <- read.csv(inputfile, blank.lines.skip = TRUE)[,c(4,2,3,5,6,7, 10,11, 21)]  #order doesn't matter, names don't matter for now either
# 2022, also importing total cohort numbers and percent tested, available this year
#colnames change a lot each year so refer to the saved file for these 7 cols you need
data_2022$ENTITY_CD <- format(data_2022$ENTITY_CD, scientific = FALSE)
data_2022$ENTITY_CD <- str_replace_all(data_2022$ENTITY_CD, ' ', '')

###################################################
# Clean Data by YEAR - Make consistent col names, and add and populate missing 2021 County columns )

# Rename 2021 ColNames to be consistent with prev years
names(data_2021)[names(data_2021) == "subgroup_name"] <- "SUBGROUP_NAME"

# Rename 2022 ColNames to be consistent with prev years
names(data_2022)[names(data_2022) == "YEAR"] <- "SY_END_DATE"
names(data_2022)[names(data_2022) == "ENTITY_CD"] <- "BEDSCODE"
names(data_2022)[names(data_2022) == "ENTITY_NAME"] <- "NAME"
names(data_2022)[names(data_2022) == "ASSESSMENT_NAME"] <- "ITEM_DESC"
names(data_2022)[names(data_2022) == "NUM_TESTED"] <- "TOTAL_TESTED"
names(data_2022)[names(data_2022) == "PER_PROF"] <- "L3.L4_PCT"


# Add missing cols to 2021 Data
data_2021$COUNTY_CODE <- "EMPTY"
data_2021$COUNTY_DESC <- "EMPTY"

# Add missing cols to 2022 Data
data_2022$COUNTY_CODE <- "EMPTY"
data_2022$COUNTY_DESC <- "EMPTY"
data_2022$SUBGROUP_CODE <- "EMPTY"


# Populate missing County Codes - setup lookup table
# # Import BEDSCODE-County LookUp Table
inputfile <- "BEDSCODE_County.csv"
BCode2County <- read.csv(inputfile)[,c(1,2,3)]  # District, districtBedsCode (1st 6 digs), County
#Create lookup key, use it with: getCountyDesc['570101'] (first 6dig of BEDSCODE)
getCountyDesc <- BCode2County$COUNTY_DESC
names(getCountyDesc) <- BCode2County$BEDSCODE

# Populate missing County_desc, using bedscode - 2021
data_2021$BEDSCODE <- format(data_2021$BEDSCODE, scientific = FALSE)
#remove spaces
data_2021$BEDSCODE <- str_replace_all(data_2021$BEDSCODE, ' ', '')
# Get rid of unneeded cohorts so next step doesn't take as long
data_2021 <- data_2021 %>% 
  filter(str_detect(ITEM_DESC, ".ELA") | str_detect(ITEM_DESC, ".ELA ")) %>%   # Keep ELA data only
  filter(SUBGROUP_CODE ==  1 | SUBGROUP_CODE ==  11 | SUBGROUP_CODE ==  15 | SUBGROUP_CODE ==  6 | SUBGROUP_CODE ==  12 | SUBGROUP_CODE ==  13 | SUBGROUP_NAME ==  "All Students" | SUBGROUP_NAME ==  "Students with Disabilities" | SUBGROUP_NAME ==  "Economically Disadvantaged" | SUBGROUP_NAME ==  "Hispanic or Latino" | SUBGROUP_NAME ==  "Non-English Language Learner" | SUBGROUP_NAME ==  "English Language Learner")
# Get missing(NA) counties
data_2021$BEDshort <- substr(data_2021$BEDSCODE, 1, 6)
data_2021$COUNTY_DESC <- getCountyDesc[data_2021$BEDshort]
# Remove temp BEDshort
data_2021 <- subset(data_2021, select = -c(BEDshort))

# Populate missing County_desc, using bedscode - 2022
data_2022$BEDSCODE <- format(data_2022$BEDSCODE, scientific = FALSE)
#remove spaces
data_2022$BEDSCODE <- str_replace_all(data_2022$BEDSCODE, ' ', '')
# Get missing(NA) counties
data_2022$BEDshort <- substr(data_2022$BEDSCODE, 1, 6)
data_2022$COUNTY_DESC <- getCountyDesc[data_2022$BEDshort]
# Remove temp BEDshort col
data_2022 <- subset(data_2022, select = -c(BEDshort))

# Add empty columns for total tested and percent tested to years prior to 2022, being each table needs same cols to bind them together
data_2016$TOTAL_COUNT <- NA
data_2016$PCT_TESTED <- NA

data_2017$TOTAL_COUNT <- NA
data_2017$PCT_TESTED <- NA

data_2018$TOTAL_COUNT <- NA
data_2018$PCT_TESTED <- NA

data_2019$TOTAL_COUNT <- NA
data_2019$PCT_TESTED <- NA

data_2021$TOTAL_COUNT <- NA
data_2021$PCT_TESTED <- NA

##################################################
# Merge Data, and Clean 
data_ALL <- rbind(data_2016, data_2017, data_2018, data_2019, data_2021, data_2022)
#remove(data_2016, data_2017, data_2018, data_2019, data_2021, data_2022)

# Use standard NA, convert to correct formats
# Keep Year Only in Date Col
data_ALL$BEDSCODE <- format(data_ALL$BEDSCODE, scientific = FALSE)
data_ALL$BEDSCODE <- str_replace_all(data_ALL$BEDSCODE, ' ', '')  #Does this put it back to sci notation? 
data_ALL$BEDSCODE <- str_replace_all(data_ALL$BEDSCODE, ' ', '')  #Repeat, some still have spaces
data_ALL$SY_END_DATE <- str_replace_all(data_ALL$SY_END_DATE, '.+/', '')
data_ALL$L3.L4_PCT <- gsub('%', '', data_ALL$L3.L4_PCT)
data_ALL$L3.L4_PCT <- strtoi(data_ALL$L3.L4_PCT)
data_ALL$SUBGROUP_CODE <- strtoi(data_ALL$SUBGROUP_CODE)
data_ALL$L3.L4_PCT[ data_ALL$L3.L4_PCT == "-" ] <- NA
data_ALL$TOTAL_TESTED <- strtoi(data_ALL$TOTAL_TESTED)
data_ALL$TOTAL_TESTED[ data_ALL$TOTAL_TESTED == "-" ] <- NA
data_ALL$NAME <- str_replace_all(data_ALL$NAME, ' $', '') 
data_ALL$NAME <- str_replace_all(data_ALL$NAME, ' $', '') # Repeat - some have 2 spaces at the end

# Filter - KEEP ONLY SUBGROUPS BELOW:
data_ALL <- data_ALL %>% 
  filter(str_detect(ITEM_DESC, ".ELA") | str_detect(ITEM_DESC, ".ELA ") | str_detect(ITEM_DESC, "^ELA3$") | str_detect(ITEM_DESC, "^ELA4$") | str_detect(ITEM_DESC, "^ELA5$") | str_detect(ITEM_DESC, "^ELA6$") | str_detect(ITEM_DESC, "^ELA7$") | str_detect(ITEM_DESC, "^ELA8$")) %>% # Keep ELA data only, all gradess
  filter(SUBGROUP_CODE ==  1 | SUBGROUP_CODE ==  11 | SUBGROUP_CODE ==  15 | SUBGROUP_CODE ==  6 | SUBGROUP_CODE ==  12 | SUBGROUP_CODE ==  13 | SUBGROUP_NAME ==  "All Students" | SUBGROUP_NAME ==  "Students with Disabilities" | SUBGROUP_NAME ==  "Economically Disadvantaged" | SUBGROUP_NAME ==  "Hispanic or Latino" | SUBGROUP_NAME ==  "Non-English Language Learner" | SUBGROUP_NAME ==  "English Language Learner")

# Filter - KEEP ONLY DISTRICT DATA (not individual schools):
data_ALL <- data_ALL %>% 
  filter(str_detect(BEDSCODE, ".0000$")) %>%
  # Remove Schools with 'ELEMENTARY' which use same elem and dist code (ending in 0000)
  #filter(!str_detect(NRC_DESC, "Charters")) %>%
  filter(!str_detect(NAME, "ELEMENTARY")) %>%
  filter(!str_detect(NAME, "ELMENTARY")) %>%
  filter(!str_detect(NAME, "CHARTER"))

# Filter - REMOVE BESCODES 0-7 (groupings and charters)(and has annoying NAs in county_desc col)
data_ALL <- data_ALL[(data_ALL$BEDSCODE != "0"),]
data_ALL <- data_ALL[(data_ALL$BEDSCODE != "1"),]
data_ALL <- data_ALL[(data_ALL$BEDSCODE != "2"),]
data_ALL <- data_ALL[(data_ALL$BEDSCODE != "3"),]
data_ALL <- data_ALL[(data_ALL$BEDSCODE != "4"),]
data_ALL <- data_ALL[(data_ALL$BEDSCODE != "5"),]
data_ALL <- data_ALL[(data_ALL$BEDSCODE != "6"),]
data_ALL <- data_ALL[(data_ALL$BEDSCODE != "7"),]


# Filter - REMOVE NYC COUNTIES  (added !comp cases so NAs aren't removed/no cols removed)
data_ALL <- data_ALL[!(startsWith(data_ALL$BEDSCODE, "34")),] # 34*  = QUEENS (county_desc)
data_ALL <- data_ALL[!(startsWith(data_ALL$BEDSCODE, "32")),] # 32* = BRONX
data_ALL <- data_ALL[!(startsWith(data_ALL$BEDSCODE, "33")),] # 33* = KINGS
data_ALL <- data_ALL[!(startsWith(data_ALL$BEDSCODE, "35")),] # 35* = RICHMOND
data_ALL <- data_ALL[!(startsWith(data_ALL$BEDSCODE, "3100")),] # 3100*, 3101*..3106* = NEW YORK
data_ALL <- data_ALL[!(startsWith(data_ALL$BEDSCODE, "3101")),] 
data_ALL <- data_ALL[!(startsWith(data_ALL$BEDSCODE, "3102")),] 
data_ALL <- data_ALL[!(startsWith(data_ALL$BEDSCODE, "3103")),] 
data_ALL <- data_ALL[!(startsWith(data_ALL$BEDSCODE, "3104")),] 
data_ALL <- data_ALL[!(startsWith(data_ALL$BEDSCODE, "3105")),] 
data_ALL <- data_ALL[!(startsWith(data_ALL$BEDSCODE, "3106")),] 

# Fix/Clean School Names 
data_ALL$NAME <- str_replace(data_ALL$NAME, "CENTRAL SCH.*", "CSD ")
data_ALL$NAME <- str_replace(data_ALL$NAME, "UNION FREE SCHOOL DISTRICT OF", "UFSD OF")  #Must be before next replace line
data_ALL$NAME <- str_replace(data_ALL$NAME, "UNION FR.*", "UFSD ")
data_ALL$NAME <- str_replace(data_ALL$NAME, "SCHOOL DISTRICT", "SD ") 
data_ALL$NAME <- str_replace_all(data_ALL$NAME, ' $', '') # Get rid of end space (above subs could have been in the middle)

# Get rid of schools, keep districts only (this gets rid of the remaining schools using its district BEDSCODE)
data_ALL <- data_ALL[(str_detect(data_ALL$NAME, "SD$") | str_detect(data_ALL$NAME, "SD ")),]

# Get rid of duplicates
#record them, just fyi
DUPS <- get_dupes(data_ALL)
# remove them
data_ALL <- unique(data_ALL)
# Remove those with duplicate bedscode, name, year (doesn't work right, including potntial dupes dups) 
# data_ALL <- data_ALL[!duplicated(data_ALL[c("SY_END_DATE","COUNTY_DESC", "BEDSCODE", "NAME", "ITEM_DESC", "SUBGROUP_NAME")]),]


############     DONE, SAVE AS CSV   ####################################
write.csv(data_ALL, output_file)
