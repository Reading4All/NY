library(tidyverse)
library(dplyr)
library(janitor) #for get_dupes funct

#######################################################################
## Creates CSV with: District data (schools removed), AND 
##                   cohorts: 1(all),11(SWD),15(econ disadvantaged),6(hispanic), 12(nonELL), 13(ELL)
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
output_file <- "SOURCEDATA_ALLSchoolsGrade3.csv"

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
data_2022 <- read.csv(inputfile, blank.lines.skip = TRUE)[,c(4,2,3,5,6,10,21)]  #order doesn't matter, names dont matter for now either
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
# Remove temp BEDshort col (col 11)
data_2021 <- data_2021[,-11]

# Populate missing County_desc, using bedscode - 2022
data_2022$BEDSCODE <- format(data_2022$BEDSCODE, scientific = FALSE)
#remove spaces
data_2022$BEDSCODE <- str_replace_all(data_2022$BEDSCODE, ' ', '')
# Get missing(NA) counties
data_2022$BEDshort <- substr(data_2022$BEDSCODE, 1, 6)
data_2022$COUNTY_DESC <- getCountyDesc[data_2022$BEDshort]
# Remove temp BEDshort col (col 11)
data_2022 <- data_2022[,-11]


##################################################
# Merge Data, and Clean 
data_ALL <- rbind(data_2016, data_2017, data_2018, data_2019, data_2021, data_2022)
#remove(data_2016, data_2017, data_2018, data_2019, data_2021)

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

# Filter - KEEP ONLY "Grade 3 ELA", and SUBGROUPS_CODES(cohorts) BELOW:
data_ALL <- data_ALL %>% 
  filter(str_detect(ITEM_DESC, "Grade 3 ELA") | str_detect(ITEM_DESC, "ELA3")) %>%   # Keep ELA data only
  filter(SUBGROUP_CODE ==  1 | SUBGROUP_CODE ==  11 | SUBGROUP_CODE ==  15 | SUBGROUP_CODE ==  6 | SUBGROUP_CODE ==  12 | SUBGROUP_CODE ==  13 | SUBGROUP_NAME ==  "All Students" | SUBGROUP_NAME ==  "Students with Disabilities" | SUBGROUP_NAME ==  "Economically Disadvantaged" | SUBGROUP_NAME ==  "Hispanic or Latino" | SUBGROUP_NAME ==  "Non-English Language Learner" | SUBGROUP_NAME ==  "English Language Learner")

# Filter - REMOVE BESCODES 0-7 (state/county groups and charters)
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

# data_ALL_orig <- data_ALL
# ## BELOW Keeps ALL DISTRICTS
# ## FYI, There are 17,887 District Rows: nrow(data_ALL_Districts)
# data_ALL_Districts <- data_ALL %>%
#   filter(str_detect(BEDSCODE, ".0000")) %>%
#   # Remove Schools with 'ELEMENTARY' which use same elem and dist code (ending in 0000)
#   #filter(!str_detect(NRC_DESC, "Charters")) %>%
#   filter(!str_detect(NAME, "ELEMENTARY")) %>%
#   filter(!str_detect(NAME, "ELMENTARY")) %>%
#   filter(!str_detect(NAME, "CHARTER"))
# data_ALL_Districts$NAME <- str_replace(data_ALL_Districts$NAME, "CENTRAL SCH.*", "CSD ")
# data_ALL_Districts$NAME <- str_replace(data_ALL_Districts$NAME, "UNION FREE SCHOOL DISTRICT OF", "UFSD OF")  #Must be before next replace line
# data_ALL_Districts$NAME <- str_replace(data_ALL_Districts$NAME, "UNION FR.*", "UFSD ")
# data_ALL_Districts$NAME <- str_replace(data_ALL_Districts$NAME, "SCHOOL DISTRICT", "SD ") 
# data_ALL_Districts$NAME <- str_replace_all(data_ALL_Districts$NAME, ' $', '') # Get rid of end space (above subs could have been in the middle)
# # Get rid of schools, keep districts only (this gets rid of the remaining schools using its district BEDSCODE)
# data_ALL_Districts <- data_ALL_Districts[(str_detect(data_ALL_Districts$NAME, "SD$") | str_detect(data_ALL_Districts$NAME, "SD ")),]
# # save dups (to see)
# DUPS <- get_dupes(data_ALL_Districts)
# # then remove them
# data_ALL_Districts <- unique(data_ALL_Districts)
# # Remove those with duplicate bedscode, name, year, 
# data_ALL_Districts <- data_ALL_Districts[!duplicated(data_ALL_Districts[c("SY_END_DATE","COUNTY_DESC", "BEDSCODE", "NAME", "ITEM_DESC", "SUBGROUP_CODE")]),]

# ## BELOW Keeps ALL SCHOOLS (even the MANY that mistakenly use their 0000$ district bedscode instead of school bedscode if their name indicates its actually a school)
data_ALL <- data_ALL %>% 
filter(!str_detect(BEDSCODE, ".0000$") | str_detect(NAME, "ELEMENTARY") | str_detect(NAME, "ELMENTARY") | str_detect(NAME, "CHARTER") | str_detect(NAME, "SCHOOL$") | str_detect(NAME, "SCHOOL $"))


# NAMES Cleanup/Shortening:
# Get rid of parenthesis
data_ALL$NAME <- str_replace_all(data_ALL$NAME, "[(]THE[)]", "")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, "[(]THE", "")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, "[(]K-3[)]", "")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, "[(]", "")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, "[)]", "")
# Shorten/Clean
data_ALL$NAME <- str_replace_all(data_ALL$NAME, ' $', '')
data_ALL$NAME <- str_replace_all(data_ALL$NAME, " MAGNET SCHOOL$", "")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, " ELEMENTARY SCHOOL$", "")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, " ELEMENTARY SCH$", "")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, " AVENUE", "AVE")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, " COMMUNITY SCHOOL$", "")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, " ACADEMY$", "")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, "NORTH ", "N.")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, "SOUTH ", "S.")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, "EAST ", "E.")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, "WEST ", "W.")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, "NORTHERN ", "N.")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, "ELEMENTARY / ", "/")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, " SCHOOL FOR BOYS", "-BOYS")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, " SCHOOL FOR GIRLS", "-GIRLS")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, " OF SCIENCE & TECHNOLOGY", " SCI&TECH")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, "PREPARATORY", "PREP")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, "CHARTER SCHOOL", "CHARTER")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, " SCHOOL", "")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, "ACADEMY FOR.*", "ACADEMY")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, "INSTITUTE OF.*", "INST.")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, "ACADEMY CHARTER .*", "CHARTER")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, "INTERMED.*", "")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, " ELEMENTARY/MIDDLE$", "")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, " ELEM SCH$", "")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, " ELEMENTAR$", "")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, " MICROSOCIETY$", "")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, " IN EDUCATION$", "")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, "BRIGHTER CHOICE ", "BRIGHTER C.")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, " ACHIEVEMENT$", "")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, "COMMUNITY", "COMM")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, "CENTRAL MIDDLE$", "")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, " HIGH$", "")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, "FINN ACADEMY-AN ELMIRA CHARTER", "FINN AC-ELMIRA CHARTER")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, "ACADEMY .*", "ACADEMY")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, "PRIMARY$", "PRI")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, "CENTRAL$", "CNTRL")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, "ALDEN PRIMARY AT TOWNLINE", "ALDEN AT TOWNLINE")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, " ELEMENTARY OF TECHNOLOGY", " ELEM OF TECH")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, " ELEM SCH OF TECHNOLOGY", " SCH OF TECH")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, " OF ACADEMIC EXCELLENCE AT ", "-AT ")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, "MARTIN LUTHER KING", "MLK")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, " MULTICULTURAL INSTITUTE", "")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, " MAGNET$", "")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, "EARLY CHILDHOOD CENTER$", "EARLY CHILDHOOD")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, " ELEMENTARY CAMPUS", "")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, "CHILDREN'S OF ROCHESTER$", "CHILDRN-ROCHESTOR")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, "ELEMENTARY/$", "")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, " ELEMENTARY /", "")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, " COMM-AT #18", "Comm-#18")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, " SCH OF EXCELLENCE", "")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, " EARLY CHILDHOOD", "")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, " SCIENCE, TECHNOLOGY, ENGINEERING, ARTS AND MATH", " SCIENCE,TECH")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, " EXPLORATIONS$", " EX")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, " BICENTENNIAL", "BICENT")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, "ALTMAR-PARISH-WILLIAMSTOWN", "ALTMAR-PARISH-W")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, "GILBERTSVILLE-MOUNT UPTON", "GILBERTSVILLE-MT.UPTON")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, "CHERRY VALLEY-SPRINGFIELD", "CHERRY VALLEY-SPRINGF")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, "MADRID-WADDINGTON", "MADRID-WADD")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, "CENTER", "CNTR")
#data_ALL$NAME <- str_replace_all(data_ALL$NAME, "ROAD", "RD")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, "AVENUE", "AVE")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, " ELEM$", "")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, " ELEMENTARY.*", "")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, " Interm Sch$", "")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, " Comm Sch Excllnce -", "")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, " Sch Of Excellence", "")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, " TECHNOLOGIES", " TECH")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, " ACAD FOR ACAD EXCELLENCE", " ACAD")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, " STREET SCHOO$", "ST")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, " ELEM/INTRMED$", "")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, "BERNE-KNOX-W", "BERNE-KNOX-W")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, "CATTARAUGUS-LITTLE VALLEY", "CATTARAUGUS LV")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, "W W.SCHOOL", "W.SCHOOL")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, "EDUCATIONAL EXCELLENCE", "ED.EXCELLENCE")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, " & Computer.*", "")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, "FOR THE PERFORMING ARTS", "-PERFORMING ARTS")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, " AT THE", "-")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, " INTERM", "")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, "/DR HOSA ZOLLICOFFER", "")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, "GER ROAD", " RD")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, "OPPENHEIM-EPHRATAH.*", "OPPENHEIM-EP.ST.JOHNSVILLE")

# Put space after any periods (being we will later lowercase it otherwise)
data_ALL$NAME <- gsub('\\.', '\\. ', data_ALL$NAME)
# Make Lowercase (except after periods/spaces)
data_ALL$NAME <- paste0(str_to_title(data_ALL$NAME))
# Post lowercase cleanup.  Remove unwanted spaces
data_ALL$NAME <- str_replace_all(data_ALL$NAME, '\\. ', '\\.') # Get rid of end space (above subs could have been in the middle)
data_ALL$NAME <- str_replace_all(data_ALL$NAME, '- ', '-') # Get rid of end space (above subs could have been in the middle)
data_ALL$NAME <- str_replace_all(data_ALL$NAME, '\'b', '\'B') # Get rid of end space (above subs could have been in the middle)
data_ALL$NAME <- str_replace_all(data_ALL$NAME, ' $', '') # Get rid of end space (above subs could have been in the middle)
data_ALL$NAME <- str_replace_all(data_ALL$NAME, "Ps ", "PS")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, " Lv$", "LV")
#Get rid of any unwanted spaces
data_ALL$NAME <- str_replace_all(data_ALL$NAME, "  ", " ")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, "  ", " ")
data_ALL$NAME <- str_replace_all(data_ALL$NAME, " $", "")

# District Cleanup/Shortening:
data_ALL$DistBEDSCODE <- substr(data_ALL$BEDSCODE, 1, 6)
data_ALL$District <- lapply(data_ALL$DistBEDSCODE, function(x) BCode2County$District[match(x, BCode2County$BEDSCODE)])
data_ALL$District <- str_replace_all(data_ALL$District, " UFSD", "")
data_ALL$District <- str_replace_all(data_ALL$District, " CSD", "")
data_ALL$District <- str_replace_all(data_ALL$District, " CITY SD", "")
data_ALL$District <- str_replace_all(data_ALL$District, " SCHOOL DISTRICT", "")
data_ALL$District <- str_replace_all(data_ALL$District, " SD$", "")
data_ALL$District <- str_replace_all(data_ALL$District, "BRUNSWICK.*", "BRUNSWICK")
#data_ALL$District <- str_replace_all(data_ALL$District, "SOUTH ", "S.")
#data_ALL$District <- str_replace_all(data_ALL$District, "NORTH ", "N.")
data_ALL$District <- str_replace_all(data_ALL$District, "CENTRAL", "CNTRL")
data_ALL$District <- str_replace_all(data_ALL$District, "[(]LAKE SHORE[)]", "")
data_ALL$District <- str_replace_all(data_ALL$District, "(LAKE SHORE)", "")
data_ALL$District <- str_replace_all(data_ALL$District, "VAN HORNESVILLE-OWEN D YOUNG", "VAN HORNESVILLE-OWEN")
data_ALL$District <- str_replace_all(data_ALL$District, "CENTRAL VALLEY AT ILION-MOHAWK", "CENTRAL VALLEY-ILION")
data_ALL$District <- str_replace_all(data_ALL$District, "ALTMAR-PARISH-WILLIAMSTOWN", "ALTMAR-PARISH-W")
data_ALL$District <- str_replace_all(data_ALL$District, "GILBERTSVILLE-MOUNT UPTON", "GILBERTSVILLE-MT.UPTON")
data_ALL$District <- str_replace_all(data_ALL$District, "HAVERSTRAW-STONY POINT.*", "HAVERSTRAW-STONY PT")
data_ALL$District <- str_replace_all(data_ALL$District, "EAST RAMAPO.*", "EAST RAMAPO")
data_ALL$District <- str_replace_all(data_ALL$District, "BURNT HILLS-BALLSTON LAKE", "BURNT HILLS-B.LAKE")
data_ALL$District <- str_replace_all(data_ALL$District, "PINE VALLEY.*", "PINE VALLEY")
data_ALL$District <- str_replace_all(data_ALL$District, "EVANS-BRANT (LAKE SHORE).*", "EVANS-BRANT")
data_ALL$District <- str_replace_all(data_ALL$District, "WINDHAM-ASHLAND-JEWETT", "WINDHAM-ASHLAND")
data_ALL$District <- str_replace_all(data_ALL$District, "VAN HORNESVILLE-OWEN D YOUNG", "VAN HORNESVILLE")
data_ALL$District <- str_replace_all(data_ALL$District, "BERNE-KNOX-W", "BERNE-KNOX-W")
data_ALL$District <- str_replace_all(data_ALL$District, "RAVENA-COEYMANS-SELKIRK", "RAVENA-COEYMANS")
data_ALL$District <- str_replace_all(data_ALL$District, "OPPENHEIM-EPHRATAH-ST. JOHNS.*", "OPPENHEIM-EP.ST.J.")
data_ALL$District <- str_replace_all(data_ALL$District, "[(](.*?)[)]", "")
data_ALL$District <- str_replace_all(data_ALL$District, "[(].*", "")

# Make the new DistrictSchool Col
data_ALL$DistrictSchool <- paste0(data_ALL$District, "/ ", data_ALL$NAME)
# Remove unwanted spaces
data_ALL$DistrictSchool <- str_replace_all(data_ALL$DistrictSchool, '  ', ' ') # Get rid of end space (above subs could have been in the middle)
data_ALL$DistrictSchool <- str_replace_all(data_ALL$DistrictSchool, '  ', ' ') # Get rid of end space (above subs could have been in the middle)
data_ALL$DistrictSchool <- str_replace_all(data_ALL$DistrictSchool, ' $', '') # Get rid of end space (above subs could have been in the middle)


# remove 2 Temp COLs 
 data_ALL <- subset(data_ALL, select = -c(DistBEDSCODE, District))


############     DONE, SAVE AS CSV   ####################################
write.csv(data_ALL, output_file)
