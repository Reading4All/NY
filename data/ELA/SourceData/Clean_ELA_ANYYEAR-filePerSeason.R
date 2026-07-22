library(tidyverse)
library(dplyr)
library(janitor) # to use getdupes()
library(odbc) #brew install unixodbc, brew install mdbtools
library(DBI)
library(Hmisc)
library(readxl)
# install: brew install mdbtools

#######################################################################
##
## Open School Data (NY)
##
## This program is free software: you can redistribute it and/or modify it under
## the terms of the GNU AFFERO GENERAL PUBLIC LICENSE (the "License") as
## published by the Free Software Foundation, either version 3 of the License, or
## any later version.
##
## This program is distributed in the hope that it will be useful, but WITHOUT
## ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
## FOR A PARTICULAR PURPOSE.  See the License for more details.
##
#######################################################################
##
##      PROGRAM DESCRIPTION: Creates and cleans one school year's NYSED ELA DATABASE and saves it to 2 CSVs - one for fall, one for spring (school year-end).
##      Cleaning info:  - keeps cohorts: 1(all),13(gen ed), low econ status, 11(SWD),15(econ disadvantaged),12 (ELL), 13 (not ELL)
##                      - formats district and school names (to look shorter and consistant on reports), not econ dis
##                      - calculates 3_8 data (nysed only gives for all students)
##
##      ** THIS is for Windows (using odbc).  For unix/mac, update, around line 60 to connect to db
##
######################################################################
##     DIRECTIONS:
##
## 1 - Enter your working directory (replace what's in the quotes): 
       w_dir <- "/Users/XXX/"
##
## 2a - Download one year's NYSED Access Database from: https://data.nysed.gov/downloads.php, to:
##     and save it to folder in your working directory called: "SRCXXXX" (where X is the year, same as DBYEAR below)
## 2b - Create a folder in working directory where your clean files will be saved, called: "CLEANED_Data"
##
## 3 - Enter the school year (school year ending, 2022 or later ONLY) and NYSED DB filename (refer to the REadMe.pdf downloaded with the NYSED DB):
       DBYEAR <- 2025
       DBNAME <- "SRC2021_GroupII.mdb"
##     db names for the past few years:
##     SRC2025_Group1.mdb    SRC2024_Group5.mdb
##     SRC2023_Group5.mdb    SRC2022_GroupIV.mdb   SRC2021_GroupII.mdb
##     SRC2020_Post-GroupIII.mdb                   no 2019 data because of Covid
##
## 4 -Run this R script 
####################################################
       
## sets working dir 
setwd(paste0(w_dir, "CLEANED_Data"))
       
## sets output csv file names
output_file_spring <- "SOURCEDATA_ELA-SPRING-20XX.csv" #keep this name as is, to prevent accidentally overiding a previous run
output_file_fall <- "SOURCEDATA_ELA-FALL-20XX.csv" #keep this name as is

## Get DB data
# set db, to db location
db <- paste0(w_dir, 'SRC', DBYEAR, '/', DBNAME)
### fyi, to print db tables (without importing them):  db_tables <- Hmisc::mdb.get(db, tables = TRUE)
## on mac, was: data <- mdb.get(db, tables = "Annual EM ELA", allow=TRUE) #allow says to NOT replace colnames with _ to .

## on windows:
con <- dbConnect(odbc::odbc(),
                 Driver = "Microsoft Access Driver (*.mdb, *.accdb)",
                 DBQ = db)

data <- dbReadTable(con, "Annual EM ELA")
dbDisconnect(con)


# remove unwanted cols
data <- data[,c(4,2,3,5,6,7,8,9,10,16,18,21)]

# specify formatting
data$ENTITY_CD <- format(data$ENTITY_CD, scientific = FALSE)
data$ENTITY_CD <- str_replace_all(data$ENTITY_CD, ' ', '') #remove any spaces
data$LEVEL3_COUNT <- strtoi(data$LEVEL3_COUNT) #change from string to int
data$LEVEL4_COUNT <- strtoi(data$LEVEL4_COUNT) #change from string to int
data$PER_PROF <- strtoi(data$PER_PROF)
data$NUM_TESTED <- strtoi(data$NUM_TESTED)
data$SUBGROUP_NAME <- as.character(data$SUBGROUP_NAME)
data$ENTITY_NAME <- as.character(data$ENTITY_NAME)
data$ASSESSMENT_NAME <- as.character(data$ASSESSMENT_NAME)
data$TOTAL_COUNT <- as.integer(data$TOTAL_COUNT)
data$NOT_TESTED <- as.integer(data$NOT_TESTED)
data$PCT_NOT_TESTED <- as.integer(data$PCT_NOT_TESTED)

# REMOVE NYC COUNTIES
data <- data[!(startsWith(data$ENTITY_CD, "34")),] # 34*  = QUEENS (county_desc)
data <- data[!(startsWith(data$ENTITY_CD, "32")),] # 32* = BRONX
data <- data[!(startsWith(data$ENTITY_CD, "33")),] # 33* = KINGS
data <- data[!(startsWith(data$ENTITY_CD, "35")),] # 35* = RICHMOND
data <- data[!(startsWith(data$ENTITY_CD, "3100")),] # 3100*, 3101*..3106* = NEW YORK
data <- data[!(startsWith(data$ENTITY_CD, "3101")),] 
data <- data[!(startsWith(data$ENTITY_CD, "3102")),] 
data <- data[!(startsWith(data$ENTITY_CD, "3103")),] 
data <- data[!(startsWith(data$ENTITY_CD, "3104")),] 
data <- data[!(startsWith(data$ENTITY_CD, "3105")),] 
data <- data[!(startsWith(data$ENTITY_CD, "3106")),]

# REMOVE BESCODES 0-7 (groupings and charters)
data <- data[(data$ENTITY_CD != "000000000000"),]
data <- data[(data$ENTITY_CD != "000000000001"),]
data <- data[(data$ENTITY_CD != "000000000002"),]
data <- data[(data$ENTITY_CD != "000000000003"),]
data <- data[(data$ENTITY_CD != "000000000004"),]
data <- data[(data$ENTITY_CD != "000000000005"),]
data <- data[(data$ENTITY_CD != "000000000006"),]
data <- data[(data$ENTITY_CD != "000000000007"),]

# KEEP ONLY ELA data and COHORTS we'll use
data <- data %>% 
#  filter(str_detect(ASSESSMENT_NAME, ".ELA") | str_detect(ASSESSMENT_NAME, ".ELA ") | str_detect(ASSESSMENT_NAME, "ELA3_8")) %>%   # Keep ELA data only
  filter(SUBGROUP_NAME ==  "All Students" | SUBGROUP_NAME ==  "Students with Disabilities" | SUBGROUP_NAME ==  "Economically Disadvantaged" | SUBGROUP_NAME ==  "Non-English Language Learner" | SUBGROUP_NAME ==  "English Language Learner" | SUBGROUP_NAME ==  "General Education Students" | SUBGROUP_NAME ==  "Not Economically Disadvantaged")
# | SUBGROUP_CODE ==  10 | SUBGROUP_CODE ==  1 | SUBGROUP_CODE ==  11 | SUBGROUP_CODE ==  15 | SUBGROUP_CODE ==  6 | SUBGROUP_CODE ==  12 | SUBGROUP_CODE ==  13 )

# Create col for FALL or SPRING test (spring is dbYEAR name, fall is previous year)
data$SEASON[data$YEAR == DBYEAR] <- 'Spring'
data$SEASON[data$YEAR == DBYEAR - 1] <- 'Fall'

# Create COUNTY cols
data$COUNTY_DESC <- "EMPTY"

### Use a lookup table to find NA/empty County Codes 
## Import BEDSCODE-County LookUp Table
# inputfile <- "EDSCODE_County.csv"
# BCode2County <- read.csv(inputfile)[,c(1,2,3)]  # District, districtBedsCode (1st 6 digs), County
inputfile <- "BEDSCODE_County2.xlsx"
BCode2County <- read_excel(inputfile)  #District, districtBedsCode (1st 2 dig), County
#Create lookup key, use it with: getCountyDesc['570101'] (first 6dig of BEDSCODE)
getCountyDesc <- BCode2County$COUNTY
names(getCountyDesc) <- BCode2County$COUNTY_CODE
# Populate missing County_desc, using bedscode - DBYEAR
# Populate county_desc col
data$COUNTY_CODE <- substr(data$ENTITY_CD, 1, 2) #first 2 are county code
data$COUNTY_DESC <- getCountyDesc[data$COUNTY_CODE]
# Remove temp COUNTY_CODE col
data <- subset(data, select = -c(COUNTY_CODE))

# Create col for ENTITY_TYPE (school or district) based on ENTITY_CD
data$ENTITY_TYPE <- 'SCHOOL'
data$ENTITY_TYPE[str_detect(data$ENTITY_CD, ".0000$")] <- 'DISTRICT'
data$ENTITY_TYPE[str_detect(data$ENTITY_CD, ".000000$")] <- 'COUNTY'
# Remove County data
data <- data[(data$ENTITY_TYPE != 'COUNTY'),]


# Calculate 3_8 data (because NYSED only gives data per grade, and total (of all grades 3_8) for 'All Students')
          # Create df that will be used to store all the new 3_8 calc'ed data 
          data_3_8toadd <- data
          # Copy and empty it, to new df to use as template
          data_3_8_empty <- data[(data$ASSESSMENT_NAME == "ELA3_8"),]
          # Clear out reading data
          data_3_8_empty[c(6:12)] <- NA_integer_
          # Create empty datatable for each subgroup
            # Cohort info:
            # Gen Ed:   SUBGROUP_CODE = 10, SUBGROUP_NAME = "General Education Students"), added 2022
            # SWD:      SUBGROUP_CODE = 11, SUBGROUP_NAME = "Students with Disabilities")
            # EconDis:  SUBGROUP_CODE = 15, SUBGROUP_NAME = "Economically Disadvantaged")
            # NotELL:   SUBGROUP_CODE = 12, SUBGROUP_NAME = "Not Limited English Proficient")
            # ELL:      SUBGROUP_CODE = 13, SUBGROUP_NAME = "Limited English Proficient")
          data_3_8_GENED <- data_3_8_empty
          data_3_8_SWD <- data_3_8_empty
          data_3_8_EconDis <- data_3_8_empty
          data_3_8_NotEconDis <- data_3_8_empty
          data_3_8_NotELL <- data_3_8_empty
          data_3_8_ELL <- data_3_8_empty
          
          # Use the 3_8 data (all students) as template, to sum the individual 3,4,5,6,7,8 data
          data_3_8toadd <- data_3_8toadd[(data_3_8toadd$ASSESSMENT_NAME != "ELA3_8"),]
          
          ###########   Populate 3_8 df for: GENED   #####
          #data_3_8_GENED$SUBGROUP_CODE <- 10
          data_3_8_GENED$SUBGROUP_NAME <- "General Education Students"
          data_3_8toadd_GENED <- data_3_8toadd[(data_3_8toadd$SUBGROUP_NAME == "General Education Students"),]
          
          ### Calculate GenEd 3_8 TOTAL TESTED
          Calculated_3_8_TEMP <- aggregate(NUM_TESTED ~ ENTITY_NAME + SUBGROUP_NAME + YEAR + SEASON, data = data_3_8toadd_GENED, FUN = sum)
          # get'TOTAL TESTED' value from Sums_3_8_GENED_NUM_TESTED
          data_3_8_GENED <- rows_update(data_3_8_GENED, Calculated_3_8_TEMP, by = c("ENTITY_NAME", "SUBGROUP_NAME", "YEAR", "SEASON"))
          
          ### Calculate GenEd 3_8 LEVEL3_COUNT
          Calculated_3_8_TEMP <- aggregate(LEVEL3_COUNT ~ ENTITY_NAME + SUBGROUP_NAME + YEAR + SEASON, data = data_3_8toadd_GENED, FUN = sum)
          # get'TOTAL TESTED' value from Sums_3_8_GENED_NUM_TESTED
          data_3_8_GENED <- rows_update(data_3_8_GENED, Calculated_3_8_TEMP, by = c("ENTITY_NAME", "SUBGROUP_NAME", "YEAR", "SEASON"))
          
          ### Calculate GenEd 3_8 LEVEL4_COUNT
          Calculated_3_8_TEMP <- aggregate(LEVEL4_COUNT ~ ENTITY_NAME + SUBGROUP_NAME + YEAR + SEASON, data = data_3_8toadd_GENED, FUN = sum)
          # get'TOTAL TESTED' value from Sums_3_8_GENED_NUM_TESTED
          data_3_8_GENED <- rows_update(data_3_8_GENED, Calculated_3_8_TEMP, by = c("ENTITY_NAME", "SUBGROUP_NAME", "YEAR", "SEASON"))
          
          ## Calculate GenEd 3_8 TOTAL_COUNT
          Calculated_3_8_TEMP <- aggregate(TOTAL_COUNT ~ ENTITY_NAME + SUBGROUP_NAME + YEAR + SEASON, data = data_3_8toadd_GENED, FUN = sum)
          # get'TOTAL TESTED' value from Sums_3_8_GENED_NUM_TESTED
          data_3_8_GENED <- rows_update(data_3_8_GENED, Calculated_3_8_TEMP, by = c("ENTITY_NAME", "SUBGROUP_NAME", "YEAR", "SEASON"))
          
          ## Calculate GenEd 3_8 not tested
          Calculated_3_8_TEMP <- aggregate(NOT_TESTED ~ ENTITY_NAME + SUBGROUP_NAME + YEAR + SEASON, data = data_3_8toadd_GENED, FUN = sum)
          # get'TOTAL TESTED' value from Sums_3_8_GENED_NUM_TESTED
          data_3_8_GENED <- rows_update(data_3_8_GENED, Calculated_3_8_TEMP, by = c("ENTITY_NAME", "SUBGROUP_NAME", "YEAR", "SEASON"))
          
          
          ###########   Populate 3_8 df for: SWD   #####
          #data_3_8_SWD$SUBGROUP_CODE <- 11
          data_3_8_SWD$SUBGROUP_NAME <- "Students with Disabilities"
          data_3_8toadd_SWD <- data_3_8toadd[(data_3_8toadd$SUBGROUP_NAME == "Students with Disabilities"),]
          
          ### Calculate SWD 3_8 TOTAL TESTED
          Calculated_3_8_TEMP <- aggregate(NUM_TESTED ~ ENTITY_NAME + SUBGROUP_NAME + YEAR + SEASON, data = data_3_8toadd_SWD, FUN = sum)
          # get'TOTAL TESTED' value from Sums_3_8_SWD_NUM_TESTED
          data_3_8_SWD <- rows_update(data_3_8_SWD, Calculated_3_8_TEMP, by = c("ENTITY_NAME", "SUBGROUP_NAME", "SEASON"))
          
          ### Calculate SWD 3_8 LEVEL3_COUNT
          Calculated_3_8_TEMP <- aggregate(LEVEL3_COUNT ~ ENTITY_NAME + SUBGROUP_NAME + YEAR + SEASON, data = data_3_8toadd_SWD, FUN = sum)
          # get'TOTAL TESTED' value from Sums_3_8_SWD_NUM_TESTED
          data_3_8_SWD <- rows_update(data_3_8_SWD, Calculated_3_8_TEMP, by = c("ENTITY_NAME", "SUBGROUP_NAME", "SEASON"))
          
          ### Calculate GenEd 3_8 LEVEL4_COUNT
          Calculated_3_8_TEMP <- aggregate(LEVEL4_COUNT ~ ENTITY_NAME + SUBGROUP_NAME + YEAR + SEASON, data = data_3_8toadd_SWD, FUN = sum)
          # get'TOTAL TESTED' value from Sums_3_8_SWD_NUM_TESTED
          data_3_8_SWD <- rows_update(data_3_8_SWD, Calculated_3_8_TEMP, by = c("ENTITY_NAME", "SUBGROUP_NAME", "SEASON"))
          
          ## Calculate GenEd 3_8 TOTAL_COUNT
          Calculated_3_8_TEMP <- aggregate(TOTAL_COUNT ~ ENTITY_NAME + SUBGROUP_NAME + YEAR + SEASON, data = data_3_8toadd_SWD, FUN = sum)
          # get'TOTAL TESTED' value from Sums_3_8_SWD_NUM_TESTED
          data_3_8_SWD <- rows_update(data_3_8_SWD, Calculated_3_8_TEMP, by = c("ENTITY_NAME", "SUBGROUP_NAME", "SEASON"))
          
          ## Calculate GenEd 3_8 not tested
          Calculated_3_8_TEMP <- aggregate(NOT_TESTED ~ ENTITY_NAME + SUBGROUP_NAME + YEAR + SEASON, data = data_3_8toadd_SWD, FUN = sum)
          # get'TOTAL TESTED' value from Sums_3_8_SWD_NUM_TESTED
          data_3_8_SWD <- rows_update(data_3_8_SWD, Calculated_3_8_TEMP, by = c("ENTITY_NAME", "SUBGROUP_NAME", "SEASON"))
          
          
          ###########   Populate 3_8 df for: EconDis   #####
          #data_3_8_EconDis$SUBGROUP_CODE <- 15
          data_3_8_EconDis$SUBGROUP_NAME <- "Economically Disadvantaged"
          data_3_8toadd_EconDis <- data_3_8toadd[(data_3_8toadd$SUBGROUP_NAME == "Economically Disadvantaged"),]
          
          ### Calculate EconDis 3_8 TOTAL TESTED
          Calculated_3_8_TEMP <- aggregate(NUM_TESTED ~ ENTITY_NAME + SUBGROUP_NAME + YEAR + SEASON, data = data_3_8toadd_EconDis, FUN = sum)
          # get'TOTAL TESTED' value from Sums_3_8_EconDis_NUM_TESTED
          data_3_8_EconDis <- rows_update(data_3_8_EconDis, Calculated_3_8_TEMP, by = c("ENTITY_NAME", "SUBGROUP_NAME", "SEASON"))
          
          ### Calculate EconDis 3_8 LEVEL3_COUNT
          Calculated_3_8_TEMP <- aggregate(LEVEL3_COUNT ~ ENTITY_NAME + SUBGROUP_NAME + YEAR + SEASON, data = data_3_8toadd_EconDis, FUN = sum)
          # get'TOTAL TESTED' value from Sums_3_8_EconDis_NUM_TESTED
          data_3_8_EconDis <- rows_update(data_3_8_EconDis, Calculated_3_8_TEMP, by = c("ENTITY_NAME", "SUBGROUP_NAME", "SEASON"))
          
          ### Calculate EconDis 3_8 LEVEL4_COUNT
          Calculated_3_8_TEMP <- aggregate(LEVEL4_COUNT ~ ENTITY_NAME + SUBGROUP_NAME + YEAR + SEASON, data = data_3_8toadd_EconDis, FUN = sum)
          # get'TOTAL TESTED' value from Sums_3_8_EconDis_NUM_TESTED
          data_3_8_EconDis <- rows_update(data_3_8_EconDis, Calculated_3_8_TEMP, by = c("ENTITY_NAME", "SUBGROUP_NAME", "SEASON"))
          
          ## Calculate EconDis 3_8 TOTAL_COUNT
          Calculated_3_8_TEMP <- aggregate(TOTAL_COUNT ~ ENTITY_NAME + SUBGROUP_NAME + YEAR + SEASON, data = data_3_8toadd_EconDis, FUN = sum)
          # get'TOTAL TESTED' value from Sums_3_8_EconDis_NUM_TESTED
          data_3_8_EconDis <- rows_update(data_3_8_EconDis, Calculated_3_8_TEMP, by = c("ENTITY_NAME", "SUBGROUP_NAME", "SEASON"))
          
          ## Calculate EconDis 3_8 not tested
          Calculated_3_8_TEMP <- aggregate(NOT_TESTED ~ ENTITY_NAME + SUBGROUP_NAME + YEAR + SEASON, data = data_3_8toadd_EconDis, FUN = sum)
          # get'TOTAL TESTED' value from Sums_3_8_EconDis_NUM_TESTED
          data_3_8_EconDis <- rows_update(data_3_8_EconDis, Calculated_3_8_TEMP, by = c("ENTITY_NAME", "SUBGROUP_NAME", "SEASON"))
          
          ###########   Populate 3_8 df for: NotEconDis   #####
          #data_3_8_NotEconDis$SUBGROUP_CODE <- 15
          data_3_8_NotEconDis$SUBGROUP_NAME <- "Economically Disadvantaged"
          data_3_8toadd_NotEconDis <- data_3_8toadd[(data_3_8toadd$SUBGROUP_NAME == "Economically Disadvantaged"),]
          
          ### Calculate NotEconDis 3_8 TOTAL TESTED
          Calculated_3_8_TEMP <- aggregate(NUM_TESTED ~ ENTITY_NAME + SUBGROUP_NAME + YEAR + SEASON, data = data_3_8toadd_NotEconDis, FUN = sum)
          # get'TOTAL TESTED' value from Sums_3_8_NotEconDis_NUM_TESTED
          data_3_8_NotEconDis <- rows_update(data_3_8_NotEconDis, Calculated_3_8_TEMP, by = c("ENTITY_NAME", "SUBGROUP_NAME", "SEASON"))
          
          ### Calculate NotEconDis 3_8 LEVEL3_COUNT
          Calculated_3_8_TEMP <- aggregate(LEVEL3_COUNT ~ ENTITY_NAME + SUBGROUP_NAME + YEAR + SEASON, data = data_3_8toadd_NotEconDis, FUN = sum)
          # get'TOTAL TESTED' value from Sums_3_8_NotEconDis_NUM_TESTED
          data_3_8_NotEconDis <- rows_update(data_3_8_NotEconDis, Calculated_3_8_TEMP, by = c("ENTITY_NAME", "SUBGROUP_NAME", "SEASON"))
          
          ### Calculate NotEconDis 3_8 LEVEL4_COUNT
          Calculated_3_8_TEMP <- aggregate(LEVEL4_COUNT ~ ENTITY_NAME + SUBGROUP_NAME + YEAR + SEASON, data = data_3_8toadd_NotEconDis, FUN = sum)
          # get'TOTAL TESTED' value from Sums_3_8_NotEconDis_NUM_TESTED
          data_3_8_NotEconDis <- rows_update(data_3_8_NotEconDis, Calculated_3_8_TEMP, by = c("ENTITY_NAME", "SUBGROUP_NAME", "SEASON"))
          
          ## Calculate NotEconDis 3_8 TOTAL_COUNT
          Calculated_3_8_TEMP <- aggregate(TOTAL_COUNT ~ ENTITY_NAME + SUBGROUP_NAME + YEAR + SEASON, data = data_3_8toadd_NotEconDis, FUN = sum)
          # get'TOTAL TESTED' value from Sums_3_8_NotEconDis_NUM_TESTED
          data_3_8_NotEconDis <- rows_update(data_3_8_NotEconDis, Calculated_3_8_TEMP, by = c("ENTITY_NAME", "SUBGROUP_NAME", "SEASON"))
          
          ## Calculate NotEconDis 3_8 not tested
          Calculated_3_8_TEMP <- aggregate(NOT_TESTED ~ ENTITY_NAME + SUBGROUP_NAME + YEAR + SEASON, data = data_3_8toadd_NotEconDis, FUN = sum)
          # get'TOTAL TESTED' value from Sums_3_8_NotEconDis_NUM_TESTED
          data_3_8_NotEconDis <- rows_update(data_3_8_NotEconDis, Calculated_3_8_TEMP, by = c("ENTITY_NAME", "SUBGROUP_NAME", "SEASON"))
          
          ###########   Populate 3_8 df for: ELL   #####
          #data_3_8_ELL$SUBGROUP_CODE <- 13
          data_3_8_ELL$SUBGROUP_NAME <- "English Language Learner"
          data_3_8toadd_ELL <- data_3_8toadd[(data_3_8toadd$SUBGROUP_NAME == "English Language Learner"),]
          
          ### Calculate ELL 3_8 TOTAL TESTED
          Calculated_3_8_TEMP <- aggregate(NUM_TESTED ~ ENTITY_NAME + SUBGROUP_NAME + YEAR + SEASON, data = data_3_8toadd_ELL, FUN = sum)
          # get'TOTAL TESTED' value from Sums_3_8_ELL_NUM_TESTED
          data_3_8_ELL <- rows_update(data_3_8_ELL, Calculated_3_8_TEMP, by = c("ENTITY_NAME", "SUBGROUP_NAME", "SEASON"))
          
          ### Calculate ELL 3_8 LEVEL3_COUNT
          Calculated_3_8_TEMP <- aggregate(LEVEL3_COUNT ~ ENTITY_NAME + SUBGROUP_NAME + YEAR + SEASON, data = data_3_8toadd_ELL, FUN = sum)
          # get'TOTAL TESTED' value from Sums_3_8_ELL_NUM_TESTED
          data_3_8_ELL <- rows_update(data_3_8_ELL, Calculated_3_8_TEMP, by = c("ENTITY_NAME", "SUBGROUP_NAME", "SEASON"))
          
          ### Calculate ELL 3_8 LEVEL4_COUNT
          Calculated_3_8_TEMP <- aggregate(LEVEL4_COUNT ~ ENTITY_NAME + SUBGROUP_NAME + YEAR + SEASON, data = data_3_8toadd_ELL, FUN = sum)
          # get'TOTAL TESTED' value from Sums_3_8_ELL_NUM_TESTED
          data_3_8_ELL <- rows_update(data_3_8_ELL, Calculated_3_8_TEMP, by = c("ENTITY_NAME", "SUBGROUP_NAME", "SEASON"))
          
          ## Calculate ELL 3_8 TOTAL_COUNT
          Calculated_3_8_TEMP <- aggregate(TOTAL_COUNT ~ ENTITY_NAME + SUBGROUP_NAME + YEAR + SEASON, data = data_3_8toadd_ELL, FUN = sum)
          # get'TOTAL TESTED' value from Sums_3_8_ELL_NUM_TESTED
          data_3_8_ELL <- rows_update(data_3_8_ELL, Calculated_3_8_TEMP, by = c("ENTITY_NAME", "SUBGROUP_NAME", "SEASON"))
          
          ## Calculate ELL 3_8 not tested
          Calculated_3_8_TEMP <- aggregate(NOT_TESTED ~ ENTITY_NAME + SUBGROUP_NAME + YEAR + SEASON, data = data_3_8toadd_ELL, FUN = sum)
          # get'TOTAL TESTED' value from Sums_3_8_ELL_NUM_TESTED
          data_3_8_ELL <- rows_update(data_3_8_ELL, Calculated_3_8_TEMP, by = c("ENTITY_NAME", "SUBGROUP_NAME", "SEASON"))
          
          
          ###########   Populate 3_8 df for: NotELL   ##### 
          #data_3_8_NotELL$SUBGROUP_CODE <- 12
          data_3_8_NotELL$SUBGROUP_NAME <- "Non-English Language Learner"
          data_3_8toadd_NotELL <- data_3_8toadd[(data_3_8toadd$SUBGROUP_NAME == "Non-English Language Learner"),]
          
          ### Calculate NotELL 3_8 TOTAL TESTED
          Calculated_3_8_TEMP <- aggregate(NUM_TESTED ~ ENTITY_NAME + SUBGROUP_NAME + YEAR + SEASON, data = data_3_8toadd_NotELL, FUN = sum)
          # get'TOTAL TESTED' value from Sums_3_8_NotELL_NUM_TESTED
          data_3_8_NotELL <- rows_update(data_3_8_NotELL, Calculated_3_8_TEMP, by = c("ENTITY_NAME", "SUBGROUP_NAME", "SEASON"))
          
          ### Calculate NotELL 3_8 LEVEL3_COUNT
          Calculated_3_8_TEMP <- aggregate(LEVEL3_COUNT ~ ENTITY_NAME + SUBGROUP_NAME + YEAR + SEASON, data = data_3_8toadd_NotELL, FUN = sum)
          # get'TOTAL TESTED' value from Sums_3_8_NotELL_NUM_TESTED
          data_3_8_NotELL <- rows_update(data_3_8_NotELL, Calculated_3_8_TEMP, by = c("ENTITY_NAME", "SUBGROUP_NAME", "SEASON"))
          
          ### Calculate NotELL 3_8 LEVEL4_COUNT
          Calculated_3_8_TEMP <- aggregate(LEVEL4_COUNT ~ ENTITY_NAME + SUBGROUP_NAME + YEAR + SEASON, data = data_3_8toadd_NotELL, FUN = sum)
          # get'TOTAL TESTED' value from Sums_3_8_NotELL_NUM_TESTED
          data_3_8_NotELL <- rows_update(data_3_8_NotELL, Calculated_3_8_TEMP, by = c("ENTITY_NAME", "SUBGROUP_NAME", "SEASON"))
          
          ## Calculate NotELL 3_8 TOTAL_COUNT
          Calculated_3_8_TEMP <- aggregate(TOTAL_COUNT ~ ENTITY_NAME + SUBGROUP_NAME + YEAR + SEASON, data = data_3_8toadd_NotELL, FUN = sum)
          # get'TOTAL TESTED' value from Sums_3_8_NotELL_NUM_TESTED
          data_3_8_NotELL <- rows_update(data_3_8_NotELL, Calculated_3_8_TEMP, by = c("ENTITY_NAME", "SUBGROUP_NAME", "SEASON"))
          
          ## Calculate NotELL 3_8 not tested
          Calculated_3_8_TEMP <- aggregate(NOT_TESTED ~ ENTITY_NAME + SUBGROUP_NAME + YEAR + SEASON, data = data_3_8toadd_NotELL, FUN = sum)
          # get'TOTAL TESTED' value from Sums_3_8_NotELL_NUM_TESTED
          data_3_8_NotELL <- rows_update(data_3_8_NotELL, Calculated_3_8_TEMP, by = c("ENTITY_NAME", "SUBGROUP_NAME", "SEASON"))
          
          ###############
          ## Combine and store all 3_8 calculated data for each year
          data_calculated <- rbind(data_3_8_GENED, data_3_8_SWD, data_3_8_EconDis, data_3_8_NotEconDis, data_3_8_NotELL, data_3_8_ELL)
          
          # Add new calculated rows to orig data
          data <- rbind(data, data_calculated)
##########################################################

          
# New cols for calculated percent proficient - BASED ON TOTAL COUNT, not just TOTAL TESTED which NYS does!
  data$Prof_Count <- with(data, LEVEL3_COUNT + LEVEL4_COUNT)
  data$NotProf_Count <- with(data, NUM_TESTED - Prof_Count)
  data$Prof_Pct <- with(data, (Prof_Count / TOTAL_COUNT) * 100)
  data$NotProf_Pct <- with(data, (NotProf_Count / TOTAL_COUNT) * 100)
  data$NotTested_Pct <- with(data, (NOT_TESTED / TOTAL_COUNT) * 100)
          
          
# Formatting
# data$ENTITY_CD <- str_replace_all(data$ENTITY_CD, ' ', '')  #Get rid of double spaces
# Keep Year Only in Date Col
data$PER_PROF <- gsub('%', '', data$PER_PROF) # not sure if needed anymore
data$PER_PROF [ data$PER_PROF  == "-" ] <- NA
data$NUM_TESTED[ data$NUM_TESTED == "-" ] <- NA
data$ENTITY_NAME <- str_replace_all(data$ENTITY_NAME, ' $', '') 
data$ENTITY_NAME <- str_replace_all(data$ENTITY_NAME, ' $', '') # Repeat - some have 2 spaces at the end

# Format, Clean: Abbreviate 'school districts to SD' to make shorter; better for plots 
data$ENTITY_NAME <- str_replace(data$ENTITY_NAME, "CENTRAL SCH_*", "CSD ")
data$ENTITY_NAME <- str_replace(data$ENTITY_NAME, "UNION FREE SCHOOL DISTRICT OF", "UFSD OF")  #Must be before next replace line
data$ENTITY_NAME <- str_replace(data$ENTITY_NAME, "UNION FR_*", "UFSD ")
data$ENTITY_NAME <- str_replace(data$ENTITY_NAME, "SCHOOL DISTRICT", "SD ") 
data$ENTITY_NAME <- str_replace(data$ENTITY_NAME, ' $', '') # Get rid of end space (above subs could have been in the middle)
data$ENTITY_NAME <- str_replace(data$ENTITY_NAME, "CENTRAL HS", "CHS")

# Format, Clean: Abbreviate Long School Names to make shorter; better for plots 
data$ENTITY_NAME <- str_replace(data$ENTITY_NAME, "SPRINGVILLE-GRIFFITH INSTITUTE CSD", "SPRINGVILLE-GRIFF INST CSD")
data$ENTITY_NAME <- str_replace(data$ENTITY_NAME, "VAN HORNESVILLE-OWEN D YOUNG CSD", "VAN HORNESVILLE-OWEN..CSD")
data$ENTITY_NAME <- str_replace(data$ENTITY_NAME, "CENTRAL VALLEY CSD AT ILION-MOHAWK", "CENTR.VALLEY CSD AT IL-MOHAWK")
data$ENTITY_NAME <- str_replace(data$ENTITY_NAME, "OPPENHEIM-EPHRATAH-ST. JOHNSVILLE CSD", "OPPENHEIM-EP.ST.JOHNSV.CSD")
data$ENTITY_NAME <- str_replace(data$ENTITY_NAME, "BELLMORE-MERRICK CENTRAL HIGH SD", "BELLMORE-MERRICK CENTRAL SD")
data$ENTITY_NAME <- str_replace(data$ENTITY_NAME, "NEW HYDE PARK-GARDEN CITY PARK UFSD", "NEW HYDE-GARDEN CITY PARK UFSD")
data$ENTITY_NAME <- str_replace(data$ENTITY_NAME, "ALTMAR-PARISH-WILLIAMSTOWN CSD", "ALTMAR-PAR.-WILLIAMSTOWN CSD")
data$ENTITY_NAME <- str_replace(data$ENTITY_NAME, "HAVERSTRAW-STONY POINT CSD [(]NORTH R", "HAVERS-STONY PT CSD(N.R")
data$ENTITY_NAME <- str_replace(data$ENTITY_NAME, "MOUNT PLEASANT-BLYTHEDALE UFSD", "MT PLEASANT-BLYTHEDALE UFSD")
data$ENTITY_NAME <- str_replace(data$ENTITY_NAME, "THE ENLARGED CITY SD  OF THE CITY OF SARATOGA SPRINGS", "SD - CITY OF SARATOGA SPRINGS")

# Format, Clean: Make lower case to make shorter; better for plots
data$ENTITY_NAME <- str_to_title(data$ENTITY_NAME)
data$ENTITY_NAME <- str_replace(data$ENTITY_NAME, "Central H", "CHS")
data$ENTITY_NAME <- str_replace(data$ENTITY_NAME, "Ufsd", "UFSD")
data$ENTITY_NAME <- str_replace(data$ENTITY_NAME, "Csd", "CSD")
data$ENTITY_NAME <- str_replace(data$ENTITY_NAME, "Sd", "SD")
data$ENTITY_NAME <- str_replace(data$ENTITY_NAME, "East Nor", "E.N")
data$ENTITY_NAME <- str_replace(data$ENTITY_NAME, "Garden City Park", "GCP")

###################################################
## Keeping for reference only - names used to be different
## Clean Data by YEAR - Make consistent col names, and add and populate missing 2021 County columns )
## Rename Year ColNames to be consistent with prev years
# names(data)[names(data) == "YEAR"] <- "YEAR"
# names(data)[names(data) == "ENTITY_CD"] <- "BEDSCODE"
# names(data)[names(data) == "ENTITY_NAME"] <- "NAME"
# names(data)[names(data) == "ASSESSMENT_NAME"] <- "ITEM_DESC"
# names(data)[names(data) == "NUM_TESTED"] <- "TOTAL_TESTED"
# names(data)[names(data) == "PER_PROF"] <- "L3.L4_PCT"
# names(data)[names(data) == "LEVEL3_COUNT"] <- "L3_COUNT"
# names(data)[names(data) == "LEVEL4_COUNT"] <- "L4_COUNT"
####################################################

# SHOULDNT HAVE dups - just a check to make sure
# find/store any Duplicates that exist
DUPS <- get_dupes(data, c("YEAR","COUNTY_DESC", "ENTITY_CD", "ENTITY_NAME", "ASSESSMENT_NAME", "SUBGROUP_NAME", "SEASON"))
# remove them
# data <- unique(data)

###############
##Separate into 2 dfs - spring, fall files
data_spring <- data %>% filter(SEASON ==  "Spring")
data_fall <- data %>% filter(SEASON ==  "Fall")

############     DONE, SAVE AS CSV   ####################################
write.csv(data_spring, output_file_spring)
write.csv(data_fall, output_file_fall)
