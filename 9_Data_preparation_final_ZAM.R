#Script for additional data preparation and disaggregation variables 



## Load packages
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(readxl, purrr, scales, lubridate, haven, labelled, shiny, shinydashboard,
               shinyWidgets, shinyauthr, shinyjs, tidyverse, fusen, plotly,
               unhcrthemes, ggforce, ggridges, DT, bslib, readr, writexl, pins, sparkline, 
               leaflet, shinyBS, mapboxer, zscorer, rlang, nipnTK, rsconnect, dm, robotoolbox,
               Microsoft365R,expss,srvyr)

#1. Load data 
FDS_ZAM2025_clean_9 <- readRDS("FDS_ZAM2025_clean_9.rds")
HHroster <- FDS_ZAM2025_clean_9$HHroster
main <- FDS_ZAM2025_clean_9$main
RA_adult <- FDS_ZAM2025_clean_9$RA_adult
RA_woman <- FDS_ZAM2025_clean_9$RA_woman
RA_caregiver <- FDS_ZAM2025_clean_9$RA_caregiver

#2. Rename child age about whom the child labour questions were asked. Renaming as we used a different variable name in the indicator scripts 
main <- main %>%
  rename(child_labor_age = child_age_labour)

#3. Household members size
# Create a new variable to count the number of household members in each household 
##Here we are using the variable "member" which assigned a 1 for each person that passes the check for actually being a household member. This check done automatically in the data collection form. 
household_counts <- HHroster %>%
  group_by(`_uuid`) %>%
  summarise(HHmembersize = sum(member == 1, na.rm = TRUE))
##Join with the main data set
main <- main %>%
  left_join(household_counts, by = "_uuid")

#Disaggregation variables 
## Roster ----
## 1. Population Groups ----
##Population group variable is available in the main dataset. Here we pull it to the HHroster dataset. 
HHroster <- HHroster %>%
  left_join(
    main %>% select(`_uuid`, Intro_07), # Select relevant columns from main
    by = c("_uuid" = "_uuid") # Match on uuid 
  ) 

## 2. Age ----

# Convert agetouse from character to numeric
HHroster$agetouse <- as.numeric(HHroster$agetouse)

### Create two new variables for different age categories
### 4 categories (0-4, 5-17, 18-59, 60+)

HHroster$HH_04_cat4 <- cut(HHroster$agetouse, breaks = c(-1, 4, 17, 59, Inf), 
                           labels = c("0-4", "5-17", "18-59", "60+"))

### 2 categories (<18, >18)

HHroster$HH_04_cat2 <- cut(HHroster$agetouse, breaks = c(-1, 17, Inf), 
                           labels = c("0-17", "18-60+"))

## 3. Disability ----

### Washington Group Index
#Disability status identifiers are calculated based on guidelines by the [Washington Group on Disability Statistics](https://www.washingtongroup-disability.com/fileadmin/uploads/wg/WG_Document__6C_-_Analytic_Guidelines_for_the_WG-ES__Stata_.pdf) for over 5-year-olds. Note that in FDS we have three levels of disability level (some difficulty, a lot of difficulty, cannot do at all, + don't know, refuse to answer).

#Disability is specified as at least one domain/question coded A LOT OF DIFFICULTY or CANNOT DO AT ALL. 

###Step 1: Add the level 0 - no difficulty, for those members above 5 years of age who were not identified as having disabilities. 
#Vision


copy_var_label <- function(orig, new_var) {
  var_label <- attr(orig, "label")
  if (!is.null(var_label)) {
    attr(new_var, "label") <- var_label
  }
  new_var
}

# Dis_03

HHroster <- HHroster %>%
  mutate(Dis_03_char = as.character(Dis_03)) %>%
  mutate(Dis_03_num = suppressWarnings(as.numeric(Dis_03_char))) %>%
  mutate(Dis_03_new = if_else(is.na(Dis_03_num) & agetouse > 5, 0, Dis_03_num)) %>%
  mutate(Dis_03_new = labelled(Dis_03_new, labels = c(No = 0, Yes = 1))) %>%
  mutate(Dis_03_new = copy_var_label(Dis_03, Dis_03_new)) %>%
  select(-Dis_03, -Dis_03_char, -Dis_03_num) %>%
  rename(Dis_03 = Dis_03_new)

# Dis_06
HHroster <- HHroster %>%
  mutate(Dis_06_char = as.character(Dis_06)) %>%
  mutate(Dis_06_num = suppressWarnings(as.numeric(Dis_06_char))) %>%
  mutate(Dis_06_new = if_else(is.na(Dis_06_num) & agetouse > 5, 0, Dis_06_num)) %>%
  mutate(Dis_06_new = labelled(Dis_06_new, labels = c(No = 0, Yes = 1))) %>%
  mutate(Dis_06_new = copy_var_label(Dis_06, Dis_06_new)) %>%
  select(-Dis_06, -Dis_06_char, -Dis_06_num) %>%
  rename(Dis_06 = Dis_06_new)

# Dis_09
HHroster <- HHroster %>%
  mutate(Dis_09_char = as.character(Dis_09)) %>%
  mutate(Dis_09_num = suppressWarnings(as.numeric(Dis_09_char))) %>%
  mutate(Dis_09_new = if_else(is.na(Dis_09_num) & agetouse > 5, 0, Dis_09_num)) %>%
  mutate(Dis_09_new = labelled(Dis_09_new, labels = c(No = 0, Yes = 1))) %>%
  mutate(Dis_09_new = copy_var_label(Dis_09, Dis_09_new)) %>%
  select(-Dis_09, -Dis_09_char, -Dis_09_num) %>%
  rename(Dis_09 = Dis_09_new)

# Dis_12
HHroster <- HHroster %>%
  mutate(Dis_12_char = as.character(Dis_12)) %>%
  mutate(Dis_12_num = suppressWarnings(as.numeric(Dis_12_char))) %>%
  mutate(Dis_12_new = if_else(is.na(Dis_12_num) & agetouse > 5, 0, Dis_12_num)) %>%
  mutate(Dis_12_new = labelled(Dis_12_new, labels = c(No = 0, Yes = 1))) %>%
  mutate(Dis_12_new = copy_var_label(Dis_12, Dis_12_new)) %>%
  select(-Dis_12, -Dis_12_char, -Dis_12_num) %>%
  rename(Dis_12 = Dis_12_new)

# Dis_15
HHroster <- HHroster %>%
  mutate(Dis_15_char = as.character(Dis_15)) %>%
  mutate(Dis_15_num = suppressWarnings(as.numeric(Dis_15_char))) %>%
  mutate(Dis_15_new = if_else(is.na(Dis_15_num) & agetouse > 5, 0, Dis_15_num)) %>%
  mutate(Dis_15_new = labelled(Dis_15_new, labels = c(No = 0, Yes = 1))) %>%
  mutate(Dis_15_new = copy_var_label(Dis_15, Dis_15_new)) %>%
  select(-Dis_15, -Dis_15_char, -Dis_15_num) %>%
  rename(Dis_15 = Dis_15_new)

# Dis_18
HHroster <- HHroster %>%
  mutate(Dis_18_char = as.character(Dis_18)) %>%
  mutate(Dis_18_num = suppressWarnings(as.numeric(Dis_18_char))) %>%
  mutate(Dis_18_new = if_else(is.na(Dis_18_num) & agetouse > 5, 0, Dis_18_num)) %>%
  mutate(Dis_18_new = labelled(Dis_18_new, labels = c(No = 0, Yes = 1))) %>%
  mutate(Dis_18_new = copy_var_label(Dis_18, Dis_18_new)) %>%
  select(-Dis_18, -Dis_18_char, -Dis_18_num) %>%
  rename(Dis_18 = Dis_18_new)


###Step 2: Generate frequency distributions on each of the WG-SS domain variables

### 0 No difficulty 
### 1	Some difficulty
### 2	A lot of difficulties
### 3	Cannot do at all
### 98	Don’t know
### 99	Refused to Answer

#Vision 
barplot(table(HHroster$Dis_03), main = "Vision")
#Hearing
barplot(table(HHroster$Dis_06), main = "Hearing")
#Mobility
barplot(table(HHroster$Dis_09), main = "Mobility")
#Cognition
barplot(table(HHroster$Dis_12), main = "Cognition")
#Self-care
barplot(table(HHroster$Dis_15), main = "Self-care")
#Communication
barplot(table(HHroster$Dis_18), main = "Communicating")


#Step 3: Construct disability variable 
#Initialize disability variable with 2
HHroster$disability <- 0

# Replace disability with NA if all specified columns are missing and age is > 5
HHroster$disability <- ifelse(
  HHroster$agetouse > 5 &
    is.na(HHroster$Dis_03) & is.na(HHroster$Dis_06) & is.na(HHroster$Dis_09) &
    is.na(HHroster$Dis_12) & is.na(HHroster$Dis_15) & is.na(HHroster$Dis_18),
  NA, 
  HHroster$disability
)

# Replace disability with 1 if any of the specified conditions are met and age is > 5
HHroster$disability <- ifelse(
  HHroster$agetouse > 5 & (
    HHroster$Dis_03 %in% c(2, 3) | HHroster$Dis_06 %in% c(2, 3) | 
      HHroster$Dis_09 %in% c(2, 3) | HHroster$Dis_12 %in% c(2, 3) | 
      HHroster$Dis_15 %in% c(2, 3) | HHroster$Dis_18 %in% c(2, 3)
  ),
  1,
  HHroster$disability
)

# Tabulate the disability variable
table(HHroster$disability, useNA = "ifany")


## 4. Country of Origin ----
#The individual information on the country of origin comes from the HHroster. To have one single variable for country of origin information, the country code for the country of enumeration (i.e. ZAM for ZAMistan) will be entered as below. This question is asked only to individuals older than 15. For individuals younger than 15, the value is equaled to the country of origin of the household (as responded by the Head of the Household)

HHroster <- HHroster %>%
  left_join(
    main %>% select(`_uuid`, origincntry), # Select relevant columns from main
    by = c("_uuid" = "_uuid") # Match on uuid 
  ) 
HHroster <- HHroster %>%
  rename(origincountry_roster = origincntry) #Rename to origincountry_roster

# Step 1: Create clean numeric vector from ID_00
HHroster <- HHroster %>%
  mutate(
    ID_00_num_raw = unclass(ID_00),          # Remove labels
    ID_00_num = suppressWarnings(as.numeric(ID_00_num_raw)) # Force numeric
  )

# Debug: check classes and unique values
print(sapply(HHroster %>% select(ID_00, ID_00_num, ID_00_num_raw), class))
print(unique(HHroster$ID_00_num))

# Step 2: Run case_when with clean numeric
HHroster <- HHroster %>%
  mutate(
    COO = case_when(
      ID_00_num == 1 ~ "ZAM",
      ID_00_num == 2 ~ as.character(ID_00_specify),
      ID_00_num == 3 ~ "Stateless",
      ID_00_num == 99 ~ "99",
      ID_00_num == 98 ~ "98",
      is.na(ID_00_num) ~ as.character(origincountry_roster),
      TRUE ~ NA_character_
    )
  ) %>%
  select(-ID_00_num, -ID_00_num_raw)

## Head of the Household (main dataset) ----
#1. Age ----
# Age with no categories 
main <- main %>%
  left_join(
    HHroster %>% select(`_uuid`, hhroster_memberID, agetouse), # Select relevant columns from hhroster
    by = c("_uuid" = "_uuid", "HHposinfo" = "hhroster_memberID") # Match on _uuid and position
  ) %>%
  rename(HH_04_HoH = agetouse) # Rename agetouse to HH_04_hOh

# 2 categories 
main <- main %>%
  left_join(
    HHroster %>% select(`_uuid`, hhroster_memberID, HH_04_cat2), # Select relevant columns from HHroster
    by = c("_uuid" = "_uuid", "HHposinfo" = "hhroster_memberID") # Match on _uuid and position 
  ) 
main <- main %>%
  rename(HH_04_HoH_cat2 = HH_04_cat2) #HH_04_HoH_cat2

# 4 categories 
main <- main %>%
  left_join(
    HHroster %>% select(`_uuid`,hhroster_memberID, HH_04_cat4), # Select relevant columns from HHroster
    by = c("_uuid" = "_uuid", "HHposinfo" = "hhroster_memberID") # Match on _uuid  and position 
  ) 
main <- main %>%
  rename(HH_04_HoH_cat4 = HH_04_cat4) #HH_04_HoH_cat4

#2. Gender ----

main <- main %>%
  left_join(
    HHroster %>% select(`_uuid`, hhroster_memberID, HH_02), # Select relevant columns from HHroster
    by = c("_uuid" = "_uuid", "HHposinfo" = "hhroster_memberID") # Match on _uuid and position 
  ) 
main <- main %>%
  rename(HH_02_HoH = HH_02) #Rename to HH_02_HoH

#3. Disability ----

main <- main %>%
  left_join(
    HHroster %>% select(`_uuid`, hhroster_memberID, disability), # Select relevant columns from HHroster
    by = c("_uuid" = "_uuid", "HHposinfo" = "hhroster_memberID") # Match on _uuid and position 
  ) 
main <- main %>%
  rename(disability_HoH = disability) #Rename to disability_HoH

#4. Country of Origin ----

main <- main %>%
  left_join(
    HHroster %>% select(`_uuid`,hhroster_memberID, COO), # Select relevant columns from HHroster
    by = c("_uuid" = "_uuid", "HHposinfo" = "hhroster_memberID") # Match on _uuid and position 
  ) 
main <- main %>%
  rename(COO_HoH = COO) #Rename to COO_HoH


## Randomly Selected Adult (RA_adult dataset) ----

#1. Age ----
# 2 categories 
RA_adult$age_selected <- as.numeric(as.character(RA_adult$age_selected))

RA_adult$RA_HH_04_cat2 <- cut(RA_adult$age_selected, breaks = c(-1, 17, Inf), 
                              labels = c("0-17", "18-60+"))

# 4 categories 
RA_adult$age_selected <- as.numeric(as.character(RA_adult$age_selected))

RA_adult$RA_HH_04_cat4 <- cut(RA_adult$age_selected, breaks = c(-1, 4, 17, 59, Inf), 
                              labels = c("0-4", "5-17", "18-59", "60+"))

#2. Population Group ----

RA_adult <- RA_adult %>%
  left_join(
    main %>% select(`_uuid`, Intro_07), # Select relevant columns from main dataset
    by = c("_uuid" = "_uuid") # Match on _uuid 
  ) 

#3. Country of origin ----

RA_adult <- RA_adult %>%
  left_join(
    HHroster %>% select(`_uuid`, COO, hhroster_memberID), # Select relevant columns from HHroster
    by = c("_uuid" = "_uuid", "selected_adultap" = "hhroster_memberID") # Match on _uuid and HHroster position
  ) 
RA_adult <- RA_adult %>%
  rename(COO_RA = COO) #Rename to COO_RA

#4. Disability ----
RA_adult <- RA_adult %>%
  left_join(
    HHroster %>% select(`_uuid`, disability, hhroster_memberID), # Select relevant columns from HHroster
    by = c("_uuid" = "_uuid", "selected_adultap" = "hhroster_memberID") # Match on _uuid and HHroster position
  ) 
RA_adult <- RA_adult %>%
  rename(disability_RA = disability) #Rename to disability_RA

#5. Gender ----

RA_adult <- RA_adult %>%
  rename(HH_02_RA = `HH_02_selected`)

## Randomly Selected Women (RA_woman dataset) ----

#1. Age ----

RA_woman$agerandomwoman <- as.numeric(as.character(RA_woman$agerandomwoman))

RA_woman$RW_HH_04_cat2 <- cut(RA_woman$agerandomwoman, breaks = c(-1, 17, Inf), 
                              labels = c("0-17", "18-60+"))

# 4 categories 
RA_woman$RW_HH_04_cat4 <- cut(RA_woman$agerandomwoman, breaks = c(-1, 4, 17, 59, Inf), 
                              labels = c("0-4", "5-17", "18-59", "60+"))

#2. Population Group  ----

RA_woman <- RA_woman %>%
  left_join(
    main %>% select(`_uuid`, Intro_07), # Select relevant columns from main dataset
    by = c("_uuid" = "_uuid") # Match on _uuid 
  ) 


#3. Country of origin ----

RA_woman <- RA_woman %>%
  left_join(
    HHroster %>% select(`_uuid`, COO, hhroster_memberID), # Select relevant columns from HHroster
    by = c("_uuid" = "_uuid", "selected_woman" = "hhroster_memberID") # Match on _uuid and HHroster position
  ) 
RA_woman <- RA_woman %>%
  rename(COO_RW = COO) #Rename to COO_RW

#4. Disability ----
RA_woman <- RA_woman %>%
  left_join(
    HHroster %>% select(`_uuid`, disability, hhroster_memberID), # Select relevant columns from HHroster
    by = c("_uuid" = "_uuid", "selected_woman" = "hhroster_memberID") # Match on _uuid and HHroster position
  ) 
RA_woman <- RA_woman %>%
  rename(disability_RW = disability) #Rename to disability_RW


## Randomly Selected caregiver (RA_caregiver dataset) ----

#1. Population Group ----

RA_caregiver <- RA_caregiver %>%
  left_join(
    main %>% select(`_uuid`, Intro_07), # Select relevant columns from main dataset
    by = c("_uuid" = "_uuid") # Match on _uuid 
  ) 

#2. Disability ----
RA_caregiver <- RA_caregiver %>%
  left_join(
    HHroster %>% select(`_uuid`, disability, hhroster_memberID), # Select relevant columns from HHroster
    by = c("_uuid" = "_uuid", "finalcaregiverPOSITION" = "hhroster_memberID") # Match on _uuid and HHroster position
  ) 
RA_caregiver <- RA_caregiver %>%
  rename(disability_RC = disability) #Rename to disability_RC

#3. Country of Origin ----

RA_caregiver <- RA_caregiver %>%
  left_join(
    HHroster %>% select(`_uuid`, COO, hhroster_memberID), # Select relevant columns from HHroster
    by = c("_uuid" = "_uuid", "finalcaregiverPOSITION" = "hhroster_memberID") # Match on _uuid and HHroster position
  ) 
RA_caregiver <- RA_caregiver %>%
  rename(COO_RC = COO) #Rename to COO_RC

#4. Gender  ----
RA_caregiver <- RA_caregiver %>%
  rename(HH_02_RC = `finalcaregiverSEX`)

#5. Age ----
RA_caregiver <- RA_caregiver %>%
  rename(HH_04_RC = `finalcaregiverAGE`)



#Crowding index

####Calculate crowding index - overcrowded when more than 3 persons share one room to sleep

table(main$HH14) ##How many separate structures or buildings do the members of your household occupy? 
table(main$HHmembersize)

main <- main %>%
  mutate(crowding=HHmembersize/HH14
  ) %>%
  mutate(crowding_cat=case_when( ##if crowding <= 3, not overcrowded 
    crowding <= 3 ~ 1, TRUE ~ 2)
  )

table(main$crowding_cat)

#Recode Intro_07 to include ASY into Refugees. 


HHroster <- HHroster %>%
  mutate(Intro_07 = if_else(Intro_07 == "2", "1", Intro_07))

main <- main %>%
  mutate(Intro_07 = if_else(Intro_07 == "2", "1", Intro_07))

RA_adult <- RA_adult %>%
  mutate(Intro_07 = if_else(Intro_07 == "2", "1", Intro_07))

RA_woman <- RA_woman %>%
  mutate(Intro_07 = if_else(Intro_07 == "2", "1", Intro_07))

RA_caregiver <- RA_caregiver %>%
  mutate(Intro_07 = if_else(Intro_07 == "2", "1", Intro_07))

#Clean Intro_07 and check refugees and former refugees

main <- main %>%
  mutate(
    Intro_07_new = case_when(
      # Rule 1: stratum contains "former" → 4
      grepl("former", stratum, ignore.case = TRUE) ~ "4" ,
      
      # Rule 2: stratum contains "refugees" but not "former" → 1
      grepl("refugees", stratum, ignore.case = TRUE) &
        !grepl("former", stratum, ignore.case = TRUE) ~ "1",
      
      # Rule 3: stratum contains "hosts" → 3
      grepl("hosts", stratum, ignore.case = TRUE) ~ "3",
      
      # Otherwise keep original
      TRUE ~ Intro_07
    ),
    changed = Intro_07_new != Intro_07
  ) %>%
  mutate(Intro_07 = Intro_07_new) %>%
  select(-Intro_07_new)




#Recode Intro_09 

main <- main %>%
  mutate(
    Intro_09 = as.character(Intro_09),
    
    Intro_09 = case_when(
      # 1. If Intro_03a_NUTS1 = ZM105 → 3 (if 1 or 2, change to 3)
      Intro_03a_NUTS1 == "ZM105" & Intro_09 %in% c("1", "2") ~ "3",
      
      # 2. If Intro_03c_NUTS3 = ZM1040101 or ZM108010 → 1 (if 2 or 3, change to 1)
      Intro_03c_NUTS3 %in% c("ZM1040101", "ZM108010") & Intro_09 %in% c("2", "3") ~ "1",
      
      # 3. If Intro_03c_NUTS3 = ZM1100022 and Hhmove = 1 → 1
      Intro_03c_NUTS3 == "ZM1100022" & Hhmove == "1" ~ "1",
      
      # 4. If Intro_03c_NUTS3 = ZM1100022 and Hhmove = 2 and Intro_03a_NUTS1_move == ZM105 → 3
      Intro_03c_NUTS3 == "ZM1100022" & Hhmove == "2" & Intro_03a_NUTS1_move == "ZM105" ~ "3",
      
      # 5. If Intro_03c_NUTS3 = ZM1100022 and Hhmove = 2 and Intro_03a_NUTS1_move != ZM105 → 1
      Intro_03c_NUTS3 == "ZM1100022" & Hhmove == "2" &
        (is.na(Intro_03a_NUTS1_move) | Intro_03a_NUTS1_move != "ZM105") ~ "1",
      
      # otherwise keep the original value
      TRUE ~ Intro_09
    ),
    
    # ensure only 1 or 3 remain
    Intro_09 = if_else(Intro_09 == "2", "1", Intro_09),
    Intro_09 = factor(Intro_09, levels = c("1", "3"))
  )

#Labels 
#Store and drop all labels from all datasets, and re-label key variables 
library(labelled)

# ---- Store VALUE labels (env) & strip VALUE labels (keep var labels) ----

# HHroster
HHroster_val_labels <- val_labels(HHroster)
HHroster <- HHroster %>% mutate(across(everything(), zap_labels))

# main
main_val_labels <- val_labels(main)
main <- main %>% mutate(across(everything(), zap_labels))

# RA_adult
RA_adult_val_labels <- val_labels(RA_adult)
RA_adult <- RA_adult %>% mutate(across(everything(), zap_labels))

# RA_woman
RA_woman_val_labels <- val_labels(RA_woman)
RA_woman <- RA_woman %>% mutate(across(everything(), zap_labels))

# RA_caregiver
RA_caregiver_val_labels <- val_labels(RA_caregiver)
RA_caregiver <- RA_caregiver %>% mutate(across(everything(), zap_labels))

# Optional: also keep a single combined object in env (handy to save later)
value_labels_all <- list(
  HHroster = HHroster_val_labels,
  main = main_val_labels,
  RA_adult = RA_adult_val_labels,
  RA_woman = RA_woman_val_labels,
  RA_caregiver = RA_caregiver_val_labels
)

saveRDS(value_labels_all, "value_labels_all.rds")

# ADD LABELS 

#### Intro_07

popgroup_labels <- c(
  "1" = "Refugees",
  "3" = "Host Community",
  "4" = "Former Refugees"
)

HHroster <- HHroster %>%
  mutate(Intro_07 = recode_factor(Intro_07, !!!popgroup_labels))

main <- main %>%
  mutate(Intro_07 = recode_factor(Intro_07, !!!popgroup_labels))

RA_adult <- RA_adult %>%
  mutate(Intro_07 = recode_factor(Intro_07, !!!popgroup_labels))

RA_woman <- RA_woman %>%
  mutate(Intro_07 = recode_factor(Intro_07, !!!popgroup_labels))

RA_caregiver <- RA_caregiver %>%
  mutate(Intro_07 = recode_factor(Intro_07, !!!popgroup_labels))


#### Define labels for gender
gender_labels <- c(
  "1" = "Male",
  "2" = "Female"
)
# Apply labels to all gender variables in one block
HHroster <- HHroster %>%
  mutate(HH_02 = recode_factor(HH_02, !!!gender_labels))

main <- main %>%
  mutate(HH_02_HoH = recode_factor(HH_02_HoH, !!!gender_labels))

RA_adult <- RA_adult %>%
  mutate(HH_02_RA = recode_factor(HH_02_RA, !!!gender_labels))

RA_caregiver <- RA_caregiver %>%
  mutate(HH_02_RC = recode_factor(HH_02_RC, !!!gender_labels))

# Define labels for disability
disability_labels <- c(
  "1" = "Disabled",
  "0" = "Non-Disabled"
)

HHroster <- HHroster %>%
  mutate(disability = recode_factor(disability, !!!disability_labels))

main <- main %>%
  mutate(disability_HoH = recode_factor(disability_HoH, !!!disability_labels))

RA_adult <- RA_adult %>%
  mutate(disability_RA = recode_factor(disability_RA, !!!disability_labels))

RA_woman <- RA_woman %>%
  mutate(disability_RW = recode_factor(disability_RW, !!!disability_labels))

RA_caregiver <- RA_caregiver %>%
  mutate(disability_RC = recode_factor(disability_RC, !!!disability_labels))

table(RA_woman$disability_RW)
table(HHroster$disability)
table(main$disability)

###Define labels for Intro_09

intro09_labels <- c(
  "1"= "Rural",
  "3"= "Urban"
)

main <- main %>%
  mutate(Intro_09 = recode_factor(Intro_09, !!!intro09_labels))

table(main$Intro_09)

###Add additional variables for some indicators


#Variables needed for education indicators - this should be adapted for each country as the age and the education levels will change
#Step 1: Create a variable assessing completion of primary school
#For those currently in school 

HHroster <- HHroster %>%
  mutate(primary_complete_cur = case_when(
    HH_Educ02a == "1" & HH_Educ03 %in% 8:24 ~ 1, #Currently enrolled in secondary school or higher, therefore primary is completed. 
    TRUE ~ NA_real_))

HHroster <- HHroster %>%
  mutate(primary_complete_past = case_when(
    HH_Educ17 == "1" & HH_Educ18 %in% 7:24 ~ 1, #Primary education completed in Zambia
    HH_Educ17 == "2" & HH_Educ17_other == "DRC" & HH_Educ18 %in% 6:19 ~ 1, #Primary education completed in DRC
    HH_Educ17 == "2" & HH_Educ17_other == "BDI" & HH_Educ18 %in% 6:22 ~ 1, #Primary education completed in Burundi
    HH_Educ17 == "2" & HH_Educ17_other == "RWA" & HH_Educ18 %in% 6:21 ~ 1, #Primary education completed in Rwanda
    HH_Educ17 == "2" & HH_Educ17_other == "SOM" & HH_Educ18 %in% 8:21 ~ 1, #Primary education completed in Somalia
    HH_Educ17 == "2" & educ_code == "OTHER" & HH_Educ18 %in% 7:24 ~ 1, #Primary education completed somewhere else
    HH_Educ17 %in% c(98,99) & HH_Educ18 %in% 7:24 ~ 1, 
    TRUE ~ NA_real_))

HHroster <- HHroster %>%
  mutate(primary_complete = case_when(
    primary_complete_cur == 1 | primary_complete_past == 1 ~ 1, #Primary completed for those currently is school (sec or higer) or those who indicated that they have completed primary school in the past
    TRUE ~ NA_real_))

#Step 2: Create a variable assessing completion of lower secondary school
#For those currently in school 
HHroster <- HHroster %>%
  mutate(lowseco_complete_cur = case_when(
    HH_Educ02a == "1" & HH_Educ03 %in% 10:24 ~ 1, #Currently enrolled in higher secondary school or higher, therefore lower secondary is completed. 
    TRUE ~ NA_real_))

HHroster <- HHroster %>%
  mutate(lowersec_complete_past = case_when(
    HH_Educ17 == "1" & HH_Educ18 %in% 9:24 ~ 1, #Lower secondary education completed in Zambia
    HH_Educ17 == "2" & HH_Educ17_other == "DRC" & HH_Educ18 %in% 8:19 ~ 1, #Lower secondary education completed in DRC
    HH_Educ17 == "2" & HH_Educ17_other == "BDI" & HH_Educ18 %in% 10:22 ~ 1, #Lower secondary education completed in Burundi
    HH_Educ17 == "2" & HH_Educ17_other == "RWA" & HH_Educ18 %in% 9:21 ~ 1, #Lower secondary education completed in Rwanda
    HH_Educ17 == "2" & HH_Educ17_other == "SOM" & HH_Educ18 %in% 12:21 ~ 1, #Lower secondary education completed in Somalia
    HH_Educ17 == "2" & educ_code == "OTHER" & HH_Educ18 %in% 9:24 ~ 1, #Lower secondary education completed somewhere else
    HH_Educ17 %in% c(98,99) & HH_Educ18 %in% 9:24 ~ 1, 
    TRUE ~ NA_real_))

HHroster <- HHroster %>%
  mutate(lowersec_complete = case_when(
    lowersec_complete_past == 1 | lowseco_complete_cur == 1 ~ 1, #Lower Secondary completed for those currently is school (higher sec or higer) or those who indicated that they have completed lower secondary school in the past
    TRUE ~ NA_real_))

#Step 3: Create a variable assessing completion of upper secondary school
#For those currently in school 
HHroster <- HHroster %>%
  mutate(upperseco_complete_cur = case_when(
    HH_Educ02a == "1" & HH_Educ03 %in% 13:24 ~ 1, #Currently enrolled in tertiary education, therefore higher secondary is completed. 
    TRUE ~ NA_real_))

HHroster <- HHroster %>%
  mutate(uppersec_complete_past = case_when(
    HH_Educ17 == "1" & HH_Educ18 %in% 12:24 ~ 1, #Upper secondary education completed in Zambia
    HH_Educ17 == "2" & HH_Educ17_other == "DRC" & HH_Educ18 %in% 12:19 ~ 1, #Upper secondary education completed in DRC
    HH_Educ17 == "2" & HH_Educ17_other == "BDI" & HH_Educ18 %in% 13:22 ~ 1, #Upper secondary education completed in Burundi
    HH_Educ17 == "2" & HH_Educ17_other == "RWA" & HH_Educ18 %in% 12:21 ~ 1, #Upper secondary education completed in Rwanda
    HH_Educ17 == "2" & HH_Educ17_other == "SOM" & HH_Educ18 %in% 12:21 ~ 1, #Upper secondary education completed in Somalia
    HH_Educ17 == "2" & educ_code == "OTHER" & HH_Educ18 %in% 12:24 ~ 1, #Upper secondary education completed somewhere else
    HH_Educ17 %in% c(98,99) & HH_Educ18 %in% 12:24 ~ 1, 
    TRUE ~ NA_real_))

HHroster <- HHroster %>%
  mutate(uppersec_complete = case_when(
    upperseco_complete_cur == 1 | uppersec_complete_past == 1 ~ 1, #Upper Secondary completed for those currently is tertiary school or those who indicated that they have completed upper secondary school in the past
    TRUE ~ NA_real_))

table(HHroster$primary_complete)
table(HHroster$lowersec_complete)
table(HHroster$uppersec_complete)


#Extract needed variables related to education from the roster to the RA_adult 
HHroster_selected <- HHroster %>%
  select('_uuid', hhroster_memberID, HH_Educ02a, HH_Educ07, HH_Educ14)
HHroster_selected <- HHroster_selected %>%
  rename(
    HH_Educ02a_RA = HH_Educ02a,
    HH_Educ07_RA = HH_Educ07,
    HH_Educ14_RA = HH_Educ14
  )
RA_adult <- RA_adult %>%
  left_join(HHroster_selected, by = c('_uuid', "selected_adultap" = "hhroster_memberID"))

#Rename anthropometric variables to facilitate the use of anthro packages in the calculation of Z-scores 
RA_caregiver <- RA_caregiver %>%
  mutate(AN8 = case_when(
    AN8 == 1 ~ "H",  # Standing height
    AN8 == 2 ~ "L"   # Recumbent length
  ))
RA_caregiver$AN7 <- as.numeric(RA_caregiver$AN7)
RA_caregiver$AN6 <- as.numeric(RA_caregiver$AN6)
RA_caregiver$AN8 <- as.character(RA_caregiver$AN8)


#Reshaping data for the calculation of **Proportion of total adult population with secure tenure 
#rights to land, (a) with legally recognized documentation, and (b) who perceive their rights to land as secure, by sex and type of tenure**


# Create a subset with `uuid` and all variables starting with `Land`
main_subset_land <- main %>%
  select(`_uuid`, contains("Land12b", ignore.case = FALSE))

main_subset_land <- main_subset_land %>%
  mutate(across(contains("Land12b"), ~ as.numeric(haven::zap_labels(.))))

main_long <- main_subset_land %>%
  pivot_longer(
    cols = contains("Land12b"),
    names_to = "variable",
    values_to = "ownership"
  )

main_long <- main_long %>%
  filter(!grepl("^Plot\\d+_Land12b_groupnote$", variable))


#Label various documents 
main_long <- main_long %>%
  mutate(
    document_label = case_when(
      str_detect(variable, "Land12b_A")  ~ "Title deed",
      str_detect(variable, "Land12b_B")  ~ "Chief certificate",
      str_detect(variable, "Land12b_C")  ~ "Certificate of customary ownership",
      str_detect(variable, "Land12b_D")  ~ "Certificate of hereditary ownership",
      str_detect(variable, "Land12b_E")  ~ "Rental contract registered",
      str_detect(variable, "Land12b_F")  ~ "Rental contract unregistered",
      str_detect(variable, "Land12b_G")  ~ "Lease registered",
      str_detect(variable, "Land12b_H")  ~ "Note on a sharecropping arrangement",
      str_detect(variable, "Land12b_I")  ~ "Letter of offer",
      str_detect(variable, "Land12b_J")  ~ "Survey plan",
      str_detect(variable, "Land12b_K")  ~ "Invitation to treat",
      str_detect(variable, "Land12b_OT") ~ "Other documents",
      TRUE ~ NA_character_
    )
  )

#Keep only rows where someone has a document, just for the time being for easy manuvering 
main_long <- main_long %>%
  filter(!is.na(ownership) & ownership != "")

main_long_expanded <- main_long %>%
  # Split space- or comma-separated IDs into separate rows
  separate_rows(ownership, sep = "\\s+") %>% 
  mutate(hhroster_memberID = as.numeric(ownership)) %>%
  select(-ownership)

# Step 2: Create binary column for the document type
main_docs_binary <- main_long_expanded %>%
  mutate(has_doc = 1) %>%
  select('_uuid', hhroster_memberID, document_label, has_doc) %>%
  filter(hhroster_memberID >= 1 & hhroster_memberID <= 50) %>%       # keep only 1–50
  distinct() %>%
  pivot_wider(
    names_from = document_label,
    values_from = has_doc,
    values_fill = 0
  )

# Define a mapping: label -> new column name
document_rename_mapping <- c(
  "Title deed"                          = "Land12b_A",
  "Chief certificate"                   = "Land12b_B",
  "Certificate of customary ownership"  = "Land12b_C",
  "Certificate of hereditary ownership" = "Land12b_D",
  "Rental contract registered"          = "Land12b_E",
  "Rental contract unregistered"        = "Land12b_F",
  "Lease registered"                    = "Land12b_G",
  "Note on a sharecropping arrangement" = "Land12b_H",
  "Letter of offer"                     = "Land12b_I",
  "Survey plan"                          = "Land12b_J",
  "Invitation to treat"                  = "Land12b_K",
  "Other documents"                      = "Land12b_OT"
)

# Keep only mappings for columns that exist in the dataset
existing_mapping <- document_rename_mapping[
  names(document_rename_mapping) %in% names(main_docs_binary)
]

# Rename columns
main_docs_binary <- main_docs_binary %>%
  rename_with(~ existing_mapping[.x], .cols = names(existing_mapping))

# Define column name -> label mapping
label_mapping <- setNames(names(document_rename_mapping), document_rename_mapping)

# Keep only labels for existing columns
existing_labels <- label_mapping[names(label_mapping) %in% names(main_docs_binary)]

# Apply labels
for (nm in names(existing_labels)) {
  var_label(main_docs_binary[[nm]]) <- existing_labels[[nm]]
}

# Merge `main_docs_binary` with `HHroster`
HHroster <- HHroster %>%
  left_join(main_docs_binary, by = c('_uuid', "hhroster_memberID"))

# Adding an indicator at household level - indicating if any member has a document for any plot 
# Aggregate at household level (uuid)
main_docs <- main_docs_binary %>%
  group_by(`_uuid`) %>%
  summarise(
    across(
      starts_with("Land12b"),
      ~ ifelse(all(is.na(.x)), 0, max(.x, na.rm = TRUE)),
      .names = "{.col}"
    ),
    .groups = "drop"
  )

# Join aggregated household-level indicators back to main
main <- main %>%
  left_join(main_docs, by = "_uuid")
 
###HERE TO CHECK IF THIS SHOULD APPLY ONLY TO HOUSEHOLDS THAT HAVE AT LEAST 1 PLOT. FOR EXAMPLE IF A HOUSEHOLD HAS 0 PLOTS (meaning they live where they live undocumented) SHOULD THEY BE ASSIGNED
###A 0 IF NO ONE HAS ANY DOCUMENTS. 

# Calculating old-age household members and looking at household that has at least one old_age household member
# Count number of old age people (60+) in each household
##Identify women who has a child below 2 - less than 24 months

#Identify household which have at least 1 woman who gave birth in the last 12 months 
# Step 1: Identify children <= 1 year whose mother is reported to live in the household (HH_18 == 1)

children_with_mothers <- HHroster %>%
  filter(agetouse < 2, HH_18 == 1, member == 1) %>%
  mutate(mother_rosterposition = as.character(HH_19)) %>%
  select('_uuid', child_rosterposition = hhroster_memberID, mother_rosterposition)

# Step 2: Check if the mother is a household member in the same household
valid_mothers <- HHroster %>%
  filter(member == 1) %>%
  mutate(hhroster_memberID = as.character(hhroster_memberID)) %>%
  select('_uuid', hhroster_memberID)

# Step 3: Join and check if mother is in household
mothers_with_child <- children_with_mothers %>%
  inner_join(
    valid_mothers,
    by = c("_uuid", "mother_rosterposition" = "hhroster_memberID")
  ) %>%
  group_by(`_uuid`) %>%
  summarise(
    num_mothers_with_child = n_distinct(mother_rosterposition),
    .groups = "drop"
  )

# Step 4: Merge into main dataset
main <- main %>%
  left_join(mothers_with_child, by = '_uuid') %>%
  mutate(
    num_mothers_with_child = replace_na(num_mothers_with_child, 0),
    has_mother_with_child = if_else(num_mothers_with_child >= 1, 1, 0)
  )

#Create a variable for adult population per household 

adult_pop <- HHroster %>%
  group_by(`_uuid`) %>%
  summarise(adult_pop = sum(agetouse >= 18, na.rm = TRUE)) 

# Add the count to the main dataset
main <- main %>%
  left_join(adult_pop, by = "_uuid") %>%
  mutate(adult_pop = replace_na(adult_pop, 0))


##create an age for elderly population

main <- main %>%
  left_join(
    HHroster %>%
      group_by(`_uuid`) %>%
      summarise(elderly_pop = sum(agetouse >= 60, na.rm = TRUE), .groups = "drop"), #in ZAM 60
    by = "_uuid"
  ) %>%
  mutate(has_old_age = if_else(elderly_pop >= 1, 1, 0))

# Summary statistics
summary(main$elderly_pop) # Number of old-age household members
table(main$has_old_age) # Households with at least one old-age member

#Create a variable for  HH members with disability per household 
# Count number of people with disabilities per household


table(HHroster$disability)

HHroster <- HHroster %>%
  mutate(disability = if_else(disability == "Disabled", 1, 0))

disability_count <- HHroster %>%
  group_by(`_uuid`) %>%
  summarise(disability_count = sum(disability, na.rm = TRUE))

# Merge with main dataset
main <- main %>%
  left_join(disability_count, by = "_uuid") %>%
  mutate(disability_count = replace_na(disability_count, 0))

# Create indicator for households with at least one person with disability
main <- main %>%
  mutate(has_disability = if_else(disability_count >= 1, 1, 0))

table(main$has_disability)

table(HHroster$disability)

HHroster <- HHroster %>%
  mutate(disability = recode_factor(disability, !!!disability_labels))

#IDENTITIY DOCUMENTS 
#BIRTH CERTIFICATE 
#Legal_10 - Do you have a birth certificate?
#Legal_11 - Among your household members do all of them, only some of them or none of them have a birth certificate? 
#Legal_12 - Please tell me the names of all household members who have a birth certificate. 


ind_cols <- grep("^Legal_12_\\d+$", names(main), value = TRUE)

# 1) Long format: which positions have a birth cert (from dummies)
sel_long <- main %>%
  select(`_uuid`, all_of(ind_cols), Legal_10, Legal_11, Legal_12) %>%
  pivot_longer(cols = all_of(ind_cols),
               names_to = "var", values_to = "val") %>%
  mutate(hhroster_memberID = as.integer(str_extract(var, "\\d+$")),
         sel = if_else(val == 1, 1L, 0L)) %>%
  select(`_uuid`, hhroster_memberID, sel, Legal_10, Legal_11, Legal_12)

# 2) Check if HOH answered that he/she has a birth certificate but did not choose his/hers name in Legal_12
sel_long <- sel_long %>%
  mutate(
    sel = case_when(
      sel == 1 ~ 1,                               # keep if already selected
      Legal_10 == 1 & hhroster_memberID == 1 ~ 1, # override HOH if missing
      is.na(Legal_10) ~ NA_integer_, 
      TRUE ~ 0
    )
  )

sel_long <- sel_long %>%
group_by(`_uuid`) %>%
  mutate(
    sel = case_when(
      Legal_11 == 1 ~ 1,    # all have
      Legal_11 %in% c(3,98,99) ~ 0,  # none have
      TRUE ~ sel             # some have (2) -> keep as is
    )
  ) %>%
  ungroup() %>%
  select(`_uuid`, hhroster_memberID, Legal_birth_cert = sel,Legal_10, Legal_11)

# 3) Merge into HHroster 

HHroster <- HHroster %>%
  left_join(sel_long, by = c("_uuid", "hhroster_memberID"))

#REFUGEE CERTIFICATE
# Legal_25 - Do you have a Refugee Certificate?
# Legal_26 - Do all members of your household, only some of them or none of them have a Refugee Certificate?
# Legal_27 - Please tell me the names of all household members who have a Refugee Certificate.

# REFUGEE CERTIFICATE
ind_cols27 <- grep("^Legal_27_\\d+$", names(main), value = TRUE)

# 1) Long format: which positions have a refugee certificate (from dummies)
sel_long_ref <- main %>%
  select(`_uuid`, all_of(ind_cols27), Legal_25, Legal_26) %>%
  pivot_longer(cols = all_of(ind_cols27),
               names_to = "var", values_to = "val") %>%
  mutate(
    hhroster_memberID = as.integer(str_extract(var, "\\d+$")),
    sel = if_else(val == 1, 1, 0)
  ) %>%
  select(`_uuid`, hhroster_memberID, sel, Legal_25, Legal_26)

# 2) HOH override: if Legal_25 == 1 but HOH (ID=1) not selected, set to 1
sel_long_ref <- sel_long_ref %>%
  mutate(
    sel = case_when(
      sel == 1 ~ 1,                                        # keep if already selected
      Legal_25 == 1 & hhroster_memberID == 1 ~ 1,          # HOH override
      is.na(Legal_25) ~ NA_integer_,                       # if unanswered, keep NA
      TRUE ~ 0
    )
  )


# 3) Household-level override using Legal_26
sel_long_ref <- sel_long_ref %>%
  group_by(`_uuid`) %>%
  mutate(
    sel = case_when(
      Legal_26 == 1 ~ 1,                     # all have
      Legal_26 %in% c(3, 98, 99) ~ 0,        # none have
      TRUE ~ sel                             # some have (2) -> keep as is
    )
  ) %>%
  ungroup() %>%
  select(`_uuid`, hhroster_memberID,
         Legal_refugee_cert = sel, Legal_25, Legal_26)

# 4) Merge into HHroster
HHroster <- HHroster %>%
  left_join(sel_long_ref, by = c("_uuid", "hhroster_memberID"))

#FUNCTION 
make_cert_long <- function(main, prefix, q_self, q_all, outname) {
  # prefix = "Legal_27" (dummy cols prefix, e.g. Legal_27_1, Legal_27_2 ...)
  # q_self = "Legal_25" (HOH self question)
  # q_all  = "Legal_26" (all/some/none question)
  # outname = "Legal_refugee_cert" (final output var name)
  
  ind_cols <- grep(paste0("^", prefix, "_\\d+$"), names(main), value = TRUE)
  
  sel_long <- main %>%
    select(`_uuid`, all_of(ind_cols), !!sym(q_self), !!sym(q_all)) %>%
    pivot_longer(cols = all_of(ind_cols),
                 names_to = "var", values_to = "val") %>%
    mutate(
      hhroster_memberID = as.integer(str_extract(var, "\\d+$")),
      sel = if_else(val == 1, 1, 0)
    ) %>%
    # HOH override
    mutate(
      sel = case_when(
        sel == 1 ~ 1,
        !!sym(q_self) == 1 & hhroster_memberID == 1 ~ 1,
        is.na(!!sym(q_self)) ~ NA_integer_,
        TRUE ~ 0
      )
    ) %>%
    # Household override
    group_by(`_uuid`) %>%
    mutate(
      sel = case_when(
        !!sym(q_all) == 1 ~ 1,
        !!sym(q_all) %in% c(3, 98, 99) ~ 0,
        TRUE ~ sel
      )
    ) %>%
    ungroup() %>%
    select(`_uuid`, hhroster_memberID,
           !!outname := sel, !!sym(q_self), !!sym(q_all))
  
  return(sel_long)
}


#WHITE REFUGEE CARD
# Legal_13a - Do you have a white Refugee ID card?
# Legal_14a - Do all members of your household, only some of them or none of them have a white Refugee ID card?
# Legal_15a - Please tell me the names of all household members who have a white Refugee ID card

# White refugee card
sel_long_white <- make_cert_long(main,
                                 prefix = "Legal_15a",
                                 q_self = "Legal_13a",
                                 q_all  = "Legal_14a",
                                 outname = "Legal_white_card")

HHroster <- HHroster %>%
  left_join(sel_long_white, by = c("_uuid", "hhroster_memberID")) 


# ORANGE REFUGEE CARD
# Legal_13b - Do you have an orange Refugee ID card?
# Legal_14b - Do all members of your household, only some of them or none of them have an orange Refugee ID card?
# Legal_15b - Please tell me the names of all household members who have an orange Refugee ID card

sel_long_orange <- make_cert_long(main,
                                 prefix = "Legal_15b",
                                 q_self = "Legal_13b",
                                 q_all  = "Legal_14b",
                                 outname = "Legal_orange_card")

HHroster <- HHroster %>%
  left_join(sel_long_orange, by = c("_uuid", "hhroster_memberID")) 

# ASYLUM SEEKERS CERTIFICATE
# Legal_35 - Do you have an Asylum seekers certificate? 
# Legal_36 - Do all members of your household, only some of them or none of them have a Asylum seekers certificate?
# Legal_37 - Please tell me the names of all household members who have a Asylum seekers certificate.

sel_long_asylum <- make_cert_long(main,
                                  prefix  = "Legal_37",
                                  q_self  = "Legal_35",
                                  q_all   = "Legal_36",
                                  outname = "Legal_asylum_cert")

HHroster <- HHroster %>%
  left_join(sel_long_asylum, by = c("_uuid", "hhroster_memberID")) 

# PASSPORT
# Legal_16 - Do you have a passport?
# Legal_17 - Do all members of your household, only some of them or none of them have a passport?
# Legal_18 - Please tell me the names of all household members who have a passport.
sel_long_passport <- make_cert_long(main,
                                    prefix  = "Legal_18",
                                    q_self  = "Legal_16",
                                    q_all   = "Legal_17",
                                    outname = "Legal_passport")

HHroster <- HHroster %>%
  left_join(sel_long_passport , by = c("_uuid", "hhroster_memberID")) 

# ALIEN REGISTRATION CARD
# Legal_13c - Do you have a Alien Registration Card? 
# Legal_14c - Do all members of your household, only some of them or none of them have a Alien Registration Card?
# Legal_15c - Please tell me the names of all household members who have a Alien Registration Card?

sel_long_alien <- make_cert_long(main,
                                 prefix  = "Legal_15c",
                                 q_self  = "Legal_13c",
                                 q_all   = "Legal_14c",
                                 outname = "Legal_alien_card")

HHroster <- HHroster %>%
  left_join(sel_long_alien, by = c("_uuid", "hhroster_memberID")) 

# REGISTRATION CERTIFICATE
# Legal_38 - Do you have a Registration Certificate? 
# Legal_39 - Do all members of your household, only some of them or none of them have a Registration Certificate?
# Legal_40 - Please tell me the names of all household members who have a Registration Certificate?

sel_long_regcert <- make_cert_long(main,
                                   prefix  = "Legal_40",
                                   q_self  = "Legal_38",
                                   q_all   = "Legal_39",
                                   outname = "Legal_registration_cert")

HHroster <- HHroster %>%
  left_join(sel_long_regcert, by = c("_uuid", "hhroster_memberID"))


# TEMPORARY RESIDENCY PERMIT
# Legal_19a - Do you have a temporary residency permit? 
# Legal_20a - Do all members of your household, only some of them or none of them have a temporary residency permit?
# Legal_21a - Please tell me the names of all household members who have a temporary residency permit?
sel_long_tempres <- make_cert_long(main,
                                   prefix  = "Legal_21a",
                                   q_self  = "Legal_19a",
                                   q_all   = "Legal_20a",
                                   outname = "Legal_temp_residency")
HHroster <- HHroster %>%
  left_join(sel_long_tempres, by = c("_uuid", "hhroster_memberID"))

# PERMANENT RESIDENCY PERMIT
# Legal_19b - Do you have a permanent residency permit? 
# Legal_20b - Do all members of your household, only some of them or none of them have a permanent residency permit?
# Legal_21b - Please tell me the names of all household members who have a permanent residency permit?
sel_long_permres <- make_cert_long(main,
                                   prefix  = "Legal_21b",
                                   q_self  = "Legal_19b",
                                   q_all   = "Legal_20b",
                                   outname = "Legal_perm_residency")

HHroster <- HHroster %>%
  left_join(sel_long_permres, by = c("_uuid", "hhroster_memberID"))

# VALID MOBILITY PASS
# Legal_41 - Do you currently have a valid mobility pass? 
# Legal_42 - Among your household members, do all of them, only some of them or none of them have a valid mobility pass?
# Legal_43 - Please tell me the names of all household members who have a valid mobility pass?
sel_long_mobpass <- make_cert_long(main,
                                   prefix  = "Legal_43",
                                   q_self  = "Legal_41",
                                   q_all   = "Legal_42",
                                   outname = "Legal_mobility_pass")

HHroster <- HHroster %>%
  left_join(sel_long_mobpass, by = c("_uuid", "hhroster_memberID"))
# EMPLOYMENT PERMIT 
# Legal_22 - Do you have an employment permit? 
# Legal_23 - Among your household members  do all of them, only some of them or none of them have an employment permit?
# Legal_24 - Please tell me the names of all household members who have an employment permit?
sel_long_employ <- make_cert_long(main,
                                  prefix  = "Legal_24",
                                  q_self  = "Legal_22",
                                  q_all   = "Legal_23",
                                  outname = "Legal_employment_permit")

HHroster <- HHroster %>%
  left_join(sel_long_employ, by = c("_uuid", "hhroster_memberID"))

# STUDY PERMIT
# Legal_44 - Do you have a study permit? 
# Legal_45 - Among your household members, do all of them, only some of them or none of them have a study permit?
# Legal_46 - Please tell me the names of all household members who have a study permit?
sel_long_study <- make_cert_long(main,
                                 prefix  = "Legal_46",
                                 q_self  = "Legal_44",
                                 q_all   = "Legal_45",
                                 outname = "Legal_study_permit")
HHroster <- HHroster %>%
  left_join(sel_long_study, by = c("_uuid", "hhroster_memberID"))

# INVESTORS PERMIT
# Legal_47 - Do you have a investors permit? 
# Legal_48 - Among your household members, do all of them, only some of them or none of them have a investors permit?
# Legal_49 - Please tell me the names of all household members who have a investors permit?
sel_long_invest <- make_cert_long(main,
                                  prefix  = "Legal_49",
                                  q_self  = "Legal_47",
                                  q_all   = "Legal_48",
                                  outname = "Legal_investors_permit")
HHroster <- HHroster %>%
  left_join(sel_long_invest, by = c("_uuid", "hhroster_memberID"))

# MARRIGE CERTIFCIATE 
# Legal_31 - Do you have a marriage certificate?
# Legal_32 - Among your household members who are married, do all of them, only some of them or none of them have a marriage certificate?
# Legal_33 - Please tell me the names of all household members who have a marriage certificate

sel_long_marriage <- make_cert_long(main,
                                    prefix  = "Legal_33",
                                    q_self  = "Legal_31",
                                    q_all   = "Legal_32",
                                    outname = "Legal_marriage_cert")
HHroster <- HHroster %>%
  left_join(sel_long_marriage, by = c("_uuid", "hhroster_memberID"))



### Add the weights 

##Import the dataset with weights

weights <- read_dta ("ZAM25_HH_Weights.dta")

##Add labels to the weights

sample_strata_labels <- c(
  "1" = "Lusaka Refugees",
  "2" = "Mantapala Refugees",
  "3" = "Mayukwayukwa Refugees",
  "4" = "Meheba Refugees",
  "5" = "Mayukwayukwa Former Refugees",
  "6" = "Meheba Former Refugees",
  "7" = "Mantapala Hosts",
  "8" = "Mayukwayukwa Hosts",
  "9" = "Meheba Hosts",
  "10" = "Out-settlement Refugees"
)

strat_order <- c(
  "Refugees total", 
  "Lusaka Refugees", 
  "Mantapala Refugees", 
  "Mayukwayukwa Refugees", 
  "Meheba Refugees", 
  "Out-settlement refugees", 
  "Mayukwayukwa Former Refugee", 
  "Meheba Former Refugees", 
  "Mantapala Hosts",
  "Mayukwayukwa Hosts", 
  "Meheba Hosts"
)

pop_labels <- c(
  "1" = "Refugees",
  "2" = "Former Refugees",
  "3" = "Hosts"
)

weights <- weights %>%
  mutate(pop_strat = recode_factor(pop_strat, !!!pop_labels))

weights <- weights %>%
  mutate(samp_strat = recode_factor(samp_strat, !!!sample_strata_labels))

var_label(weights) <- list(
  var_labels <- c(
    samp_strat                  = "Sampling Strata",
    wgh_strata_spec             = "Sampling Strata Specific Weights",
    pop_strat                   = "Population Strata",
    pop_str                     = "Population Structure",
    wgh_samp_pop_restr          = "National Population Weights",
    n_rand_resp                 = "Number of Eligible Random Respondents",
    wgh_strata_spec_resp        = "Sampling Strata Specific Weights Random Respondent",
    wgh_samp_pop_restr_resp     = "National Population Weights Random Respondent",
    n_rand_u5                   = "Number of Eligible Random Children",
    wgh_strata_spec_u5          = "Sampling Strata Specific Weights Random Child",
    wgh_samp_pop_restr_u5       = "National Population Weights Random Child",
    n_rand_w                    = "Number of Eligible Women",
    wgh_strata_spec_w           = "Sampling Strata Specific Weights Random Woman",
    wgh_samp_pop_restr_w        = "National Population Weights Random Woman"
  ))

###Merging

main <- main %>%
  left_join(weights %>% select(`_uuid`, wgh_strata_spec, pop_strat, wgh_samp_pop_restr, samp_strat,
                               pop_strat), by = "_uuid")

HHroster <- HHroster %>%
  left_join(weights %>% select(`_uuid`, wgh_strata_spec, pop_strat, wgh_samp_pop_restr, samp_strat,
                               pop_strat), by = "_uuid")

RA_adult <- RA_adult %>%
  left_join(weights %>% select(`_uuid`, wgh_strata_spec_resp, wgh_samp_pop_restr_resp, samp_strat,
                               pop_strat), by = "_uuid")

RA_woman <- RA_woman %>%
  left_join(weights %>% select(`_uuid`, wgh_strata_spec_w, wgh_samp_pop_restr_w, samp_strat,
                               pop_strat), by = "_uuid")

RA_caregiver <- RA_caregiver %>%
  left_join(weights %>% select(`_uuid`, wgh_strata_spec_u5, wgh_samp_pop_restr_u5, samp_strat,
                               pop_strat), by = "_uuid")
  

###Add header variables to other datasets ----

##Sample strata variable

###Location and urban/rural variables from main dataset


HHroster <- HHroster %>%
  left_join(main %>% select("_uuid", Intro_03a_NUTS1, Intro_03b_NUTS2,Intro_03c_NUTS3, Intro_09, 
                            start, end), by = "_uuid")

RA_adult <- RA_adult %>%
  left_join(main %>% select("_uuid", Intro_03a_NUTS1, Intro_03b_NUTS2,Intro_03c_NUTS3, Intro_09, 
                            start, end), by = "_uuid")

RA_woman <- RA_woman %>%
  left_join(main %>% select("_uuid", Intro_03a_NUTS1, Intro_03b_NUTS2,Intro_03c_NUTS3, Intro_09, 
                            start, end), by = "_uuid")

RA_caregiver <- RA_caregiver %>%
  left_join(main %>% select("_uuid", Intro_03a_NUTS1, Intro_03b_NUTS2,Intro_03c_NUTS3, Intro_09, 
                            start, end), by = "_uuid")


###Move headers
main <- main %>%
  select(
    start, end, Intro_07, Intro_03a_NUTS1, Intro_03b_NUTS2, Intro_03c_NUTS3,
    HH_04_HoH, HH_02_HoH, disability_HoH, COO_HoH, Intro_09, `_uuid`,
    wgh_strata_spec, wgh_samp_pop_restr, samp_strat, pop_strat,
    everything()
  )

HHroster <- HHroster %>%
  select(
    start, end, Intro_07, Intro_03a_NUTS1, Intro_03b_NUTS2, Intro_03c_NUTS3, agetouse,
    HH_04_cat2, HH_04_cat4, HH_02, disability, COO, Intro_09, `_uuid`,
    wgh_strata_spec, wgh_samp_pop_restr, samp_strat, pop_strat,
    everything()
  )

RA_adult <- RA_adult %>%
  select(
    start, end, Intro_07, Intro_03a_NUTS1, Intro_03b_NUTS2, Intro_03c_NUTS3,
    age_selected, HH_02_RA, disability_RA, COO_RA, Intro_09, `_uuid`,
    wgh_strata_spec_resp, wgh_samp_pop_restr_resp, samp_strat, pop_strat,
    everything()
  )

RA_woman <- RA_woman %>%
  select(
    start, end, Intro_07, Intro_03a_NUTS1, Intro_03b_NUTS2, Intro_03c_NUTS3,
    agerandomwoman, disability_RW, COO_RW, Intro_09, `_uuid`, 
    wgh_strata_spec_w, wgh_samp_pop_restr_w, samp_strat, pop_strat,
    everything()
  )

RA_caregiver <- RA_caregiver %>%
  select(
    start, end, Intro_07, Intro_03a_NUTS1, Intro_03b_NUTS2, Intro_03c_NUTS3,
    HH_04_RC, HH_02_RC, disability_RC, COO_RC, Intro_09, `_uuid`,
    wgh_strata_spec_u5, wgh_samp_pop_restr_u5, samp_strat, pop_strat,
    everything()
  )


#Drop all rows with missing values in weights - this is only for now to be able to run the analysis 

#main
# Count & store dropped cases
missing_wt_main <- main %>%
  filter(is.na(wgh_samp_pop_restr)) %>%
  select('_uuid', samp_strat)

main <- main %>%
  filter(!is.na(wgh_samp_pop_restr))

cat("Number of cases dropped due to missing weights:", nrow(missing_wt_main), "\n")
if (nrow(missing_wt_main) > 0) {
  cat("Dropped _uuid values:\n")
  print(missing_wt_main)
}

#HH_roster
missing_wt_HHroster <- HHroster %>%
  filter(is.na(wgh_samp_pop_restr)) %>%
  select('_uuid', samp_strat)

HHroster <- HHroster %>%
  filter(!is.na(wgh_samp_pop_restr))


#RA_adult 
# Count & store dropped cases
missing_wt_RA <- RA_adult %>%
  filter(is.na(wgh_samp_pop_restr_resp)) %>%
  select('_uuid', samp_strat)

RA_adult <- RA_adult %>%
  filter(!is.na(wgh_samp_pop_restr_resp))

#RA_woman
# Count & store dropped cases
missing_wt_Rw <- RA_woman %>%
  filter(is.na(wgh_samp_pop_restr_w)) %>%
  select('_uuid', samp_strat)

RA_woman <- RA_woman %>%
  filter(!is.na(wgh_samp_pop_restr_w))

#RA_caregiver
# Count & store dropped cases
missing_wt_RC <- RA_caregiver %>%
  filter(is.na(wgh_samp_pop_restr_u5)) %>%
  select('_uuid', samp_strat)

RA_caregiver <- RA_caregiver %>%
  filter(!is.na(wgh_samp_pop_restr_u5))

saveRDS(HHroster, "HHroster.rds")
saveRDS(main, "main.rds")
saveRDS(RA_adult,"RA_adult.rds")
saveRDS(RA_woman, "RA_woman.rds")
saveRDS(RA_caregiver, "RA_caregiver.rds")

saveRDS(HHroster,              
        "HHroster.rds")

saveRDS(main,          
        "main.rds")

saveRDS(RA_adult,          
        "RA_adult.rds")

saveRDS(RA_woman,      
        "RA_woman.rds")
saveRDS(RA_caregiver,      
        "RA_caregiver.rds")

# library(srvyr)
# 
# # Helper function for QA logging + survey design creation - ONLY FOR NOW SINCE WE HAVE MISSING WEIGHTS
# make_design <- function(data, weight_var, strata_var = samp_strat) {
#   missing_wt <- data %>%
#     filter(is.na({{ weight_var }})) %>%
#     select('_uuid', {{ strata_var }})
#   
#   cat("\nNumber of cases dropped due to missing", rlang::as_name(enquo(weight_var)), ":", nrow(missing_wt), "\n")
#   if (nrow(missing_wt) > 0) {
#     print(missing_wt)
#   }
#   
#   data %>%
#     filter(!is.na({{ weight_var }})) %>%
#     as_survey_design(
#       strata = {{ strata_var }},
#       weights = {{ weight_var }},
#       nest = TRUE
#     )
# }
# 
# # Household-level
# FDS_ZAM_2025_main <- make_design(main, wgh_samp_pop_restr)
# 
# # Adult respondent
# FDS_ZAM_2025_RA_adult <- make_design(RA_adult, wgh_samp_pop_restr_resp)
# 
# # Woman respondent
# FDS_ZAM_2025_RA_woman <- make_design(RA_woman, wgh_samp_pop_restr_w)
# 
# # Under-5 caregiver
# FDS_ZAM_2025_RA_caregiver <- make_design(RA_caregiver, wgh_samp_pop_restr_u5)

#National level
FDS_ZAM_2025_main <- main %>%
  as_survey_design(
    strata = samp_strat,
    weights = wgh_samp_pop_restr,
    nest = TRUE
  )
FDS_ZAM_2025_HHroster <- HHroster %>%
  as_survey_design(
    strata = samp_strat,           # Specify the column with cluster IDs
    weights = wgh_samp_pop_restr, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )

FDS_ZAM_2025_RA_adult <- RA_adult %>%
  as_survey_design(
    strata = samp_strat,           # Specify the column with cluster IDs
    weights = wgh_samp_pop_restr_resp, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )

FDS_ZAM_2025_RA_caregiver <- RA_caregiver %>%
  as_survey_design(
    strata = samp_strat,           # Specify the column with cluster IDs
    weights = wgh_samp_pop_restr_u5, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )


FDS_ZAM_2025_RA_woman <- RA_woman %>%
  as_survey_design(
    strata = samp_strat,           # Specify the column with cluster IDs
    weights = wgh_samp_pop_restr_w, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )
#Subnational level 
FDS_ZAM_2025_main_strat <- main %>%
  as_survey_design(
    strata = samp_strat,
    weights = wgh_strata_spec,
    nest = TRUE
  )
FDS_ZAM_2025_HHroster_strat <- HHroster %>%
  as_survey_design(
    strata = samp_strat,           # Specify the column with cluster IDs
    weights = wgh_strata_spec, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )

FDS_ZAM_2025_RA_adult_strat <- RA_adult %>%
  as_survey_design(
    strata = samp_strat,           # Specify the column with cluster IDs
    weights = wgh_strata_spec_resp, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )

FDS_ZAM_2025_RA_caregiver_strat <- RA_caregiver %>%
  as_survey_design(
    strata = samp_strat,           # Specify the column with cluster IDs
    weights = wgh_strata_spec_u5, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )


FDS_ZAM_2025_RA_woman_strat <- RA_woman %>%
  as_survey_design(
    strata = samp_strat,           # Specify the column with cluster IDs
    weights = wgh_strata_spec_w, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )

## Create survey objects to do analysis on a sampling strata level ----

# # Household-level (strata-specific)
# FDS_ZAM_2025_main_strat <- make_design(main, wgh_strata_spec)
# 
# # HH roster (strata-specific)
# FDS_ZAM_2025_HHroster_strat <- make_design(HHroster, wgh_strata_spec)
# 
# # Adult respondent (strata-specific)
# FDS_ZAM_2025_RA_adult_strat <- make_design(RA_adult, wgh_strata_spec_resp)
# 
# # Under-5 caregiver (strata-specific)
# FDS_ZAM_2025_RA_caregiver_strat <- make_design(RA_caregiver, wgh_strata_spec_u5)
# 
# # Woman respondent (strata-specific)
# FDS_ZAM_2025_RA_woman_strat <- make_design(RA_woman, wgh_strata_spec_w)


saveRDS(FDS_ZAM_2025_main,              "FDS_ZAM_2025_main.rds")
saveRDS(FDS_ZAM_2025_RA_adult,          "FDS_ZAM_2025_RA_adult.rds")
saveRDS(FDS_ZAM_2025_RA_woman,          "FDS_ZAM_2025_RA_woman.rds")
saveRDS(FDS_ZAM_2025_RA_caregiver,      "FDS_ZAM_2025_RA_caregiver.rds")

saveRDS(FDS_ZAM_2025_main_strat,        "FDS_ZAM_2025_main_strat.rds")
saveRDS(FDS_ZAM_2025_HHroster_strat,    "FDS_ZAM_2025_HHroster_strat.rds")
saveRDS(FDS_ZAM_2025_RA_adult_strat,    "FDS_ZAM_2025_RA_adult_strat.rds")
saveRDS(FDS_ZAM_2025_RA_caregiver_strat,"FDS_ZAM_2025_RA_caregiver_strat.rds")
saveRDS(FDS_ZAM_2025_RA_woman_strat,    "FDS_ZAM_2025_RA_woman_strat.rds")


saveRDS(FDS_ZAM_2025_main,              
        "FDS_ZAM_2025_main.rds")

saveRDS(FDS_ZAM_2025_RA_adult,          
        "FDS_ZAM_2025_RA_adult.rds")

saveRDS(FDS_ZAM_2025_RA_woman,          
        "FDS_ZAM_2025_RA_woman.rds")

saveRDS(FDS_ZAM_2025_RA_caregiver,      
        "FDS_ZAM_2025_RA_caregiver.rds")

saveRDS(FDS_ZAM_2025_main_strat,        
        "FDS_ZAM_2025_main_strat.rds")

saveRDS(FDS_ZAM_2025_HHroster_strat,    
        "FDS_ZAM_2025_HHroster_strat.rds")

saveRDS(FDS_ZAM_2025_RA_adult_strat,    
        "FDS_ZAM_2025_RA_adult_strat.rds")

saveRDS(FDS_ZAM_2025_RA_caregiver_strat,
        "FDS_ZAM_2025_RA_caregiver_strat.rds")

saveRDS(FDS_ZAM_2025_RA_woman_strat,    
        "FDS_ZAM_2025_RA_woman_strat.rds")
