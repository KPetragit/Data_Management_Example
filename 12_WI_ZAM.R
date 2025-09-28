###Wealth Index 

### For the composite indicator calculate new variables 

#Data Cleaning

plots_wb <- list()
dat_wb <- list()

main_WI <- main

main_WI$Improved_san <- main_WI$type_san_facility
main_WI$Clean_cookingfuel <- main_WI$RBM20802


table(main_WI$Improved_dw, useNA = "ifany") #drinking water from an improved source
table(main_WI$Improved_dw_final, useNA = "ifany") # improved drinking water, less than 30 minute away
table(main_WI$electricity, useNA = "ifany") # hh has electricity
table(main_WI$shared_san_facility, useNA = "ifany") #Do you share this toilet facility with others who are not members of your household?
table(main_WI$Improved_san, useNA = "ifany") #What kind of toilet facility do members of your household use most of the time?
table(main_WI$crowding_cat, useNA = "ifany") # #Crowding index - overcrowded when more than 3 persons share one room to sleep
table(main_WI$Clean_cookingfuel, useNA = "ifany") # clean cooking - primary reliance on clean (cooking) fuels and technology

main_WI$shared_san_facility[is.na(main_WI$shared_san_facility)] <- 0

table(main_WI$Land01, main_WI$Intro_07)# -- dont use land ownership
table(main_WI$House01, main_WI$Intro_07)# --  use house ownership

#House and Land ownership
main_WI <- main_WI %>%
  mutate(House01 = ifelse(is.na(House01) & Land02 == 1, 1, House01))%>%
  mutate(across(
    c(Land01, House01),
    ~ ifelse(. == 1, 1, ifelse(. %in% c(98, 99), NA, 0)) %>% replace_na(0)
  ))


#Housing
main_WI <- main_WI %>%
  mutate(
    HH02_WI = case_when(
      HH02 %in% c(11, 12, 13) ~ "natural",
      HH02 %in% c(21, 22, 23, 24, 25, 26, 50) ~ "rudimentary",
      HH02 %in% c(31, 32, 33, 34, 35, 36) ~ "finished",
      TRUE ~ NA_character_
    ),
    HH03_WI = case_when(
      HH03 %in% c(11, 12, 13) ~ "natural",
      HH03 %in% c(21, 22, 23, 24) ~ "rudimentary",
      HH03 %in% c(31, 32, 33, 34, 35, 36) ~ "finished",
      TRUE ~ NA_character_
    ),
    HH06_WI = case_when(
      HH06 %in% c(11, 12) ~ "natural",
      HH06 %in% c(21, 22) ~ "rudimentary",
      HH06 %in% c(31, 32, 33, 34, 35, 36) ~ "finished",
      TRUE ~ NA_character_
    ),
    HH02_finished = case_when(HH02_WI == "finished" ~ 1, TRUE ~ 0),
    HH03_finished = case_when(HH03_WI == "finished" ~ 1, TRUE ~ 0),
    HH06_finished = case_when(HH06_WI == "finished" ~ 1, TRUE ~ 0), 
    HH02_rudimentary = case_when(HH02_WI == "rudimentary" ~ 1, TRUE ~ 0),
    HH03_rudimentary = case_when(HH03_WI == "rudimentary" ~ 1, TRUE ~ 0),
    HH06_rudimentary = case_when(HH06_WI == "rudimentary" ~ 1, TRUE ~ 0), 
    HH02_natural = case_when(HH02_WI == "natural" ~ 1, TRUE ~ 0),
    HH03_natural = case_when(HH03_WI == "natural" ~ 1, TRUE ~ 0),
    HH06_natural = case_when(HH06_WI == "natural" ~ 1, TRUE ~ 0)
  )



table(main_WI$Assets041) #dairy cattle
table(main_WI$Assets042) #dairy cattle
table(main_WI$Assets043) #dairy cattle
table(main_WI$Assets044) #dairy cattle
table(main_WI$Assets045) #dairy cattle
table(main_WI$Assets046) #dairy cattle
table(main_WI$Assets047) #dairy cattle



#Livestock ownership
##bin all livestock variables
main_WI <- main_WI %>%
  mutate(across(starts_with("Assets04"), ~ ifelse(is.na(.) | . == 99, 0, .))) %>%
  mutate(
    Assets04a_1 = ifelse(Assets041 >= 1 & Assets041 <= 4, 1, 0),
    Assets04a_2 = ifelse(Assets041 >= 5 & Assets041 <= 9, 1, 0),
    Assets04a_3 = ifelse(Assets041 >= 10 & Assets041 != 99, 1, 0),
    Assets04b_1 = ifelse(Assets042 >= 1 & Assets042 <= 4, 1, 0),
    Assets04b_2 = ifelse(Assets042 >= 5 & Assets042 <= 9, 1, 0),
    Assets04b_3 = ifelse(Assets042 >= 10 & Assets042 != 99, 1, 0),
    Assets04c_1 = ifelse(Assets043 >= 1 & Assets043 <= 4, 1, 0),
    Assets04c_2 = ifelse(Assets043 >= 5 & Assets043 <= 9, 1, 0),
    Assets04c_3 = ifelse(Assets043 >= 10 & Assets043 != 99, 1, 0),
    Assets04d_1 = ifelse(Assets044 >= 1 & Assets044 <= 4, 1, 0),
    Assets04d_2 = ifelse(Assets044 >= 5 & Assets044 <= 9, 1, 0),
    Assets04d_3 = ifelse(Assets044 >= 10 & Assets044 != 99, 1, 0),
    Assets04e_1 = ifelse(Assets045 >= 1 & Assets045 <= 4, 1, 0),
    Assets04e_2 = ifelse(Assets045 >= 5 & Assets045 <= 9, 1, 0),
    Assets04e_3 = ifelse(Assets045 >= 10 & Assets045 != 99, 1, 0),
    Assets04f_1 = ifelse(Assets046 >= 1 & Assets046 <= 4, 1, 0),
    Assets04f_2 = ifelse(Assets046 >= 5 & Assets046 <= 9, 1, 0),
    Assets04f_3 = ifelse(Assets046 >= 10 & Assets046 != 99, 1, 0),
    Assets04h_1 = ifelse(Assets047 >= 1 & Assets047 <= 4, 1, 0),
    Assets04h_2 = ifelse(Assets047 >= 5 & Assets047 <= 9, 1, 0),
    Assets04h_3 = ifelse(Assets047 >= 10 & Assets047 != 99, 1, 0)
  )%>%
  select(-c(Assets041, Assets042, Assets043, Assets044, Assets045, Assets046, Assets047, Assets03))

main_WI <- main_WI %>%
  set_variable_labels(
    Assets04a_1 = "Cows/bulls: 1-4",
    Assets04a_2 = "Cows/bulls: 5-9",
    Assets04a_3 = "Cows/bulls: 10+",
    Assets04b_1 = "Other cattle: 1-4",
    Assets04b_2 = "Other cattle: 5-9",
    Assets04b_3 = "Other cattle: 10+",
    Assets04c_1 = "Horses/donkeys/mules: 1-4",
    Assets04c_2 = "Horses/donkeys/mules: 5-9",
    Assets04c_3 = "Horses/donkeys/mules: 10+",
    Assets04d_1 = "Goats: 1-4",
    Assets04d_2 = "Goats: 5-9",
    Assets04d_3 = "Goats: 10+",
    Assets04e_1 = "Sheep: 1-4",
    Assets04e_2 = "Sheep: 5-9",
    Assets04e_3 = "Sheep: 10+",
    Assets04f_1 = "Chicken or other poultry: 1-4",
    Assets04f_2 = "Chicken or other poultry: 5-9",
    Assets04f_3 = "Chicken or other poultry: 10+",
    Assets04h_1 = "Camels: 1-4",
    Assets04h_2 = "Camels: 5-9",
    Assets04h_3 = "Camels: 10+"
  )





##recode assets

main_WI <- main_WI %>%
  mutate(
    # Ownership of assets
    Assets01b_bin   = if_else(Assets01b == 1, 1, 0),   # television
    Assets01d_bin   = if_else(Assets01d == 1, 1, 0),   # refrigerator
    Assets01g_bin   = if_else(Assets01g == 1, 1, 0),   # air conditioner
    Assets01j_bin   = if_else(Assets01j == 1, 1, 0),   # grain mill
    Assets01l_bin   = if_else(Assets01l == 1, 1, 0),   # generator
    Assets01m_bin   = if_else(Assets01m == 1, 1, 0),   # solar panel
    Assets01o_bin   = if_else(Assets01o == 1, 1, 0),   # modem / router
    Assets01p_bin   = if_else(Assets01p == 1, 1, 0),   # cable
    Assets01q_bin   = if_else(Assets01q == 1, 1, 0),   # water pump
    Assets01s_b_bin = if_else(Assets01s_b == 1, 1, 0), # bed
    Assets01t_bin   = if_else(Assets01t == 1, 1, 0),   # sofa
    Assets01v_bin   = if_else(Assets01v == 1, 1, 0),   # washing machine
    Assets01w_bin   = if_else(Assets01w == 1, 1, 0),   # camera
    Assets01y_bin   = if_else(Assets01y == 1, 1, 0),   # hammer mill
    Assets01z_bin   = if_else(Assets01z == 1, 1, 0),   # geyser (water heater)
    Assets01aa_bin  = if_else(Assets01aa == 1, 1, 0),  # plough
    Assets01ab_bin  = if_else(Assets01ab == 1, 1, 0),  # microwave
    
    # Ownership of working assets
    Assets02a_bin   = if_else(Assets02a == 1, 1, 0),   # hand watch
    Assets02b_bin   = if_else(Assets02b == 1, 1, 0),   # mobile phone
    Assets02c_bin   = if_else(Assets02c == 1, 1, 0),   # smart phone
    Assets02d_bin   = if_else(Assets02d == 1, 1, 0),   # bicycle
    Assets02e_bin   = if_else(Assets02e == 1, 1, 0),   # motorcycle / scooter
    Assets02f_a_bin = if_else(Assets02f_a == 1, 1, 0), # car
    Assets02f_b_bin = if_else(Assets02f_b == 1, 1, 0), # truck
    Assets02g_bin   = if_else(Assets02g == 1, 1, 0),   # boat without motor
    Assets02h_bin   = if_else(Assets02h == 1, 1, 0),   # boat with motor
    Assets02i_bin   = if_else(Assets02i == 1, 1, 0),   # animal-drawn cart
    Assets02j_bin   = if_else(Assets02j == 1, 1, 0),   # tractor
    Assets02l_bin   = if_else(Assets02l == 1, 1, 0), #laptop,
    
    HH_30a = if_else(HH_30a==1,1,0),
    crowding_cat= if_else(crowding_cat==1,1,0)
  )



main_WI_final <- main_WI %>%
  select(
    # Core service access variables
    `_uuid`,
    wgh_strata_spec,
    samp_strat,
    Intro_09,
    Intro_07,
    Improved_dw,             # Improved drinking water
    Improved_dw_final,      # Improved drinking water, <30 min
    electricity,            # Has electricity
    shared_san_facility,     # Shared toilet facility?
    Improved_san,            # Type of toilet facility
    crowding_cat,            # Overcrowding index
    Clean_cookingfuel,       # Clean cooking fuel
    #  Land01,  --not using land since refugees appear to face barriers to land ownership
    House01,
    HH_30a,
    
    # Housing materials
    HH02_finished,
    HH03_finished,
    HH06_finished,
    HH02_rudimentary,
    HH03_rudimentary,
    HH06_rudimentary,
    HH02_natural, 
    HH03_natural,
    HH06_natural,
    
    ##Assets
    
    Assets01b_bin,
    Assets01d_bin,
    Assets01g_bin, 
    Assets01j_bin,
    Assets01l_bin, 
    Assets01m_bin, 
    Assets01o_bin,
    Assets01p_bin,
    Assets01q_bin, 
    Assets01s_b_bin,
    Assets01t_bin, 
    Assets01v_bin,
    Assets01w_bin, 
    Assets01y_bin,
    Assets01z_bin,
    Assets01aa_bin,
    Assets01ab_bin, 
    
    # Ownership of working assets
    Assets02a_bin,
    Assets02b_bin, 
    Assets02c_bin,  
    Assets02d_bin, 
    Assets02e_bin, 
    Assets02f_a_bin,
    Assets02f_b_bin,
    Assets02g_bin,
    Assets02h_bin,
    Assets02i_bin, 
    Assets02j_bin, 
    Assets02l_bin,
    
    
    ##Assets ( cattle)
    Assets04a_1,
    Assets04a_2,
    Assets04a_3, 
    Assets04b_1, 
    Assets04b_2, 
    Assets04b_3, 
    Assets04c_1, 
    Assets04c_2,
    Assets04c_3, 
    Assets04d_1,
    Assets04d_2,
    Assets04d_3,
    Assets04e_1,
    Assets04e_2, 
    Assets04e_3,
    Assets04f_1, 
    Assets04f_2, 
    Assets04f_3, 
    Assets04h_1, 
    Assets04h_2, 
    Assets04h_3
  )


main_WI_final <- main_WI_final %>%
  mutate(
    across(
      c(Assets01y_bin, Assets01z_bin, Assets01aa_bin, Assets01ab_bin),
      ~ replace_na(., 0)
    )
  )

#remove variables with zero variation
zero_sd_vars_main <- sapply(main_WI_final[ , 7:ncol(main_WI_final)], function(x) sd(x, na.rm = TRUE) == 0)
removed_vars_main <- names(main_WI_final)[which(zero_sd_vars_main) + 6]  # +6 to account for starting at column 5
main_WI_final <- main_WI_final[ , !(names(main_WI_final) %in% removed_vars_main)]
cat("Removed variables from main:", removed_vars_main, "\n")


###WI is calculated by using PCA (principal component analysis)
binary_vars <- main_WI_final[, 7:ncol(main_WI_final)]
binary_vars <- data.frame(lapply(binary_vars, function(x) as.numeric(as.character(x))))

unique_vals <- lapply(binary_vars, unique)
print(unique_vals)

#calc combined PCA
main_pca <- psych::principal(
  binary_vars,
  rotate = "varimax",
  nfactors = 1,
  covar = TRUE,
  cor = "tet",
  scores = TRUE
)



main_WI_final$comscore <- as.numeric(main_pca$scores[, 1])

#Common/Join PCA Quintiles
main_WI_final <- main_WI_final %>%
  filter(!is.na(comscore)) %>%
  mutate(
    wealth_quintile = ntile(comscore, 5),
    wealth_quintile = factor(wealth_quintile, levels = 1:5, 
                             labels = c("Poorest", "Second", "Middle", "Fourth", "Richest"))
  )




#IF RUNNING PCA JOINTLY FOR URBAN AND RURAL
main_WI_final %>%
  filter(!is.na(wealth_quintile)) %>%
  mutate(wealth_quintile = fct_rev(wealth_quintile)) %>%  # Reverse order
  ggplot(aes(x = samp_strat, fill = wealth_quintile)) +
  geom_bar(position = "fill") +
  xlab("Province") +
  ylab("Percentage") +
  ggtitle("Wealth by Province") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_unhcr_d(guide = guide_legend(reverse = TRUE)) +  # Use UNHCR color palette
  theme_unhcr() +
  coord_flip()


main_WI_final %>%
  filter(Intro_07 %in% c("Refugees", "Host Community", "Former Refugees"), !is.na(wealth_quintile)) %>%
  mutate(wealth_quintile = fct_rev(wealth_quintile)) %>%  # Reverse order
  ggplot(aes(x = Intro_07, fill = wealth_quintile)) +
  geom_bar(position = "fill") +
  xlab("Population Group") +
  ylab("Percentage") +
  ggtitle("Wealth Distribution: Refugees, Former Refugees vs. Host Community") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_unhcr_d(guide = guide_legend(reverse = TRUE)) +  # Use UNHCR color palette
  theme_unhcr() +
  coord_flip()



dat_wb$wi_results <- main_WI_final %>%
  group_by(Intro_07, wealth_quintile) %>%
  summarise(
    weighted_n = sum(wgh_strata_spec, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Intro_07) %>%
  mutate(
    weighted_pct = round(weighted_n / sum(weighted_n) * 100, 1)
  ) %>%
  select(Intro_07, wealth_quintile, weighted_pct)



dat_wb$wi_results_strata <- main_WI_final %>%
  group_by(samp_strat, wealth_quintile) %>%
  summarise(
    weighted_n = sum(wgh_strata_spec, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(samp_strat) %>%
  mutate(
    weighted_pct = round(weighted_n / sum(weighted_n) * 100, 1)
  ) %>%
  select(samp_strat, wealth_quintile, weighted_pct)




main_WI_long <- main_WI_final%>%
  pivot_longer(
    cols = c(samp_strat, Intro_07),
    names_to = "group_type",
    values_to = "group_value"
  )

main <- main %>%
  left_join(
    main_WI_final %>% select(`_uuid`, wealth_quintile),
    by = "_uuid"
  )

FDS_ZAM_2025_main <- main %>%
  as_survey_design(
    strata = samp_strat,
    weights = wgh_samp_pop_restr,
    nest = TRUE
  )

FDS_ZAM_2025_main_strat <- main %>%
  as_survey_design(
    strata = samp_strat,
    weights = wgh_strata_spec,
    nest = TRUE
  )

saveRDS(FDS_ZAM_2025_main,              "FDS_ZAM_2025_main.rds")
saveRDS(FDS_ZAM_2025_main_strat,        "FDS_ZAM_2025_main_strat.rds")
saveRDS(main, "main.rds")

saveRDS(main,          
        "C:/Users/KAPS/OneDrive - UNHCR/300 - ST - Survey Team - Main/Survey Programme Team/Projects/FDS/Countries/Zambia/Data Management/4 Use/With indicators/main.rds")



saveRDS(FDS_ZAM_2025_main,              
        "C:/Users/KAPS/OneDrive - UNHCR/300 - ST - Survey Team - Main/Survey Programme Team/Projects/FDS/Countries/Zambia/Data Management/4 Use/With indicators/FDS_ZAM_2025_main.rds")


saveRDS(FDS_ZAM_2025_main_strat,        
        "C:/Users/KAPS/OneDrive - UNHCR/300 - ST - Survey Team - Main/Survey Programme Team/Projects/FDS/Countries/Zambia/Data Management/4 Use/With indicators/FDS_ZAM_2025_main_strat.rds")
