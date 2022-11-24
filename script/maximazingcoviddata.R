
# scripts
# cleaning -> merging_covid_scb -> maximazingcoviddata




library(tidyverse)
library(lubridate)

hbt2 <- read_csv(here::here("data", "covid2_rensad.csv"))

grund_hpb <- read_delim("C:/Users/Daniel/OneDrive - gih.se/PHD/Elins och Daniels gemensamma mapp/Other_R_projects/HPI/covid/data/Leverans SCB dec 2020/EEB_lev_HPB_CET_20201202.txt", 
                        "\t", escape_double = FALSE, trim_ws = TRUE)

#hea <- read_delim("C:/Users/Daniel/OneDrive - gih.se/PHD/Elins och Daniels gemensamma mapp/Other_R_projects/HPI/covid/data/Leverans SCB dec 2020/EEB_lev_Hea_20201202.txt", 
#                  "\t", escape_double = FALSE, trim_ws = TRUE)

scb <- EEB_lev_SCB_Grunduppgifter_New <- read_delim("C:/Users/Daniel/OneDrive - gih.se/PHD/Elins och Daniels gemensamma mapp/Other_R_projects/HPI/covid/data/Leverans SCB dec 2020/EEB_lev_SCB_Grunduppgifter_New.txt", 
                                             "\t", escape_double = FALSE, trim_ws = TRUE)

severe <- hbt2 %>% filter(SevereCOVID == 1) #%>%
 #group_by(LopNr) %>% mutate(n=n()) %>% filter(n == 1) %>% ungroup() %>% 
 #filter(Astrand_TO2 > 1 &  TobaccoSmoking > 0) 

grund <- grund_hpb %>% select(LopNr,Performed, CET_GlobalTestnumber, CET, HPB_GlobalTestnumber, HPB, Astrand_TO2, EkB_TO2, HeightCM, Exercise, ExerciseWeekly, SedentarySparetime, TobaccoSmoking)

#hea1 <- hea %>% select(LopNr, Performed, Created, SedentarySparetime, Exercise, ExerciseWeekly) %>% mutate(hea = "hea")


all <- severe %>% #filter(Astrand_TO2 > 1 & HPB == 1) 
  left_join(grund, by = "LopNr") %>% 
  #left_join(hea1, by = "LopNr") %>% 
  mutate(Performed = dmy(Performed)) %>% # Performed.x = dmy(Performed.x), Performed.y = dmy(Performed.y)) %>% 
  na_if(0) %>% 
  mutate(Astrand_TO2 = coalesce(Astrand_TO2, EkB_TO2)) 

#write_csv(all, here::here("severe.csv"))

all <- read_csv(here::here("severe.csv"))

#%>% filter(Astrand_TO2 > 1 &  TobaccoSmoking > 0) 
# those with 1 test
all_with_1_test <- all %>% 
  group_by(LopNr) %>% 
  mutate(n=n()) %>%
  filter(n == 1) %>% 
  select(LopNr, indexdate, Performed.x, Astrand_TO2, TobaccoSmoking)

# people with both Åtest and tobacco smoking
all2 <- all %>% 
  group_by(LopNr) %>% 
  mutate(n=n())  %>% 
  filter(n > 1) %>%  # take away those with less than 1 test
  filter(Astrand_TO2 > 1 &  TobaccoSmoking > 0) %>% 
  arrange(LopNr, indexdate, Performed.x) %>% 
  group_by(LopNr) %>% 
  filter(HPB_GlobalTestnumber == max(HPB_GlobalTestnumber)) %>% 
  distinct(LopNr, .keep_all = TRUE) %>%
  select(LopNr, indexdate, Performed.x, Astrand_TO2, TobaccoSmoking)



# all_with_1_test %>%  full_join(all2) %>% view() # test with join

# all with only one test or both smoking and åstrand
tests_to_filter <- all_with_1_test %>% bind_rows(all2) %>% filter(!indexdate == Performed.x) 

# sent to elin
# tests_to_filter <- all_with_1_test %>% bind_rows(all2) %>% filter(!indexdate == Performed.x) %>% mutate(diff_days= (indexdate - Performed.x),
#                                                                                                        diff_year = as.numeric(diff_days/365)) %>% select(LopNr, Performed.x)


write_csv(tests_to_filter, here::here("testshpi.csv"))

tests_to_filter <- read_csv(here::here("testshpi.csv")) %>% select(LopNr, Performed.x, indexdate)
#tests_to_filter %>% summarise(mean(diff_year))

gh <- grund_hpb %>% 
mutate(Performed = dmy(Performed)) 


filtered <- tests_to_filter %>%  left_join(gh, by = c("LopNr", "Performed.x" = "Performed")) 



cleaned <- filtered %>% rename("Performed" = Performed.x) %>% 
  mutate(Performed_year = year(Performed),
         Astrand_rel_VO2 = Astrand_MaxVO2 / WeightKG * 1000,
         EkB_rel_VO2 = EkB_MaxVO2 / WeightKG * 1000,
         Age = case_when(Age < 14 ~ NA_real_, # have to take away individuals not ages
                         Age > 85 ~ NA_real_,
                         TRUE ~ Age),
         Gender = case_when(Gender == "other" ~ NA_character_,
                            TRUE ~ Gender),
         HeightCM = case_when(HeightCM < 139 ~ NA_real_,
                              HeightCM > 214 ~ NA_real_,
                              HeightCM == 100 & BMI == 100 & WeightKG == 100 ~ NA_real_,
                              TRUE ~ HeightCM),
         WaistCircumference = case_when(WaistCircumference <57 ~ NA_real_,
                                        WaistCircumference > 180 ~ NA_real_,
                                        WaistCircumference <= 145 & BMI > 40 ~ NA_real_,
                                        TRUE ~ WaistCircumference),
         WeightKG = case_when(WeightKG < 30 ~ NA_real_,
                              WeightKG > 240 ~ NA_real_,
                              HeightCM == 100 & BMI == 100 & WeightKG == 100 ~ NA_real_,
                              TRUE ~ WeightKG),
         BMI = case_when(BMI < 13 ~ NA_real_,
                         BMI > 75 ~ NA_real_,
                         HeightCM == 100 & BMI == 100 & WeightKG == 100 ~ NA_real_,
                         BMI > 50 & HeightCM < 139 ~ NA_real_,
                         TRUE ~ BMI),
         BloodPressureSystolic = case_when(BloodPressureSystolic < 70 ~ NA_real_,
                                           BloodPressureSystolic > 260 ~ NA_real_,
                                           BloodPressureSystolic - BloodPressureDiastolic < 15 ~ NA_real_,
                                           BloodPressureSystolic - BloodPressureDiastolic > 160 ~ NA_real_,
                                           TRUE ~ BloodPressureSystolic),
         BloodPressureDiastolic = case_when(BloodPressureDiastolic < 30 ~ NA_real_,
                                            BloodPressureDiastolic > 160 ~NA_real_,
                                            BloodPressureSystolic - BloodPressureDiastolic < 15 ~ NA_real_,
                                            BloodPressureSystolic - BloodPressureDiastolic > 160 ~ NA_real_,
                                            TRUE ~ BloodPressureDiastolic),
         FinalWatt_atest = case_when(FinalWatt < 50 ~ NA_real_,
                                     TRUE ~ FinalWatt),
         WorkPulseHighWatt_atest = case_when(WorkPulseHighWatt < 108 ~ NA_real_,
                                             WorkPulseHighWatt > 170 ~ NA_real_,
                                             TRUE ~ WorkPulseHighWatt),
         Astrand_rel_VO2 = case_when(Astrand_rel_VO2 < 15 | Astrand_rel_VO2 > 80 ~ NA_real_,
                                     TRUE ~ Astrand_rel_VO2),
         WorkPulseLowWatt_EkB = case_when(WorkPulseLowWatt < 51 ~ NA_real_,
                                          WorkPulseLowWatt > 140 ~ NA_real_,
                                          TRUE ~ WorkPulseLowWatt),
         WorkPulseHighWatt_EkB = case_when(WorkPulseHighWatt < 70 ~ NA_real_,
                                           WorkPulseHighWatt > 180 ~ NA_real_,
                                           TRUE ~ WorkPulseHighWatt),
         EkB_rel_VO2 = case_when(EkB_rel_VO2 < 24 & Gender == "Male" ~ NA_real_,
                                 EkB_rel_VO2 > 76 & Gender == "Male" ~ NA_real_,
                                 EkB_rel_VO2 < 19 & Gender == "Female" ~ NA_real_,
                                 EkB_rel_VO2 > 62 & Gender == "Female" ~ NA_real_,
                                 TRUE ~ EkB_rel_VO2),
         FinalWattFactor = (WorkPulseHighWatt_EkB - WorkPulseLowWatt_EkB) / (FinalWatt - 30),
         EkB_rel_VO2 = case_when(FinalWattFactor < 0.2 | FinalWattFactor >1 ~ NA_real_,
                                 TRUE ~ EkB_rel_VO2),
         Ssyk1_hpi2 =  str_sub(Profession2Code, end = -4L),
         Ssyk1_hpi = str_sub(ProfessionCode, end = -4L),
         Ssyk2_hpi2 =  str_sub(Profession2Code, end = -3L),
         Ssyk2_hpi = str_sub(ProfessionCode, end = -3L),
         Ssyk1_hpi_combined = coalesce(Ssyk1_hpi2, Ssyk1_hpi),
         Ssyk2_hpi_combined = coalesce(Ssyk2_hpi2, Ssyk2_hpi),
         Ssyk_hpi_scb_wb_hl = case_when(Ssyk1_hpi_combined >= 1  & Ssyk1_hpi_combined <= 3 ~ 'White-collar high-skilled',
                                        Ssyk1_hpi_combined >= 4  & Ssyk1_hpi_combined <= 5 ~ 'White-collar low-skilled',
                                        Ssyk1_hpi_combined >= 6  & Ssyk1_hpi_combined <= 7 ~ 'Blue-collar high-skilled',
                                        Ssyk1_hpi_combined >= 8  & Ssyk1_hpi_combined <= 9 ~ 'Blue-collar low-skilled'),
         agegroup_3 = case_when(Age >= 50  & Age <= 65 ~ '50-65',
                                Age >= 35  & Age <= 49 ~ '35-49',
                                Age >= 18  & Age <= 34 ~ '18-34'),
         agegroup_4 = case_when(Age >= 60  & Age <= 75 ~ '60-75',
                                Age >= 50  & Age <= 59 ~ '50-59',
                                Age >= 35  & Age <= 49 ~ '35-49',
                                Age >= 18  & Age <= 34 ~ '18-34'),
         agegroup_6 = case_when(Age >= 65  & Age <= 75 ~ '65-75',
                                Age >= 55  & Age <= 64 ~ '55-64',
                                Age >= 45  & Age <= 54 ~ '45-54',
                                Age >= 35  & Age <= 44 ~ '35-44',
                                Age >= 25  & Age <= 34 ~ '25-34',
                                Age >= 18  & Age <= 24 ~ '18-24'),
         Astrand_rel_VO2_32_di = case_when(Astrand_rel_VO2 < 32 ~ 1,
                                           Astrand_rel_VO2 >= 32 ~ 0)#,
         #mutate(across(Total_GlobalTestnumber:SymptomInsomnia, ~na_if(., 0)))
         ) %>% 
    na_if(0) %>% 
  mutate(Astrand_rel_VO2 =coalesce(Astrand_rel_VO2, EkB_rel_VO2))


#check data
cleaned %>% ungroup() %>%   skimr::skim()
cleaned %>% count(TobaccoSmoking)

#haven::write_sav(hbt2, here::here("data", "Controls_COVID_rensad_210318.sav"))

# change date for scb data that is loaded in begining of script
scb <- scb %>% mutate(Performed = dmy(Performed)) 


# lopnr to be removed from HPI data before mergin and for the right data. the new 73 observations
remove <- read_csv(here::here("testshpi.csv")) %>% 
  select(LopNr, indexdate)

severe <- hbt2 %>% filter(SevereCOVID == 1) %>% 
  select(LopNr,
         indexdate,
         date_C00_D48:SevereCOVID) %>% 
  right_join(remove) %>% 
  #select(!indexdate) %>% 
  mutate(changed_variables = "changed")


# join with scb data and with covid variables
cleaned2 <-  cleaned %>% left_join(scb) %>% 
  left_join(severe, by = c("LopNr", "indexdate")) %>% 
  mutate(Created = dmy(Created),
         Ssyk1_hpi = as.double(Ssyk1_hpi),
         Ssyk2_hpi = as.double(Ssyk2_hpi)) %>% 
  ungroup() %>% 
         select(-c(CreatedTodaytoo,
                indexdate)  )




cleaned2 <- cleaned2 %>% 
  mutate(Ssyk3_combined1 = case_when(Performed <= "2009-12-31" ~ Ssyk3),
         Ssyk3_combined2 = case_when(Performed >= "2010-01-01" & Performed <= "2013-12-31" ~ Ssyk3_J16),
         Ssyk3_combined3 = case_when(Performed >="2014-01-01" ~ Ssyk3_2012_J16), 
         Ssyk3_scb_combined = coalesce(Ssyk3_combined1,Ssyk3_combined2,Ssyk3_combined3, Ssyk3, Ssyk3_J16, Ssyk3_2012_J16),
         Ssyk2_scb_combined = str_sub(Ssyk3_scb_combined, end = -2L),
         Ssyk1_scb_combined = str_sub(Ssyk3_scb_combined, end = -3L),
         Ssyk1_scb_combined = na_if(Ssyk1_scb_combined, 0)) %>% 
  mutate(across(where(is.character), na_if, "*"),
         across(where(is.character), na_if, "**"),
         across(where(is.character), na_if, "***"),
         across(where(is.character), na_if, "****")) %>% 
  mutate(Ssyk1_hpi_scb_combined = coalesce(Ssyk1_hpi_combined, Ssyk1_scb_combined),
         Ssyk2_hpi_scb_combined = coalesce(Ssyk2_hpi_combined, Ssyk2_scb_combined),
         Ssyk_hpi_scb_wb_hl = case_when(Ssyk1_hpi_scb_combined >= 1  & Ssyk1_hpi_scb_combined <= 3 ~ 'White-collar high-skilled',
                                        Ssyk1_hpi_scb_combined >= 4  & Ssyk1_hpi_scb_combined <= 5 ~ 'White-collar low-skilled',
                                        Ssyk1_hpi_scb_combined >= 6  & Ssyk1_hpi_scb_combined <= 7 ~ 'Blue-collar high-skilled',
                                        Ssyk1_hpi_scb_combined >= 8  & Ssyk1_hpi_scb_combined <= 9 ~ 'Blue-collar low-skilled'),
         Ssyk_hpi_scb_hl = case_when(Ssyk1_hpi_scb_combined >= 1  & Ssyk1_hpi_scb_combined <= 3 ~ 'High-skilled',
                                     Ssyk1_hpi_scb_combined >= 4  & Ssyk1_hpi_scb_combined <= 5 ~ 'Low-skilled',
                                     Ssyk1_hpi_scb_combined >= 6  & Ssyk1_hpi_scb_combined <= 7 ~ 'High-skilled',
                                     Ssyk1_hpi_scb_combined >= 8  & Ssyk1_hpi_scb_combined <= 9 ~ 'Low-skilled'))

cleaned2 %>% ungroup() %>%   skimr::skim()

# read hpb2 cleaned version
hbt2 <- read_csv(here::here("data", "Covid_med_ssyk_2021-03-29.csv")) %>% 
  mutate(Kon = coalesce(Kon.x,Kon.y)) %>%  
  select(-c(     Ssyk3_scb_combined,
                 Ssyk2_scb_combined,
                 Ssyk1_scb_combined,
                 #Ssyk1_scb_hpi_combined, 
                 Ssyk1_hpi_scb_combined,
                 Ssyk1_scb_combined,
                 Ssyk1_hpi2,
                 Ssyk1_hpi,
                 Ssyk2_hpi2,
                 SsykAr,
                 SsykAr,
                 #Ssyk_hpi_scb_hl,
                 #Ssyk_hpi_scb_wb_hl,
                 Ssyk3,
                 SsykAr_J16,
                 Ssyk3_combined2,
                 Ssyk3_combined2,
                 Ssyk3_combined1,
                 Ssyk3_combined3,
                 Ssyk1_scb_hpi_combined,
                 Ssyk12_hpi_scb_combined,
                 Ssyk2_scb_hpi_combined,
                 Kon.x,
                 Kon.y)) 





coalesce_join <- function(x, y, 
                          by = NULL, suffix = c(".x", ".y"), 
                          join = dplyr::full_join, ...) {
  joined <- join(x, y, by = by, suffix = suffix, ...)
  # names of desired output
  cols <- union(names(x), names(y))
  
  to_coalesce <- names(joined)[!names(joined) %in% cols]
  suffix_used <- suffix[ifelse(endsWith(to_coalesce, suffix[1]), 1, 2)]
  # remove suffixes and deduplicate
  to_coalesce <- unique(substr(
    to_coalesce, 
    1, 
    nchar(to_coalesce) - nchar(suffix_used)
  ))
  
  coalesced <- purrr::map_dfc(to_coalesce, ~dplyr::coalesce(
    joined[[paste0(.x, suffix[1])]], 
    joined[[paste0(.x, suffix[2])]]
  ))
  names(coalesced) <- to_coalesce
  
  dplyr::bind_cols(joined, coalesced)[cols]
}

# take away cases and add the new (old) cases, coalesce join function.
hbt_new <- hbt2 %>% anti_join(remove) %>% 
  coalesce_join(cleaned2, by = c("LopNr", "Performed")) %>% 
  relocate(c(changed_variables,
             #Ssyk1_hpi_scb_combined,
             Ssyk_hpi_scb_hl,
             Ssyk_hpi_scb_wb_hl),
           .after = LopNr) %>% 
  select(-c(     Ssyk3_scb_combined,
                 Ssyk2_scb_combined,
                 Ssyk1_scb_combined,
                 #Ssyk1_scb_hpi_combined, 
                 Ssyk1_hpi_scb_combined,
                 Ssyk1_scb_combined,
                 Ssyk1_hpi2,
                 Ssyk1_hpi,
                 Ssyk2_hpi2,
                 SsykAr,
                 SsykAr,
                 #Ssyk_hpi_scb_hl,
                 #Ssyk_hpi_scb_wb_hl,
                 Ssyk3,
                 SsykAr_J16,
                 Ssyk3_combined2,
                 Ssyk3_combined1,
                 Ssyk3_combined3,
                 #Ssyk1_scb_hpi_combined,
                 #Ssyk12_hpi_scb_combined,
                 #Ssyk2_scb_hpi_combined,
                 dataset_scb,
                 CSFVI:Ssyk4))


hbt_new %>%  filter(changed_variables=="changed") %>% ungroup() %>%   skimr::skim()

write_csv(hbt_new, here::here("data", "Covid_II_73_utbytta_2021-03-31.csv"))

hbt_new <-read_csv(here::here("data", "Covid_II_73_utbytta_2021-03-31.csv"))



haven::write_sav(hbt_new, here::here("data", "Covid_II_73_utbytta_2021-03-31.sav"))

###################################################################
Ssyk1_scb_hpi_combined
glimpse(hbt_new)
hbt_new %>% filter(SevereCOVID==1) %>% skimr::skim()

hbt_new2 <- hbt2 %>% anti_join(remove) %>% bind_rows(cleaned2)%>% count(SevereCOVID)

hbt_new %>% distinct(LopNr)
hbt_new %>% count(SevereCOVID)
hbt_new %>% count(TobaccoSmoking)
hbt_new %>% count(changed_variables)
hbt_new %>% count(Ssyk_hpi_scb_wb_hl)
hbt2%>% count(ssyk)
cleaned2 %>% count(SevereCOVID)

hbt_new %>%  select(indexdate, Astrand_rel_VO2, TobaccoSmoking, EkB_rel_VO2, changed_variables) %>% mutate(Astrand_rel_VO2 =coalesce(Astrand_rel_VO2, EkB_rel_VO2))%>%  filter(changed_variables== "changed") %>% view()  %>%  skimr::skim()

hbt_new %>% select(indexdate, Astrand_rel_VO2, TobaccoSmoking, changed_variables) %>%  filter(changed_variables== "changed") %>% view()






###########################################################################################
#########################################################################################
#########################################################################################




# dataframe to take away those with less than 1 test
remove <- all %>% 
  group_by(LopNr) %>% 
  mutate(n=n())  %>% 
  filter(n > 1) %>% # take away those with less than 1 test
  filter(Astrand_TO2 > 1 &  TobaccoSmoking > 0) %>% 
  arrange(LopNr, indexdate, Performed.x) %>% 
  group_by(LopNr) %>% 
  filter(HPB_GlobalTestnumber == max(HPB_GlobalTestnumber)) %>% 
  mutate(rem = 1) %>%  
  select(LopNr, rem)

# both åstrand and smoking is NA
all %>% ungroup() %>% 
  group_by(LopNr) %>% 
  mutate(n=n()) %>% 
  filter(n > 1) %>% 
  na_if(0) %>% 
  left_join(remove, by = "LopNr") %>% 
  mutate(Astrand_TO2 = coalesce(Astrand_TO2, EkB_TO2)) %>% 
  filter(is.na(rem)) %>% # what is left
  group_by(LopNr) %>% 
  filter(is.na(TobaccoSmoking) & is.na(Astrand_TO2)) %>% 
  mutate(n2=n()) 



# tobacco smoking >1
only_smoking <- all %>%
  group_by(LopNr) %>% 
  mutate(n=n()) %>% 
  filter(n > 1) %>% 
  left_join(remove, by = "LopNr") %>% 
  filter(is.na(rem)) %>% # what is left after removing those with less than 2 tests
  group_by(LopNr) %>% 
  filter(!is.na(TobaccoSmoking) | !is.na(Astrand_TO2)) %>% # take away where both is NA
  filter(TobaccoSmoking > 0 ) %>% 
  select(LopNr, indexdate, Performed.x, Astrand_TO2, TobaccoSmoking)
  
only_atest <- all %>%
  group_by(LopNr) %>% 
  mutate(n=n()) %>% 
  filter(n > 1) %>% 
  left_join(remove, by = "LopNr") %>% 
  filter(is.na(rem)) %>% # what is left after removing those with less than 2 tests
  group_by(LopNr) %>% 
  filter(!is.na(TobaccoSmoking) | !is.na(Astrand_TO2)) %>% # take away where both is NA
  filter(Astrand_TO2 > 0 ) %>% 
  select(LopNr, indexdate, Performed.x, Astrand_TO2, EkB_TO2, TobaccoSmoking)
  
  
only_atest %>% inner_join(only_smoking, by = "LopNr") %>% mutate(diff_date = Performed.x.x - Performed.x.y) %>% 
  group_by(LopNr) %>% 
  filter(diff_date == min(diff_date)) %>% 
view()














  filter(TobaccoSmoking > 0 & Performed.x == max(Performed.x) | Astrand_TO2 >1 & Performed.x == max(Performed.x)) %>% 
  mutate(n2=n()) %>% 
    group_by(LopNr) %>% 
    filter(min() )
  view()


TobaccoSmoking > 0 & Performed.x == max(Performed.x)
& Astrand_TO2 >1 & Performed.x == max(Performed.x)

write_csv(all2, here::here("severe2.csv"))
