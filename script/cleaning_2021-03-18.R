
library(tidyverse)

hbt <- haven::read_sav(here::here("data", "Controls_COVID_210317.sav"))

glimpse(hbt)



hbt2 <- hbt %>% 
  mutate(Astrand_rel_VO2 = Astrand_MaxVO2 / WeightKG * 1000,
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
                                           Astrand_rel_VO2 >= 32 ~ 0),
         mutate(across(Performed_year:DesiredWeightKG_1, ~na_if(., 0))))
                                  


haven::write_sav(hbt2, here::here("data", "Controls_COVID_rensad_210318.sav"))

write_csv(hbt2, here::here("data", "covid2_rensad.csv"))



Ssyk_hpi_scb_wb_hl = case_when(Ssyk1_hpi_combined >= 1  & Ssyk1_hpi_combined <= 3 ~ 'White-collar high-skilled',
                               Ssyk1_hpi_combined >= 4  & Ssyk1_hpi_combined <= 5 ~ 'White-collar low-skilled',
                               Ssyk1_hpi_combined >= 6  & Ssyk1_hpi_combined <= 7 ~ 'Blue-collar high-skilled',
                               Ssyk1_hpi_combined >= 8  & Ssyk1_hpi_combined <= 9 ~ 'Blue-collar low-skilled'),
Ssyk_hpi_scb_hl = case_when(Ssyk1_hpi_scb_combined >= 1  & Ssyk1_hpi_scb_combined <= 3 ~ 'High-skilled',
                            Ssyk1_hpi_scb_combined >= 4  & Ssyk1_hpi_scb_combined <= 5 ~ 'Low-skilled',
                            Ssyk1_hpi_scb_combined >= 6  & Ssyk1_hpi_scb_combined <= 7 ~ 'High-skilled',
                            Ssyk1_hpi_scb_combined >= 8  & Ssyk1_hpi_scb_combined <= 9 ~ 'Low-skilled'),