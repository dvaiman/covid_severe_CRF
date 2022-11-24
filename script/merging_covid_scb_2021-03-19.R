

library(tidyverse)
library(lubridate)

EEB_lev_SCB_Grunduppgifter_New <- read_delim("C:/Users/Daniel/OneDrive - gih.se/PHD/Elins och Daniels gemensamma mapp/Paper 2/Data/Leverans SCB dec 2020/EEB_lev_SCB_Grunduppgifter_New.txt", 
                                             "\t", escape_double = FALSE, trim_ws = TRUE)

hbt2 <- read_csv(here::here("data", "covid2_rensad.csv"))

scb <- EEB_lev_SCB_Grunduppgifter_New %>% select(LopNr, Performed, Kon, Civil, Kommun, Ssyk3:SsykAr_J16) %>% 
  mutate(Performed = dmy(Performed)) %>% 
  mutate(Ssyk3_combined1 = case_when(Performed <= "2009-12-31" ~ Ssyk3),
         Ssyk3_combined2 = case_when(Performed >= "2010-01-01" & Performed <= "2013-12-31" ~ Ssyk3_J16),
         Ssyk3_combined3 = case_when(Performed >="2014-01-01" ~ Ssyk3_2012_J16), 
         Ssyk3_scb_combined = coalesce(Ssyk3_combined1,Ssyk3_combined2,Ssyk3_combined3, Ssyk3, Ssyk3_J16, Ssyk3_2012_J16),
         Ssyk2_scb_combined = str_sub(Ssyk3_scb_combined, end = -2L),
         Ssyk1_scb_combined = str_sub(Ssyk3_scb_combined, end = -3L),
         Ssyk1_scb_combined = na_if(Ssyk1_scb_combined, 0)) %>% 
  mutate(dataset_scb = "scb",
         across(where(is.character), na_if, "*"),
         across(where(is.character), na_if, "**"),
         across(where(is.character), na_if, "***"),
         across(where(is.character), na_if, "****"))



hbt2_scb <- hbt2 %>% #select(LopNr, indexdate, Performed, Age, sex, Ssyk1_hpi_combined, Ssyk2_hpi_combined) %>% 
  left_join(scb, by = c("LopNr", "indexdate" = "Performed" ))



hbt2_scb <- hbt2_scb %>% mutate(Ssyk1_scb_hpi_combined = coalesce(Ssyk1_scb_combined, Ssyk1_hpi_combined),
                              Ssyk2_scb_hpi_combined = coalesce(Ssyk2_scb_combined, Ssyk2_hpi_combined),
                              Ssyk1_hpi_scb_combined = coalesce(Ssyk1_hpi_combined, Ssyk1_scb_combined),
                              Ssyk2_hpi_scb_combined = coalesce(Ssyk2_hpi_combined, Ssyk2_scb_combined),
                              Ssyk_hpi_scb_wb_hl = case_when(Ssyk1_hpi_scb_combined >= 1  & Ssyk1_hpi_scb_combined <= 3 ~ 'White-collar high-skilled',
                                                             Ssyk1_hpi_scb_combined >= 4  & Ssyk1_hpi_scb_combined <= 5 ~ 'White-collar low-skilled',
                                                             Ssyk1_hpi_scb_combined >= 6  & Ssyk1_hpi_scb_combined <= 7 ~ 'Blue-collar high-skilled',
                                                             Ssyk1_hpi_scb_combined >= 8  & Ssyk1_hpi_scb_combined <= 9 ~ 'Blue-collar low-skilled'),
                              Ssyk_hpi_scb_hl = case_when(Ssyk1_hpi_scb_combined >= 1  & Ssyk1_hpi_scb_combined <= 3 ~ 'High-skilled',
                                                          Ssyk1_hpi_scb_combined >= 4  & Ssyk1_hpi_scb_combined <= 5 ~ 'Low-skilled',
                                                          Ssyk1_hpi_scb_combined >= 6  & Ssyk1_hpi_scb_combined <= 7 ~ 'High-skilled',
                                                          Ssyk1_hpi_scb_combined >= 8  & Ssyk1_hpi_scb_combined <= 9 ~ 'Low-skilled'),
                              Ssyk12_hpi_scb_combined = case_when(Ssyk2_hpi_scb_combined >= 10 & Ssyk2_hpi_scb_combined <= 19 ~ 1.0,
                                                                  Ssyk2_hpi_scb_combined >= 20 & Ssyk2_hpi_scb_combined <= 21 ~ 2.1,
                                                                  Ssyk2_hpi_scb_combined == 22         ~ 2.2,
                                                                  Ssyk2_hpi_scb_combined == 23         ~ 2.3,
                                                                  Ssyk2_hpi_scb_combined >= 24 & Ssyk2_hpi_scb_combined <= 29 ~ 2.4,
                                                                  Ssyk2_hpi_scb_combined >= 30 & Ssyk2_hpi_scb_combined <= 39 ~ 3.0,
                                                                  Ssyk2_hpi_scb_combined >= 40 & Ssyk2_hpi_scb_combined <= 49 ~ 4.0,
                                                                  Ssyk2_hpi_scb_combined >= 50 & Ssyk2_hpi_scb_combined <= 52 ~ 5.0,
                                                                  #Ssyk2_hpi_scb_combined == 53         ~ 5.3, # underskoterskor
                                                                  Ssyk2_hpi_scb_combined == 53         ~ 5.0,
                                                                  Ssyk2_hpi_scb_combined >= 54 & Ssyk2_hpi_scb_combined <= 59 ~ 5.0,
                                                                  Ssyk2_hpi_scb_combined >= 60 & Ssyk2_hpi_scb_combined <= 69 ~ 6.0,
                                                                  Ssyk2_hpi_scb_combined >= 70 & Ssyk2_hpi_scb_combined <= 79 ~ 7.0,
                                                                  Ssyk2_hpi_scb_combined >= 80 & Ssyk2_hpi_scb_combined <= 82 ~ 8.0,
                                                                  Ssyk2_hpi_scb_combined == 83         ~ 8.3,
                                                                  Ssyk2_hpi_scb_combined >= 84 & Ssyk2_hpi_scb_combined <= 89 ~ 8.4,
                                                                  Ssyk2_hpi_scb_combined >= 90 & Ssyk2_hpi_scb_combined <= 99 ~ 9.0,
                                                                  TRUE ~ NA_real_))

glimpse(hbt2_scb) %>%  count( Ssyk1_hpi_scb_combined)

hbt2_scb %>%  count( Ssyk1_hpi_scb_combined)

# hbt2_scb  <- hbt2_scb %>%  
#   select(LopNr, 
#          indexdate, 
#          Performed, 
#          Age, 
#          sex, 
#          Kon, 
#          Civil,
#          Kommun, 
#          Ssyk1_hpi_scb_combined,
#          Ssyk_hpi_scb_wb_hl, 
#          Ssyk_hpi_scb_hl)



haven::write_sav(hbt2_scb, here::here("data", "Covid_extra_scb_data_2021-03-19.sav"))

# write_csv(hbt2, here::here("data", "Covid_extra_scb_data_2021-03-19.csv"))


write_csv(hbt2_scb, here::here("data", "Covid_med_ssyk_2021-03-29.csv"))
