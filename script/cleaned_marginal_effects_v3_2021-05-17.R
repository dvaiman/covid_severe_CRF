

# 1 Reduce the x axis from 90% to 60%
# 2 Use the same amount of N in all of BMI, CRF etc,
# 3 low and high CRF - THen the two lines in each graph


# Libraries ---------------------------------------------------------------


library(tidyverse)
library(lubridate)
library(ragg)
library(svglite)
library(ggeffects)
library(ggside)
library(ggforce)
library(ggtext)
library(ggnewscale)
library(glue)
# partial effects plot
# https://rpubs.com/milesdwilliams15/328471
# https://strengejacke.github.io/ggeffects/articles/introduction_partial_residuals.html


# Set theme ---------------------------------------------------------------


## change global theme settings (for all following plots)
theme_set(theme_minimal(base_size = 12, base_family = "Roboto Condensed"))
## modify plot elements globally (for all following plots)
theme_update(
  axis.ticks = element_line(color = "grey92"),
  axis.ticks.length = unit(.5, "lines"),
  panel.grid.minor = element_blank(),
  legend.title = element_text(size = 12),
  legend.text = element_text(color = "grey30"),
  plot.title = element_text(size = 18, face = "bold"),
  plot.subtitle = element_text(size = 12, color = "grey30"),
  plot.caption = element_text(size = 9, margin = margin(t = 15))
)




# Load data ---------------------------------------------------------------


cc_match <- read_csv(here::here("data", "cc_match.csv")) %>% mutate(Performed_year_cat = case_when(Performed_year_2 < 2010 ~ "<2010",
                                                                                                   Performed_year_2 >= 2010 & Performed_year_2 <= 2014  ~ "2010-2014",
                                                                                                   Performed_year_2 > 2014  ~ ">2014"),
                                                                    Performed_year_cat = as.factor(Performed_year_cat),
                                                                    SevereCOVID = as.factor(SevereCOVID),
                                                                    sex = as.factor(sex),
                                                                    #TobaccoSmoking = as.factor(TobaccoSmoking),
                                                                    #StressOverall = as.factor(StressOverall),
                                                                    #Diet = as.factor(Diet),
                                                                    #Antal_tidigare_sjd = as.factor(Antal_tidigare_sjd),
                                                                    Utbildning = as.factor(Utbildning)
) %>% 
  filter(BMI_ny < 50)



# remove missing to get sam amount of n in all models ----------------------



# http://jonthegeek.com/2018/06/04/writing-custom-tidyverse-functions/




# Base adjusted with all n ------------------------------------------------

fitted_crf_all <- cc_match %>% 
  glm(SevereCOVID ~ EstVO2max_ml + Performed_year_cat + sex+ Age, 
      data = .,  family = binomial) %>% 
  ggeffect(terms = c("EstVO2max_ml [all]")) %>% 
  mutate(group2 = "Cardiorespiratory fitness",
         group3 = "adj_all")

fitted_bmi_all <-  cc_match %>% 
  glm(SevereCOVID ~ BMI_ny + Performed_year_cat + sex+ Age, 
      data = .,  family = binomial) %>% 
  ggeffect(terms = c("BMI_ny [all]")) %>% 
  mutate(group2 = "Body Mass Index",
         group3 = "adj_all")

fitted_bp_sys_all <-  cc_match %>% 
  glm(SevereCOVID ~ BloodPressureSystolic + Performed_year_cat +  sex+ Age, 
      data = .,  family = binomial) %>% 
  ggeffect(terms = c("BloodPressureSystolic [all]")) %>% 
  mutate(group2 = "Systolic blood pressure",
         group3 = "adj_all")

fitted_bp_dia_all <-  cc_match %>% 
  glm(SevereCOVID ~ BloodPressureDiastolic + Performed_year_cat +  sex+ Age, 
      data = .,  family = binomial) %>% 
  ggeffect(terms = c("BloodPressureDiastolic [all]")) %>% 
  mutate(group2 = "Diastolic blood pressure",
         group3 = "adj_all")

fitted_waist_all <-  cc_match %>%
  glm(SevereCOVID ~ WaistCircumference + Performed_year_cat +  sex+ Age, 
      data = .,  family = binomial) %>% 
  ggeffect(terms = c("WaistCircumference [all]")) %>% 
  mutate(group2 = "Waist circumference",
         group3 = "adj_all")


# Base adjusted -----------------------------------------------------------


fitted_crf <- cc_match %>% 
  drop_na(EstVO2max_ml, Performed_year_cat, sex, Age, 
          TobaccoSmoking, Utbildning, StressOverall, Diet, Antal_tidigare_sjd,
          BMI_ny) %>% 
  glm(SevereCOVID ~ EstVO2max_ml + Performed_year_cat + sex+ Age, 
      data = .,  family = binomial) %>% 
  ggeffect(terms = c("EstVO2max_ml [all]")) %>% 
  mutate(group2 = "Cardiorespiratory fitness",
         group3 = "adj")



fitted_bmi <-  cc_match %>% 
  drop_na(SevereCOVID, Performed_year_cat, sex, Age, 
          TobaccoSmoking, Utbildning, StressOverall, Diet, Antal_tidigare_sjd, 
          EstVO2max_ml) %>% 
  glm(SevereCOVID ~ BMI_ny + Performed_year_cat + sex+ Age, 
      data = .,  family = binomial) %>% 
  ggeffect(terms = c("BMI_ny [all]")) %>% 
  mutate(group2 = "Body Mass Index",
         group3 = "adj")

fitted_bp_sys <-  cc_match %>% 
  drop_na(SevereCOVID, Performed_year_cat, sex, Age, 
          TobaccoSmoking, Utbildning, StressOverall, Diet, Antal_tidigare_sjd, 
          EstVO2max_ml)%>% 
  glm(SevereCOVID ~ BloodPressureSystolic + Performed_year_cat +  sex+ Age, 
      data = .,  family = binomial) %>% 
  ggeffect(terms = c("BloodPressureSystolic [all]")) %>% 
  mutate(group2 = "Systolic blood pressure",
         group3 = "adj")

fitted_bp_dia <-  cc_match %>% 
  drop_na(SevereCOVID, Performed_year_cat, sex, Age, 
          TobaccoSmoking, Utbildning, StressOverall, Diet, Antal_tidigare_sjd, 
          EstVO2max_ml)%>% 
  glm(SevereCOVID ~ BloodPressureDiastolic + Performed_year_cat +  sex+ Age, 
      data = .,  family = binomial) %>% 
  ggeffect(terms = c("BloodPressureDiastolic [all]")) %>% 
  mutate(group2 = "Diastolic blood pressure",
         group3 = "adj")

fitted_waist <-  cc_match %>%
  drop_na(SevereCOVID, Performed_year_cat, sex, Age, 
          TobaccoSmoking, Utbildning, StressOverall, Diet, Antal_tidigare_sjd, 
          EstVO2max_ml) %>% 
  glm(SevereCOVID ~ WaistCircumference + Performed_year_cat +  sex+ Age, 
      data = .,  family = binomial) %>% 
  ggeffect(terms = c("WaistCircumference [all]")) %>% 
  mutate(group2 = "Waist circumference",
         group3 = "adj")

#fitted_income <-  cc_match %>% 
#  glm(SevereCOVID ~ CSFVI + Performed_year_cat +  sex+ Age, 
#      data = .,  family = binomial) %>% 
#  ggeffect(terms = c("CSFVI [all]")) %>% 
#  mutate(group2 = "Income")



# Semi adjusted -----------------------------------------------------------




fitted_crf_adj <-  cc_match %>% 
  drop_na(SevereCOVID, Performed_year_cat, sex, Age, 
          TobaccoSmoking, Utbildning, StressOverall, Diet, Antal_tidigare_sjd, 
          EstVO2max_ml) %>% 
  glm(SevereCOVID ~ EstVO2max_ml + Performed_year_cat + sex+ Age + 
        TobaccoSmoking + Utbildning + StressOverall + Diet + Antal_tidigare_sjd, 
      data = .,  family = binomial) %>% 
  ggeffect(terms = c("EstVO2max_ml [all]")) %>% 
  mutate(group2 = "Cardiorespiratory fitness",
         group3 = "adj2")

fitted_bmi_adj <-  cc_match %>% 
  drop_na(SevereCOVID, Performed_year_cat, sex, Age, 
          TobaccoSmoking, Utbildning, StressOverall, Diet, Antal_tidigare_sjd, 
          EstVO2max_ml) %>% 
  glm(SevereCOVID ~ BMI_ny + Performed_year_cat + sex+ Age+ 
        TobaccoSmoking + Utbildning + StressOverall + Diet + Antal_tidigare_sjd, 
      data = .,  family = binomial) %>% 
  ggeffect(terms = c("BMI_ny [all]")) %>% 
  mutate(group2 = "Body Mass Index",
         group3 = "adj2")

fitted_bp_sys_adj <-  cc_match %>% 
  drop_na(SevereCOVID, Performed_year_cat, sex, Age, 
          TobaccoSmoking, Utbildning, StressOverall, Diet, Antal_tidigare_sjd, 
          EstVO2max_ml) %>% 
  glm(SevereCOVID ~ BloodPressureSystolic + Performed_year_cat +  sex+ Age+ 
        TobaccoSmoking + Utbildning + StressOverall + Diet + Antal_tidigare_sjd, 
      data = .,  family = binomial) %>% 
  ggeffect(terms = c("BloodPressureSystolic [all]")) %>% 
  mutate(group2 = "Systolic blood pressure",
         group3 = "adj2")

fitted_bp_dia_adj <-  cc_match %>% 
  drop_na(SevereCOVID, Performed_year_cat, sex, Age, 
          TobaccoSmoking, Utbildning, StressOverall, Diet, Antal_tidigare_sjd, 
          EstVO2max_ml) %>% 
  glm(SevereCOVID ~ BloodPressureDiastolic + Performed_year_cat +  sex+ Age+ 
        TobaccoSmoking + Utbildning + StressOverall + Diet + Antal_tidigare_sjd, 
      data = .,  family = binomial) %>% 
  ggeffect(terms = c("BloodPressureDiastolic [all]")) %>% 
  mutate(group2 = "Diastolic blood pressure",
         group3 = "adj2")

fitted_waist_adj <-  cc_match %>% 
  drop_na(SevereCOVID, Performed_year_cat, sex, Age, 
          TobaccoSmoking, Utbildning, StressOverall, Diet, Antal_tidigare_sjd, 
          EstVO2max_ml) %>% 
  glm(SevereCOVID ~ WaistCircumference + Performed_year_cat +  sex+ Age+ 
        TobaccoSmoking + Utbildning + StressOverall + Diet + Antal_tidigare_sjd, 
      data = .,  family = binomial) %>% 
  ggeffect(terms = c("WaistCircumference [all]")) %>% 
  mutate(group2 = "Waist circumference",
         group3 = "adj2")

#fitted_income <-  cc_match %>% 
#  glm(SevereCOVID ~ CSFVI + Performed_year_cat +  sex+ Age+ TobaccoSmoking + SUN2000Niva_Old + StressOverall + Alcohol, 
#      data = .,  family = binomial) %>% 
#  ggeffect(terms = c("CSFVI [all]")) %>% 
#  mutate(group2 = "Income")


# fully adjusted ----------------------------------------------------------


fitted_crf_adj2 <-  cc_match %>% 
  drop_na(SevereCOVID, Performed_year_cat, sex, Age, 
          TobaccoSmoking, Utbildning, StressOverall, Diet, Antal_tidigare_sjd, 
          EstVO2max_ml) %>% 
  glm(SevereCOVID ~ EstVO2max_ml + Performed_year_cat + sex+ Age + 
        TobaccoSmoking + Utbildning + StressOverall + Diet + Antal_tidigare_sjd + 
        BMI_ny, 
      data = .,  family = binomial) %>% 
  ggeffect(terms = c("EstVO2max_ml [all]"), partial.residuals = TRUE) %>% 
  mutate(group2 = "Cardiorespiratory fitness",
         group3 = "adj3")

fitted_bmi_adj2 <-  cc_match %>% 
  drop_na(SevereCOVID, Performed_year_cat, sex, Age, 
          TobaccoSmoking, Utbildning, StressOverall, Diet, Antal_tidigare_sjd, 
          EstVO2max_ml) %>% 
  glm(SevereCOVID ~ BMI_ny + Performed_year_cat + sex+ Age+ 
        TobaccoSmoking + Utbildning + StressOverall + Diet + Antal_tidigare_sjd +
        EstVO2max_ml, #EstVO2max_Lmin + WeightKG, 
      data = .,  family = binomial) %>% 
  ggeffect(terms = c("BMI_ny [all]"), partial.residuals = TRUE) %>% 
  mutate(group2 = "Body Mass Index",
         group3 = "adj3")

fitted_bp_sys_adj2 <-  cc_match %>% 
  drop_na(SevereCOVID, Performed_year_cat, sex, Age, 
          TobaccoSmoking, Utbildning, StressOverall, Diet, Antal_tidigare_sjd, 
          EstVO2max_ml) %>% 
  glm(SevereCOVID ~ BloodPressureSystolic + Performed_year_cat +  sex+ Age+ 
        TobaccoSmoking + Utbildning + StressOverall + Diet + Antal_tidigare_sjd +
        EstVO2max_ml, #EstVO2max_Lmin + WeightKG,  
      data = .,  family = binomial) %>% 
  ggeffect(terms = c("BloodPressureSystolic [all]")) %>% 
  mutate(group2 = "Systolic blood pressure",
         group3 = "adj3")

fitted_bp_dia_adj2 <-  cc_match %>% 
  drop_na(SevereCOVID, Performed_year_cat, sex, Age, 
          TobaccoSmoking, Utbildning, StressOverall, Diet, Antal_tidigare_sjd, 
          EstVO2max_ml) %>% 
  glm(SevereCOVID ~ BloodPressureDiastolic + Performed_year_cat +  sex+ Age+ 
        TobaccoSmoking + Utbildning + StressOverall + Diet + Antal_tidigare_sjd +
        EstVO2max_ml, #EstVO2max_Lmin + WeightKG,  
      data = .,  family = binomial) %>% 
  ggeffect(terms = c("BloodPressureDiastolic [all]")) %>% 
  mutate(group2 = "Diastolic blood pressure",
         group3 = "adj3")


fitted_waist_adj2 <-  cc_match %>% 
  drop_na(SevereCOVID, Performed_year_cat, sex, Age, 
          TobaccoSmoking, Utbildning, StressOverall, Diet, Antal_tidigare_sjd, 
          EstVO2max_ml) %>% 
  glm(SevereCOVID ~ WaistCircumference + Performed_year_cat + sex + Age + 
        TobaccoSmoking + Utbildning + StressOverall + Diet + Antal_tidigare_sjd + 
        EstVO2max_ml, #EstVO2max_Lmin + WeightKG,  
      data = .,  family = binomial) %>% 
  ggeffect(terms = c("WaistCircumference [all]")) %>% 
  mutate(group2 = "Waist circumference",
         group3 = "adj3")


# Join differently adjusted model into one dataset ------------------------



bineded <-  bind_rows(fitted_crf_all, fitted_bmi_all, fitted_waist_all, fitted_bp_sys_all, fitted_bp_dia_all,
                      fitted_crf, fitted_bmi, fitted_waist, fitted_bp_sys, fitted_bp_dia,
                      fitted_crf_adj, fitted_bmi_adj, fitted_waist_adj, fitted_bp_sys_adj, fitted_bp_dia_adj,
                      fitted_crf_adj2, fitted_bmi_adj2, fitted_waist_adj2, fitted_bp_sys_adj2, fitted_bp_dia_adj2)



# Remove BMI > 40 from the dataset ----------------------------------------

bineded <- bineded %>% mutate(x = case_when(!x <= 40 & group2 == "Body Mass Index" ~ NA_real_,
                                            !x <= 120 & group2 == "Diastolic blood pressure" ~ NA_real_,
                                            !x <= 130 & group2 == "Waist circumference" ~ NA_real_,
                                                          TRUE ~ x)) %>% 
                                drop_na(x)



# data for boxplots -------------------------------------------------------

match <- cc_match %>% 
  mutate(BMI_ny = case_when(!BMI_ny <= 40 ~ NA_real_,
                       TRUE ~ BMI_ny),
         WaistCircumference = case_when(!WaistCircumference <= 130~ NA_real_,
                            TRUE ~ WaistCircumference),) %>% 
  drop_na(SevereCOVID, Performed_year_cat, sex, Age, 
          TobaccoSmoking, Utbildning, StressOverall, Diet, Antal_tidigare_sjd, 
          EstVO2max_ml) %>% 
  select(LopNr,
         SevereCOVID,
         EstVO2max_ml, 
         BMI_ny, 
         WaistCircumference, 
         BloodPressureSystolic,
         BloodPressureDiastolic, 
         #CSFVI
  ) %>% 
  pivot_longer(-c(LopNr, SevereCOVID),
               values_to = "x",
               names_to = "group2") %>% 
  mutate(group2 = case_when(group2 == "EstVO2max_ml" ~ "Cardiorespiratory fitness", 
                            group2 == "BMI_ny" ~ "Body Mass Index",
                            group2 == "WaistCircumference" ~ "Waist circumference",
                            group2 == "BloodPressureSystolic" ~ "Systolic blood pressure",
                            group2 == "BloodPressureDiastolic" ~ "Diastolic blood pressure"),
         #group2 == "CSFVI" ~ "Income"
         group2 = fct_relevel(group2, "Cardiorespiratory fitness", 
                              "Body Mass Index", 
                              "Waist circumference", 
                              "Diastolic blood pressure",
                              "Systolic blood pressure"),
         n=n()) #%>% 
# left_join(bineded) 



#predicted mean
bmi_lab <- bineded %>% filter(group2 == "Body Mass Index") %>%  
  filter(x==30 | x==29.997) %>% 
  group_by(group3) %>% 
  summarise(mean = round(mean(predicted, na.rm = TRUE),2)*100,
            mean_low = round(mean(conf.low, na.rm = TRUE),2)*100,
            mean_high = round(mean(conf.high, na.rm = TRUE),2)*100)

crf_lab <- bineded %>% filter(group2 == "Cardiorespiratory fitness") %>%  
  filter(x==32 | x==31.997) %>% 
  group_by(group3) %>% 
  summarise(mean = round(mean(predicted, na.rm = TRUE),2)*100,
            mean_low = round(mean(conf.low, na.rm = TRUE),2)*100,
            mean_high = round(mean(conf.high, na.rm = TRUE),2)*100)

sys_lab <- bineded %>% filter(group2 == "Systolic blood pressure") %>%  
  filter(x==140) %>% 
  group_by(group3) %>% 
  summarise(mean = round(mean(predicted, na.rm = TRUE),2)*100,
            mean_low = round(mean(conf.low, na.rm = TRUE),2)*100,
            mean_high = round(mean(conf.high, na.rm = TRUE),2)*100)

dia_lab <- bineded %>% filter(group2 == "Diastolic blood pressure") %>%  
  filter(x==90) %>% 
  group_by(group3) %>% 
  summarise(mean = round(mean(predicted, na.rm = TRUE),2)*100,
            mean_low = round(mean(conf.low, na.rm = TRUE),2)*100,
            mean_high = round(mean(conf.high, na.rm = TRUE),2)*100)

waist_lab <- bineded %>% filter(group2 == "Waist circumference") %>%  
  filter(x==88 ) %>% 
  group_by(group3) %>% 
  summarise(mean = round(mean(predicted, na.rm = TRUE),2)*100,
            mean_low = round(mean(conf.low, na.rm = TRUE),2)*100,
            mean_high = round(mean(conf.high, na.rm = TRUE),2)*100)

waist2_lab <- bineded %>% filter(group2 == "Waist circumference") %>% # view()
  filter(x==95|x==94.5|x==95.5) %>% 
  group_by(group3) %>% 
  summarise(mean = round(mean(predicted, na.rm = TRUE),2)*100,
            mean_low = round(mean(conf.low, na.rm = TRUE),2)*100,
            mean_high = round(mean(conf.high, na.rm = TRUE),2)*100)

waist3_lab <- bineded %>% filter(group2 == "Waist circumference") %>%  
  filter(x==102) %>% 
  group_by(group3) %>% 
  summarise(mean = round(mean(predicted, na.rm = TRUE),2)*100,
            mean_low = round(mean(conf.low, na.rm = TRUE),2)*100,
            mean_high = round(mean(conf.high, na.rm = TRUE),2)*100)



labels<-data.frame(
  group2=c("Body Mass Index", "Cardiorespiratory fitness", "Diastolic blood pressure",  "Systolic blood pressure", "Waist circumference"), # "Income",
  x=c(30, 32, 90,  140, 100),
  y=c(0.28, 0.20, 0.15,  0.12, 0.11),
  xmin = c(25,22, 76, 124, 89),
  xmax = c(35, 41, 104, 155, 110),
  meanx=c(26.6,33.3,80,129,95.3),
  a_xmin = c(38, 50, 108, 170, 125), # for rectangle annotation
  a_xmax = c(Inf,Inf,Inf,Inf,Inf ),# for rectangle annotation
  a_ymin = c(-Inf, -Inf,-Inf,-Inf,-Inf),# for rectangle annotation
  a_ymax =  c(Inf,Inf,Inf,Inf,Inf ),# for rectangle annotation
  label=c(glue("Probability at<br>
               BMI 30 (Obesity)<br>
               <span style = 'color:#eeb454;'>{bmi_lab$mean[1]}% ({bmi_lab$mean_low[1]}%-{bmi_lab$mean_high[1]}%)</span><br>
               <span style = 'color:#be4a48;'>{bmi_lab$mean[2]}% ({bmi_lab$mean_low[2]}%-{bmi_lab$mean_high[2]}%)</span><br>
               {bmi_lab$mean[3]}% ({bmi_lab$mean_low[3]}%-{bmi_lab$mean_high[3]}%)"), 
          glue("Probability at<br>
               32ml/kg/min (Low CRF)<br>
               <span style = 'color:#eeb454;'>{crf_lab$mean[1]}% ({crf_lab$mean_low[1]}%-{crf_lab$mean_high[1]}%)</span><br>
               <span style = 'color:#be4a48;'>{crf_lab$mean[2]}% ({crf_lab$mean_low[2]}%-{crf_lab$mean_high[2]}%)</span><br>
               {crf_lab$mean[3]}% ({crf_lab$mean_low[3]}%-{crf_lab$mean_high[3]}%)"), 
          glue("Probability at<br>
               90mmHg (High blood pressure)<br>
               <span style = 'color:#eeb454;'>{sys_lab$mean[1]}% ({sys_lab$mean_low[1]}%-{sys_lab$mean_high[1]}%)</span><br>
               <span style = 'color:#be4a48;'>{sys_lab$mean[2]}% ({sys_lab$mean_low[2]}%-{sys_lab$mean_high[2]}%)</span><br>
               {sys_lab$mean[3]}% ({sys_lab$mean_low[3]}%-{sys_lab$mean_high[3]}%)"),  
          glue("Probability at<br>
               140mmHg (High blood pressure)<br> 
               <span style = 'color:#eeb454;'>{dia_lab$mean[1]}% ({dia_lab$mean_low[1]}%-{dia_lab$mean_high[1]}%)</span><br>
               <span style = 'color:#be4a48;'>{dia_lab$mean[2]}% ({dia_lab$mean_low[2]}%-{dia_lab$mean_high[2]}%)</span><br>
               {dia_lab$mean[3]}% ({dia_lab$mean_low[3]}%-{dia_lab$mean_high[3]}%)"), 
          glue("Probability at<br>
               95cm (Abdominal obesity)<br>
               <span style = 'color:#eeb454;'>{waist2_lab$mean[1]}% ({waist2_lab$mean_low[1]}%-{waist2_lab$mean_high[1]}%)</span><br>
               <span style = 'color:#be4a48;'>{waist2_lab$mean[2]}% ({waist2_lab$mean_low[2]}%-{waist2_lab$mean_high[2]}%)</span><br>
               {waist2_lab$mean[3]}% ({waist2_lab$mean_low[3]}%-{waist2_lab$mean_high[3]}%)"))) %>% 
  mutate(group2 = fct_relevel(group2, "Cardiorespiratory fitness", 
                              "Body Mass Index", 
                              "Waist circumference", 
                              "Diastolic blood pressure",
                              "Systolic blood pressure"))


rectangle<-data.frame(
  group2=c("Body Mass Index", "Cardiorespiratory fitness", "Diastolic blood pressure",  "Systolic blood pressure", "Waist circumference"), # "Income",
  a_xmin = c(38, 49.5, 107.5, 170, 125), # for rectangle annotation
  a_xmax = c(Inf,Inf,Inf,Inf,Inf ),# for rectangle annotation
  a_ymin = c(-Inf, -Inf,-Inf,-Inf,-Inf),# for rectangle annotation
  a_ymax =  c(Inf,Inf,Inf,Inf,Inf )# for rectangle annotation
) %>% 
  mutate(group2 = fct_relevel(group2, "Cardiorespiratory fitness", 
                              "Body Mass Index", 
                              "Waist circumference", 
                              "Diastolic blood pressure",
                              "Systolic blood pressure"))


bineded %>% tibble() %>% 
  mutate(risk=case_when(group2 == "Body Mass Index" & x > 30 ~ "risk",
                        group2 == "Body Mass Index" & x <= 30 ~ "no risk",
                        group2 == "Cardiorespiratory fitness" & x <= 30 ~ "risk",
                        group2 == "Cardiorespiratory fitness" & x > 30 ~ "no risk",
                        group2 == "Diastolic blood pressure" & x > 90 ~ "risk",
                        group2 == "Diastolic blood pressure" & x <= 90 ~ "no risk",
                        group2 == "Systolic blood pressure" & x > 140 ~ "risk",
                        group2 == "Systolic blood pressure" & x <= 140 ~ "no risk",
                        group2 == "Waist circumference" & x > 100 ~ "risk",
                        group2 == "Waist circumference" & x <= 100 ~ "no risk",
                        #group2 == "Income" & x > 2000000 ~ "risk",
                        #group2 == "Income" & x <= 2000000 ~ "no risk"
  ),
  #         order=case_when(group2 == "BMI" & x > 30 ~ 2,
  #                        group2 == "Cardiorespiratory fitness"  ~ 1,
  #                        group2 == "Diastolic blood pressure"  ~ 4,
  #                        group2 == "Systolic blood pressure" ~ 5,
  #                        group2 == "Waist circumference" ~ 3
  #         ),
  #         group2 = fct_reorder(group2, order)
  group2 = fct_relevel(group2, "Cardiorespiratory fitness", 
                       "Body Mass Index", 
                       "Waist circumference", 
                       "Diastolic blood pressure",
                       "Systolic blood pressure")
  ) %>%  
  arrange(group2) %>% 
  ggplot(aes(x, predicted)) +
  #geom_smooth(se=FALSE) +
  
  #geom_segment(data=labels, aes(x = x, xend = x, y = -Inf, yend = 0.51), color = "grey60") +
  #geom_segment(data=labels, aes(x = xmin, xend = xmax, y = 0.43, yend = 0.43), color = "grey60") +
  #geom_vline(data=labels, aes(xintercept = x), color = "grey60") +
  #geom_richtext(data = labels, aes(x = x, y = 0.50, label = label),
  #              size =3.25,
  #              label.colour = "transparent",
  #              family = "Roboto Condensed") +
  geom_rect(data = rectangle , aes(xmin = a_xmin,
                                   xmax = a_xmax,
                                   ymin = a_ymin,
                                   ymax = a_ymax),
            fill="grey80",
            alpha = 0.2,
            inherit.aes = FALSE)  +
  geom_line(aes( group = group3,color = group3),size = .8, show.legend = FALSE) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high,  group = group3, fill = group3), alpha = .1, show.legend = FALSE) +
  #geom_mark_circle(data = labels, aes(x,y, label = label), 
  #                 color="transparent", 
  #                 con.cap = 0, 
  #                 con.arrow = grid::arrow(),
  #                 con.colour = "grey50",
  #                 label.buffer = unit(15, 'mm'),
  #                 size = 3,
  #                 label.colour = "grey50",
  #                 label.fontsize = 8)+
  #geom_xsidedensity(aes(y=stat(density))) +
  #geom_xsidedensity(data = match, aes(y=stat(density), xcolor = SevereCOVID, xfill = SevereCOVID), alpha =0.3) +
coord_cartesian(clip = "off") +
  scale_y_continuous(labels = c("10%","30%","50%", "70%"), 
                     breaks = c(.1,.3,.5, .7)#,
                     #limits = c(0,2)
                     ) + # scales::percent
  scale_color_manual(values = c("#509b9a", "grey20", "#e1650a",  "#863338")  # values = scico::scico(4, begin = .2, palette = "berlin"), labels =c("setosa", "versicolor", "virginica")
  )+
  scale_fill_manual(values = c("#509b9a", "grey20", "#e1650a",  "#863338")  #values = scico::scico(4, begin = .2, palette = "berlin")
                    )+ 
  ggnewscale::new_scale_color() + 
  geom_xsideboxplot(data = match, aes(x=x, y=10, color=SevereCOVID),show.legend = FALSE,
                    orientation = "y") +
  #geom_xsideviolin(data = match, aes(x=x, y=n, color=SevereCOVID)) +
  ggside(x.pos = "bottom") +
  scale_color_manual(values = c("grey70", "grey30"))+
  facet_wrap(~group2, ncol = 3, scales = "free_x") +
  #guides(color = guide_legend(title="Model",  
  #                            keywidth = unit(2, "cm"),
  #                            keyheight = unit(0.5, "cm"), 
  #                            title.position  = 'top', 
  #                            title.hjust = .5,
  #                            barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))+
  labs(#title = "Marginal effect plot for severe Covid",
    #subtitle = "M1 adjusted for age and sex, M2 also for lifestylefactor, M3 also for BMI or CRF. \nBoxplots is cases and non cases",
    x = " ",
    y = " Predicted probability of severe Covid"
  ) +
  theme(legend.position="top",
        strip.text = element_text(face = "bold", size = 11),
        plot.title.position = "plot",
        ggside.panel.scale.x = .2,
        axis.title.y = element_text(color = "grey60"),
        axis.title.y.left = element_text(margin = margin(15, 15, 15, 15)))


ggsave(here::here("marg_plot_v4.svg"), width = 10, height = 6)

# plot for extracting legend

WaistCircumference + Performed_year_cat + sex + Age + 
  TobaccoSmoking + Utbildning + StressOverall + Diet + Antal_tidigare_sjd + 
  EstVO2max_ml, #EstVO2max_Lmin + WeightKG, 

# education, diet, stress, smoking, earlier disease

le <- bineded %>% tibble() %>% 
  mutate(risk=case_when(group2 == "Body Mass Index" & x > 30 ~ "risk",
                        group2 == "Body Mass Index" & x <= 30 ~ "no risk",
                        group2 == "Cardiorespiratory fitness" & x <= 30 ~ "risk",
                        group2 == "Cardiorespiratory fitness" & x > 30 ~ "no risk",
                        group2 == "Diastolic blood pressure" & x > 90 ~ "risk",
                        group2 == "Diastolic blood pressure" & x <= 90 ~ "no risk",
                        group2 == "Systolic blood pressure" & x > 140 ~ "risk",
                        group2 == "Systolic blood pressure" & x <= 140 ~ "no risk",
                        group2 == "Waist circumference" & x > 100 ~ "risk",
                        group2 == "Waist circumference" & x <= 100 ~ "no risk",
                        #group2 == "Income" & x > 2000000 ~ "risk",
                        #group2 == "Income" & x <= 2000000 ~ "no risk"
  ),
  group2 = fct_relevel(group2, "Cardiorespiratory fitness", 
                       "Body Mass Index", 
                       "Waist circumference", 
                       "Diastolic blood pressure",
                       "Systolic blood pressure")
  ) %>%  
  ggplot(aes(x, predicted)) +
  geom_line(aes( group = group3,color = group3), show.legend = TRUE) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high,  group = group3, fill = group3), alpha = .2, show.legend = T) +
  scale_y_continuous(labels = scales::percent, breaks = c(.1,.3,.5,.7,.9)) +
  scale_color_manual(values = c("grey20","#509b9a",  "#e1650a",  "#863338") #values = scico::scico(3, begin = .2, palette = "lajolla"), labels =c("Adjusted for sex, age and performed year", "+ education, diet, stress, smoking and previous disease", "+ BMI or CRF")
  ) +
  scale_fill_manual(  values = c("grey20","#509b9a",  "#e1650a",  "#863338" )# values = scico::scico(3, begin = .2, palette = "lajolla"), labels =c("Adjusted for sex, age and performed year", "+ education, diet, stress, smoking and previous disease", "+ BMI or CRF")
                      )+
  guides(color = guide_legend(title="",  
                              keywidth = unit(2, "cm"),
                              keyheight = unit(0.5, "cm"), 
                              title.position  = 'top', 
                              title.hjust = .5,
                              barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')),
         fill = guide_legend(title="",  
                             keywidth = unit(2, "cm"),
                             keyheight = unit(0.5, "cm"), 
                             title.position  = 'top', 
                             title.hjust = .5,
                             barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')) )


le2 <- ggplot(data = match) +
  geom_boxplot( aes(x=x, y=10, color=SevereCOVID),show.legend = TRUE,
                orientation = "y") +
  scale_color_manual(values = c("grey70", "grey30"), labels =c("Non-cases", "Cases")) +
  guides(color = guide_legend(title="Boxplot",  
                              reverse = TRUE,
                              keywidth = unit(1, "cm"),
                              keyheight = unit(0.5, "cm"), 
                              title.position  = 'top', 
                              title.hjust = .5,
                              barwidth = unit(10, 'lines'), barheight = unit(.5, 'lines')))



# https://stackoverflow.com/questions/12041042/how-to-plot-just-the-legends-in-ggplot2
library(ggpubr)
# Extract the legend. Returns a gtable
leg <- get_legend(le)
leg2 <- get_legend(le2)
# Convert to a ggplot and print
leg <- as_ggplot(leg)
leg2 <- as_ggplot(leg2)

library(cowplot)

ggdraw() + 
  draw_plot(leg, .45, .3, .7, .15) +
  draw_plot(leg2, .45, .15, .7, .15)


ggsave(here::here("legend2.svg"), width = 8, height = 6)

############################################################################
############################################################################
############################################################################
############################################################################








bineded %>% tibble() %>% filter(!group3 == "adj_all") %>% 
  mutate(risk=case_when(group2 == "Body Mass Index" & x > 30 ~ "risk",
                        group2 == "Body Mass Index" & x <= 30 ~ "no risk",
                        group2 == "Cardiorespiratory fitness" & x <= 30 ~ "risk",
                        group2 == "Cardiorespiratory fitness" & x > 30 ~ "no risk",
                        group2 == "Diastolic blood pressure" & x > 90 ~ "risk",
                        group2 == "Diastolic blood pressure" & x <= 90 ~ "no risk",
                        group2 == "Systolic blood pressure" & x > 140 ~ "risk",
                        group2 == "Systolic blood pressure" & x <= 140 ~ "no risk",
                        group2 == "Waist circumference" & x > 100 ~ "risk",
                        group2 == "Waist circumference" & x <= 100 ~ "no risk",
                        #group2 == "Income" & x > 2000000 ~ "risk",
                        #group2 == "Income" & x <= 2000000 ~ "no risk"
  ),
  #         order=case_when(group2 == "BMI" & x > 30 ~ 2,
  #                        group2 == "Cardiorespiratory fitness"  ~ 1,
  #                        group2 == "Diastolic blood pressure"  ~ 4,
  #                        group2 == "Systolic blood pressure" ~ 5,
  #                        group2 == "Waist circumference" ~ 3
  #         ),
  #         group2 = fct_reorder(group2, order)
  group2 = fct_relevel(group2, "Cardiorespiratory fitness", 
                       "Body Mass Index", 
                       "Waist circumference", 
                       "Diastolic blood pressure",
                       "Systolic blood pressure")
  ) %>%  
  arrange(group2) %>% 
  ggplot(aes(x, predicted)) +
  #geom_smooth(se=FALSE) +
  
  #geom_segment(data=labels, aes(x = x, xend = x, y = -Inf, yend = 0.51), color = "grey60") +
  #geom_segment(data=labels, aes(x = xmin, xend = xmax, y = 0.43, yend = 0.43), color = "grey60") +
  #geom_vline(data=labels, aes(xintercept = x), color = "grey60") +
  #geom_richtext(data = labels, aes(x = x, y = 0.50, label = label),
  #              size =3.25,
  #              label.colour = "transparent",
  #              family = "Roboto Condensed") +
  geom_rect(data = rectangle , aes(xmin = a_xmin,
                                   xmax = a_xmax,
                                   ymin = a_ymin,
                                   ymax = a_ymax),
            fill="grey80",
            alpha = 0.2,
            inherit.aes = FALSE)  +
  geom_line(aes( group = group3,color = group3),size=.8, show.legend = FALSE) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high,  group = group3, fill = group3), alpha = .1, show.legend = FALSE) +
  #geom_mark_circle(data = labels, aes(x,y, label = label), 
  #                 color="transparent", 
  #                 con.cap = 0, 
  #                 con.arrow = grid::arrow(),
  #                 con.colour = "grey50",
  #                 label.buffer = unit(15, 'mm'),
  #                 size = 3,
  #                 label.colour = "grey50",
  #                 label.fontsize = 8)+
  #geom_xsidedensity(aes(y=stat(density))) +
  #geom_xsidedensity(data = match, aes(y=stat(density), xcolor = SevereCOVID, xfill = SevereCOVID), alpha =0.3) +
coord_cartesian(clip = "off") +
  scale_y_continuous(labels = c("10%","20%","30%","40%"), 
                     breaks = c(.1,.2,.3,.4)#,
                     #limits = c(0,2)
  ) + # scales::percent
  scale_color_manual(values = c("#509b9a",  "#e1650a",  "#863338")  # values = scico::scico(4, begin = .2, palette = "berlin"), labels =c("setosa", "versicolor", "virginica")
  )+
  scale_fill_manual(values = c("#509b9a",  "#e1650a",  "#863338")) + #values = scico::scico(4, begin = .2, palette = "berlin"))+ 
  ggnewscale::new_scale_color() + 
  geom_xsideboxplot(data = match, aes(x=x, y=10, color=SevereCOVID),show.legend = FALSE,
                    orientation = "y") +
  #geom_xsideviolin(data = match, aes(x=x, y=n, color=SevereCOVID)) +
  ggside(x.pos = "bottom") +
  scale_color_manual(values = c("grey70", "grey30"))+
  facet_wrap(~group2, ncol = 3, scales = "free_x") +
  #guides(color = guide_legend(title="Model",  
  #                            keywidth = unit(2, "cm"),
  #                            keyheight = unit(0.5, "cm"), 
  #                            title.position  = 'top', 
  #                            title.hjust = .5,
  #                            barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))+
  labs(#title = "Marginal effect plot for severe Covid",
    #subtitle = "M1 adjusted for age and sex, M2 also for lifestylefactor, M3 also for BMI or CRF. \nBoxplots is cases and non cases",
    x = " ",
    y = " Predicted probability of severe Covid"
  ) +
  theme(legend.position="top",
        strip.text = element_text(face = "bold", size = 11),
        plot.title.position = "plot",
        ggside.panel.scale.x = .2,
        axis.title.y = element_text(color = "grey60"),
        axis.title.y.left = element_text(margin = margin(15, 15, 15, 15)))


ggsave(here::here("marg_plot_v5.svg"), width = 10, height = 6)





























































############################################################################
############################################################################
############################################################################
############################################################################
# Marginal effects interaction plots - in groups of year category

fitted_crf <-  cc_match %>% 
  glm(SevereCOVID ~ EstVO2max_ml * Performed_year_cat + sex+ Age, 
      data = .,  family = binomial) %>% 
  ggeffect(terms = c("EstVO2max_ml [all]", "Performed_year_cat")) %>% 
  mutate(group2 = "CRF")

fitted_bmi <-  cc_match %>% 
  glm(SevereCOVID ~ BMI_ny * Performed_year_cat + sex+ Age, 
      data = .,  family = binomial) %>% 
  ggeffect(terms = c("BMI_ny [all]", "Performed_year_cat")) %>% 
  mutate(group2 = "BMI")

fitted_bp_sys <-  cc_match %>% 
  glm(SevereCOVID ~ BloodPressureSystolic*Performed_year_cat +  sex+ Age, 
      data = .,  family = binomial) %>% 
  ggeffect(terms = c("BloodPressureSystolic [all]", "Performed_year_cat")) %>% 
  mutate(group2 = "Diastolic BP")

fitted_bp_dia <-  cc_match %>% 
  glm(SevereCOVID ~ BloodPressureSystolic*Performed_year_cat +  sex+ Age, 
      data = .,  family = binomial) %>% 
  ggeffect(terms = c("BloodPressureSystolic [all]", "Performed_year_cat")) %>% 
  mutate(group2 = "Systolic BP")

fitted_waist <-  cc_match %>% 
  glm(SevereCOVID ~ WaistCircumference*Performed_year_cat +  sex+ Age, 
      data = .,  family = binomial) %>% 
  ggeffect(terms = c("WaistCircumference [all]", "Performed_year_cat")) %>% 
  mutate(group2 = "Waist circumference")

fitted_income <-  cc_match %>% 
  glm(SevereCOVID ~ CSFVI*Performed_year_cat +  sex+ Age, 
      data = .,  family = binomial) %>% 
  ggeffect(terms = c("CSFVI [all]", "Performed_year_cat")) %>% 
  mutate(group2 = "Income")

bineded <-  bind_rows(fitted_crf, fitted_bmi, fitted_waist, fitted_bp_sys, fitted_bp_dia, fitted_income)

bineded %>% 
  ggplot(aes(x, predicted)) +
  #geom_smooth(se=FALSE) +
  geom_line(aes( group = group, color = group)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high,  group = group, fill = group), alpha = .2, show.legend = FALSE) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = scico::scico(3, begin = .2, palette = "lajolla"))+
  scale_fill_manual(values = scico::scico(3, begin = .2, palette = "lajolla"))+
  facet_wrap(~group2, ncol = 2, scales = "free") +
  guides(color = guide_legend(title="Performed year",  
                              keywidth = unit(2, "cm"),
                              keyheight = unit(0.5, "cm"), 
                              title.position  = 'top', 
                              title.hjust = .5,
                              barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))+
  labs(title = "Marginal effect plot for severe Covid",
       subtitle = "Adjusted for Age, gender and year performed",
       x = " ",
       y = " Predicted probabilities of severe Covid"
  ) +  
  theme(legend.position="top",
        plot.title.position = "plot")


cc_match %>% 
  ggplot(aes(EstVO2max_ml)) +
  geom_violin(aes(y=SevereCOVID, color = SevereCOVID, group = SevereCOVID)) +
  geom_boxplot(aes(y=SevereCOVID, color = SevereCOVID, group = SevereCOVID)) +
  geom_point(aes(y=SevereCOVID, color = SevereCOVID, group = SevereCOVID),position = position_jitter(), alpha = 0.2) 



# adjuste model
cc_match %>% 
  glm(SevereCOVID ~ BMI_ny + Performed_year_cat + sex+ Age+ 
        TobaccoSmoking + Utbildning + StressOverall + Diet + Antal_tidigare_sjd +
        EstVO2max_ml, #EstVO2max_Lmin + WeightKG, 
      data = .,  family = binomial) %>% 
  broom::tidy()
