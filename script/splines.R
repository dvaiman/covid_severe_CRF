
https://blogs.uoregon.edu/rclub/2016/04/14/plotting-logistic-regressions-part-3/

library(broom)
library(tidyverse)
library(ragg)


hbt2 <- read_csv(here::here("data", "covid2_rensad.csv"))



# fix vars
hbt2 <- hbt2 %>%   mutate(
                           EkB_rel_VO2=na_if(EkB_rel_VO2 ,0) ,
                           Astrand_rel_VO2 = coalesce(Astrand_rel_VO2, EkB_rel_VO2),
                           mean_arterial_bp = (BloodPressureSystolic + BloodPressureDiastolic*2) /3,
                           BMI = WeightKG / (HeightCM/100)^2,
                           BMI = case_when(BMI > 70 ~ NA_real_,
                                           TRUE ~ BMI))


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







hbt2 %>% select(BMI) %>% arrange(desc( BMI)) 


  select(mean_arterial_bp, BloodPressureSystolic, BloodPressureDiastolic)  %>%
  ggplot(aes(mean_arterial_bp, BloodPressureDiastolic)) + 
  geom_point(aes(mean_arterial_bp, BloodPressureDiastolic), alpha = 0.1) +
  geom_point(aes(mean_arterial_bp, BloodPressureSystolic), alpha = 0.1, color= "pink")
  geom_histogram()
  
  hbt2 %>% select(BMI, Astrand_rel_VO2, WaistCircumference, SevereCOVID) %>%
    group_by(SevereCOVID) %>%  
    mutate(norm_BMI = normalize3(BMI),
           norm_Astrand_rel_VO2 = normalize3(Astrand_rel_VO2),
           norm_WaistCircumference = normalize3(WaistCircumference)) %>% 
    skimr::skim() %>% 
    mutate_if(is.numeric, round, 2) %>% gt::gt()

  
ggformula
geom_spline


https://stackoverflow.com/questions/43792902/hazard-ratio-plot-with-confidence-waist-in-ggplot2


glimpse(hbt2)




ggplot(hbt2, aes(x=Astrand_rel_VO2, y=SevereCOVID, group=as.factor(Performed_year_cat), color=as.factor(Performed_year_cat)))+
  geom_jitter(width = 0, height = 0.05, alpha = 0.1) +
  stat_smooth(method = "glm", method.args = list(family = binomial))



sir_covid


stratifiera : 
Performed_year_cat


mean arterial bloodpressure
crf
midjemått
BMI

justerat ålder, kön.

hbt2 %>% mutate(SevereCOVID = as.factor(SevereCOVID),
                sex = as.factor(sex),
                Performed_year_cat = as.factor(Performed_year_cat)) %>% 
mutate(fit = glm(as.factor(SevereCOVID) ~ Astrand_rel_VO2+ Age + sex, data = .))

fit = glm(as.factor(SevereCOVID) ~ Astrand_rel_VO2+ Age + sex, data = .)

#ggpredict
https://cran.r-project.org/web/packages/ggiraphExtra/vignettes/ggPredict.html

# create datasets
hbt_crf <- hbt2 %>%
  select(LopNr, indexdate, Astrand_rel_VO2, Performed_year_cat, sex, Age, SevereCOVID) %>% 
  drop_na() %>% 
  mutate(#SevereCOVID = as.factor(SevereCOVID),
    sex = as.factor(sex),
    Performed_year_cat = as.factor(Performed_year_cat),
    di = ifelse(Astrand_rel_VO2 < 32, "risk", "norisk")) 

hbt_bp <- hbt2 %>%
  select(LopNr, indexdate, mean_arterial_bp, Performed_year_cat, sex, Age, SevereCOVID) %>% 
  drop_na() %>% 
  mutate(#SevereCOVID = as.factor(SevereCOVID),
    sex = as.factor(sex),
    Performed_year_cat = as.factor(Performed_year_cat),
    di = ifelse(mean_arterial_bp > 60, "risk", "norisk")) 

hbt_waist <- hbt2 %>%
  select(LopNr, indexdate, WaistCircumference, Performed_year_cat, sex, Age, SevereCOVID) %>% 
  drop_na() %>% 
  mutate(#SevereCOVID = as.factor(SevereCOVID),
    sex = as.factor(sex),
    Performed_year_cat = as.factor(Performed_year_cat),
    di = ifelse(WaistCircumference > 90, "risk", "norisk")) 

hbt_bmi <- hbt2 %>%
  select(LopNr, indexdate, BMI, Performed_year_cat, sex, Age, SevereCOVID) %>% 
  drop_na() %>% 
  mutate(#SevereCOVID = as.factor(SevereCOVID),
    sex = as.factor(sex),
    Performed_year_cat = as.factor(Performed_year_cat),
    di = ifelse(BMI > 30, "risk", "norisk")) 




# Normalize  
normalize3 <- function(x, na.rm = TRUE) (x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)
DF %>% mutate_at('avg', normalize3) #manual version of mean = 0 sd = 1

# to be added to augment function
# interval = c("none", "confidence", "prediction")
hbt_crf %>% 
  mutate(Astrand_rel_VO2 = normalize3(Astrand_rel_VO2)) %>% select(Astrand_rel_VO2)
  glm(SevereCOVID ~ Astrand_rel_VO2  + sex+ Age, 
      data = .,  family = binomial) %>% 
  tidy(conf.int = TRUE)
  augment(data = hbt_crf, type.predict = "link", 
          #type.residuals = c("deviance"), 
          #se_fit = TRUE, 
          conf.int = TRUE,
          interval = "prediction",
          conf.level = 0.95)

# model and fitted values
fitted_crf <-  hbt_crf %>% 
  #mutate(Astrand_rel_VO2 = normalize3(Astrand_rel_VO2)) %>% 
  glm(SevereCOVID ~ Astrand_rel_VO2  + sex+ Age, 
      data = .,  family = binomial) %>% 
  augment(data = hbt_crf, type.predict = "link", type.residuals = c("deviance"), se_fit = TRUE) %>% 
           # mutate(Astrand_rel_VO2 = normalize3(Astrand_rel_VO2)), type.predict = "link", type.residuals = c("deviance"), se_fit = TRUE, interval = "confidence") %>% # change link to response for exp
  mutate(died = ifelse(SevereCOVID  == 1, Astrand_rel_VO2, NA),
         survived     = ifelse(SevereCOVID  == 0, Astrand_rel_VO2, NA),
         lower = exp(.fitted - 1.96*.se.fit),
         upper = exp(.fitted + 1.96*.se.fit),
         .fitted = exp(.fitted)) %>% 
 select(LopNr, indexdate, Astrand_rel_VO2, .fitted, Age, SevereCOVID, .se.fit, lower, upper, died, survived, di,Performed_year_cat) %>% 
  mutate(group = "crf") %>% 
  rename("values" = Astrand_rel_VO2)


fitted_bp <-  hbt_bp %>% 
  glm(SevereCOVID ~ mean_arterial_bp  + sex+ Age, 
      data = .,  family = binomial) %>% 
  augment(data = hbt_bp, type.predict = "link", type.residuals = c("deviance"), se_fit = TRUE) %>% # change link to response for exp
  mutate(died = ifelse(SevereCOVID  == 1, mean_arterial_bp, NA),
         survived     = ifelse(SevereCOVID  == 0, mean_arterial_bp, NA),
         lower = exp(.fitted - 1.96*.se.fit),
         upper = exp(.fitted + 1.96*.se.fit),
         .fitted = exp(.fitted)) %>% 
  select(LopNr, indexdate, mean_arterial_bp, .fitted, Age, SevereCOVID, .se.fit, lower, upper, died, survived, di, Performed_year_cat) %>% 
  mutate(group = "bp")%>% 
  rename("values" = mean_arterial_bp)

fitted_waist <-  hbt_waist %>% 
  glm(SevereCOVID ~ WaistCircumference  + sex+ Age, 
      data = .,  family = binomial) %>% 
  augment(data = hbt_waist, type.predict = "link", type.residuals = c("deviance"), se_fit = TRUE) %>% # change link to response for exp
  mutate(died = ifelse(SevereCOVID  == 1, WaistCircumference, NA),
         survived     = ifelse(SevereCOVID  == 0, WaistCircumference, NA),
         lower = exp(.fitted - 1.96*.se.fit),
         upper = exp(.fitted + 1.96*.se.fit),
         .fitted = exp(.fitted)) %>% 
  select(LopNr, indexdate, WaistCircumference, .fitted, Age, SevereCOVID, .se.fit, lower, upper, died, survived, di, Performed_year_cat) %>% 
  mutate(group = "waist")%>% 
  rename("values" = WaistCircumference)

fitted_bmi <-  hbt_bmi %>% 
  #mutate(BMI = normalize3(BMI)) %>% 
  glm(SevereCOVID ~ BMI  + sex+ Age, 
      data = .,  family = binomial) %>% 
  augment(data = hbt_bmi, type.predict = "link", type.residuals = c("deviance"), se_fit = TRUE) %>% 
         #   mutate(BMI = normalize3(BMI)), type.predict = "link", type.residuals = c("deviance"), se_fit = TRUE) %>% # change link to response for exp
  mutate(died = ifelse(SevereCOVID  == 1, BMI, NA),
         survived     = ifelse(SevereCOVID  == 0, BMI, NA),
         lower = exp(.fitted - 1.96*.se.fit),
         upper = exp(.fitted + 1.96*.se.fit),
         .fitted = exp(.fitted)) %>% 
  select(LopNr, indexdate, BMI, .fitted, Age, SevereCOVID, .se.fit, lower, upper, died, survived, di, Performed_year_cat)%>% 
  mutate(group = "bmi")%>% 
  rename("values" = BMI)


fitted <- fitted_crf %>% bind_rows(fitted_bp, fitted_waist, fitted_bmi)



p1<- fitted %>% 
  ggplot(aes(values, .fitted, group = group)) +
  stat_smooth(color = "darkorange", method="glm", method.args=list(family=binomial), level=0.95) +
  #geom_rug(aes(x = died), color = "indianred", sides = "t", alpha = 0.5, length = unit(0.05, "npc")) +
  #geom_rug(aes(x = survived), color = "steelblue3",outside = FALSE, sides = "b", alpha = 0.2, length = unit(0.05, "npc")) +
  scale_y_continuous(expand = c(0,0)
   # limits =  c(-0.01, 0.14)
    ) +
  #coord_cartesian(clip = "off") +
  #geom_boxplot(inherit.aes = F, aes(y = 0.025, x=values, group = SevereCOVID, color = as.character(SevereCOVID)), orientation = "y", width = 0.02,size = 0.2, outlier.size = .5) +
  labs(title = "Predicted probability for different lifestyle indicators"#, 
       #subtitle = "Red = severe covid, Blue = no covid"
       )+
  facet_wrap(~ group, scales = "free", ncol = 4) +
  theme_minimal() +
  theme(legend.position = "none",
        #strip.text = element_blank()
        )

install.packages("distributional")
library(ggdist)


p2<- fitted %>% 
  ggplot(aes(values)) +
  geom_density() +
  #geom_boxplot(width = 0.5) +
  facet_wrap(~ group, scales = "free_x", ncol = 4) +
  theme_void() +
  theme(legend.position = "none",
        strip.text = element_blank())

p3<- fitted %>% 
  group_by(group) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  ggplot(aes(x=values)) +
  #geom_density() +
  #geom_violin(aes(y=0), orientation = "y") +
  geom_boxplot(aes(group = SevereCOVID, color = as.character(SevereCOVID)), orientation = "y", width = 0.2,size = 0.2, outlier.size = .5) +
  facet_wrap(~ group, scales = "free", ncol = 4) +
  scale_color_manual(values = c("steelblue3","indianred")) +
  theme_void() +
  theme(legend.position = "none",
        #strip.text = element_blank()
        )

library(patchwork)

p3/p2/p1 + 
  plot_layout(heights = c(0.5, 1, 4)) +
  plot_annotation(title = "Predicted probability for different lifestyle indicators", 
                  subtitle = "Red = severe covid, Blue = no covid")


fitted %>% 
  ggplot(aes(values)) +
stat_dist_halfeye(
  aes(dist = dist_student_t(df = df.residual(fitted), mu = .fitted, sigma = .se.fit)), 
  scale = .5
)+
  facet_wrap(~ group, scales = "free_x", ncol = 4) +
  theme_minimal()

augment(mod_log, type.predict = 'response', newdata = at_vals, se_fit = TRUE) %>%
  mutate(lower = .fitted - 1.96*.se.fit,
         upper = .fitted + 1.96*.se.fit) #%>%
  #mutate_if(is.numeric, ~ round(.,3))

type.predict = c("link", "response", "terms"),
type.residuals = c("deviance", "pearson"),
se_fit = FALSE,


predict(model, interval = "prediction")


fitted_vals %>% 
  ggplot(aes(Astrand_rel_VO2, .fitted, group = Performed_year_cat , color = Performed_year_cat  #, group = sex, color = sex
             )) +
  stat_smooth(data = fitted_crf, aes(Astrand_rel_VO2, .fitted), method="glm", method.args=list(family=binomial), color = "blue", inherit.aes = FALSE) +
stat_smooth(data = fitted_bp, aes(mean_arterial_bp, .fitted), method="glm", method.args=list(family=binomial), color = "red", inherit.aes = FALSE) +
stat_smooth(data = fitted_waist, aes(WaistCircumference, .fitted), method="glm", method.args=list(family=binomial), color = "darkgreen", inherit.aes = FALSE) +
  stat_smooth(data = fitted_bmi, aes(BMI, .fitted), method="glm", method.args=list(family=binomial), color = "darkorange", inherit.aes = FALSE) +
 geom_text(aes(y = 0.15, x = 80, label = "crf, n = "), color = "blue") +
  geom_text(aes(y = 0.12, x = 80, label = "blood pressure"), color = "red") +
  geom_text(aes(y = 0.9, x = 80, label = "waistcircumf."), color = "darkgreen") +
  geom_text(aes(y = 0.6, x = 80, label = "BMI"), color = "darkorange") +
  theme_minimal()

  #geom_smooth() +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              fill = "steelblue2", alpha = 0.2) 


  geom_line(aes(Astrand_rel_VO2, .fitted, group = 1))



fitted_vals %>% group_by(sex) %>% 
  summarise(.fitted = mean(.fitted), Astrand_rel_VO2 = mean(Astrand_rel_VO2)) %>% 
  ggplot() +
  geom_line(aes(Astrand_rel_VO2, .fitted))

Upper = ilink(fit + (2 * se.fit)),
Lower = ilink(fit - (2 * se.fit))


%>% 
  mutate(died = ifelse(SevereCOVID  == 1, size, NA),
         suvived     = ifelse(SevereCOVID  == 0, size, NA))



    
    base <-
    ggplot(fitted_vals, aes(x = Astrand_rel_VO2)) +
    geom_line(aes(y = .fitted), color = "blue") +
    labs(x = "Size", y = "Survival")
  
  base + 
    geom_rug(aes(x = died), sides = "b", alpha = 0.2) +
    geom_rug(aes(x = suvived), sides = "t", alpha = 0.2)
  
  
  
  
  ###   ######  ######  ######  ######  ######
  
  library(ggiraphExtra)
  
  devtools::install_github("cardiomoon/ggiraphExtra")
  
  
  ggPredict(fitted_vals, 
            colorn = 100,
            point = FALSE, 
            se = TRUE, 
            interactive=FALSE, 
            jitter=FALSE) +
    theme_bw()
  
  
  ######  ######  ######  ######  ######
  
  library(MASS)
  data(bacteria)
  l_mod <- glm(y ~ trt + week, data = bacteria, family = binomial)
  summ(l_mod)     
  effect_plot(l_mod, pred = week, interval = TRUE, y.label = "% testing positive")
  
  
  #########################################
  
  library(ggeffects)
  
  mydf <- ggpredict(fitted_vals, terms = c("Astrand_rel_VO2", "sex"))
  ggplot(mydf, aes(x, predicted)) +
    geom_line(aes(group = group)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, group = group), alpha = .1)
  
  
  fitted_crf <-  hbt_crf %>% 
    glm(SevereCOVID ~ Astrand_rel_VO2 + Performed_year_cat + sex+ Age, 
        data = .,  family = binomial) %>% 
    ggeffect(terms = c("Astrand_rel_VO2", "Performed_year_cat")) %>% 
    mutate(group2 = "CRF")
  
  fitted_bp <-  hbt_bp %>% 
    glm(SevereCOVID ~ mean_arterial_bp + Performed_year_cat + sex+ Age, 
        data = .,  family = binomial) %>% 
    ggeffect(terms = c("mean_arterial_bp", "Performed_year_cat")) %>% 
    mutate(group2 = "Mean arterial BP")
  
  fitted_waist <-  hbt_waist %>% 
    glm(SevereCOVID ~ WaistCircumference + Performed_year_cat + sex+ Age, 
        data = .,  family = binomial) %>% 
    ggeffect(terms = c("WaistCircumference", "Performed_year_cat")) %>% 
    mutate(group2 = "Waist circumference")
  
  fitted_bmi  <-  hbt_bmi %>% 
    glm(SevereCOVID ~ BMI + Performed_year_cat + sex+ Age, 
        data = .,  family = binomial) %>% 
    ggeffect(terms = c("BMI", "Performed_year_cat")) %>% 
    mutate(group2 = "BMI")
  
bineded <-  bind_rows(fitted_crf, fitted_waist, fitted_bp, fitted_bmi)
  
  fitted_waist %>% ggeffect(terms = c("WaistCircumference")) %>% plot() +
    geom_point() 
  
  fitted_crf %>% ggeffect(terms = c("Astrand_rel_VO2")) %>% plot()
  
  fitted_bp %>% ggeffect(terms = c("mean_arterial_bloodpressure"))%>% plot()
  
  fitted_bmi%>% plot()
  
  bineded %>% 
    ggplot(aes(x, predicted)) +
    #geom_smooth(se=FALSE) +
    geom_line(aes( group = group, color = group)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high,  group = group), alpha = .1) +
      scale_y_continuous(labels = scales::percent) +
    scale_color_manual(values = scico::scico(3, begin = 0, palette = "bamako"))+
    facet_wrap(~group2, ncol = 1, scales = "free") +
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
  
  
  
  ####################################
  
  fitted_crf <-  hbt_crf %>% 
    glm(SevereCOVID ~ Astrand_rel_VO2 + sex+ Age, 
        data = .,  family = binomial) %>% 
    ggeffect(terms = c("Astrand_rel_VO2")) %>% 
    mutate(group2 = "CRF")
  
  fitted_bp <-  hbt_bp %>% 
    glm(SevereCOVID ~ mean_arterial_bp +  sex+ Age, 
        data = .,  family = binomial) %>% 
    ggeffect(terms = c("mean_arterial_bp")) %>% 
    mutate(group2 = "Mean arterial BP")
  
  fitted_waist <-  hbt_waist %>% 
    glm(SevereCOVID ~ WaistCircumference +  sex+ Age, 
        data = .,  family = binomial) %>% 
    ggeffect(terms = c("WaistCircumference")) %>% 
    mutate(group2 = "Waist circumference")
  
  fitted_bmi  <-  hbt_bmi %>% 
    glm(SevereCOVID ~ BMI +  sex+ Age, 
        data = .,  family = binomial) %>% 
    ggeffect(terms = c("BMI")) %>% 
    mutate(group2 = "BMI")
  
  bineded <-  bind_rows(fitted_crf, fitted_waist, fitted_bp, fitted_bmi)
  
  
  bineded %>% 
    ggplot(aes(x, predicted)) +
    #geom_smooth(se=FALSE) +
    geom_line(aes(), color = "darkgreen") +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1, fill = "orange") +
    scale_y_continuous(labels = scales::percent) +
    scale_color_manual(values = scico::scico(3, begin = 0, palette = "bamako"))+
    facet_wrap(~group2, ncol = 1, scales = "free") +
    guides(color = guide_legend(title="Performed year",  
                                keywidth = unit(2, "cm"),
                                keyheight = unit(0.5, "cm"), 
                                title.position  = 'top', 
                                title.hjust = .5,
                                barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))+
    labs(title = "Marginal effect plot for severe Covid",
         subtitle = "Adjusted for Age and gender",
         x = " ",
         y = " Predicted probabilities of severe Covid"
    ) +  
    theme(legend.position="top",
          plot.title.position = "plot")
  
  
  hbt2 %>% group_by(Performed_year_cat, SevereCOVID) %>% select(Age, BMI, Astrand_rel_VO2, agegroup_4, Gender) %>% skimr::skim()
  
  ################################################################# 
  
  sjp.glm(mod, type = "eff", show.ci = T)
  
  #####################################
  # bootstrap or simulatin cI
  https://cran.r-project.org/web/packages/glm.predict/vignettes/predicts.html
  
  ##########################################
  # about odds and probability
  https://stats.stackexchange.com/questions/34636/interpretation-of-simple-predictions-to-odds-ratios-in-logistic-regression
  
  
  # calculating CI for probability
  https://stats.stackexchange.com/questions/29044/plotting-confidence-intervals-for-the-predicted-probabilities-from-a-logistic-re
  
  ###############################################
  # odda scale
  # https://stat-ata-asu.github.io/MultipleAndLogisticRegression/logistic-regression.html
  
  odds = probability / 1 - probability
  
  mutate(odds_hat = .fitted / (1 - .fitted))
  
  # log_odds scale
  mutate(log_odds_hat = log(.fitted / (1 - .fitted)))
  
  ##############################################
  ##############################################
  
  #ggplot2: How to combine histogram, rug plot, and logistic regression prediction in a single graph
  # https://stackoverflow.com/questions/35366499/ggplot2-how-to-combine-histogram-rug-plot-and-logistic-regression-prediction
  ##############################################  ##############################################  
# comparing different yearg groups  
  

  fitted %>% 
    ggplot(aes(values, .fitted)) + 
    #stat_summary(aes(group = Performed_year_cat, color = Performed_year_cat ),fun = mean, geom="line") +
    geom_violin(varwidth = FALSE ,aes(x = values, group= SevereCOVID), position = position_dodge(width = 0.4), orientation = "y", color = "grey80") +
    #geom_boxplot(width = 0.003, varwidth = FALSE ,aes(x = values, group= SevereCOVID), position = position_dodge(width = 0.4), orientation = "y", color = "grey80") +
    geom_smooth(aes(group = Performed_year_cat, color = Performed_year_cat), fullrange = FALSE#, se = FALSE, method="glm", method.args=list(family=binomial), level=0.95
                )+
    
    #scico::scale_color_scico(palette = "bamako", direction = -1) +
    #scale_y_continuous(limits = c(0, .01)) +
    scale_color_manual(values = scico::scico(3, begin = 0, palette = "bamako"))+
    guides(color = guide_legend(title="Performed year",  
                                keywidth = unit(2, "cm"),
                                keyheight = unit(0.5, "cm"), 
                                title.position  = 'top', 
                                title.hjust = .5,
           barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines'))) +
    facet_wrap(~ group, scales = "free", ncol = 1)+
    labs(subtitle = "BMI, mean arterial blood pressure, fitness and waist circumference",
         title = "Probability of severe Covid",
         x = '',
         y = 'Probability') +
    theme(legend.position="top",
          plot.title.position = "plot")
  
  

  
  
  fitted %>% 
    ggplot(aes(values, .fitted)) +
    stat_summary(aes(group=group, color = group))
  
  ###################################################
  X1_range <- seq(from=min(data$X1), to=max(data$X1), by=.01)
  
  
  seq_along and seq_len
  
  hbt_waist %>% mutate(WaistCircumference2 = seq(1, 151193, by = length())) %>% select(WaistCircumference, WaistCircumference2 )
  
  fitted_waist <-  hbt_waist %>% 
    glm(SevereCOVID ~ WaistCircumference  + sex+ Age, 
        data = .,  family = binomial) %>% 
    augment(data = hbt_waist, type.predict = "link", type.residuals = c("deviance"), se_fit = TRUE) %>% # change link to response for exp
    mutate(died = ifelse(SevereCOVID  == 1, WaistCircumference, NA),
           survived     = ifelse(SevereCOVID  == 0, WaistCircumference, NA),
           lower = exp(.fitted - 1.96*.se.fit),
           upper = exp(.fitted + 1.96*.se.fit),
           .fitted = exp(.fitted)) %>% 
    select(LopNr, indexdate, WaistCircumference, .fitted, Age, SevereCOVID, .se.fit, lower, upper, died, survived, di, Performed_year_cat) %>% 
    mutate(group = "waist")%>% 
    rename("values" = WaistCircumference)
  
  
  c<-fitted_waistc %>% 
    ggplot(aes(values, .fitted)) + 
    stat_summary(aes(group = Performed_year_cat, color = Performed_year_cat ),fun = mean, geom="line") +
    #geom_violin(varwidth = FALSE ,aes(x = values, group= SevereCOVID), position = position_dodge(width = 0.4), orientation = "y", color = "grey80") +
    #geom_boxplot(width = 0.003, varwidth = FALSE ,aes(x = values, group= SevereCOVID), position = position_dodge(width = 0.4), orientation = "y", color = "grey80") +
    geom_smooth(aes(group = Performed_year_cat, color = Performed_year_cat), fullrange = FALSE, se = FALSE, method="glm", method.args=list(family=binomial), level=0.95
    )
  
  d<-fitted_waistd %>% 
    ggplot(aes(values, .fitted)) + 
    stat_summary(aes(group = Performed_year_cat, color = Performed_year_cat ),fun = mean, geom="line") +
    #geom_violin(varwidth = FALSE ,aes(x = values, group= SevereCOVID), position = position_dodge(width = 0.4), orientation = "y", color = "grey80") +
    #geom_boxplot(width = 0.003, varwidth = FALSE ,aes(x = values, group= SevereCOVID), position = position_dodge(width = 0.4), orientation = "y", color = "grey80") +
    geom_smooth(aes(group = Performed_year_cat, color = Performed_year_cat), fullrange = FALSE, se = FALSE, method="glm", method.args=list(family=binomial), level=0.95
    )
  
  c+d

  #########################
  
a<-  glm(formula = vs ~ disp, family = binomial, data = mtcars) %>% 
    augment(type.predict = "response") %>% 
    ggplot(aes(disp, .fitted)) + 
    geom_line(aes(group=1)) + 
    geom_point() + 
  geom_smooth(aes(group = 1), fullrange = FALSE, se = FALSE, method="glm", method.args=list(family=binomial), level=0.95) +
    geom_smooth(aes(group = 1), fullrange = FALSE, se = FALSE, color = "indianred") +
    labs(subtitle = "vs ~ disp")
  
b<-  glm(formula = vs ~ disp + mpg, family = binomial, data = mtcars) %>% 
    augment(type.predict = "response") %>% 
    ggplot(aes(disp, .fitted)) + 
    geom_line(aes(group=1)) + 
    geom_point() + 
    geom_smooth(aes(group = 1), fullrange = FALSE, se = FALSE, method="glm", method.args=list(family=binomial), level=0.95) +
    geom_smooth(aes(group = 1), fullrange = FALSE, se = FALSE, color = "indianred") +
    labs(subtitle = "vs ~ disp + mpg")
  
c<- glm(formula = vs ~ disp + mpg + hp, family = binomial, data = mtcars) %>% 
    augment(type.predict = "response") %>% 
    ggplot(aes(disp, .fitted)) + 
    geom_line(aes(group=1)) + 
    geom_point() + 
    geom_smooth(aes(group = 1), fullrange = FALSE, se = FALSE, method="glm", method.args=list(family=binomial), level=0.95) +
    geom_smooth(aes(group = 1), fullrange = FALSE, se = FALSE, color = "indianred") +
    labs(subtitle = "vs ~ disp + mpg + hp")
  
:a+b+c 
 
  glm(formula = vs ~ disp + am, family = binomial, data = mtcars) %>% 
    augment(type.predict = "response") %>% 
    ggplot(aes(disp, .fitted)) + 
    geom_line(aes(group=am)) + 
    geom_point() + 
    geom_smooth(aes(group = am), fullrange = FALSE, se = FALSE, method="glm", method.args=list(family=binomial), level=0.95) +
    geom_smooth(aes(group = am), fullrange = FALSE, se = FALSE, color = "indianred") +
    labs(subtitle = "")
  
  
  # marginal effects or marginal means
  # Marginal effects at the means https://www3.nd.edu/~rwilliam/stats/Margins01.pdf
  
  
 e<- glm(formula = vs ~ disp + mpg + hp , family = binomial, data = mtcars)  %>% 
    ggeffect(terms = c("disp")) %>% plot() + geom_point() +
   labs(title = "vs ~ disp + mpg + hp")
  
  
  d<-glm(formula = vs ~ disp + mpg , family = binomial, data = mtcars)  %>% 
    ggeffect(terms = c("disp")) %>% plot() + geom_point()+
    labs(title = "vs ~ disp + mpg")
  
  f<-glm(formula = vs ~ disp , family = binomial, data = mtcars)  %>% 
    ggeffect(terms = c("disp")) %>% plot() + geom_point()+
    labs(title = "vs ~ disp")
  
  
  f+d+e
  #################################################
  
  Orange %>% 
    group_by(Tree) %>%
    summarize(correlation = cor(age, circumference))
  
  
  mtcars %>% 
    group_by(hp) %>%
  glm(formula = vs ~ disp + mpg , family = binomial, data = .)  %>% 
    ggeffect(terms = c("disp")) %>%  view()

  
  mtcars %>%
    nest(data = -am) %>% 
    mutate(
      fit = map(data, ~ glm(formula = vs ~ disp + mpg , family = binomial, data = .x)),
      tidied = map(fit, ggeffect, terms=c('disp'))
    )   %>% 
    unnest(tidied) %>% 
    ggplot(aes(x, predicted)) +
    #geom_smooth(se=FALSE) +
    geom_line(aes(), color = "darkgreen") +
    facet_wrap(~am)

  
  groups_waist  <- hbt2 %>%
    nest(data = -Performed_year_cat) %>% 
    mutate(
      fit_w = map(data, ~ glm(SevereCOVID ~ WaistCircumference +  sex + Age, family = binomial, data = .x)),
      tidied_w = map(fit_w, ggeffect, terms=c('WaistCircumference'))
    )   %>% ungroup() %>% 
    select(!data, -fit_w) %>% 
  unnest(c(tidied_w), .drop = T)
    
  
  #########################################################
  
 #  Group by dataframe and model
  
groups_all  <- hbt2 %>%
    nest(data = -Performed_year_cat) %>% 
    mutate(
      fit_w = map(data, ~ glm(SevereCOVID ~ WaistCircumference +  sex + Age, family = binomial, data = .x)),
      fit_bp = map(data, ~ glm(SevereCOVID ~ mean_arterial_bp +  sex + Age, family = binomial, data = .x)),
      fit_crf = map(data, ~ glm(SevereCOVID ~ Astrand_rel_VO2 +  sex + Age, family = binomial, data = .x)),
      fit_bmi = map(data, ~ glm(SevereCOVID ~ BMI +  sex + Age, family = binomial, data = .x)),
      tidied_w = map(fit_w, ggeffect, terms=c('WaistCircumference')),
      tidied_bp = map(fit_bp, ggeffect, terms=c('mean_arterial_bp')),
      tidied_crf = map(fit_crf, ggeffect, terms=c('Astrand_rel_VO2')),
      tidied_bmi = map(fit_bmi, ggeffect, terms=c('BMI'))
    )   %>% 
  select(!data, -fit_w, -fit_bp, -fit_crf, -fit_bmi) %>% 
  pivot_longer(-Performed_year_cat) %>% 
    unnest(c(value))
  
  groups_all %>% filter(name == "tidied_crf" | name == "tidied_bmi", x == 30)
  
hbt2 %>% select(Performed_year_cat, mean_arterial_bp, SevereCOVID, Age, Gender) %>%  
  mutate(SevereCOVID = as.factor(SevereCOVID)) %>% 
  group_by(Performed_year_cat) %>% skimr::skim() 

hbt2 %>% select(Performed_year_cat, mean_arterial_bp, SevereCOVID, Age, Gender) %>%  
  summarise(bp = quantile(mean_arterial_bp, c(0.25, 0.5, 0.75), na.rm=TRUE), 
            Age = quantile(Age, c(0.25, 0.5, 0.75), na.rm=TRUE), 
            q = c(0.25, 0.5, 0.75), n(), mean(SevereCOVID))


hbt2 %>% select(Performed_year_cat, mean_arterial_bp, SevereCOVID, Age, Gender) %>%  
  group_by(Performed_year_cat, SevereCOVID) %>% summarise(mean_arterial_bp = mean(mean_arterial_bp, na.rm=T)) %>% 
  ggplot(aes(y=mean_arterial_bp, x= SevereCOVID , group =Performed_year_cat, color =Performed_year_cat )) + 
  geom_line()


# crosstab why bp is different 2010-2014
hbt2 %>% select(BloodPressureDiastolic, BloodPressureSystolic, Performed_year_cat, mean_arterial_bp, SevereCOVID, Age, Gender) %>% 
  group_by(mean_arterial_bp_ntile = ntile(mean_arterial_bp, 10)) %>% 
  mutate( mean_art = mean(mean_arterial_bp),
          mean_art = mean(BloodPressureDiastolic),
          mean_art = mean(BloodPressureSystolic)) %>% 
  group_by(Performed_year_cat) %>%  
  janitor::tabyl(mean_art,Performed_year_cat, SevereCOVID) %>% 
  janitor::adorn_percentages(denominator = "col") %>% 
  janitor::adorn_rounding(digits = 2)%>% 
  janitor::adorn_totals("row") %>% 
  janitor::adorn_pct_formatting() %>% 
  janitor::adorn_ns() 


hbt2 %>% select(BMI, Astrand_rel_VO2) %>% filter(BMI > 30) %>% summarise(mean(Astrand_rel_VO2, na.rm=T))
  

hbt2 %>% select(BloodPressureDiastolic, BloodPressureSystolic, Performed_year_cat, mean_arterial_bp, SevereCOVID, Age, Gender) %>% 
  group_by(mean_arterial_bp_ntile = ntile(mean_arterial_bp, 10)) %>% 
  mutate( mean_art = mean(mean_arterial_bp),
          mean_dia = mean(BloodPressureDiastolic),
          mean_sys = mean(BloodPressureSystolic)) %>% 
  group_by(Performed_year_cat, mean_arterial_bp) %>% 
  summarise(mean(mean_dia),
            mean(mean_sys)) %>% view()
  


skimr::skim()

groups_all %>% 
  ggplot(aes(x, predicted)) +
  #geom_smooth(se=FALSE) +
  geom_line(aes(group =Performed_year_cat, color = Performed_year_cat)) +
  facet_wrap(~name, scales = "free") +
  scale_color_manual(values = scico::scico(3, begin = .2, palette = "lajolla"))+
  scale_y_continuous(labels = scales::percent) +
  guides(color = guide_legend(title="Performed year",  
                              keywidth = unit(2, "cm"),
                              keyheight = unit(0.5, "cm"), 
                              title.position  = 'top', 
                              title.hjust = .5,
                              barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines'))) +
  labs(subtitle = "Each line is one model i.e. derrived from different data frames, all adjusted for age and sex",
       title = "Marginal effects",
       x = '',
       y = 'Probability of severe Covid') +
  theme(legend.position="top",
        plot.title.position = "plot")
