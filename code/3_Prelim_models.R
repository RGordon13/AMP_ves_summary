#' ------------------------------------------------------------
#' 
#' 
#' Modeling for AMP vessel summary
#' 
#' 
#' 
#' 
#' 
#'-------------------------------------------------------------



# Load libraries ----------------------------------------------------------

# make sure working directory is good
setwd(here::here())
getwd()

# libraries

library(readr)
library(tidyverse)
library(broom) # looking at model results
library(performance) # model performance
library(modelr)

# for fitting
library(betareg)
library(glmmTMB)
library(lme4) # fitting mixed models

# simulate residuals
library(DHARMa)

# for viz
library(ggplot2)
theme_set(theme_classic(base_size = 14))



# Load dataset ------------------------------------------------------------


vessels <- read_csv("data/All_sites_tables/hourly_pres_allsites_local.csv") |>
  # set order of weekdays
  mutate("Weekday_fac" = as_factor(Weekday),
         "Weekday_fac" = fct_relevel(Weekday_fac, c("Sunday","Monday","Tuesday","Wednesday",
                                                    "Thursday","Friday","Saturday")))
all_ves_wkdy <- read_csv("output/All_ves_by_weekday.csv")

ves_weekday <- all_ves_wkdy |>
  group_by(network, npz_id, Dep_ID, Dep, Date_week) |>
  # get n_days per week in dataset
  summarize(n_days = n(),
            week_sum = sum(Total_ves_dep, na.rm = TRUE)) |>
  # only keep full weeks n_days == 7
  # only keep weeks with presence
  filter(n_days == 7,
         week_sum > 0) |>
  # get original weekday counts
  left_join(all_ves_wkdy)|>
  mutate("Weekday_fac" = as_factor(Weekday),
         "Weekday_fac" = fct_relevel(Weekday_fac, c("Su","M","T","W",
                                                    "Th","F","Sa")),
         npz_dep = as_factor(paste0(npz_id, Dep_ID)))

# Modeling ----------------------------------------------------------------

# 1. Viz data

# questions of interest
# 1 -- does day of the week affect N vessels in each park?
# 2 -- does that effect vary by park? (interaction effect)



# 2. Fit model

# try glm with Poisson error for count data

# effect of weekday on total vessels
ves_weekday_glm <- glm(Total_ves_dep ~ Weekday_fac*npz_dep, 
                       family = poisson(link = "log"),
                       data = ves_weekday)
summary(ves_weekday_glm)

check_model(ves_weekday_glm)

### try mixed effects model next

# vessels ~ weekday + 1|SiteID

ves_weekday_glmm <- glmmTMB::glmmTMB(Total_ves_dep ~ Weekday_fac*npz_dep +
                                       (1|network),
                                     family = poisson(link = "log"),
                                     data = ves_weekday)
summary(ves_weekday_glmm)

# Family: poisson  ( log )
# Formula:          Total_Vessels ~ Weekday_fac + (1 | SiteID)
# Data: vessels
# 
# AIC      BIC   logLik deviance df.resid 
# 10654.0  10710.7  -5319.0  10638.0     8744 
# 
# Random effects:
#   
#   Conditional model:
#   Groups Name        Variance Std.Dev.
# SiteID (Intercept) 1.125    1.061   
# Number of obs: 8752, groups:  SiteID, 9
# 
# Conditional model:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)          -1.67741    0.36367  -4.612 3.98e-06 ***
#   Weekday_facMonday    -0.33492    0.09061  -3.696 0.000219 ***
#   Weekday_facTuesday   -0.04128    0.08127  -0.508 0.611518    
# Weekday_facWednesday -0.08448    0.08112  -1.041 0.297667    
# Weekday_facThursday   0.09035    0.07627   1.185 0.236138    
# Weekday_facFriday    -0.00735    0.07645  -0.096 0.923411    
# Weekday_facSaturday   0.32635    0.07229   4.514 6.35e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1






# 3. Check model assumptions

check_model(ves_weekday_lm)

check_model(ves_weekday_glm) 
# normality of residuals looks AWFUL, others look okay-ish :-/

check_outliers(ves_weekday_glm)
plot(check_normality(ves_weekday_glm))

# try DHARMa approach
simulateResiduals(ves_weekday_glm) |> plot()
# fails all tests apart from HOV... boooo


check_model(ves_weekday_glmm) # not GREAT, maybe send this to Jarrett to discuss?



# 4. Evaluate model fit

# glm
tidy(ves_weekday_glm)
confint(ves_weekday_glm)
# Waiting for profiling to be done...
#                             2.5 %     97.5 %
#   (Intercept)         -1.50474955 -1.2816756
# Weekday_facMonday     -0.51968902 -0.1647747
# Weekday_facTuesday    -0.12718626  0.1909732
# Weekday_facWednesday  -0.17782407  0.1397972
# Weekday_facThursday    0.06325035  0.3614272
# Weekday_facFriday     -0.02412116  0.2747303
# Weekday_facSaturday    0.31696258  0.5990511


# mixed model
confint(ves_weekday_glmm)

#                                  2.5 %      97.5 %     Estimate
# (Intercept)                -2.39019633 -0.96462632 -1.677411326
# Weekday_facMonday          -0.51250202 -0.15732857 -0.334915294
# Weekday_facTuesday         -0.20057343  0.11801416 -0.041279633
# Weekday_facWednesday       -0.24346588  0.07450728 -0.084479297
# Weekday_facThursday        -0.05912668  0.23983098  0.090352148
# Weekday_facFriday          -0.15719177  0.14249194 -0.007349917
# Weekday_facSaturday         0.18465567  0.46803836  0.326347019
# Std.Dev.(Intercept)|SiteID  0.63957149  1.75918989  1.060720364






# 5. Vizualize fitted model

ggplot(data = vessels,
       aes(x = Weekday_fac,
           y = Total_Vessels)) +
  geom_point() +
  stat_smooth(method = "glm", # tell ggplot how to fit
              # provide list of method arguments to specify family
              method.args = list(
                family = poisson(link = "log")
              ))




