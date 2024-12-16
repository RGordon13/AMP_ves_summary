#' ---------------------------------------------------------
#' 
#' 
#' Summarize vessel activity across 9 marine parks
#' 
#' Updated Sept 2024
#' ---------------------------------------------------------


#### Load libraries ####

# NOAA-approved tidyverse
tidyverse_short<-c("broom","cli","crayon","dbplyr","dplyr","dtplyr","forcats","ggplot2","googledrive","googlesheets4","hms","httr","jsonlite","lubridate","magrittr","modelr","pillar","purrr","readr","readxl","reprex","rlang","rstudioapi","rvest","stringr","tibble","tidyr","xml2") 
lapply(tidyverse_short, require, character.only = TRUE) 

# source helper file
source("scripts/AMP_summary_ves_funs.R")

#### Load in data ####
# hourly presence

hp_cgmp <- read_csv("data_inputs/Hourly_Presence/R_AMP_CodGrounds_201904_Vessel_Bio_HourlyPres_D2b.csv") |>
  mutate(Begin_Date = as_date(Begin_Date, format = "%m/%d/%Y"))|>
  select(SiteID, Begin_Date, Begin_hour, CPA, CPA_M,TA,TA_M,TB) 

hp_simp <- read_csv("data_inputs/Hourly_Presence/R_AMP_EastPimpernel_201906_VesselHourlyPres_D2b.csv") |>
  rename("SiteID" = "Site_ID") |>
  mutate(Begin_Date = as_date(Begin_Date, format = "%m/%d/%Y"))|>
  select(SiteID, Begin_Date, Begin_hour, CPA, CPA_M,TA,TA_M,TB)

hp_mur <- read_csv("data_inputs/Hourly_Presence/R_AMP_MRE_VesselHourlyPres_D1.csv") |> 
  mutate(Begin_Date = as_date(Begin_Date, format = "%m/%d/%Y"))|>
  select(SiteID, Begin_Date, Begin_hour, CPA, CPA_M,TA,TA_M,TB)
  
hp_dne <- read_csv("data_inputs/Hourly_Presence/R_DNE_VesselBio_HourlyPres_D1_Sept.csv") |>
  mutate(Begin_Date = as_date(Begin_Date, format = "%m/%d/%Y"))|>
  select(SiteID, Begin_Date, Begin_hour, CPA, CPA_M,TA,TA_M,TB)

hp_ngn <- read_csv("data_inputs/Hourly_Presence/R_NGN_Vessel_Bio_HourlyPres_D1.csv") |>
  mutate(Begin_Date = as_date(Begin_Date, format = "%m/%d/%Y"))|>
  select(SiteID, Begin_Date, Begin_hour, CPA, CPA_M,TA,TA_M,TB)

hp_trw <- read_csv("data_inputs/Hourly_Presence/TRW D1_HourlyPresence.csv") |>
  mutate(Begin_Date = as_date(Begin_Date, format = "%m/%d/%Y"))|>
  select(SiteID, Begin_Date, Begin_hour, CPA, CPA_M,TA,TA_M,TB, PresentTotal)

hp_sws <- read_csv("data_inputs/Hourly_Presence/SWS_D1_VesselHourlyPres_updated.csv") |>
  mutate(Begin_Date = as_date(Begin_Date, format = "%m/%d/%Y"))|>
  rename("MANEUVER" = "M") |>
  select(SiteID, Begin_Date, Begin_hour, TRANSIT, MANEUVER)

hp_jur <- read_csv("data_inputs/Hourly_Presence/JURIEN_202201_JNE_VesselHourlyPres_UTC+1.csv") |>
  mutate(Begin_Date = as_date(Begin_Date, format = "%m/%d/%Y"))|>
  rename("MANEUVER" = "M",
         "TRANSIT" = "TR") |>
  select(SiteID, Begin_Date, Begin_hour, TRANSIT, MANEUVER)

hp_geo <- read_csv("data_inputs/Hourly_Presence/GEO_D1_VesselHourlyPres.csv") |>
  mutate(Begin_Date = as_date(Begin_Date, format = "%m/%d/%Y"))|>
  rename("MANEUVER" = "M",
         "TRANSIT" = "TR") |>
  select(SiteID, Begin_Date, Begin_hour, TRANSIT, MANEUVER)

# High prob inside park tables

ins_cgmp <- read_csv("data_inputs/HighProb_Inside/HighProb_Inside_Park_CGMP_SIMP_D2b.csv") |>
  rename("SiteID" = "Site",
         "Begin_Date" = "Date") |>
  filter(SiteID == "CGMP",
         !is.na(Begin_Date)) |>
  mutate(SiteID = "Cod Grounds") |>
  mutate(Begin_hour = hour(Start_Time),
         I_O = "Inside")|>
  select(SiteID, Behavior, Filename, Begin_Date, Start_Time, End_Time, Begin_hour, I_O)

ins_simp <- read_csv("data_inputs/HighProb_Inside/HighProb_Inside_Park_EP_D2b.csv") |>
  rename("SiteID" = "Site",
         "Begin_Date" = "Date") |>
  filter(!is.na(Begin_Date))|>
  mutate(Begin_hour = hour(Start_Time),
         I_O = "Inside")|>
  select(SiteID, Behavior, Filename, Begin_Date, Start_Time, End_Time, Begin_hour, I_O)


ins_dne <- read_csv("data_inputs/HighProb_Inside/DNE_D1_High_Prob_Inside.csv") |>
  rename("SiteID" = "Site",
       "Begin_Date" = "Date") |>
  filter(!is.na(Begin_Date))|>
  mutate(Begin_hour = hour(Start_Time),
         I_O = "Inside")|>
  select(SiteID, Behavior, Filename, Begin_Date, Start_Time, End_Time, Begin_hour, I_O)

ins_ngn <- read_csv("data_inputs/HighProb_Inside/NGN_D1_High_prob_inside.csv") |>
  rename("SiteID" = "Site",
         "Begin_Date" = "Date") |>
  filter(!is.na(Begin_Date))|>
  mutate(Begin_hour = hour(Start_Time),
         I_O = "Inside")|>
  select(SiteID, Behavior, Filename, Begin_Date, Start_Time, End_Time, Begin_hour, I_O)

ins_trw <- read_csv("data_inputs/HighProb_Inside/TRE_TRW_D1_HighProb_Inside_SmMed.csv") |>
  rename("SiteID" = "Site",
         "Begin_Date" = "Date") |>
  filter(SiteID == "TRW") |>
  mutate(SiteID = "TRW D1") |>
  filter(!is.na(Begin_Date))|>
  mutate(Begin_hour = hour(Start_Time),
         I_O = "Inside") |>
  select(SiteID, Behavior, Filename, Begin_Date, Start_Time, End_Time, Begin_hour, I_O)

ins_geo <- read_csv("data_inputs/HighProb_Inside/GEO_D1_HighProb_Inside.csv") |>
  rename("SiteID" = "Site",
         "Begin_Date" = "Date") |>
  filter(SiteID == "GEO") |>
  mutate(SiteID = "GEO") |>
  filter(!is.na(Begin_Date))|>
  mutate(Begin_hour = hour(Start_Time),
         I_O = "Inside") |>
  select(SiteID, Behavior, Filename, Begin_Date, Start_Time, End_Time, Begin_hour, I_O)

ins_jur <- read_csv("data_inputs/HighProb_Inside/JNE_D1_HighProb_Inside_a001_UTC+1.csv") |>
  rename("SiteID" = "Site",
         "Begin_Date" = "Date") |>
  filter(SiteID == "JNE") |>
  mutate(SiteID = "JNE") |>
  filter(!is.na(Begin_Date))|>
  mutate(Begin_hour = hour(Start_Time),
         I_O = "Inside") |>
  select(SiteID, Behavior, Filename, Begin_Date, Start_Time, End_Time, Begin_hour, I_O)


#### Ins Park tables -- reshape into hp ####

# reshape into HP to merge with HP tables
ins_cgmp_hp <- inside_tables_to_hp(ins_table = ins_cgmp, site_id = "Cod Grounds")
ins_simp_hp <- inside_tables_to_hp(ins_table = ins_simp, site_id = "EP")
ins_dne_hp <- inside_tables_to_hp(ins_table = ins_dne, site_id = "DNE")
ins_ngn_hp <- inside_tables_to_hp(ins_table = ins_ngn, site_id = "NGN")
ins_trw_hp <- inside_tables_to_hp(ins_table = ins_trw, site_id = "TRW D1")
ins_geo_hp <- inside_tables_to_hp(ins_table = ins_geo, site_id = "GEO")
ins_jur_hp <- inside_tables_to_hp(ins_table = ins_jur, site_id = "JNE")






#### HOURLY PRES -- reshape data and adjust time zones ####

# CGMP already in local time (AEST)
hp_cgmp_local <- hp_cgmp |>
  left_join(ins_cgmp_hp, by = c("Begin_Date","SiteID","Begin_hour"))|>
  mutate(Total_Vessels = rowSums(across(c(CPA, CPA_M,TA,TA_M,TB))),
                        # create y/n column for vessel presence
                        ves_yn = ifelse(Total_Vessels == 0, "N", "Y"),
                        # pull hour as time object along with date
                        Hr_time = paste0(Begin_Date," ", Begin_hour, ":00"),
                        # change to local time zone
                        Hr_local = as_datetime(Hr_time, format = "%Y-%m-%d %H:%M"),
                        # pull out new hour and date in local time
                        Begin_Hour_loc = as.numeric(hour(Hr_local)),
                        Begin_Date_loc = date(Hr_local),
                        # add weekday column
                        Weekday = weekdays(Begin_Date_loc),
                        Week = week(Begin_Date_loc),
                        Month = month(Begin_Date_loc),
                        Day_month = day(Begin_Date_loc)) |>
  select(SiteID, Total_Vessels, Begin_Hour_loc, Begin_Date_loc, 
         ves_yn, Weekday, Week, Month, Day_month, trans_inside, man_inside, total_inside)

# SIMP already in local time (AEST)
hp_simp_local <- hp_simp |>
  left_join(ins_simp_hp, by = c("Begin_Date","SiteID","Begin_hour"))|>
  mutate(Total_Vessels = rowSums(across(c(CPA, CPA_M,TA,TA_M,TB))),
         # create y/n column for vessel presence
         ves_yn = ifelse(Total_Vessels == 0, "N", "Y"),
         # pull hour as time object along with date
         Hr_time = paste0(Begin_Date," ", Begin_hour, ":00"),
         # change to local time zone
         Hr_local = as_datetime(Hr_time, format = "%Y-%m-%d %H:%M"),
         # pull out new hour and date in local time
         Begin_Hour_loc = as.numeric(hour(Hr_local)),
         Begin_Date_loc = date(Hr_local),
         # add weekday column
         Weekday = weekdays(Begin_Date_loc),
         Week = week(Begin_Date_loc),
         Month = month(Begin_Date_loc),
         Day_month = day(Begin_Date_loc))|>
  select(SiteID, Total_Vessels, Begin_Hour_loc, Begin_Date_loc, 
         ves_yn, Weekday, Week, Month, Day_month, trans_inside, man_inside, total_inside)

hp_mur_local <- hp_mur |>
  mutate(Total_Vessels = rowSums(across(c(CPA, CPA_M,TA,TA_M,TB))),
         # create y/n column for vessel presence
         ves_yn = ifelse(Total_Vessels == 0, "N", "Y"),
         # pull hour as time object along with date
         Hr_time = paste0(Begin_Date," ", Begin_hour, ":00"),
         # assign time zone
         Hr_time = force_tz(as_datetime(Hr_time, format = "%Y-%m-%d %H:%M"), tz = "Australia/NSW"),
         # change to local time zone
         Hr_local = with_tz(Hr_time, tz = "Australia/ACT"),
         # pull out new hour and date in local time
         Begin_Hour_loc = as.numeric(hour(Hr_local)),
         Begin_Date_loc = date(Hr_local),
         # add weekday column
         Weekday = weekdays(Begin_Date_loc),
         Week = week(Begin_Date_loc),
         Month = month(Begin_Date_loc),
         Day_month = day(Begin_Date_loc),
         trans_inside = 0, 
         man_inside = 0, 
         total_inside = 0)|>
  select(SiteID, Total_Vessels, Begin_Hour_loc, Begin_Date_loc, 
         ves_yn, Weekday, Week, Month, Day_month, trans_inside, man_inside, total_inside)

# Dampier (already in local tz)
hp_dne_local <- hp_dne |>
  left_join(ins_dne_hp, by = c("Begin_Date","SiteID","Begin_hour"))|>
  mutate(Total_Vessels = rowSums(across(c(CPA, CPA_M,TA,TA_M,TB))),
         # create y/n column for vessel presence
         ves_yn = ifelse(Total_Vessels == 0, "N", "Y"),
         # pull hour as time object along with date
         Hr_time = paste0(Begin_Date," ", Begin_hour, ":00"),
         # change to local time zone
         Hr_local = as_datetime(Hr_time, format = "%Y-%m-%d %H:%M"),
         # pull out new hour and date in local time
         Begin_Hour_loc = as.numeric(hour(Hr_local)),
         Begin_Date_loc = date(Hr_local),
         # add weekday column
         Weekday = weekdays(Begin_Date_loc),
         Week = week(Begin_Date_loc),
         Month = month(Begin_Date_loc),
         Day_month = day(Begin_Date_loc))|>
  select(SiteID, Total_Vessels, Begin_Hour_loc, Begin_Date_loc, 
         ves_yn, Weekday, Week, Month, Day_month, trans_inside, man_inside, total_inside)

# Ningaloo -- UTC to AWST
hp_ngn_local <- hp_ngn |>
  left_join(ins_ngn_hp, by = c("Begin_Date","SiteID","Begin_hour"))|>
  mutate(Total_Vessels = rowSums(across(c(CPA, CPA_M,TA,TA_M,TB))),
         # create y/n column for vessel presence
         ves_yn = ifelse(Total_Vessels == 0, "N", "Y"),
         # pull hour as time object along with date
         Hr_time = paste0(Begin_Date," ", Begin_hour, ":00"),
         # assign time zone
         Hr_time = force_tz(as_datetime(Hr_time, format = "%Y-%m-%d %H:%M"), tz = "Etc/GMT"),
         # change to local time zone
         Hr_local = with_tz(Hr_time, tz = "Australia/West"),
         # pull out new hour and date in local time
         Begin_Hour_loc = as.numeric(hour(Hr_local)),
         Begin_Date_loc = date(Hr_local),
         # add weekday column
         Weekday = weekdays(Begin_Date_loc),
         Week = week(Begin_Date_loc),
         Month = month(Begin_Date_loc),
         Day_month = day(Begin_Date_loc))|>
  select(SiteID, Total_Vessels, Begin_Hour_loc, Begin_Date_loc, 
         ves_yn, Weekday, Week, Month, Day_month, trans_inside, man_inside, total_inside)

# Two rocks -- UTC to AWST
hp_trw_local <- hp_trw |>
  left_join(ins_trw_hp, by = c("Begin_Date","SiteID","Begin_hour"))|>
  mutate(Total_Vessels = PresentTotal,
         # create y/n column for vessel presence
         ves_yn = ifelse(Total_Vessels == 0, "N", "Y"),
         # pull hour as time object along with date
         Hr_time = paste0(Begin_Date," ", Begin_hour, ":00"),
         # assign time zone
         Hr_time = force_tz(as_datetime(Hr_time, format = "%Y-%m-%d %H:%M"), tz = "Etc/GMT"),
         # change to local time zone
         Hr_local = with_tz(Hr_time, tz = "Australia/West"),
         # pull out new hour and date in local time
         Begin_Hour_loc = as.numeric(hour(Hr_local)),
         Begin_Date_loc = date(Hr_local),
         # add weekday column
         Weekday = weekdays(Begin_Date_loc),
         Week = week(Begin_Date_loc),
         Month = month(Begin_Date_loc),
         Day_month = day(Begin_Date_loc))|>
  select(SiteID, Total_Vessels, Begin_Hour_loc, Begin_Date_loc, 
         ves_yn, Weekday, Week, Month, Day_month, trans_inside, man_inside, total_inside)

# SWC --UTC to AWST
hp_sws_local <- hp_sws |>
  mutate(Total_Vessels = rowSums(across(c(TRANSIT, MANEUVER))),
         # create y/n column for vessel presence
         ves_yn = ifelse(Total_Vessels == 0, "N", "Y"),
         # pull hour as time object along with date
         Hr_time = paste0(Begin_Date," ", Begin_hour, ":00"),
         # assign time zone
         Hr_time = force_tz(as_datetime(Hr_time, format = "%Y-%m-%d %H:%M"), tz = "Etc/GMT"),
         # change to local time zone
         Hr_local = with_tz(Hr_time, tz = "Australia/West"),
         # pull out new hour and date in local time
         Begin_Hour_loc = as.numeric(hour(Hr_local)),
         Begin_Date_loc = date(Hr_local),
         # add weekday column
         Weekday = weekdays(Begin_Date_loc),
         Week = week(Begin_Date_loc),
         Month = month(Begin_Date_loc),
         Day_month = day(Begin_Date_loc),
         trans_inside = 0, 
         man_inside = 0, 
         total_inside = 0)|>
  select(SiteID, Total_Vessels, Begin_Hour_loc, Begin_Date_loc, 
         ves_yn, Weekday, Week, Month, Day_month,trans_inside, man_inside, total_inside)

# Geographe -- UTC to AWST
hp_geo_local <- hp_geo |>
  left_join(ins_geo_hp, by = c("Begin_Date","SiteID","Begin_hour"))|>
  mutate(Total_Vessels = rowSums(across(c(TRANSIT, MANEUVER))),
         # create y/n column for vessel presence
         ves_yn = ifelse(Total_Vessels == 0, "N", "Y"),
         # pull hour as time object along with date
         Hr_time = paste0(Begin_Date," ", Begin_hour, ":00"),
         # assign time zone
         Hr_time = force_tz(as_datetime(Hr_time, format = "%Y-%m-%d %H:%M"), tz = "Etc/GMT"),
         # change to local time zone
         Hr_local = with_tz(Hr_time, tz = "Australia/West"),
         # pull out new hour and date in local time
         Begin_Hour_loc = as.numeric(hour(Hr_local)),
         Begin_Date_loc = date(Hr_local),
         # add weekday column
         Weekday = weekdays(Begin_Date_loc),
         Week = week(Begin_Date_loc),
         Month = month(Begin_Date_loc),
         Day_month = day(Begin_Date_loc))|>
  select(SiteID, Total_Vessels, Begin_Hour_loc, Begin_Date_loc, 
         ves_yn, Weekday, Week, Month, Day_month,trans_inside, man_inside, total_inside)

# Jurien -- UTC+1 to AWST
hp_jur_local <- hp_jur |>
  left_join(ins_jur_hp, by = c("Begin_Date","SiteID","Begin_hour"))|>
  mutate(Total_Vessels = rowSums(across(c(TRANSIT, MANEUVER))),
         # create y/n column for vessel presence
         ves_yn = ifelse(Total_Vessels == 0, "N", "Y"),
         # pull hour as time object along with date
         Hr_time = paste0(Begin_Date," ", Begin_hour, ":00"),
         # assign time zone
         Hr_time = force_tz(as_datetime(Hr_time, format = "%Y-%m-%d %H:%M"), tz = "Etc/GMT-1"),
         # change to local time zone
         Hr_local = with_tz(Hr_time, tz = "Australia/West"),
         # pull out new hour and date in local time
         Begin_Hour_loc = as.numeric(hour(Hr_local)),
         Begin_Date_loc = date(Hr_local),
         # add weekday column
         Weekday = weekdays(Begin_Date_loc),
         Week = week(Begin_Date_loc),
         Month = month(Begin_Date_loc),
         Day_month = day(Begin_Date_loc))|>
  select(SiteID, Total_Vessels, Begin_Hour_loc, Begin_Date_loc, 
         ves_yn, Weekday, Week, Month, Day_month,trans_inside, man_inside, total_inside)


#### bring all HP data together ####
all_sites_hp <- as.data.frame(rbind(hp_cgmp_local, hp_simp_local,
                     hp_dne_local, hp_mur_local, hp_ngn_local, 
                     hp_trw_local, hp_sws_local, hp_geo_local, hp_jur_local))

write.csv(all_sites_hp, "data_outputs/hourly_pres_allsites_local.csv")


#### PLOTS! ####


# make list of site labels for plotting
site_labs <- as_labeller(
  c("Cod Grounds" = "Cod Grounds", 
    "DNE" = "Dampier", 
    "EP" = "Solitary \nIslands",
    "MRE" = "Murat",
    "NGN" = "Ningaloo", 
    "SWC" = "South-west \nCorner",
    "TRW D1" = "Two Rocks",
    "JNE" = "Jurien",
    "GEO" = "Geographe"))

#### TOTAL BY SITE - ALL VES ####

all_ves_by_site <- all_sites_hp |>
  group_by(SiteID) |>
  summarise(n_days = n_distinct(Begin_Date_loc),
            Total_ves_dep = sum(Total_Vessels, na.rm = TRUE),
            Total_ves_per_day = Total_ves_dep/n_days,
            Total_inside_ves_dep = sum(total_inside, na.rm = TRUE),
            Total_inside_ves_per_day = Total_inside_ves_dep/n_days)



# reshape for stacked bar plot
all_ves_in_out <- all_ves_by_site |>
  mutate(Outside = Total_ves_dep - Total_inside_ves_dep)|>
  rename("Inside" ="Total_inside_ves_dep") |>
  pivot_longer(cols = c("Outside","Inside"),
               names_to = "Total_ins_out",
               values_to = "Total")
  

# stacked barplot inside-outside
ggplot(data = all_ves_in_out,
       mapping = aes(x = SiteID,
                     y = Total,
                     fill = Total_ins_out)) +
  geom_bar(position = "fill", stat = "identity")+
  labs(x = "Site", y = "Proportion", fill = "") +
  scale_x_discrete(labels = c("Cod Grounds", "Dampier", "Solitary Islands", "Murat", "Ningaloo", 
                              "South-west Corner", "Two Rocks", "Geographe","Jurien"))+
  facet_grid(cols = vars(SiteID), scales = "free_x",
             labeller = site_labs)+
  theme_bw()+
  theme(text = element_text(size = 18),
        axis.text.x=element_blank(), 
        axis.ticks.x = element_blank())

# Save figure
ggsave("Figures/Ins-out_by_site_9NPZ.jpg", device = "jpeg",
       width=10, height=8, units="in", dpi=300)



# plot
ggplot(data = all_ves_by_site,
       mapping = aes(x = SiteID,
                     y = Total_ves_per_day)) +
  geom_col(fill = "#895c89") +
  labs(x = "Site", y = "N Vessels / Recording Day") +
  scale_x_discrete( labels = site_labs)+
  theme_bw()+
  theme(text = element_text(size = 18))


# Save figure
ggsave("Figures/Ves_per_day_by_site_9NPZ.jpg", device = "jpeg",
       width=10, height=8, units="in", dpi=300)


#### DIEL PLOT - ALL VES ####
all_sites_diel <- all_sites_hp |>
  group_by(SiteID, Begin_Hour_loc) |>
  summarise(Total_ves = sum(Total_Vessels, na.rm=TRUE))



ggplot(data = all_sites_diel,
       mapping = aes(x = Begin_Hour_loc,
                     y = Total_ves)) +
  geom_col(fill = "#895c89") +
  labs(x = "Hour of Day (local)", y = "N Vessels") +
  facet_grid(rows = vars(SiteID),
             labeller = site_labs) +
  theme_bw() +
  theme(text = element_text(size = 18))

# Save figure
ggsave("Figures/Diel_total_by_site_9NPZ.jpg", device = "jpeg",
       width=9, height=9, units="in", dpi=300)


#### WEEKDAY PLOT - ALL VES ####
all_sites_week <- all_sites_hp |>
  mutate(Weekday = factor(Weekday, levels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))) |>
  group_by(SiteID, Weekday) |>
  summarise(Total_ves = sum(Total_Vessels, na.rm=TRUE))

levels(all_sites_week$Weekday) <- c("S","M","T","W","R","F","Sa")
  
ggplot(data = all_sites_week,
       mapping = aes(x = Weekday,
                     y = Total_ves)) +
  geom_col(fill = "#895c89") +
  labs(x = "Weekday", y = "N Vessels") +
  facet_grid(rows = vars(SiteID),
             labeller = site_labs) +
  theme_bw() +
  theme(text = element_text(size = 18))


# Save figure
ggsave("Figures/Weekday_total_by_site_9NPZ.jpg", device = "jpeg",
       width=9, height=9, units="in", dpi=300)


#### INSIDE PARK PLOTS ####

# total by site

all_inside_by_site <- all_sites_hp |>
  group_by(SiteID) |>
  summarise(n_days = n_distinct(Begin_Date_loc),
            Total_ves = sum(total_inside, na.rm = TRUE),
            Total_ves_dep = sum(Total_ves, na.rm = TRUE),
            Total_ves_per_day = Total_ves_dep/n_days)

# plot
ggplot(data = all_ves_by_site,
       mapping = aes(x = SiteID,
                     y = Total_inside_ves_per_day)) +
  geom_col(fill = "#895c89") +
  labs(x = "Site", y = "N Vessels / Recording Day") +
  scale_x_discrete(labels = c("Cod Grounds", "Dampier", "Solitary Islands", "Murat", "Ningaloo", 
                              "South-west Corner", "Two Rocks"))+
  theme_bw() +
  theme(text = element_text(size = 18))



# Save figure
ggsave("Figures/Inside_ves_per_day_by_site.jpg", device = "jpeg",
       width=10, height=8, units="in", dpi=300)

#### INSIDE - DIEL ####

inside_diel <- all_sites_hp |>
  group_by(SiteID, Begin_Hour_loc) |>
  summarise(transit_tot = sum(trans_inside, na.rm=TRUE),
            maneuv_tot = sum(man_inside, na.rm = TRUE),
            total_ins = sum(total_inside, na.rm = TRUE)) |>
  pivot_longer(cols = transit_tot:total_ins,
               names_to = "Behavior", 
               values_to = "Count") |>
  filter(Behavior != "total_ins")

ggplot(data = inside_diel,
       mapping = aes(x = Begin_Hour_loc,
                     y = Count, fill = Behavior)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Hour of Day (local)", y = "N Vessels", fill = "Behavior") +
  facet_grid(rows = vars(SiteID),
             labeller = site_labs) +
  # legend 
  scale_fill_viridis_d(option = "E", labels = c("Maneuver", "Transit"))+
  theme_bw() +
  theme(text = element_text(size = 18))

# Save figure
ggsave("Figures/Diel_inside_by_site.jpg", device = "jpeg",
       width=9, height=9, units="in", dpi=300)



#### WEEKDAY - INSIDE ####


inside_week <- all_sites_hp |>
  mutate(Weekday = factor(Weekday, levels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))) |>
  group_by(SiteID, Weekday) |>
  summarise(transit_tot = sum(trans_inside, na.rm=TRUE),
            maneuv_tot = sum(man_inside, na.rm = TRUE),
            total_ins = sum(total_inside, na.rm = TRUE)) |>
  pivot_longer(cols = transit_tot:total_ins,
               names_to = "Behavior", 
               values_to = "Count") |>
  filter(Behavior != "total_ins")

levels(inside_week$Weekday) <- c("S","M","T","W","R","F","Sa")


ggplot(data = inside_week,
       mapping = aes(x = Weekday,
                     y = Count, fill = Behavior)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Day of Week", y = "N Vessels", fill = "Behavior") +
  facet_grid(rows = vars(SiteID),
             labeller = site_labs) +
  # legend 
  scale_fill_viridis_d(option = "E", labels = c("Maneuver", "Transit"))+
  theme_bw() +
  theme(text = element_text(size = 18))

# Save figure
ggsave("Figures/Weekday_inside_by_site.jpg", device = "jpeg",
       width=9, height=9, units="in", dpi=300)


#### SAVE SUMMARY TABLES ####
write_csv(all_ves_by_site, "data_outputs/All_ves_by_site.csv")
write_csv(all_inside_by_site, "data_outputs/All_inside_by_site.csv")
write_csv(all_ves_in_out, "data_outputs/All_ves_ins_out.csv")
