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


# Assign user-defined inputs ----------------------------------------------

# Since server folders are in a standard structure, use parent folder to get list of all deployments
dep_names <- list.dirs(tk_choose.dir(caption = "Select parent dir for all deployment folders"), recursive = FALSE, full.names = FALSE)

# select the deployment(s) to be plotted
dep_list <- dlg_list(title = "Select deployments to plot", choices = dep_names, multiple = TRUE)$res |>
  str_sub(start = 16)

# apply getDeploymentInfo() from AMP_pkgs_funs.R to each deployment
#   prompts user for site name, start/end date, and time zones
dep_info <- dep_list |>
  map(~getDeploymentInfo(.)) |>
  set_names(dep_list)


# Load data ---------------------------------------------------------------

# For each deployment in the dep_info list, load in hourly presence table & compiled selection table
hp_og <- dep_info |>
  map(~read_csv(choose.files(caption = paste0({.}$site_id, {.}$dep_id, " Hourly Presence sheet .csv"))))|>
  # use imap() to get info based on index of each iteration
  # in this case, we want the name of the list element, designated as ".y"
  imap(~mutate(., Dep_ID = .y))

# For each deployment in the dep_info list, load in compiled selection table
selns_og <- dep_info |>
  map(~read_delim(choose.files(caption = paste0({.}$site_id, {.}$dep_id, " Complied seln table .txt"))))|>
  # use imap() to get info based on index of each iteration
  # in this case, we want the name of the list element, designated as ".y"
  imap(~mutate(., Dep_ID = .y)) 


# For each deployment in the dep_info list, load in I-O excel files
ins_out_og <- dep_info |>
  map(~read_xlsx(choose.files(caption = paste0({.}$site_id, {.}$dep_id, " I-O table .xlsx"))))|>
  # use imap() to get info based on index of each iteration
  # in this case, we want the name of the list element, designated as ".y"
  imap(~mutate(., Dep_ID = .y))

# Mutate data ------------------------------------------------------------


#### Add some columns, update time zone ####
# making this to be able to add column names
new_cols <- c("Maneuver" = 0,"Transit" = 0, "Not_Assigned" = 0)
hp_allcols <- hp_og |>
  # add columns for Transit and Maneuver if they don't exist
  map(~add_column(., !!!new_cols[!names(new_cols) %in% names(.)])
  ) 


# Hourly presence
hp_data <- hp_allcols |>
  map(~rename(., 
              any_of(c("TR" = "Transit",
              "M" = "Maneuver",
              "Dep" = "Dep_ID")))) |>
  map(~relocate(., 
                TR, .after = last_col())) |>
  map(~relocate(., 
                Not_Assigned, .after = last_col())) |>
  map(~relocate(., 
                M, .after = last_col())) |>
  map2(.y = dep_info, 
       ~mutate(.x,
               Site_ID = {.y}$site_id, 
               Dep_ID = {.y}$dep_id,
               Total_Vessels = rowSums(across(c(TR, M))),
               # create y/n column for vessel presence
               ves_yn = ifelse(Total_Vessels == 0, "N", "Y"),
               # pull hour as time object along with date
               Hr_time = paste(Begin_Date, Begin_Hour, ":00"),
               # get components of date-time object and assign time zone
               Hr_time = parse_date_time(Hr_time, "ymd H:M", tz = {.y}$tz_files),
               # change to local time zone
               Hr_local = with_tz(Hr_time, tzone = {.y}$tz_local),
               # pull out new hour and date in local time
               Begin_Hour_loc = as.numeric(hour(Hr_local)),
               Begin_Date_loc = date(Hr_local),
               # add weekday column
               Weekday = weekdays(Begin_Date_loc))) |>
  bind_rows()

# Create CSV to make data input easier. Maybe eventually add some logic in script to bypass generating this?
# write_csv(hp_data, "data_outputs/total_ves_hp_by_site.csv")



# Selection tables --------------------------------------------------------


# reshape selection tables to plot duration
selns_data <- selns_og |>
  map(~rename(., "Dep" = "Dep_ID")) |>
  map2(.y = dep_info, 
       ~mutate(.x, 
               Begin_Date = ymd(Begin_Date),
               Begin_file_date = ymd(Begin_file_date),
               Site_ID = {.y}$site_id, 
               # Dep = {.y}$dep_id,
               # re-code anything with a maneuver as "Maneuver" (e.g., "Maneuver+CPA" = "Maneuver")
               Behavior = gsub(pattern = ".*Maneuver.*", replacement = "Maneuver", x = Behavior),
               # re-code all transit as "Transit" (e.g., "TransitA" = "Transit")
               # important that this happens AFTER the Maneuver line above so that "TransitAManeuver" doesn't get re-coded
               Behavior = gsub(pattern = ".*Transit.*", replacement = "Transit", x = Behavior),
               Behavior = gsub(pattern = "CPAManeuver", replacement = "Maneuver", x = Behavior),
               Behavior = gsub(pattern = "CPA", replacement = "Transit", x = Behavior),
               Behavior = gsub(pattern = " ", replacement = "", x = Behavior),
               DeltaHours = Delta_Time_s/3600,
               # Total_Vessels = rowSums(across(c(TR,M))),
               # pull hour as time object along with date
               Hr_time = paste(Begin_Date, Begin_Clock),
               # get components of date-time object and assign time zone
               Hr_time = parse_date_time(Hr_time, "y/m/d H:M:S", tz = {.y}$tz_files),
               # change to local time zone
               Hr_local = with_tz(Hr_time, tzone = {.y}$tz_local),
               # pull out new hour and date in local time
               Begin_Hour_loc = as.numeric(hour(Hr_local)),
               Begin_Date_loc = date(Hr_local),
               # add weekday column
               Weekday = weekdays(Begin_Date_loc))) |>
  bind_rows()


# Bind rows to get df for inside-outside tables
# don't need to do other adjustments bc will join with seln tables

ins_out_data <- ins_out_og |>
  map(~select(., Filename, Date, Selection, used, pins_sm, pins_med, pins_lg, pins_ovrll, Dep_ID)) |>
  map(~mutate(., Date = as.numeric(Date),
              Date = as.character(Date))) |>
  bind_rows()


ins_vessels <- ins_out_data |>
  mutate(begin_file_date = ymd(str_sub(Date, 1,6)),
                      seln_num = as.numeric(gsub(pattern = "S", replacement = "", x = Selection)))|>
  left_join(selns_data, 
            by = c("Dep_ID" = "Dep",
                   "seln_num" = "Selection", 
                   "begin_file_date" = "Begin_file_date"))

#### LEFT OFF HERE -- join not behaving?? ####
# Warning message:
#   In left_join(mutate(ins_out_data, begin_file_date = ymd(str_sub(Date,  :
#                                                                     Detected an unexpected many-to-many relationship between `x` and `y`.
#                                                                   ℹ Row 71 of `x` matches multiple rows in `y`.
#                                                                   ℹ Row 488 of `y` matches multiple rows in `x`.
#                                                                   ℹ If a many-to-many relationship is expected, set `relationship = "many-to-many"` to silence this warning.
# 



#### Ins Park tables -- reshape into hp ####

# map this over dep_list?
# reshape into HP to merge with HP tables
ins_cgmp_hp <- inside_tables_to_hp(ins_table = ins_cgmp, site_id = "Cod Grounds")
ins_simp_hp <- inside_tables_to_hp(ins_table = ins_simp, site_id = "EP")
ins_dne_hp <- inside_tables_to_hp(ins_table = ins_dne, site_id = "DNE")
ins_ngn_hp <- inside_tables_to_hp(ins_table = ins_ngn, site_id = "NGN")
ins_trw_hp <- inside_tables_to_hp(ins_table = ins_trw, site_id = "TRW D1")
ins_geo_hp <- inside_tables_to_hp(ins_table = ins_geo, site_id = "GEO")
ins_jur_hp <- inside_tables_to_hp(ins_table = ins_jur, site_id = "JNE")







#### bring all HP data together ####
all_sites_hp <- as.data.frame(rbind(hp_cgmp_local, hp_simp_local,
                     hp_dne_local, hp_mur_local, hp_ngn_local, 
                     hp_trw_local, hp_sws_local, hp_geo_local, hp_jur_local))

write.csv(all_sites_hp, "data_outputs/hourly_pres_allsites_local.csv")

