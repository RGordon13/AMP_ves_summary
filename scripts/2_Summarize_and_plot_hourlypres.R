#' ---------------------------------------------------------
#' 
#' 
#' Summarize vessel activity across 9 marine parks
#' 
#' Updated Jan 2025
#' ---------------------------------------------------------


# Load in libraries and dataset -------------------------------------------


#### Load libraries ####

# NOAA-approved tidyverse
tidyverse_short<-c("broom","cli","crayon","dbplyr","dplyr","dtplyr","forcats","ggplot2","googledrive","googlesheets4","hms","httr","jsonlite","lubridate","magrittr","modelr","pillar","purrr","readr","readxl","reprex","rlang","rstudioapi","rvest","stringr","tibble","tidyr","xml2") 
lapply(tidyverse_short, require, character.only = TRUE) 

library(scales)
library(ggalt)
library(ggsci)
library(ggdist)

theme_set(theme_bw(
  base_size = 14
))

# source helper file
source("scripts/AMP_summary_ves_funs.R")


#### Load compiled hourly presence table ####
# This is generated using Reformat_data_for_HP script

all_sites_hp <- read_csv("data_inputs/All_sites_tables/hourly_pres_allsites_local.csv")
all_sites_inout <- read_csv("data_inputs/All_sites_tables/total_ves_ins_out_by_site.csv")

  
# Reshape data for plotting -----------------------------------------------

#### Add columns to original dataset to help group for plotting ####

all_sites_hp <- all_sites_hp |>
  mutate(
    # replace NA's with 0's to accurately incorporate hours with presence = 0
    Total_Vessels = replace_na(Total_Vessels, 0),
    total_inside = replace_na(total_inside, 0),
    
    # add categorical Y/N column for vessels present to calculate proportions
    ves_yn = ifelse(Total_Vessels == 0, "N","Y"),
    ins_ves_yn = ifelse(total_inside == 0, "N","Y"),
    ins_ves_small_yn = ifelse(total_inside_small == 0, "N","Y"),
    
    # add date without year to collapse across years
    Month = month(Begin_Date_loc),
    Week = isoweek(Begin_Date_loc),
    Day_month = mday(Begin_Date_loc),
    Date_noyr = as_date(paste0(Month, "-", Day_month), format = "%m-%d"),
    
    # add network for grouping/faceting later
    network = case_when(Site_ID == "CG" | Site_ID == "EP" | Site_ID == "WP" ~ "Temperate East",
                        Site_ID == "DNE" | Site_ID == "DSW" | Site_ID == "NGN"| Site_ID == "NGS" ~ "Northwest",
                        TRUE ~ "South-west"
    ),
    npz_id = case_when(
      # Cod Grounds
      Site_ID == "CG" ~ "CGMP", 
      # Solitary Islands
      Site_ID == "EP" | Site_ID == "WP" ~ "SIMP",
      # Dampier
      Site_ID == "DNE" | Site_ID == "DSW" ~ "DMP",
      # Ningaloo
      Site_ID == "NGN"| Site_ID == "NGS" ~ "NMP", 
      # Two Rocks
      Site_ID == "TRE" | Site_ID == "TRW" ~ "TRMP", 
      # Jurien
      Site_ID == "JSE" | Site_ID == "JNE" ~ "JMP", 
      # Geographe
      Site_ID == "GEO" ~ "GMP",
      # Murat
      Site_ID == "MRE" | Site_ID == "MRW" ~ "MMP",
      # South-west Corner
      Site_ID == "SWS" ~ "SWCMP" 
    )
  )




#### Total Vessels by site ####

# Plot: x-axis = sites, y-axis = totals (single value)
#
# count up total vessels and vessels inside parks
all_ves_by_site <- all_sites_hp |>
  group_by(network, npz_id, Site_ID, Dep) |>
  summarise(n_days = n_distinct(Begin_Date_loc),
            Total_ves_dep = sum(Total_Vessels, na.rm = TRUE),
            Total_inside_ves_dep = sum(total_inside, na.rm = TRUE),
            Total_inside_ves_small = sum(total_inside_small, na.rm = TRUE))

ggplot(data = all_ves_by_site,
       aes(x = Dep,
           y = Total_ves_dep,
           fill = npz_id)) +
  geom_col(show.legend = FALSE,
           color = "black") +
  scale_fill_viridis_d()+
  facet_grid(~network, 
             scales = "free_x", 
             space = "free", 
             drop = TRUE) +
  labs(x = "NPZ",
       y = "N vessels") +
  theme(
    axis.text.x = element_text(angle = 90),
    strip.text.x = element_text(face = "bold")
    )


# Total Vessels per week for plots
all_ves_by_week <- all_sites_hp |>
  group_by(network, npz_id, Site_ID, Dep, Week) |>
  summarise(n_days = n_distinct(Begin_Date_loc),
            Total_ves_dep = sum(Total_Vessels, na.rm = TRUE),
            Total_inside_ves_dep = sum(total_inside, na.rm = TRUE),
            Total_inside_ves_small = sum(total_inside_small, na.rm = TRUE)
            ) |>
  mutate(
    network = fct(network),
    network = fct_relevel(network, c("Temperate East","Northwest"))
  )

ggplot(data = all_ves_by_week,
       aes(x = npz_id,
           y = Total_ves_dep,
           fill = network,
           color = network))+
  geom_boxplot(aes(group = Dep),
               alpha = 0.5)+
  scale_fill_viridis_d(guide = "none")+
  scale_color_viridis_d(guide = "none")+
  facet_grid(~network, 
             scales = "free_x", 
             space = "free", 
             drop = TRUE) +
  labs(x = "NPZ",
       y = "N vessels per week") +
  theme(
    strip.text.x = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold")
  )




# Total Vessels per day for plots
all_ves_by_date <- all_sites_hp |>
  group_by(network, npz_id, Site_ID, Dep, Date_noyr) |>
  summarise(n_days = n_distinct(Begin_Date_loc),
            Total_ves_dep = sum(Total_Vessels, na.rm = TRUE),
            Total_inside_ves_dep = sum(total_inside, na.rm = TRUE),
            Total_inside_ves_small = sum(total_inside_small, na.rm = TRUE)
  ) |>
  mutate(
    network = fct(network),
    network = fct_relevel(network, c("Temperate East","Northwest")),
    Date_noyr = as.Date(Date_noyr, format = "%Y-%m-%d")
  ) 

ggplot(data = all_ves_by_date,
       aes(x = Date_noyr,
           y = Total_ves_dep,
           fill = network))+
  geom_col(aes(group = Dep),
               alpha = 0.5,
           position = position_identity(),
           show.legend = FALSE)+
  scale_fill_viridis_d(guide = "none")+
  # scale_x_date(minor_breaks = "2 weeks") +
  labs(x = "Date",
       y = "N vessels per day") +
  theme(
    strip.text.y = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold")
  ) +
  facet_grid(rows = vars(network), 
             # scales = "free_y",
             # space = "free", 
             drop = TRUE)



#### Proportion presence ####

# Total vessels
#
# Plot: x-axis = sites, y-axis = proportion hours with vessel present (single value per deployment)
#
# Proportion of hours with vessel present for total deployment
all_ves_prop_hrs <- all_sites_hp |>
  # group by y/n column
  group_by(network, npz_id, Site_ID, Dep, Week, ves_yn) |>
  # get total number of hours for Y and N groups per each hour per deployment
  summarize(n_hours = n())|>
  mutate(freq = n_hours/sum(n_hours)) |>
  ungroup() |>
  complete(Site_ID, ves_yn,
           fill = list(ves_yn = "Y", freq = 0)) |>
  group_by(Site_ID, ves_yn) |>
  # reshape for easier plotting
  pivot_longer(cols = c("freq"),
               values_to = "Perc_per_hour") |>
  mutate(network = factor(network, levels = c("Temperate East","Northwest","South-west")))

# Try to plot!
ggplot(data = all_ves_prop_hrs |> filter(ves_yn == "Y"),
       aes(x = npz_id,
           y = Perc_per_hour,
           fill = network,
           color = network))+
  geom_boxplot(
    # aes(group = Dep),
    position = position_dodge2(preserve = "single"),
    alpha = 0.5,
    width = 0.5,
    outliers = FALSE)+
  geom_point(alpha = 0.5, 
             position = position_jitter(width = 0.1)) +
  scale_fill_viridis_d(end = 0.75,
                       direction = -1,
                       guide = "none")+
  scale_color_viridis_d(end = 0.75,
                        direction = -1,
                        guide = "none")+
  facet_grid(~network, 
             scales = "free_x", 
             space = "free", 
             drop = TRUE) +
  labs(x = "NPZ",
       y = "Proportion hours per week with vessels") +
  theme(
    strip.text.x = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold")
  )

ggsave(paste0("Figures/", "Weekly_prop_hrs_npz_ntwk.png"), width=10, height=8,
       units="in", dpi=300)




# Vessels inside parks -- N hours with vessel inside park out of N deployment hours
ins_ves_prop_hrs <- all_sites_hp |>
  # group by y/n column to collapse into hours
  group_by(network, npz_id, Site_ID, Dep, Week, ins_ves_yn) |>
  # get total number of hours for Y and N groups per each hour per deployment
  summarize(n_hours = n())|>
  mutate(freq = n_hours/sum(n_hours)) |>
  ungroup() |>
  complete(Site_ID, ins_ves_yn,
           fill = list(ins_ves_yn = "Y", freq = 0)) |>
  group_by(Site_ID, ins_ves_yn) |>
  # reshape for easier plotting
  pivot_longer(cols = c("freq"),
               values_to = "Perc_per_hour") |>
  # add network for grouping/faceting later
  mutate(
    network = case_when(Site_ID == "CG" | Site_ID == "EP" | Site_ID == "WP" ~ "Temperate East",
                        Site_ID == "DNE" | Site_ID == "DSW" | Site_ID == "NGN"| Site_ID == "NGS" ~ "Northwest",
                        TRUE ~ "South-west"
    ),
    network = factor(network, levels = c("Temperate East","Northwest","South-west")),
    npz_id = case_when(
      # Cod Grounds
      Site_ID == "CG" ~ "CGMP", 
      # Solitary Islands
      Site_ID == "EP" | Site_ID == "WP" ~ "SIMP",
      # Dampier
      Site_ID == "DNE" | Site_ID == "DSW" ~ "DMP",
      # Ningaloo
      Site_ID == "NGN"| Site_ID == "NGS" ~ "NMP", 
      # Two Rocks
      Site_ID == "TRE" | Site_ID == "TRW" ~ "TRMP", 
      # Jurien
      Site_ID == "JSE" | Site_ID == "JNE" ~ "JMP", 
      # Geographe
      Site_ID == "GEO" ~ "GMP",
      # Murat
      Site_ID == "MRE" | Site_ID == "MRW" ~ "MMP",
      # South-west Corner
      Site_ID == "SWS" ~ "SWCMP" 
    )
  )
#
#
#


# Try to plot!
ggplot(data = ins_ves_prop_hrs |> filter(ins_ves_yn == "Y"),
       aes(x = npz_id,
           y = Perc_per_hour,
           fill = network,
           color = network))+
  geom_boxplot(
    # aes(group = Dep),
    position = position_dodge2(preserve = "single"),
    alpha = 0.5,
    width = 0.5,
    outliers = FALSE)+
  geom_point(alpha = 0.5, 
             position = position_jitter(width = 0.1)) +
  scale_fill_viridis_d(end = 0.75,
                       direction = -1,
                       guide = "none")+
  scale_color_viridis_d(end = 0.75,
                        direction = -1,
                        guide = "none")+
  facet_grid(~network, 
             scales = "free_x", 
             space = "free", 
             drop = TRUE) +
  labs(x = "NPZ",
       y = "Proportion hours per week \nwith vessels inside NPZ") +
  theme(
    strip.text.x = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold")
  )

ggsave(paste0("Figures/", "Weekly_prop_hrs_npz_ntwk_ins.png"), width=10, height=8,
       units="in", dpi=300)








# Proportion of 'vessel hours' with inside vessels over whole deployment
# Plot: x-axis = site, y-axis = proportion of total vessel hours with inside vessels (single value)
# i.e., "of all hours with a vessel, X% contain a vessel inside NPZ"
ins_ves_prop_allves <- all_sites_hp |>
  # group by vessel condition to get counts
  group_by(network, SiteID, ves_yn, ins_ves_yn) |>
  # get total number of hours for Y and N groups per each hour per deployment
  summarize(n_hours = n())|>
  mutate(freq = n_hours/sum(n_hours)) |>
  ungroup() |>
  complete(SiteID, ins_ves_yn,
           fill = list(ins_ves_yn = "Y", freq = 0)) |>
  group_by(SiteID, ins_ves_yn) |>
  # reshape for easier plotting
  pivot_longer(cols = c("freq"),
               values_to = "Perc_per_hour")
#
#
#



#### Diel patterns ####

# Percent vessels per hour of day
#
# Plot: x-axis = hour of day (local time), y-axis = proportion hours vessel present (single value)


all_diel_perc <- all_sites_hp |>
  # group by Begin Hour to collapse into hours
  group_by(network, SiteID, Begin_Hour_loc, ves_yn) |>
  # get total number of hours for Y and N groups per each hour per deployment
  summarize(n_hours = n())|>
  mutate(freq = n_hours/sum(n_hours)) |>
  ungroup() |>
  complete(SiteID, Begin_Hour_loc, ves_yn,
           fill = list(ves_yn = "Y", freq = 0)) |>
  group_by(SiteID, Begin_Hour_loc, ves_yn) |>
  # reshape for easier plotting
  pivot_longer(cols = c("freq"),
               values_to = "Perc_per_hour")


# Percent hours with inside vessels per 'vessel hours' hour of day
#
# Plot: x-axis = hour of day (local time), y-axis = proportion hours vessel present inside the park (single value)

# of hours with acoustically detected vessels, __% had presence inside the park
ins_diel_perc <- all_sites_hp |>
  # group by Begin Hour to collapse into hours
  group_by(network, SiteID, Begin_Hour_loc, ves_yn, ins_ves_yn) |>
  # get total number of hours for Y and N groups per each hour per deployment
  summarize(n_hours = n())|>
  mutate(freq = n_hours/sum(n_hours)) |>
  ungroup() |>
  complete(SiteID, ins_ves_yn,
           fill = list(ins_ves_yn = "Y", freq = 0)) |>
  group_by(SiteID, ins_ves_yn) |>
  # reshape for easier plotting
  pivot_longer(cols = c("freq"),
               values_to = "Perc_per_hour")
#
#
#




#### Weekday patterns ####

# Weekday totals

# Plot: x-axis = weekday, y-axis = median +/- 25th - 75th percentiles (single value)
#
# count up total vessels and vessels inside parks
all_ves_by_weekday <- all_sites_hp |>
  group_by(network, SiteID, Date_noyr, Weekday) |>
  summarise(n_days = n_distinct(Date_noyr),
            Total_ves_dep = sum(Total_Vessels, na.rm = TRUE),
            Total_ves_per_day = Total_ves_dep/n_days,
            Total_inside_ves_dep = sum(total_inside, na.rm = TRUE),
            Total_inside_ves_per_day = Total_inside_ves_dep/n_days) |>
  mutate(Weekday = factor(Weekday, levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")))
levels(all_ves_by_weekday$Weekday) <- c("M","T","W","R","F","Sa","Su")

weekday_summary <- all_ves_by_weekday |>
  group_by(network, SiteID, Weekday) |>
  summarise(total_weekday = sum(Total_ves_dep),
            median_ves_weekday = median(Total_ves_dep),
            weekday_lower = quantile(Total_ves_dep, 0.25), 
            weekday_upper = quantile(Total_ves_dep, 0.75),
            total_ins_weekday = sum(Total_inside_ves_dep),
            median_ins_ves_weekday = median(Total_inside_ves_dep),
            weekday_ins_lower = quantile(Total_inside_ves_dep, 0.25), 
            weekday_ins_upper = quantile(Total_inside_ves_dep, 0.75)) |>
  mutate(Weekday = factor(Weekday, levels=c("M","T","W","R","F","Sa","Su")))

# Weekday proportional presence
# of all available hours represented by X weekday, Y% had vessels present
all_perc_weekday <- all_sites_hp |>
  # group by date 
  group_by(network, SiteID, Weekday, ves_yn) |>
  # get total number of hours for Y and N groups per each hour per deployment
  summarize(n_hours = n())|>
  mutate(freq = n_hours/sum(n_hours)) |>
  ungroup() |>
  complete(SiteID, Weekday, ves_yn,
           fill = list(ves_yn = "Y", freq = 0)) |>
  group_by(SiteID, Weekday, ves_yn) |>
  # reshape for easier plotting
  pivot_longer(cols = c("freq"),
               values_to = "Perc_per_hour")


#### Patterns over deployment period ####


# total vessels per week
weekly_ves <- all_sites_hp |>  
  mutate(iso_week = isoweek(Begin_Date_loc)) |>
  group_by(network, SiteID, iso_week) |>
  summarize(mean_ves = mean(Total_Vessels, na.rm = TRUE),
            med_ves = median(Total_Vessels, na.rm = TRUE),
            sd_ves = sd(Total_Vessels, na.rm = TRUE),
            sd_lower = mean_ves - (0.5*sd_ves),
            sd_upper = mean_ves + (0.5*sd_ves))


# total vessels & summary stats per day
daily_ves <- all_sites_hp |>  
  group_by(network, SiteID, Date_noyr) |>
  summarize(mean_ves = mean(Total_Vessels, na.rm = TRUE),
            med_ves = median(Total_Vessels, na.rm = TRUE),
            sd_ves = sd(Total_Vessels, na.rm = TRUE),
            sd_lower = mean_ves - (0.5*sd_ves),
            sd_upper = mean_ves + (0.5*sd_ves))



# Plots -------------------------------------------------------------------

#### set up useful for all plots ####

# make list of site labels for plotting
site_labs <- as_labeller(
  c("CG" = "Cod Grounds", 
    "DNE" = "Dampier", 
    "EP" = "Solitary \nIslands",
    "MRE" = "Murat",
    "NGN" = "Ningaloo", 
    "SWC" = "South-west \nCorner",
    "TRW D1" = "Two Rocks",
    "JNE" = "Jurien",
    "GEO" = "Geographe"))




#### Weekday ####


# test plot to check whether weekday ordering worked
ggplot(data = weekday_summary,
       mapping = aes(x = Weekday, 
                     y = median_ves_weekday)) +
  geom_col() +
  geom_linerange(aes(ymin = weekday_lower,
                     ymax = weekday_upper))+
  facet_wrap(~SiteID) +
  theme_bw()

# # Save figure
# ggsave("Figures/weekday_by_site_qt25-75.jpg", device = "jpeg",
#        width=10, height=8, units="in", dpi=300)


# test plot to check whether weekday ordering worked
ggplot(data = all_ves_by_weekday,
       mapping = aes(x = Weekday, 
                     y = Total_ves_dep, color = network)) +
  geom_boxplot()+
  facet_wrap(~c(network)) +
  theme_bw()

# # Save figure
ggsave("Figures/weekday_by_network_boxplot.jpg", device = "jpeg",
       width=10, height=8, units="in", dpi=300)


# Weekday rainclouds ------------------------------------------------------


# What about a raincloud plot for all sites combined? 
# or maybe facet/color by network?
library(ggdist)

# start with boxplot
ggplot(data = all_ves_by_weekday,
       mapping = aes(x = Weekday, 
                     y = Total_ves_dep,
                     color = network)) +
  # add half violin to plot alongside boxplot 
  ggdist::stat_halfeye(aes(fill = network),
                       alpha = 0.5,
                       width = 0.4, # adjust height
                       # justification = -0.2, # move to the right
                       # .width = 0 # remove slab interval
                       )+
  # add skinny boxplot
  geom_boxplot(width = 0.15,
               alpha = 0.3,
               outlier.colour = NA)+ 

    # add rain to raincloud
  ggdist::stat_dots(
    aes(fill = network),
    # point to the left
    side = "left",
    # move to the left
    justification = 1.1,
    binwidth = 0.1,
    layout = "hex",
    overflow = "compress") +
  facet_grid(rows = vars(network),
             scales = "free_y") +
  theme_bw()


# start with boxplot
ggplot(data = all_ves_by_weekday |> filter(network == "Temperate East"),
       mapping = aes(x = Weekday, 
                     y = Total_ves_dep,
                     color = network)) +
  # add half violin to plot alongside boxplot 
  ggdist::stat_halfeye(aes(fill = network),
                       alpha = 0.5,
                       width = 0.4, # adjust height
                       # justification = -0.2, # move to the right
                       # .width = 0 # remove slab interval
  )+
  # add skinny boxplot
  geom_boxplot(width = 0.15,
               alpha = 0.3,
               outlier.colour = NA)+ 
  
  # add rain to raincloud
  ggdist::stat_dots(
    aes(fill = network),
    # point to the left
    side = "left",
    # move to the left
    justification = 1.1,
    binwidth = 0.3,
    layout = "hex",
    overflow = "compress") +
  facet_grid(rows = vars(network),
             scales = "free_y") +
  theme_bw()
ggsave("Figures/weekday_by_network_raincloud.jpg", device = "jpeg",
       width=10, height=8, units="in", dpi=300)


# 
#   #add raw data as overlay
#   geom_point(alpha = 0.2,
#              position = position_jitter(seed = 1, width = 0.2))


### Weekday inside

# By site
ggplot(data = all_ves_by_weekday,
       mapping = aes(x = Weekday, 
                     y = Total_inside_ves_per_day)) +
  geom_boxplot() +
  # geom_linerange(aes(ymin = weekday_ins_lower,
  #                    ymax = weekday_ins_upper))+
  facet_wrap(~SiteID) +
  theme_bw()

# Save figure
ggsave("Figures/inside_weekday_by_site_boxplot.jpg", device = "jpeg",
       width=10, height=8, units="in", dpi=300)


# By site
ggplot(data = all_ves_by_weekday,
       mapping = aes(x = Weekday, 
                     y = Total_inside_ves_per_day)) +
  geom_boxplot() +
  # geom_linerange(aes(ymin = weekday_ins_lower,
  #                    ymax = weekday_ins_upper))+
  facet_wrap(~network) +
  theme_bw()

# Save figure
ggsave("Figures/inside_weekday_by_network_boxplot.jpg", device = "jpeg",
       width=10, height=8, units="in", dpi=300)





#### Polar plot diel ####
ggplot(data = all_diel_perc |> 
         filter(ves_yn=="Y"), # only plot 'yes' proportions
       aes(x=Begin_Hour_loc, 
           y = Perc_per_hour, 
           fill = Perc_per_hour,))+
  geom_line(aes(group = SiteID))+
  coord_polar(theta = "x", start = 0) +
  # scale_fill_viridis_c(labels = label_number(accuracy = 0.01), 
  #                      begin = 0, end = 1, limits = c(0,1), 
  #                      breaks = c(0,0.25,0.5,0.75,1), 
  #                      option = "B")+
  scale_x_continuous(breaks=seq(0,23,4), 
                     minor_breaks = seq(0:23),
                     expand = c(0,0)) +
  # scale_y_continuous(limits=c(0,1),
  #                    breaks=seq(0,1,0.25),
  #                    expand = c(0, 0)) +
  ylab("N hours present / Total deployment Hours") +
  # ylim(0,1)+
  xlab("Hour")+
  theme(text=element_text(size=20),
        strip.background = element_rect(color = "white", fill = "black"),
        strip.text.x = element_text(colour = "white", face = "bold"),
        strip.text.y = element_text(colour = "white", face = "bold"),
        plot.title = element_text(hjust = 0.5))+
  #R turns the proportion values into bins in the legend when I change the title, so I'm removing it instead
  theme(legend.title=element_blank()) +
  facet_wrap(facets = vars(SiteID), nrow = 3,
             scales = "free_y")

# # Save figure
# ggsave("Figures/Diel_by_site_fixed_y.jpg", device = "jpeg",
#        width=10, height=8, units="in", dpi=300)



#### Heat map date vs. diel ####

ggplot(all_sites_hp, 
       aes(x = Date_noyr , 
           y = Begin_Hour_loc, 
           fill= Total_Vessels)) +
  geom_tile()+
  scale_fill_viridis_c()+
  facet_grid(rows = vars(SiteID), scales = "free_x")+
  ylab("Begin Hour")+
  xlab("Date")+
  ggtitle("Vessel Activity")+
  # scale_x_date(
  #   name="Julian Day", 
  #   date_breaks="1 month",
  #   date_labels="%b",
  #   expand=expansion(0,0))+  
  theme(text=element_text(size=18), 
        strip.background = element_rect(color = "white", fill = "black"), 
        strip.text.x = element_text(colour = "white", face = "bold"), 
        strip.text.y = element_text(colour = "white", face = "bold"), 
        plot.title = element_text(hjust = 0.5))


# Save figure
ggsave("Figures/Diel_allsites_heatmap.jpg", device = "jpeg",
       width=10, height=8, units="in", dpi=300)


# boxplot diel
ggplot(data = all_sites_hp, 
       aes(x = Begin_Hour_loc, 
           y = Total_Vessels,
           group = SiteID)) +
  geom_boxplot()+
  # scale_fill_viridis_c()+
  facet_grid(rows = vars(SiteID), scales = "free_x")+
  xlab("Begin Hour")+
  xlab("Total Vessels")+
  theme(text=element_text(size=18), 
        strip.background = element_rect(color = "white", fill = "black"), 
        strip.text.x = element_text(colour = "white", face = "bold"), 
        strip.text.y = element_text(colour = "white", face = "bold"), 
        plot.title = element_text(hjust = 0.5))


# Save figure
ggsave("Figures/Diel_allsites_heatmap.jpg", device = "jpeg",
       width=10, height=8, units="in", dpi=300)




# weekly summary collapsed into ISO weeks
ggplot(data = weekly_ves,
       aes(x = iso_week)) + 
  geom_ribbon(aes(ymin = sd_lower, 
                  ymax = sd_upper,
                  group = SiteID,
                  fill = network),
              alpha = 0.25)+
  geom_line(mapping = aes(y = mean_ves, 
                          color = network,
                          group = SiteID)) +
  facet_wrap(network~SiteID)+
  labs(x = "Julian Day", y = "Mean Vessels/Hour")



# daily summary for Julian Day

ggplot(data = daily_ves,
       aes(x = day_index)) + 
  geom_ribbon(aes(ymin = sd_lower, 
                  ymax = sd_upper,
                  group = SiteID,
                  fill = network),
              alpha = 0.25)+
  geom_line(mapping = aes(y = mean_ves, 
                          color = network,
                          group = SiteID)) +
  facet_grid(rows = vars(SiteID))+
  labs(x = "Julian Day", 
       y = "Mean Vessels/Day +/- SD") +
  theme(text=element_text(size=18), 
        strip.background = element_rect(color = "white", fill = "black"), 
        strip.text.x = element_text(colour = "white", face = "bold"), 
        strip.text.y = element_text(colour = "white", face = "bold"), 
        plot.title = element_text(hjust = 0.5))

ggsave("Figures/Daily_meanSD_by_site.jpg", device = "jpeg",
        width=10, height=8, units="in", dpi=300)
  






#### Inside vessels ####
# Add some inside/outside columns
all_ves_in_out <- all_sites_hp |>
  mutate(trans_inside = replace_na(trans_inside, 0),
         man_inside = replace_na(man_inside, 0),
         total_inside = replace_na(total_inside, 0),
         total_outside = Total_Vessels - total_inside)#|>
  # pivot_longer(cols = c("Outside","Inside"),
  #              names_to = "Total_ins_out",
  #              values_to = "Total")



# reshape for stacked bar plot
# Add some inside/outside columns
ins_ves_by_site <- all_ves_by_site |>
  mutate(Outside = Total_ves_dep - Total_inside_ves_dep) |>
  rename("Inside" = "Total_inside_ves_dep") |>
pivot_longer(cols = c("Outside","Inside"),
             names_to = "Total_ins_out",
             values_to = "Total")




ins_diel_perc <- all_ves_in_out |>
  mutate(ins_yn = ifelse(total_inside == 0, "N","Y"))|>
  # group by Begin Hour to collapse into hours
  group_by(SiteID, Begin_Hour_loc, ins_yn) |>
  # get total number of hours for Y and N groups per each hour per deployment
  summarize(n_hours = n())|>
  mutate(freq = n_hours/sum(n_hours)) |>
  ungroup() |>
  complete(SiteID, Begin_Hour_loc, ins_yn,
           fill = list(ins_yn = "Y", freq = 0)) |>
  group_by(SiteID, Begin_Hour_loc, ins_yn) |>
  # reshape for easier plotting
  pivot_longer(cols = c("freq"),
               values_to = "Perc_per_hour")




#### INSIDE Polar plot diel ####
ggplot(data=ins_diel_perc |> 
         filter(ins_yn=="Y"), # only plot 'yes' proportions
       aes(x=Begin_Hour_loc, 
           y = Perc_per_hour, 
           fill = Perc_per_hour,))+
  geom_bar(stat="identity")+
  coord_polar(theta = "x", start = 0) +
  scale_fill_viridis_c(labels = label_number(accuracy = 0.01), 
                       begin = 0, end = 1, limits = c(0,1), 
                       breaks = c(0,0.25,0.5,0.75,1), 
                       option = "B")+
  scale_x_continuous(breaks=seq(0,23,4), 
                     minor_breaks = seq(0:23),
                     expand = c(0,0)) +
  # scale_y_continuous(limits=c(0,1),
  #                    breaks=seq(0,1,0.25),
  #                    expand = c(0, 0)) +
  ylab("N hours present inside / Total deployment Hours") +
  # ylim(0,1)+
  xlab("Hour")+
  theme(text=element_text(size=20),
        strip.background = element_rect(color = "white", fill = "black"),
        strip.text.x = element_text(colour = "white", face = "bold"),
        strip.text.y = element_text(colour = "white", face = "bold"),
        plot.title = element_text(hjust = 0.5))+
  #R turns the proportion values into bins in the legend when I change the title, so I'm removing it instead
  theme(legend.title=element_blank()) +
  facet_wrap(facets = vars(SiteID), nrow = 3,
             scales = "free_y")



# stacked barplot inside-outside
ggplot(data = ins_ves_by_site,
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
       width=14, height=8, units="in", dpi=300)



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
write_csv(weekday_summary, "data_outputs/All_ves_by_weekday.csv")
write_csv(all_ves_prop_hrs, "data_outputs/All_ves_by_hour.csv")
