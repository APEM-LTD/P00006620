##########################################
####### Load libraries data ##############
##########################################

# to install hetoolkit from github
install.packages("devtools")
library(devtools)
install_github("APEM-LTD/hetoolkit")
library(hetoolkit)

library(hetoolkit) # for EA hydro-ecology toolkit
library(readxl)
library(dplyr)
library(ggplot2)
library(viridis) # for ggplot colour schemes
library(gridExtra) # for arranging multiple ggplots in a single figure
library(directlabels) # for labelling lines in plots
library(ggrepel) # for gglot labels
library(GGally) # for ggpairs function
library(gridExtra) # for multi-plots
library(lmerTest) # for lme4 function
library(mgcv) # for gam modelling
library(lubridate) # for date manipulation



##########################################
############### Import data ##############
##########################################

MasterData <- "Data/P6620 Master dataset_2022-05-31.xlsx"

Schemes <- read_excel(MasterData, sheet = "Schemes")
Flows <- read_excel(MasterData, sheet = "Velocity_Master")
RawFlows <- read_excel(MasterData, sheet = "Flows")

##########################################
############### Data review ##############
##########################################

#create new rows in velocity master sheet

#create a mean velocity column

Flows <- subset(Flows, select = -c(count))

Flows <- Flows %>%
  transform(velocity = as.numeric(velocity)) %>% #change column from character to numeric caused by '-' and '?'entries
  transform(depth = as.numeric(depth)) %>% #change column from character to numeric
  dplyr::filter(velocity != "DRY") %>% #don't include the spot gauging location that are detailed as DRY
  dplyr::group_by(site_name, year, month) %>%
  dplyr::mutate(chan_width_m = max(dist_from_bank, na.rm = TRUE)) %>%
  dplyr::mutate(mean_vel_msec = mean(velocity, na.rm = TRUE)) %>% #calculate mean velocities for each specific site/year/month
  dplyr::mutate(max_vel_msec = max(velocity, na.rm = TRUE)) %>% #find the max velocity for each specific site/year/month
  dplyr::mutate(sd_vel = sd(velocity, na.rm = TRUE)) %>% #calculate standard deviation velocities for each specific site/year/month
  dplyr::mutate(mean_depth_cm = mean(depth, na.rm = TRUE)) %>% #calculate mean velocities for each specific site/year/month
  dplyr::mutate(max_depth_cm = max(depth, na.rm = TRUE)) %>% #find the max depth for each specific site/year/month
  dplyr::mutate(sd_depth = sd(depth, na.rm = TRUE)) %>% #calculate standard deviation depth for each specific site/year/month
  dplyr::mutate(discharge = mean_vel_msec * mean_depth_cm * chan_width_m) %>% #calculate discharges by multiplying mean velocity by mean depth by channel width per specific transect
  dplyr::mutate(trans_points= (count = n())) %>%
  dplyr::mutate(gravel = ifelse(substrate_type %in% c("gravel","gravels", "Gravel", "Gravels"), TRUE, FALSE)) %>%
  ungroup()


