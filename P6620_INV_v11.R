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

MasterData <- "Data/APEM_Model_Database_MASTER_2021-12-16-1050.xlsx"

Schemes <- read_excel(MasterData, sheet = "Schemes")
INV_Sites <- read_excel(MasterData, sheet = "INV_Sites")
INV_Samples <- read_excel(MasterData, sheet = "INV_Samples")
INV_Sites2Schemes <- read_excel(MasterData, sheet = "INV_Sites2Schemes")
RawFlows <- read_excel("Data/Flows_2021-12-16-1046.xlsx", sheet = "Flows")

INV_Samples <- INV_Samples %>%
  filter(site_id != "INV-Ver12") %>% # filtered out INV-Ver12 due to being an AMP5 restoration site
  filter(year >= 2014 & year <= 2020) 


##########################################
############### Data review ##############
##########################################

#join the sites and inv-sites tables by the site_code attribute and define it in an object table_join
table_join_INV <- INV_Samples %>% left_join(INV_Sites, by = c("site_id"))
table_join_INV$watercourse <- as.factor(table_join_INV$watercourse)
table_join_INV$site_id <- as.factor(table_join_INV$site_id)

#create a ggplot object with year vs rmni eqr#
#ggplot(data = table_join, aes(x = year, y = raw_rmni_eqr)) + geom_point() + facet_wrap(vars(watercourse))

# years of data per site
table_join_INV %>%
  select(year, site_id, watercourse) %>%
  dplyr::group_by(year, site_id, watercourse) %>%
  dplyr::summarise(nsamples = n()) %>%
  dplyr::group_by(site_id, watercourse) %>%
  dplyr::summarise(nyears = n()) %>%
  #ungroup() %>%
  ggplot(aes(x = nyears)) +
  geom_histogram(position = "stack", binwidth=1) +
  labs(x = "Number of years of data", y = "Number of sites")

ggsave(filename = "Outputs/Macroinvertebrates/hist1_INV.png") 

#### for inverts, also calculate number of samples per site...
table_join_INV %>%
  select(year, site_id, watercourse) %>%
  filter(is.na(site_id) == FALSE) %>%
  dplyr::group_by(site_id) %>%
  dplyr::summarise(nsamples = n()) %>%
  ggplot(aes(x = nsamples)) +
  geom_histogram(position = "stack", binwidth=1) +
  labs(x = "Number of Samples", y = "Number of Sites")

ggsave(filename = "Outputs/Macroinvertebrates/INV_samples_per_site.png")

table_join_INV %>%
  select(year, site_id, watercourse) %>%
  filter(is.na(site_id) == FALSE) %>%
  dplyr::group_by(year, site_id, watercourse) %>%
  dplyr::summarise(n = n()) %>%
  filter(year >= 2014) %>%
  ggplot(aes(year, site_id, fill = n)) + 
  geom_tile(colour = "white") + 
  #scale_fill_viridis(option = "inferno", direction=1) + 
  labs(fill = "Number of samples") +
  labs(x = "Year", y = "Site ID") +
  #scale_y_discrete(limits = rev(levels(table_join$watercourse))) +
  scale_x_continuous(breaks = c(2014:2020)) +
  facet_grid(vars(watercourse), space = "free", scale = "free_y") +
  theme(axis.text.y = element_text(size = 6)) 

ggsave(filename = "Outputs/Macroinvertebrates/Data coverage tiles.png")

# # data coverage
# table_join_INV %>%
#   select(year, site_id, watercourse) %>%
#   filter(is.na(site_id) == FALSE) %>%
#   dplyr::group_by(year, site_id, watercourse) %>%
#   dplyr::summarise(nsamples = n()) %>% # counts number of samples
#   # dplyr::group_by(site_id) %>%
#   #mutate(nyears = n()) %>% 
#   # drop sites with only a single year of data
#   filter(nsamples <= 6) %>%
#   
# 
#   #plots the table into a tile plot
#   ggplot(aes(year, site_id, fill = nsamples)) + 
#   geom_tile(colour = "white") + 
#   #scale_fill_viridis(option = "inferno", direction=1) + 
#   scale_x_continuous(breaks = c(2014:2020)) +
#   labs(fill = "Number of samples") + # legend caption
#   labs(x = "Year", y = "Site ID") +
#   #scale_y_discrete(limits = rev(levels(table_join$watercourse))) + 
#   facet_grid(vars(watercourse), space = "free", scale = "free_y") +
#   theme(axis.text.y = element_text(size = 4)) 



# cross-correlation plots
GGally::ggpairs(table_join_INV[,c("WHPT_ASPT_OE", "WHPT_NTAXA_OE", "LIFE_F_OE", "PSI_OE")]) 

ggsave(filename = "Outputs/Macroinvertebrates/Metrics_correlation.png", width = 18, height = 15, units = "cm")

# time series plots (by site) - LIFE_F_OE
table_join_INV %>%
  left_join(INV_Sites2Schemes, by = c("site_id")) %>%
  filter(is.na(site_id) == FALSE) %>%
  ggplot(aes(x = year, y = LIFE_F_OE, colour = control_impact, group = site_id)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(vars(watercourse)) +
  scale_color_manual(values=c("blue", "red", "blue", "grey")) +
  labs(x = "Year", colour = "", title = "Lotic-invertebrate Index for Flow Evaluation: Family Level (LIFE_F_OE)")
 
 
ggsave(filename = "Outputs/Macroinvertebrates/Time_series_LIFE_F_OE.png", width = 28, height = 18, units = "cm")

# time series plots (by site) - WHPT_ASPT_OE
table_join_INV %>%
  left_join(INV_Sites2Schemes, by = c("site_id")) %>%
  filter(is.na(site_id) == FALSE) %>%
  ggplot(aes(x = year, y = WHPT_ASPT_OE, colour = control_impact, group = site_id)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(vars(watercourse)) +
  scale_color_manual(values=c("blue", "red", "blue", "grey")) +
  labs(x = "Year", colour = "", title = "WHPT - Average Score Per Taxon (WHPT_ASPT_OE)") 

ggsave(filename = "Outputs/Macroinvertebrates/Time_series_WHPT_ASPT_OE_.png", width = 28, height = 18, units = "cm")

# time series plots (by site) - WHPT_NTAXA_OE
table_join_INV %>%
  left_join(INV_Sites2Schemes, by = c("site_id")) %>%
  filter(is.na(site_id) == FALSE) %>%
  ggplot(aes(x = year, y = WHPT_NTAXA_OE, colour = control_impact, group = site_id)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(vars(watercourse)) +
  scale_color_manual(values=c("blue", "red", "blue", "grey")) +
  labs(x = "Year", colour = "", title = "WHPT - Number of Taxa (WHPT_NTAXA_OE)") 

ggsave(filename = "Outputs/Macroinvertebrates/Time_series_WHPT_NTAXA_OE.png", width = 28, height = 18, units = "cm")

# time series plots (by site) - PSI_OE
table_join_INV %>%
  left_join(INV_Sites2Schemes, by = c("site_id")) %>%
  filter(is.na(site_id) == FALSE) %>%
  ggplot(aes(x = year, y = PSI_OE, colour = control_impact, group = site_id)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(vars(watercourse)) +
  scale_color_manual(values=c("blue", "red", "blue", "grey")) +
  labs(x = "Year", colour = "", title = "Proportion of Sediment-sensitive Invertebrates (PSI_OE)") 

ggsave(filename = "Outputs/Macroinvertebrates/Time_series_PSI_OE.png", width = 28, height = 18, units = "cm")

# boxplots by year
table_join_INV %>%
  left_join(INV_Sites2Schemes, by = c("site_id")) %>%
  filter(is.na(site_id) == FALSE) %>%
  mutate(control_impact = if_else(is.na(control_impact) == TRUE, "background_control", control_impact)) %>%
  filter(control_impact != "impact") %>%  # exclude impact sites
  mutate(year = factor(year)) %>%
  ggplot(aes(x = year, y = LIFE_F_OE)) + 
  geom_boxplot() + 
  facet_wrap(vars(watercourse)) +
  theme(panel.spacing.x = unit(0.75, "lines"), axis.text.x = element_text(size = 8)) + # create a large gap between plots side by side, and reduce axis text size
  labs(x = "Year", title = "Lotic-invertebrate Index for Flow Evaluation: Family Level (LIFE_F_OE)") +
  scale_x_discrete(breaks = c(1990, 2000, 2010, 2020)) # reduce number of x axis labels to make less busy


ggsave(filename = "Outputs/Macroinvertebrates/Boxplot_LIFE.png", width = 28, height = 18, units = "cm")


#second variable...
table_join_INV %>%
  left_join(INV_Sites2Schemes, by = c("site_id")) %>%
  filter(is.na(site_id) == FALSE) %>%
  mutate(control_impact = if_else(is.na(control_impact) == TRUE, "background_control", control_impact)) %>%
  filter(control_impact != "impact") %>%  # exclude impact sites
  mutate(year = factor(year)) %>%
  ggplot(aes(x = year, y = WHPT_ASPT_OE)) + 
  geom_boxplot() + 
  facet_wrap(vars(watercourse)) +
  theme(panel.spacing.x = unit(0.75, "lines"), axis.text.x = element_text(size = 8)) +
  labs(x = "Year", title = "WHPT - Average Score Per Taxon (WHPT_ASPT_OE)") +
  scale_x_discrete(breaks = c(1990, 2000, 2010, 2020))


ggsave(filename = "Outputs/Macroinvertebrates/Boxplot_WHPT_ASPT.png", width = 28, height = 18, units = "cm")

#third variable
table_join_INV %>%
  left_join(INV_Sites2Schemes, by = c("site_id")) %>%
  filter(is.na(site_id) == FALSE) %>%
  mutate(control_impact = if_else(is.na(control_impact) == TRUE, "background_control", control_impact)) %>%
  filter(control_impact != "impact") %>%  # exclude impact sites
  mutate(year = factor(year)) %>%
  ggplot(aes(x = year, y = WHPT_NTAXA_OE)) + 
  geom_boxplot() + 
  facet_wrap(vars(watercourse)) +
  theme(panel.spacing.x = unit(0.75, "lines"), axis.text.x = element_text(size = 8)) +
  labs(x = "Year", title = "WHPT - Number of Taxa (WHPT_NTAXA_OE)") +
  scale_x_discrete(breaks = c(1990, 2000, 2010, 2020))

ggsave(filename = "Outputs/Macroinvertebrates/Boxplot_WHPT_NTAXA.png", width = 28, height = 18, units = "cm")

#fourth variable
table_join_INV %>%
  left_join(INV_Sites2Schemes, by = c("site_id")) %>%
  filter(is.na(site_id) == FALSE) %>%
  mutate(control_impact = if_else(is.na(control_impact) == TRUE, "background_control", control_impact)) %>%
  filter(control_impact != "impact") %>%  # exclude impact sites
  mutate(year = factor(year)) %>%
  ggplot(aes(x = year, y = PSI_OE)) + 
  geom_boxplot() + 
  facet_wrap(vars(watercourse)) +
  theme(panel.spacing.x = unit(0.75, "lines"), axis.text.x = element_text(size = 8)) +
  labs(x = "Year", title = "Proportion of Sediment-sensitive Invertebrates (PSI_OE)") +
  scale_x_discrete(breaks = c(1990, 2000, 2010, 2020))

ggsave(filename = "Outputs/Macroinvertebrates/Boxplot_PSI.png", width = 28, height = 18, units = "cm")

##########################################
#### Visualisation of data by scheme #####
##########################################

# MY_SCHEME <- "Mim-01"
# MY_METRIC <- "LIFE_F_OE"
# MY_LAB <- "LIFE"

inv_plot <- function(MY_SCHEME, MY_METRIC, MY_LAB){
  
  ## for a chosen scheme and metric, this function produces a time series plot of metric scores for impact and control sites, with background control sites as grey bands
  
  ## find impact samples for scheme of interest
    scheme_sites <- INV_Sites2Schemes %>%
      filter(scheme_id == MY_SCHEME & control_impact == "impact")
    
    ## prepare sample data for control and impact sites
    scheme_samples <- INV_Samples %>%
      # call the metric of interest the "response"
      dplyr::mutate(response = pull(INV_Samples, MY_METRIC)) %>% 
      # exclude samples with missing metric scores
      filter(is.na(response) == FALSE) %>% 
      # filter on sites of interest
      filter(site_id %in% scheme_sites$site_id) %>% 
      # bring in control/impact info
      left_join(scheme_sites, by = "site_id") %>% 
      # combine us and ds controls
      mutate(control_impact = ifelse(control_impact == "impact", "impact", "control")) %>% 
      # select only the columns we need
      select(site_id, control_impact, year, response) %>%
      #average multiple samples in same year
      dplyr::group_by(site_id, control_impact, year) %>%
      dplyr::summarise(response = mean(response)) %>% 
      # calculate number of years of data for each site
      dplyr::group_by(site_id) %>%
      dplyr::mutate(nyears = n()) %>% 
      # drop sites with only a single year of data
      filter(nyears > 1) %>% 
      # create labels for most recent sample at each site, to be plotted at end of each line
      dplyr::group_by(site_id) %>%
      dplyr::mutate(max_year = max(year)) %>% 
      ungroup() %>%
      dplyr::mutate(label = if_else(year == max_year, as.character(site_id), NA_character_)) %>% 
      dplyr::mutate(control_impact = factor(control_impact))
    
    ## prepare sample data for background control sites
    control_samples <- INV_Samples %>%
      # call the metric of interest the "response"
      mutate(response = pull(INV_Samples, MY_METRIC)) %>%
      # exclude samples with missing metric scores
      filter(is.na(response) == FALSE) %>% 
      # filter out control and impact sites associated with scheme of interest 
      filter(!site_id %in% scheme_sites$site_id) %>% 
      # select only the columns we need
      select(site_id, year, response) %>%
      # average multiple samples in same year
      dplyr::group_by(site_id, year) %>%
      dplyr::summarise(response = mean(response)) %>%
      # count number of years of data per site
      dplyr::group_by(site_id) %>%
      dplyr::mutate(n = n()) %>% 
      # only keep sites with 2+ years of data
      filter(n > 1) 
    
    ## calculate annual summary statistics for control sites
    control_summary <- control_samples %>%  
      dplyr::group_by(year) %>%
      dplyr::summarise(u95 = quantile(response, 0.975, na.rm = TRUE),
                u75 = quantile(response, 0.875, na.rm = TRUE),
                u50 = quantile(response, 0.75, na.rm = TRUE),
                median = quantile(response, 0.5, na.rm = TRUE),
                l50 = quantile(response, 0.25, na.rm = TRUE),
                l75 = quantile(response, 0.125, na.rm = TRUE),
                l95 = quantile(response, 0.025, na.rm = TRUE))
    
    ## estimate mean of controls in each year
    mod <- lmer(response ~ as.factor(year) + (1|site_id),
                data = control_samples) 
    control_mean <- data.frame(year = seq(from = 2014, to = 2020, by = 1), mean = ggeffects::ggpredict(mod, terms = "year")$predicted)
  
  ## create a table to show date of completion of each scheme - ready for geom_line insert into plot
  schemesTable1 <- Schemes %>%
    filter(scheme_id == MY_SCHEME)
  x <- schemesTable1$year_complete
  line_data <- data.frame(year = x, stringsAsFactors = FALSE)

   
  ##  old plot with median line
  p <- ggplot(data = control_summary, aes(x = year)) + 
    # plot background control sites as grey bands
    geom_line(aes(y = median), colour = "grey25", size = 1) + 
    geom_ribbon(aes(ymin = u95, ymax = l95), alpha = 0.06) +
    geom_ribbon(aes(ymin = u75, ymax = l75), alpha = 0.12) +
    geom_ribbon(aes(ymin = u50, ymax = l50), alpha = 0.18) +
    # plot control and impact sites as points/lines
    geom_point(data = scheme_samples, aes(x = year, y = response, colour = control_impact, group = site_id), size = 1.8) +
    geom_line(data = scheme_samples, aes(x = year, y = response, colour = control_impact, group = site_id), size = 0.6) +
    # colScale 
    scale_color_manual(values=c("control" = "royalblue", "impact" = "red")) +
    # make space to right, and add labels
    scale_x_continuous(breaks = c(2014:2020), limits = c(2014, 2022)) +
    ggrepel::geom_label_repel(data = scheme_samples, aes(x = year, y = response, label = label, colour = control_impact), size = 4, nudge_x = 5, na.rm = TRUE, show.legend = FALSE,  segment.linetype = 5) + 
    # label axes
    labs(x = "Year", colour = "", y = MY_LAB) +
    # position legend at top
    theme(legend.position = "none") +
    # create a dashed vertical line on the plot to show when the schemes were completed
    geom_vline(data = line_data, aes(xintercept = year), linetype = 4)
  
  
  ##  new plot with modelled mean line
  p2 <- ggplot(data = control_summary) + 
    # plot background control sites as grey bands
    #geom_line(aes(y = median), colour = "grey25", size = 1) + 
    geom_ribbon(aes(x = year, ymin = u95, ymax = l95), alpha = 0.06) +
    geom_ribbon(aes(x = year, ymin = u75, ymax = l75), alpha = 0.12) +
    geom_ribbon(aes(x = year, ymin = u50, ymax = l50), alpha = 0.18) +
    # plot control and impact sites as points/lines
    geom_point(data = scheme_samples, aes(x = year, y = response, group = site_id), size = 1.8, colour = "red") +
    geom_line(data = scheme_samples, aes(x = year, y = response, group = site_id), size = 0.7, colour = "red") +
    # plot modelled annual mean value as blue line
    geom_line(data = control_mean, aes(x = year, y = mean), size = 0.7, colour = "blue") +   
    # colScale 
    #scale_color_manual(values=c("control" = "royalblue", "impact" = "red")) +
    # make space to right, and add labels
    scale_x_continuous(breaks = c(2014:2020), limits = c(2014, 2022)) +
    ggrepel::geom_label_repel(data = scheme_samples, aes(x = year, y = response, label = label), colour = "red", size = 4, nudge_x = 5, na.rm = TRUE, show.legend = FALSE,  segment.linetype = 5) + 
    # label axes
    labs(x = "Year", colour = "", y = MY_LAB) +
    # position legend at top
    theme(legend.position = "none") +
    # create a dashed vertical line on the plot to show when the schemes were completed
    geom_vline(data = line_data, aes(xintercept = year), linetype = 4)

  return(p2)
  
  
}

## Produce plots for each scheme in turn

# count number of schemes
n_schemes <- length(Schemes$scheme_id)

for(i in 1:n_schemes){
  
  ith_scheme <- Schemes$scheme_id[i]
  
  # manually set the metrics and associated axis labels
  p1 <- inv_plot(MY_SCHEME = ith_scheme, MY_METRIC = "LIFE_F_OE", MY_LAB = "LIFE O/E")
  p2 <- inv_plot(MY_SCHEME = ith_scheme, MY_METRIC = "WHPT_ASPT_OE", MY_LAB = "WHPT_ASPT O/E")
  p3 <- inv_plot(MY_SCHEME = ith_scheme, MY_METRIC = "WHPT_NTAXA_OE", MY_LAB = "WHPT_NTAXA 0/E")
  p4 <- inv_plot(MY_SCHEME = ith_scheme, MY_METRIC = "PSI_OE", MY_LAB = "PSI O/E")
  
  # Arrange 4 ggplots into a single figure
  p_all <- gridExtra::grid.arrange(
    grobs = list(p1, p2, p3, p4),
    widths = c(1, 1),
    heights = c(1, 1),
    layout_matrix = rbind(c(2,3),
                          c(1,4))
  )
  
ggsave(p_all, filename = paste0("Outputs/Macroinvertebrates/",ith_scheme,".png"), width = 24, height = 18, units = "cm")
  
}





##########################################
################# Modelling ##############
##########################################

## Get list of impact sites

impact_sites <- INV_Sites2Schemes %>%
  filter(control_impact == "impact") %>%
  select(scheme_id, site_id, control_impact)


## Prepare MI data

all_data <- INV_Samples %>%
  # select required columns
  select(sample_id,	data_source,	site_id,	ea_site_code, date,	year,	month,	
         dec_year, WHPT_ASPT_OE,	WHPT_NTAXA_OE,	PSI_OE,	LIFE_F_OE) %>%
  # drop samples from locations not in sites list
  filter(site_id %in% INV_Sites$site_id) %>%
  # restrict to 2014-2020 samples
  filter(year > 2013 & year < 2021) %>%
  # create season column
  mutate(Season = if_else(month %in% c(3,4,5), "Spring",   
                          if_else(month %in% c(6,7,8), "Summer",
                                  if_else(month %in% c(9,10,11), "Autumn", "Winter")))) %>%
  # filter out summer and winter samples (for modelling)
  filter(Season %in% c("Spring", "Autumn")) %>%
  # rename year column (for join_he function later)
  dplyr::rename(Year = year) %>%
  # create biol_site_id column for joining flow stats later
  mutate(biol_site_id = site_id) %>%
  # join in site characteristics
  left_join(dplyr::select(INV_Sites, c(site_id, watercourse, site_name, slope, distance_from_source, regime_type, substrate_index)), by = c("site_id")) %>%
  # set levels of regime_type
  mutate(regime_type = factor(regime_type, levels = c("P", "SP", "E"))) %>%
  # link (impact) sites to schemes
  left_join(impact_sites, by = c("site_id")) %>%
  # join in details of schemes
  left_join(select(Schemes, c(scheme_id, scheme_name, date_complete, year_complete)), by = c("scheme_id")) %>%
  # flag non-impact sites as controls
  mutate(control_impact = ifelse(is.na(control_impact) == FALSE, "impact", "control")) %>%
  # calculate years since restoration works completed
  mutate(years_since_restoration = ifelse(control_impact == "impact" & date > date_complete, (date - date_complete)/365, 0)) %>%
  mutate(years_post_restoration = ceiling(years_since_restoration)) %>%
  # flag post-restoration samples
  mutate(before_after = ifelse(years_since_restoration == 0, "before", "after")) %>%
  mutate(before_after = factor(before_after, levels = c("before", "after"))) %>%
  # find and exclude sites with only one sample
  dplyr::group_by(site_id) %>%
  dplyr::mutate(nsamples = n()) %>% 
  filter(nsamples > 1) 


## list of sites
all_sites <- all_data %>%
  dplyr::group_by(site_id, watercourse, control_impact) %>%
  dplyr::summarise(n_surveys = n())


## view number of post-restoration samples
all_data %>%
  filter(control_impact == "impact") %>%
  filter(years_post_restoration > 0) %>%
  mutate(years_post_restoration = factor(years_post_restoration)) %>%
  ggplot(aes(x = years_post_restoration)) +
  geom_bar() +
  labs(x = "Number of years post-restoration", y = "Number of surveys")

ggsave(filename = "Outputs/Macroinvertebrates/CountPostSamples.png")


## Prepare flow data
sites_to_plot <- c("MAC-Bea05", "MAC-Gad07", "MAC-Lea05", "MAC-Mim08", "MAC-Mis17", "MAC-Ver14")

flow_data <- RawFlows %>%
  select(site_id, date, flow_m3s) %>%
  filter(date >= "2014-04-01") %>%
  filter(date < "2020-10-01") %>%
  dplyr::rename(flow = flow_m3s,
                flow_site_id = site_id) %>%
  mutate(dec_year = lubridate::decimal_date(date))  

## plot raw flows
flows_plot <- flow_data %>%
  filter(flow_site_id %in% sites_to_plot) %>%
  arrange(dec_year) %>%
  ggplot() +
  geom_line(aes(x = dec_year, y = flow)) +
  scale_x_continuous(minor_breaks = seq(2011, 2021, by = 1), breaks = seq(2011, 2021, by = 1)) +
  labs(y = "Flow (m3/s)", x = "Year") +
  facet_wrap(~flow_site_id, ncol = 2) +
  theme(legend.position = "top") 

ggsave(plot = flows_plot, filename = paste0("Outputs/Macroinvertebrates/flowsplot.png"), width = 18, height = 24, units = "cm")

## Calculate flow statistics
flow_stats_all <- calc_flowstats(data = flow_data,
                                 site_col = "flow_site_id",
                                 date_col = "date",
                                 flow_col = "flow")
flow_stats <- flow_stats_all[[1]] 
flow_stats <- flow_stats %>%
  select(flow_site_id, season, water_year, Q95, Q10, Q95z, Q10z)


## visualise flow stats
start_dates <- flow_stats %>%
  filter(flow_site_id %in% sites_to_plot) %>%
  #dplyr::group_by(season, water_year) %>%
  #dplyr::summarise(Q95 = mean(Q95),
  #                 Q10 = mean(Q10)) %>%
  mutate(date = if_else(season =="Summer", water_year + 1 + 92/365, water_year + 275/365))

end_dates <- flow_stats %>%
  filter(flow_site_id %in% sites_to_plot) %>%
  #dplyr::group_by(season, water_year) %>%
  #dplyr::summarise(Q95 = mean(Q95),
  #                 Q10 = mean(Q10)) %>%
  mutate(date = if_else(season =="Summer", water_year + 1 + 274/365, water_year + 1 + 91/365)) 

flows_plot2 <- bind_rows(start_dates, end_dates) %>%
  arrange(date) %>%
  ggplot() +
  geom_line(aes(x = date, y = Q95, colour = "Q95")) +
  geom_line(aes(x = date, y = Q10, colour = "Q10")) +
  scale_x_continuous(minor_breaks = seq(2011, 2021, by = 1), breaks = seq(2011, 2021, by = 1)) +
  scale_colour_manual(values = c("red", "blue")) +
  labs(y = "Flow (m3/s)", x = "Year", colour = "") +
  facet_wrap(~flow_site_id, ncol = 2) +
  theme(legend.position = "top") 

ggsave(plot = flows_plot2, filename = paste0("Outputs/Macroinvertebrates/flowsplot2.png"), width = 18, height = 24, units = "cm")

## Join MI data and flow statistics

# create two-column table mapping biology sites to flow sites
mapping <- data.frame(flow_site_id = unique(all_data$biol_site_id), 
                      biol_site_id = unique(all_data$biol_site_id))

# make year continuous
all_data$Year <- as.numeric(all_data$Year)

# join flow statistics to biology data

join_data <- join_he(biol_data = all_data, 
                     flow_stats = flow_stats, 
                     mapping = mapping, 
                     lag_vars = c("Q95z", "Q10z"), 
                     LS1 = TRUE, 
                     LS2 = TRUE)


# get rid of blank rows
join_data <- join_data %>%
  filter(is.na(site_id) == FALSE)



MY_METRIC = "LIFE_F_OE"
MY_LABEL = "LIFE O/E"
MOD_TYPE = "GAM" # GAM or LM

inv_models <- function(MY_METRIC, MY_LABEL, MOD_TYPE){
  
  # create response variable
  response <- pull(join_data, MY_METRIC)
  model_data <- cbind(join_data, response = response)
  model_data$site_id <- as.factor(model_data$site_id)
  
  # fit model 
  if(MOD_TYPE == "LM"){
    mod <- lmer(response ~ 
                  Q95z +
                  Q95zLS1 +  
                  watercourse + 
                  as.factor(years_post_restoration) + 
                  Season +
                  slope + 
                  distance_from_source +
                  #before_after +
                  (1|site_id), 
                data = subset(model_data, is.na(response) == FALSE), 
                REML = FALSE) 
  }
  if(MOD_TYPE == "GAM"){
    mod <- gam(response ~ 
                 as.factor(watercourse) + 
                 as.factor(regime_type) +
                 #as.factor(Year) +
                 #as.factor(years_post_restoration) + 
                 s(years_post_restoration, k = 3) +
                 as.factor(Season) +
                 s(distance_from_source, k = 3) +
                 s(substrate_index, k = 3) +
                 #s(slope, k = 3) +
                 s(Q95z, k = 3) +
                 s(Q95zLS1, k = 3) +
                 s(Q95zLS2, k = 3) +
                 s(site_id, bs = "re"),
               data = subset(model_data, is.na(response) == FALSE), 
               REML = FALSE) 
  }
  
  
  print(summary(mod))
  print(anova(mod))
  
  # plot coefficient estimates
  #coef_plot <- sjPlot::plot_model(mod, type = "est", pred.type = "fe", show.values = TRUE, value.offset = .4, title = "", colors = "bw")
  #ggsave(plot = coef_plot, filename = paste0("Outputs/Macroinvertebrates/coefplot_",MY_METRIC,".png"), width = 15, height = 15, units = "cm")
  
  # generate prediction plots
  pplots <- sjPlot::plot_model(mod, type = "pred", pred.type = "fe", title = "", axis.title = MY_LABEL, colors = "bw") 
  #pplots[[1]]  <- pplots[[1]] + ylim(0.6, 1.2)
  
  # combine prediction plots into one figure
  all_plots <- grid.arrange(
    grobs = pplots[c(1:10)],
    widths = c(1,1),
    heights = c(1,1,1,1,1),
    layout_matrix = rbind(c(1,2),
                          c(3,4),
                          c(5,6),
                          c(7,8),
                          c(9,10))
  )
  
  # save figure
  ggsave(plot = all_plots, filename = paste0("Outputs/Macroinvertebrates/predplot_",MY_METRIC,".png"), width = 18, height = 24, units = "cm")
  
  
  #plot(mod, se = TRUE, shade = TRUE, shade.col = "lightblue", seWithMean = FALSE, scale = -1, ylab = MY_LABEL, pages = 1, all.terms = TRUE, rug = TRUE, residuals = FALSE, pch = 1, cex = 1, shift = coef(mod)[1]) 
  
}


## model each metric in turn, and save model results to txt file

filename <- paste("Outputs/Macroinvertebrates/modelling_",format(Sys.time(), "%Y%m%d"),".txt",sep="")
sink(filename, append = FALSE, type = "output", split = TRUE)

cat("\t") #tab separator
cat("\t") #tab separator
cat("WHPT")
cat("\t") #tab separator
inv_models(MY_METRIC = "WHPT_ASPT_OE", MY_LABEL = "WHPT-ASPT O/E", MOD_TYPE = "GAM")
cat("\t") #tab separator
cat("\t") #tab separator
cat("NTAXA")
cat("\t") #tab separator
inv_models(MY_METRIC = "WHPT_NTAXA_OE", MY_LABEL = "WHPT-NTAXA O/E", MOD_TYPE = "GAM")
cat("\t") #tab separator
cat("\t") #tab separator
cat("LIFE")
cat("\t") #tab separator
inv_models(MY_METRIC = "LIFE_F_OE", MY_LABEL = "LIFE O/E", MOD_TYPE = "GAM")
cat("\t") #tab separator
cat("\t") #tab separator
cat("PSI")
cat("\t") #tab separator
inv_models(MY_METRIC = "PSI_OE", MY_LABEL = "PSI O/E", MOD_TYPE = "GAM")

sink() 


