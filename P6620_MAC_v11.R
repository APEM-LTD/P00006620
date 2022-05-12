##########################################
####### Load libraries data ##############
##########################################

# to install hetoolkit from github
install.packages("devtools")
library(devtools)
install_github("APEM-LTD/hetoolkit")

library(hetoolkit) # for EA hydro-ecology toolkit
library(readxl)
library(dplyr)
library(ggplot2)
library(viridis) # for ggplot colour schemes
library(gridExtra) # for arranging multiple ggplots in a single figure
library(directlabels) # for labelling lines in plots
library(ggrepel)
library(GGally) # for ggpairs function
library(gamm4) # for mixed-effects models
library(gridExtra) # for multi-plots
library(lmerTest) # for lme4 function
library(mgcv) # for gam modelling
library(ggeffects) # for model predictions


##########################################
############### Import data ##############
##########################################

#MasterData <- "Data/APEM_Model_Database_MASTER_2021-12-16-1050.xlsx"
MasterData <- "Data/APEM_Model_Database_MASTER_2022-02-01-1643.xlsx"


Schemes <- read_excel(MasterData, sheet = "Schemes")
MAC_Sites <- read_excel(MasterData, sheet = "MAC_Sites")
MAC_Samples <- read_excel(MasterData, sheet = "MAC_Samples")
MAC_Sites2Schemes <- read_excel(MasterData, sheet = "MAC_Sites2Schemes")
RawFlows <- read_excel("Data/Flows_2021-12-16-1046.xlsx", sheet = "Flows")

##########################################
############### Data review ##############
##########################################

#join the sites and mac-sites tables by the site_code attribute and define it in an object table_join
table_join <- left_join(MAC_Samples, MAC_Sites, by = c("site_id"))
table_join$watercourse <- as.factor(table_join$watercourse)
table_join$site_id <- as.factor(table_join$site_id)

#create a ggplot object with year vs rmni eqr#
#ggplot(data = table_join, aes(x = year, y = raw_rmni_eqr)) + geom_point() + facet_wrap(vars(watercourse))

# years of data per site
table_join %>%
  select(year, site_id, watercourse) %>%
  group_by(year, site_id, watercourse) %>%
  dplyr::summarise(nsamples = n()) %>%
  group_by(site_id, watercourse) %>%
  dplyr::summarise(nyears = n()) %>%
  #ungroup() %>%
  ggplot(aes(x = nyears)) +
  geom_histogram(position = "stack", binwidth=1) +
  labs(x = "Number of years of data", y = "Number of sites")

ggsave(filename = "Outputs/Macrophytes/hist1.png") 


# data coverage
table_join %>%
  select(year, site_id, watercourse) %>%
  group_by(year, site_id, watercourse) %>%
  dplyr::summarise(nsamples = n()) %>% # counts number of samples
  ggplot(aes(year, site_id, fill = nsamples)) + 
  geom_tile(colour = "white") + 
  #scale_fill_viridis(option = "inferno", direction=1) + 
  labs(fill = "Number of samples") + # legend caption
  #scale_y_discrete(limits = rev(levels(table_join$watercourse))) + 
  facet_grid(vars(watercourse), space = "free", scale = "free_y") +
  theme(axis.text.y = element_text(size = 6)) 

ggsave(filename = "Outputs/Macrophytes/data_coverage.png", width = 18, height = 24, units = "cm") 

# cross-correlation plots
GGally::ggpairs(table_join[,c("rmni_score", "rmhi_score", "raw_ntaxa_eqr", "%_filamentous_algae")]) 

ggsave(filename = "Outputs/Macrophytes/correlations.png", width = 18, height = 15, units = "cm") 


# time series plots (by site)
table_join %>%
  left_join(MAC_Sites2Schemes, by = c("site_id")) %>%
  ggplot(aes(x = year, y = rmni_score, colour = control_impact, group = site_id)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(vars(watercourse)) +
  scale_color_manual(values=c("blue", "red", "blue", "grey")) +
  labs(x = "Year", colour = "", title = "River Macrophyte Nutrient Index (RMNI)") 

table_join %>%
  left_join(MAC_Sites2Schemes, by = c("site_id")) %>%
  ggplot(aes(x = year, y = number_of_aquatic_taxa, colour = control_impact, group = site_id)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(vars(watercourse)) +
  scale_color_manual(values=c("blue", "red", "blue", "grey")) +
  labs(x = "Year", colour = "", title = "Number of taxa (NTAXA)") 


# boxplots by year
table_join %>%
  left_join(MAC_Sites2Schemes, by = c("site_id")) %>% # brings in control/impact info
  mutate(control_impact = if_else(is.na(control_impact) == TRUE, "background_control", control_impact)) %>%
  filter(control_impact != "impact") %>%  # exclude impact sites (NOT equal to)
  mutate(year = factor(year)) %>% # make the year variable a categorical factor, to get one boxplot per year
  ggplot(aes(x = year, y = rmni_score)) + 
    geom_boxplot() + 
    facet_wrap(vars(watercourse)) +
    labs(x = "Year", title = "River Macrophyte Nutrient Index (RMNI)") 

ggsave(filename = "Outputs/Macrophytes/boxplot_rmni.png", width = 28, height = 18, units = "cm") 

# now the same for a second variable...
table_join %>%
  left_join(MAC_Sites2Schemes, by = c("site_id")) %>%
  mutate(control_impact = if_else(is.na(control_impact) == TRUE, "background_control", control_impact)) %>%
  filter(control_impact != "impact") %>%  # exclude impact sites
  mutate(year = factor(year)) %>%
  ggplot(aes(x = year, y = number_of_aquatic_taxa)) + 
  geom_boxplot() + 
  facet_wrap(vars(watercourse)) +
    labs(x = "Year", title = "Number of taxa (NTAXA)") 

ggsave(filename = "Outputs/Macrophytes/boxplot_ntaxa.png", width = 28, height = 18, units = "cm") 
    


##########################################
#### Visualisation of data by scheme #####
##########################################

# MY_SCHEME <- "Gad-02"
# MY_METRIC <- "rmhi_score"
# MY_LAB <- "RMHI"

mac_plot <- function(MY_SCHEME, MY_METRIC, MY_LAB){

  ## for a chosen scheme and metric, this function produces a time series plot of metric scores for impact and control sites, with background control sites as grey bands
  
  ## find impact samples for scheme of interest
  scheme_sites <- MAC_Sites2Schemes %>%
    filter(scheme_id == MY_SCHEME & control_impact == "impact")
  
  ## prepare sample data for control and impact sites
  scheme_samples <- MAC_Samples %>%
    # call the metric of interest the "response"
    mutate(response = pull(MAC_Samples, MY_METRIC)) %>% 
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
    group_by(site_id, control_impact, year) %>%
    dplyr::summarise(response = mean(response)) %>% 
    # calculate number of years of data for each site
    group_by(site_id) %>%
    dplyr::mutate(nyears = n()) %>% 
    # drop sites with only a single year of data
    filter(nyears > 1) %>% 
    # create labels for most recent sample at each site, to be plotted at end of each line
    dplyr::group_by(site_id) %>%
    dplyr::mutate(max_year = max(year)) %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(label = if_else(year == max_year, as.character(site_id), NA_character_)) 
  
  
  ## prepare sample data for background control sites
  control_samples <- MAC_Samples %>%
    # call the metric of interest the "response"
    mutate(response = pull(MAC_Samples, MY_METRIC)) %>%
    # exclude samples with missing metric scores
    filter(is.na(response) == FALSE) %>% 
    # filter out control and impact sites associated with scheme of interest 
    filter(!site_id %in% scheme_sites$site_id) %>% 
    # select only the columns we need
    select(site_id, year, response) %>%
    # average multiple samples in same year
    group_by(site_id, year) %>%
    dplyr::summarise(response = mean(response)) %>%
    # count number of years of data per site
    dplyr::group_by(site_id) %>%
    dplyr::mutate(n = n()) %>% 
    # only keep sites with 2+ years data
    filter(n > 1) 
  
  ## calculate annual summary statistics for control sites
  control_summary <- control_samples %>% 
    group_by(year) %>%
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
  control_mean <- data.frame(year = seq(from = 2015, to = 2021, by = 1), mean = ggeffects::ggpredict(mod, terms = "year")$predicted)
  
  ## get completion year for plotting
  schemesTable1 <- Schemes %>%
    filter(scheme_id == MY_SCHEME)
  x <- schemesTable1$year_complete
  line_data <- data.frame(year = x, stringsAsFactors = FALSE)
  
    ## old plot with median line
  # note than mean line may be > or < majority of data observed in 2021 because it represents the predicted mean value *across all sites in control_samples*, whereas only a subset of those were actually surveyed in 2021
  p <- ggplot(data = control_summary, aes(x = year)) + 
      # plot background control sites as grey bands
      geom_line(aes(y = median), colour = "grey25", size = 1) + 
      geom_ribbon(aes(ymin = u95, ymax = l95), alpha = 0.06) +
      geom_ribbon(aes(ymin = u75, ymax = l75), alpha = 0.12) +
      geom_ribbon(aes(ymin = u50, ymax = l50), alpha = 0.18) +
      # plot control and impact sites as points/lines
      geom_point(data = scheme_samples, aes(x = year, y = response, colour = control_impact, group = site_id), size = 1.8) +
      geom_line(data = scheme_samples, aes(x = year, y = response, colour = control_impact, group = site_id), size = 0.6) +
      scale_color_manual(values=c("red")) +
      # make space to right, and add labels
      scale_x_continuous(breaks = c(2015:2021), limits = c(2015, 2023)) +
      ggrepel::geom_label_repel(data = scheme_samples, aes(x = year, y = response, label = label, colour = control_impact), nudge_x = 10, na.rm = TRUE, show.legend = FALSE,  segment.linetype = 5) + 
      # label axes
      labs(x = "Year", colour = "", y = MY_LAB) +
      # position legend at top
      theme(legend.position = "none") +
      # create a dashed vertical line on the plot to show when the schemes were completed
      geom_vline(data = line_data, aes(xintercept = year), linetype = 4)
  
  ## new plot with modelled mean line 
  p2 <- ggplot(data = control_summary) + 
    # plot background control sites as grey bands
    #geom_line(aes(y = median), colour = "grey25", size = 1) + 
    geom_ribbon(aes(x = year, ymin = u95, ymax = l95), alpha = 0.06) +
    geom_ribbon(aes(x = year,ymin = u75, ymax = l75), alpha = 0.12) +
    geom_ribbon(aes(x = year,ymin = u50, ymax = l50), alpha = 0.18) +
    # plot control and impact sites as points/lines
    geom_point(data = scheme_samples, aes(x = year, y = response, group = site_id), size = 1.8, colour = "red") +
    geom_line(data = scheme_samples, aes(x = year, y = response, group = site_id), size = 0.7, colour = "red") +
    # plot modelled annual mean value as blue line
    geom_line(data = control_mean, aes(x = year, y = mean), size = 0.7, colour = "blue") +   
    #scale_color_manual(values=c("blue","red")) +
    # make space to right, and add labels
    scale_x_continuous(breaks = c(2015:2021), limits = c(2015, 2023)) +
    ggrepel::geom_label_repel(data = scheme_samples, aes(x = year, y = response, label = label), colour = "red", nudge_x = 10, na.rm = TRUE, show.legend = FALSE,  segment.linetype = 5) + 
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
  p1 <- mac_plot(MY_SCHEME = ith_scheme, MY_METRIC = "rmni_score", MY_LAB = "RMNI")
  p2 <- mac_plot(MY_SCHEME = ith_scheme, MY_METRIC = "rmhi_score", MY_LAB = "RMHI")
  p3 <- mac_plot(MY_SCHEME = ith_scheme, MY_METRIC = "number_of_aquatic_taxa", MY_LAB = "NTAXA")
  p4 <- mac_plot(MY_SCHEME = ith_scheme, MY_METRIC = "%_filamentous_algae", MY_LAB = "%ALG")
  
  # Arrange 4 ggplots into a single figure
  p_all <- gridExtra::grid.arrange(
    grobs = list(p1, p2, p3, p4),
    widths = c(1, 1),
    heights = c(1, 1),
    layout_matrix = rbind(c(1,2),
                          c(3,4))
  )
  
  # Saves plots
  ggsave(p_all, filename = paste0("Outputs/Macrophytes/",ith_scheme,".png"), width = 24, height = 18, units = "cm")
  
}




##########################################
################# Modelling ##############
##########################################


## prepare data

# get list of impact sites
impact_sites <- MAC_Sites2Schemes %>%
  filter(control_impact == "impact") %>%
  select(scheme_id, site_id, control_impact)

all_data <- MAC_Samples %>%
  # select required columns
  select(sample_id,	data_source, site_id,	ea_site_code, date,	year,	month,	
         dec_year, rmhi_score,	rmni_score,	number_of_aquatic_taxa,	'%_filamentous_algae', dry_this_year, dry_last_year) %>%
  # calculate water year and lagged water year
  mutate(water_year = dplyr::if_else(month < 10, year - 1, year),
         water_year1 = water_year - 1) %>%
  # rename %ALG variable
  mutate(pc_alg = .[[12]]) %>%
  # round NTAXA to integer
  mutate(number_of_aquatic_taxa = round(number_of_aquatic_taxa,0)) %>%
  # rename year column (for join_he function later)
  dplyr::mutate(Year = year) %>%
  # create biol_site_id column for joining flow stats later
  dplyr::mutate(biol_site_id = site_id) %>%
  # join scheme details to samples data
  left_join(dplyr::select(MAC_Sites, c(site_id, watercourse, site_name, distance_from_source, regime_type)), by = c("site_id")) %>%
  left_join(impact_sites, by = c("site_id")) %>%
  left_join(select(Schemes, c(scheme_id, scheme_name, date_complete, year_complete)), by = c("scheme_id")) %>%
  # set levels of regime_type
  mutate(regime_type = factor(regime_type, levels = c("P", "SP", "E"))) %>%
  # create season column
  mutate(Season = if_else(month %in% c(3,4,5), "Spring",   
                          if_else(month %in% c(6,7,8), "Summer",
                                  if_else(month %in% c(9,10,11), "Autumn", "Winter")))) %>%
  # flag non-impact sites as controls
  mutate(control_impact = ifelse(is.na(control_impact) == FALSE, "impact", "control")) %>%
  # calculate years since restoration works completed
  mutate(years_since_restoration = ifelse(control_impact == "impact" & date > date_complete, (date - date_complete)/365, 0)) %>%
  mutate(years_post_restoration = ceiling(years_since_restoration)) %>%
  # flag post-restoration samples
  mutate(before_after = ifelse(years_since_restoration == 0, "before", "after")) %>%
  mutate(before_after = factor(before_after, levels = c("before", "after"))) %>%
  # find and exclude sites with only one sample
  group_by(site_id) %>%
  dplyr::mutate(nsamples = n()) %>% 
  filter(nsamples > 1) %>%
  # convert year to factor
  mutate(year = factor(year))

## list of sites
all_sites <- all_data %>%
  group_by(site_id, watercourse, control_impact) %>%
  dplyr::summarise(n_surveys = n())


## view number of post-restoration samples
all_data %>%
  filter(control_impact == "impact") %>%
  filter(years_post_restoration > 0) %>%
  mutate(years_post_restoration = factor(years_post_restoration)) %>%
  ggplot(aes(x = years_post_restoration, fill = data_source)) +
  geom_bar() +
  labs(x = "Number of years post-restoration", y = "Number of surveys", fill = "Data source")
ggsave(filename = "Outputs/Macrophytes/CountPostSamples.png")


## Prepare flow data
sites_to_plot <- c("MAC-Bea05", "MAC-Gad07", "MAC-Lea05", "MAC-Mim08", "MAC-Mis17", "MAC-Ver14")

flow_data <- RawFlows %>%
  select(site_id, date, flow_m3s) %>%
  filter(date >= "2014-04-01") %>%
  filter(date < "2020-10-01") %>%
  dplyr::rename(flow = flow_m3s) %>%
  mutate(year = lubridate::year(date),
         month = lubridate::month(date),
         dec_year = lubridate::decimal_date(date),
         water_year = dplyr::if_else(month < 10, year - 1, year)) %>%
  # focus on summer period only (due to incomplete data for 2014 and 2020)
  filter(month >= 4 & month <= 9)


## plot raw flows
flows_plot <- flow_data %>%
  filter(site_id %in% sites_to_plot) %>%
  arrange(dec_year) %>%
  ggplot() +
  geom_line(aes(x = dec_year, y = flow)) +
  scale_x_continuous(minor_breaks = seq(2011, 2021, by = 1), breaks = seq(2011, 2021, by = 1)) +
  labs(y = "Flow (m3/s)", x = "Year") +
  facet_wrap(~site_id, ncol = 2) +
  theme(legend.position = "top") 

#ggsave(plot = flows_plot, filename = paste0("Outputs/Macroinvertebrates/flowsplot.png"), width = 18, height = 24, units = "cm")

## calculate annual flow statistics

# find when minimum flows occur
flow_data %>%
  dplyr::group_by(month) %>%
  dplyr::summarise(mean_flow = mean(flow)) %>%
  ggplot(aes(x = month, y = mean_flow)) +
    geom_line()
  
# calculate annual Q90 flow 
flow_stats <- flow_data %>%
  dplyr::group_by(site_id, water_year) %>%
  dplyr::summarise(Q90 = quantile(flow, 0.1)) %>%
  dplyr::group_by(site_id) %>%
  dplyr::mutate(Q90mean = mean(Q90),
         Q90sd = sd(Q90),
         Q90z = (Q90 - Q90mean) / Q90sd)

flow_stats2 <- flow_stats %>%
  dplyr::rename(Q90zL1 = Q90z)

## link flow stats to ecology data

join_data <- all_data %>%
  left_join(flow_stats, by = c("site_id", "water_year")) %>%
  left_join(flow_stats2, by = c("site_id", "water_year1" = "water_year"))



## modelling function

# MY_METRIC = "pc_alg"
# MY_LABEL = "%ALG"

mac_models <- function(MY_METRIC, MY_LABEL){
  
  # create response variable
  response <- pull(join_data, MY_METRIC)
  model_data <- cbind(join_data, response = response)
  model_data$site_id <- as.factor(model_data$site_id)
  
  # fit model
  if(MY_METRIC == "number_of_aquatic_taxa"){
    mod <- gam(response ~ 
                 as.factor(watercourse) + 
                 as.factor(regime_type) +
                 s(years_post_restoration, k = 3) + 
                 #as.factor(year) +
                 s(month, k = 3) +
                 s(distance_from_source, k = 3) +
                 s(Q90z, k = 3) +
                 s(Q90zL1, k = 3) +
                 #as.factor(dry_this_year) +
                 #as.factor(dry_last_year) +
                 #s(year, bs = "re") +
                 s(site_id, bs = "re"),
               data = subset(model_data, is.na(response) == FALSE), 
               REML = FALSE, 
               family = poisson(link = "log"))
  } 
  
  if(MY_METRIC == "pc_alg"){
    mod <- gam(log(response + 1) ~ 
                 as.factor(watercourse) + 
                 as.factor(regime_type) +
                 s(years_post_restoration, k = 3) + 
                 #as.factor(year) +
                 s(month, k = 3) +
                 s(distance_from_source, k = 3) +
                 s(Q90z, k = 3) +
                 s(Q90zL1, k = 3) +
                 #as.factor(dry_this_year) +
                 #as.factor(dry_last_year) +
                 #s(year, bs = "re") +
                 s(site_id, bs = "re"),
               data = subset(model_data, is.na(response) == FALSE), 
               REML = FALSE, 
               family = poisson(link = "log"))
  } else {
    mod <- gam(response ~ 
                 as.factor(watercourse) + 
                 as.factor(regime_type) +
                 s(years_post_restoration, k = 3) + 
                 #as.factor(year) +
                 s(month, k = 3) +
                 s(distance_from_source, k = 3) +
                 s(Q90z, k = 3) +
                 s(Q90zL1, k = 3) +
                 #as.factor(dry_this_year) +
                 #as.factor(dry_last_year) +
                 #s(year, bs = "re") +
                 s(site_id, bs = "re"),
               data = subset(model_data, is.na(response) == FALSE), 
               REML = FALSE) 
  }
 

  print(summary(mod))
  print(anova(mod))
  gam.check(mod)
  
  # plot coefficient estimates
  coef_plot <- sjPlot::plot_model(mod, type = "est", pred.type = "fe", show.values = TRUE, value.offset = .4, title = "", colors = "bw")
  
  ggsave(plot = coef_plot, filename = paste0("Outputs/Macrophytes/coefplot_",MY_METRIC,".png"), width = 15, height = 15, units = "cm")
  
  # generate prediction plots
  pplots <- sjPlot::plot_model(mod, type = "pred", pred.type = "fe", title = "", axis.title = MY_LABEL, colors = "bw")
  
 # sjPlot::plot_model(mod, type = "pred", pred.type = "fe", title = "", axis.title = MY_LABEL, colors = "bw", transform = "exp") # can't get vthis to work for %ALG
  
  # combine prediction plots into one figure
  all_plots <- grid.arrange(
    grobs = pplots,
    widths = c(1,1),
    heights = c(1,1,1,1),
    layout_matrix = rbind(c(1,2),
                          c(3,4),
                          c(5,6),
                          c(7,8))
  )
  
  # save figure
  ggsave(plot = all_plots, filename = paste0("Outputs/Macrophytes/predplot_",MY_METRIC,".png"), width = 18, height = 23, units = "cm")
  
}


## model each metric in turn, and save model results to txt file

filename <- paste("Outputs/Macrophytes/modelling_",format(Sys.time(), "%Y%m%d"),".txt",sep="")
sink(filename, append = FALSE, type = "output", split = TRUE)

cat("\t") #tab separator
cat("\t") #tab separator
cat("RMNI")
cat("\t") #tab separator
mac_models(MY_METRIC = "rmni_score", MY_LABEL = "RMNI")
cat("\t") #tab separator
cat("\t") #tab separator
cat("RMHI")
cat("\t") #tab separator
mac_models(MY_METRIC = "rmhi_score", MY_LABEL = "RMHI")
cat("\t") #tab separator
cat("\t") #tab separator
cat("NTAXA")
cat("\t") #tab separator
mac_models(MY_METRIC = "number_of_aquatic_taxa", MY_LABEL = "NTAXA")
cat("\t") #tab separator
cat("\t") #tab separator
cat("%ALG")
cat("\t") #tab separator
mac_models(MY_METRIC = "pc_alg", MY_LABEL = "%ALG")

sink() 
