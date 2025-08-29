
### script for extracting most common cancers in Finland in 2023 ###

# set working directory

# setwd('your/working/directory')

# read data

## uncomment the following line and include the location of the data file ## 
# incidence <- read.csv('path/to/data/incidence.csv', header = T, sep = ',')
incidence <- read.csv('notebooks/Artikkeli/incidence.csv', header = T, sep = ',')

# choose 2023 data
incidence_2023 <- incidence[incidence$Year == 2023,]

# construct different subgroups based on age and gender
females_30_39_years_old <- incidence_2023[incidence_2023$Age.group == '30-39' & incidence_2023$Sex == 'Female',]
females_40_49_years_old <- incidence_2023[incidence_2023$Age.group == '40-49' & incidence_2023$Sex == 'Female',]
females_50_59_years_old <- incidence_2023[incidence_2023$Age.group == '50-59' & incidence_2023$Sex == 'Female',]
females_60_69_years_old <- incidence_2023[incidence_2023$Age.group == '60-69' & incidence_2023$Sex == 'Female',]
females_70_79_years_old <- incidence_2023[incidence_2023$Age.group == '70-79' & incidence_2023$Sex == 'Female',]

males_30_39_years_old <- incidence_2023[incidence_2023$Age.group == '30-39' & incidence_2023$Sex == 'Male',]
males_40_49_years_old <- incidence_2023[incidence_2023$Age.group == '40-49' & incidence_2023$Sex == 'Male',]
males_50_59_years_old <- incidence_2023[incidence_2023$Age.group == '50-59' & incidence_2023$Sex == 'Male',]
males_60_69_years_old <- incidence_2023[incidence_2023$Age.group == '60-69' & incidence_2023$Sex == 'Male',]
males_70_79_years_old <- incidence_2023[incidence_2023$Age.group == '70-79' & incidence_2023$Sex == 'Male',]

# sort data frames based on absolute number of cancer diagnoses
females_30_39_years_old <- females_30_39_years_old[order(females_30_39_years_old$Diagnosed.cancer.cases, decreasing = T),]
females_40_49_years_old <- females_40_49_years_old[order(females_40_49_years_old$Diagnosed.cancer.cases, decreasing = T),]
females_50_59_years_old <- females_50_59_years_old[order(females_50_59_years_old$Diagnosed.cancer.cases, decreasing = T),]
females_60_69_years_old <- females_60_69_years_old[order(females_60_69_years_old$Diagnosed.cancer.cases, decreasing = T),]
females_70_79_years_old <- females_70_79_years_old[order(females_70_79_years_old$Diagnosed.cancer.cases, decreasing = T),]

males_30_39_years_old <- males_30_39_years_old[order(males_30_39_years_old$Diagnosed.cancer.cases, decreasing = T),]
males_40_49_years_old <- males_40_49_years_old[order(males_40_49_years_old$Diagnosed.cancer.cases, decreasing = T),]
males_50_59_years_old <- males_50_59_years_old[order(males_50_59_years_old$Diagnosed.cancer.cases, decreasing = T),]
males_60_69_years_old <- males_60_69_years_old[order(males_60_69_years_old$Diagnosed.cancer.cases, decreasing = T),]
males_70_79_years_old <- males_70_79_years_old[order(males_70_79_years_old$Diagnosed.cancer.cases, decreasing = T),]

# replace cancer sites as 'Other' outside top 10 most common cancers
females_30_39_years_old$Cancer.site <- replace(females_30_39_years_old$Cancer.site, 11:nrow(females_30_39_years_old), 'Other')
females_40_49_years_old$Cancer.site <- replace(females_40_49_years_old$Cancer.site, 11:nrow(females_40_49_years_old), 'Other')
females_50_59_years_old$Cancer.site <- replace(females_50_59_years_old$Cancer.site, 11:nrow(females_50_59_years_old), 'Other')
females_60_69_years_old$Cancer.site <- replace(females_60_69_years_old$Cancer.site, 11:nrow(females_60_69_years_old), 'Other')
females_70_79_years_old$Cancer.site <- replace(females_70_79_years_old$Cancer.site, 11:nrow(females_70_79_years_old), 'Other')

males_30_39_years_old$Cancer.site <- replace(males_30_39_years_old$Cancer.site, 11:nrow(males_30_39_years_old), 'Other')
males_40_49_years_old$Cancer.site <- replace(males_40_49_years_old$Cancer.site, 11:nrow(males_40_49_years_old), 'Other')
males_50_59_years_old$Cancer.site <- replace(males_50_59_years_old$Cancer.site, 11:nrow(males_50_59_years_old), 'Other')
males_60_69_years_old$Cancer.site <- replace(males_60_69_years_old$Cancer.site, 11:nrow(males_60_69_years_old), 'Other')
males_70_79_years_old$Cancer.site <- replace(males_70_79_years_old$Cancer.site, 11:nrow(males_70_79_years_old), 'Other')

# group by cancer site

# install required packages for grouping
# install.packages('dplyr')
library(dplyr)

females_30_39_years_old_plot <- as.data.frame(females_30_39_years_old %>% group_by(Cancer.site) %>%
                                                summarise(Cancer.cases.sum = sum(Diagnosed.cancer.cases, na.rm = T),
                                                          .groups = 'drop') %>%
                                                filter(Cancer.site != 'Other')) # exclude the 'Other' cancer sites for. However, if there is an interest to
# study the incidence of the most common cancers in 2023 with respect to all 'Other' cancer types, comment this line out

females_40_49_years_old_plot <- as.data.frame(females_40_49_years_old %>% group_by(Cancer.site) %>%
                                                summarise(Cancer.cases.sum = sum(Diagnosed.cancer.cases, na.rm = T),
                                                          .groups = 'drop') %>%
                                                filter(Cancer.site != 'Other'))

females_50_59_years_old_plot <- as.data.frame(females_50_59_years_old %>% group_by(Cancer.site) %>%
                                                summarise(Cancer.cases.sum = sum(Diagnosed.cancer.cases, na.rm = T),
                                                          .groups = 'drop') %>%
                                                filter(Cancer.site != 'Other'))

females_60_69_years_old_plot <- as.data.frame(females_60_69_years_old %>% group_by(Cancer.site) %>%
                                                summarise(Cancer.cases.sum = sum(Diagnosed.cancer.cases, na.rm = T),
                                                          .groups = 'drop') %>%
                                                filter(Cancer.site != 'Other'))

females_70_79_years_old_plot <- as.data.frame(females_70_79_years_old %>% group_by(Cancer.site) %>%
                                                summarise(Cancer.cases.sum = sum(Diagnosed.cancer.cases, na.rm = T),
                                                          .groups = 'drop') %>%
                                                filter(Cancer.site != 'Other'))

# similar grouping based on cancer site for males
males_30_39_years_old_plot <- as.data.frame(males_30_39_years_old %>% group_by(Cancer.site) %>%
                                              summarise(Cancer.cases.sum = sum(Diagnosed.cancer.cases, na.rm = T),
                                                        .groups = 'drop') %>%
                                              filter(Cancer.site != 'Other'))

males_40_49_years_old_plot <- as.data.frame(males_40_49_years_old %>% group_by(Cancer.site) %>%
                                              summarise(Cancer.cases.sum = sum(Diagnosed.cancer.cases, na.rm = T),
                                                        .groups = 'drop') %>%
                                              filter(Cancer.site != 'Other'))

males_50_59_years_old_plot <- as.data.frame(males_50_59_years_old %>% group_by(Cancer.site) %>%
                                              summarise(Cancer.cases.sum = sum(Diagnosed.cancer.cases, na.rm = T),
                                                        .groups = 'drop') %>%
                                              filter(Cancer.site != 'Other'))

males_60_69_years_old_plot <- as.data.frame(males_60_69_years_old %>% group_by(Cancer.site) %>%
                                              summarise(Cancer.cases.sum = sum(Diagnosed.cancer.cases, na.rm = T),
                                                        .groups = 'drop') %>%
                                              filter(Cancer.site != 'Other'))

males_70_79_years_old_plot <- as.data.frame(males_70_79_years_old %>% group_by(Cancer.site) %>%
                                              summarise(Cancer.cases.sum = sum(Diagnosed.cancer.cases, na.rm = T),
                                                        .groups = 'drop') %>%
                                              filter(Cancer.site != 'Other'))

# define color and line style palette

library(scales)

# the following color and line style palette are compatible with the online version of the article
color_palette = c('Breast' = '#F8766D', 'Thyroid gland' = '#00BFC1', 'Colon' = '#CD9500', 'Ovary' = '#998EFF', 
                  'Myeloproliferative neoplasms' = '#00BA38', 'Hodgkin lymphoma' = '#00C19F', 'Melanoma of the skin' = '#93AA00',
                  'Other brain & meninges & CNS' = '#EA8332', 'Glioma' = '#00B9E3',
                  'Cervix uteri' = '#619CFF', 'Corpus uteri' = '#00BE6E', 'Meningeoma' = '#00A4FF', 'Rectum & rectosigmoid' = '#F27D54',
                  'CNS & nerve sheath tumor' = '#C19B00', 'Pancreas' = '#00A8FF', 'Skin squamous cell carcinoma' = '#ACA300',
                  'Acute lymphoblastic leukaemia/lymphoma' = '#F57961', 'Acute myeloid leukaemia' = '#8993FF', 'Lung & trachea' = '#00B822',
                  'Diffuse B lymphoma' = '#65B200', 'Testis' = '#00B2F4', 'Bladder & urinary tract' = '#C97BFF', 'Kidney' = '#E28900', 'Prostate' = '#7797FF',
                  'Pharynx' = '#BF80FF', 'Soft tissues' = '#FA7378', 'Liver' = '#B484FF', 'Other' = '#A9A9A9')

line_style_palette = c('Breast' = 'solid', 'Cervix uteri' = 'twodash', 'Colon' = 'solid', 'Glioma' = 'twodash', 
                       'Hodgkin lymphoma' = 'solid', 'Melanoma of the skin' = 'twodash', 'Myeloproliferative neoplasms' = 'solid',
                       'Other brain & meninges & CNS' = 'twodash', 'Ovary' = 'solid',
                       'Thyroid gland' = 'twodash', 'Corpus uteri' = 'solid', 'Meningeoma' = 'solid', 'Rectum & rectosigmoid' = 'twodash',
                       'Lung & trachea' = 'twodash', 'Pancreas' = 'twodash', 'Skin squamous cell carcinoma' = 'solid',
                       'Acute lymphoblastic leukaemia/lymphoma' = 'twodash', 'Acute myeloid leukaemia' = 'solid', 'CNS, nerve sheath tumor' = 'twodash',
                       'Diffuse B lymphoma' = 'twodash', 'Testis' = 'solid', 'Bladder & urinary tract' = 'twodash', 'Kidney' = 'solid', 'Prostate' = 'solid',
                       'Pharynx' = 'solid', 'Soft tissues' = 'solid', 'Liver' = 'solid', 'Other' = 'twodash')

# plot histograms of the most common cancer for each subgroup

# install.packages('cowplot')
library(cowplot)
# install.packages('ggplot2')
library(ggplot2)
library(stringr)

# define auxiliary function to help with the plotting
plot_histogram <- function(data, title, print_version) {
  hist <- ggplot(data = data, aes(x = Cancer.site, y = Cancer.cases.sum, fill = as.factor(Cancer.site))) +
    geom_col() +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.line = element_line(colour = 'black'),
      axis.title.x = element_blank(),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 8)
    ) +
    ylab('Diagnosed cancer cases') +
    ggtitle(title) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 22)) +
    guides(fill = 'none')
  
  # conditional color scale; if print_version is true then grey scale will be used
  if (print_version) {
    hist <- hist + scale_fill_grey(start = 0.1, end = 0.9)
  } else {
    hist <- hist + scale_fill_manual(values = color_palette)
  }
  
  return(hist)
}

# define the data to be plotted both for females and males
data_females <- list(females_30_39_years_old_plot,
                     females_40_49_years_old_plot,
                     females_50_59_years_old_plot,
                     females_60_69_years_old_plot,
                     females_70_79_years_old_plot)

data_males <- list(males_30_39_years_old_plot,
                   males_40_49_years_old_plot,
                   males_50_59_years_old_plot,
                   males_60_69_years_old_plot,
                   males_70_79_years_old_plot)

# define titles for the subplots
titles <- c(
  'Age group: 30-39 years',
  'Age group: 40-49 years',
  'Age group: 50-59 years',
  'Age group: 60-69 years',
  'Age group: 70-79 years'
)

# histograms representing most common cancers across different age groups, females 

# choose whether print_version is true or not
print_version <- F

# call the function defined above
histograms_females <- mapply(plot_histogram, data_females, titles, print_version, SIMPLIFY = FALSE)

# access individual plots
p1 <- histograms_females[[1]]
p2 <- histograms_females[[2]]
p3 <- histograms_females[[3]]
p4 <- histograms_females[[4]]
p5 <- histograms_females[[5]]

# construct title
title <- ggdraw() + 
  draw_label(
    'Diagnosed cases of the 10 most common cancers among females in Finland in 2023',
    fontface = 'bold',
    x = 0.5,
    hjust = 0.5
  ) +
  theme(
    plot.margin = margin(0, 0, 0, 0)
  )

# create a layout with 2 rows, 3 histograms on the first row and 2 on the second
top_row <- plot_grid(p1, p2, p3, ncol = 3)
bottom_row <- plot_grid(NULL, p4, p5, NULL, ncol = 4, rel_widths = c(1, 2, 2, 1))

subplots <- plot_grid(top_row, bottom_row, 
                      ncol = 1, rel_heights = c(1, 1))

final_plot <- plot_grid(title, subplots, ncol = 1, rel_heights = c(0.1, 1))

# save the resulting plot in EPS format with 800 dpi
ggsave(paste0(ifelse(print_version, 'print_', 'online_'), 'most_common_cancers_females_2023.eps'), plot = final_plot, width = 12, height = 8, units = 'in', 
       dpi = 800, device = 'eps')

# then same for males, i.e., plotting the histograms representing most common cancers across different age groups, males
histograms_males <- mapply(plot_histogram, data_males, titles, print_version, SIMPLIFY = FALSE)

p1 <- histograms_males[[1]]
p2 <- histograms_males[[2]]
p3 <- histograms_males[[3]]
p4 <- histograms_males[[4]]
p5 <- histograms_males[[5]]

title <- ggdraw() + 
  draw_label(
    'Diagnosed cases of the 10 most common cancers among males in Finland in 2023',
    fontface = 'bold',
    x = 0.5,
    hjust = 0.5
  ) +
  theme(
    plot.margin = margin(0, 0, 0, 0)
  )

top_row <- plot_grid(p1, p2, p3, ncol = 3)
bottom_row <- plot_grid(NULL, p4, p5, NULL, ncol = 4, rel_widths = c(1, 2, 2, 1))

subplots <- plot_grid(top_row, bottom_row, 
                      ncol = 1, rel_heights = c(1, 1))

final_plot <- plot_grid(title, subplots, ncol = 1, rel_heights = c(0.1, 1))

ggsave(paste0(ifelse(print_version, 'print_', 'online_'), 'most_common_cancers_males_2023.eps'), plot = final_plot, width = 12, height = 8, units = 'in', 
       dpi = 800, device = 'eps')

# extract the most common cancer types
females_30_39_years_old_most_common_cancers <- females_30_39_years_old_plot[,1][females_30_39_years_old_plot['Cancer.site'] != 'Other']
females_40_49_years_old_most_common_cancers <- females_40_49_years_old_plot[,1][females_40_49_years_old_plot['Cancer.site'] != 'Other']
females_50_59_years_old_most_common_cancers <- females_50_59_years_old_plot[,1][females_50_59_years_old_plot['Cancer.site'] != 'Other']
females_60_69_years_old_most_common_cancers <- females_60_69_years_old_plot[,1][females_60_69_years_old_plot['Cancer.site'] != 'Other']
females_70_79_years_old_most_common_cancers <- females_70_79_years_old_plot[,1][females_70_79_years_old_plot['Cancer.site'] != 'Other']

males_30_39_years_old_most_common_cancers <- males_30_39_years_old_plot[,1][males_30_39_years_old_plot['Cancer.site'] != 'Other']
males_40_49_years_old_most_common_cancers <- males_40_49_years_old_plot[,1][males_40_49_years_old_plot['Cancer.site'] != 'Other']
males_50_59_years_old_most_common_cancers <- males_50_59_years_old_plot[,1][males_50_59_years_old_plot['Cancer.site'] != 'Other']
males_60_69_years_old_most_common_cancers <- males_60_69_years_old_plot[,1][males_60_69_years_old_plot['Cancer.site'] != 'Other']
males_70_79_years_old_most_common_cancers <- males_70_79_years_old_plot[,1][males_70_79_years_old_plot['Cancer.site'] != 'Other']

# filter the original 'incidence' dataframe including all years from 1963 to 2023 based on the identified most common cancers in 2023
females_30_39_years_old_ts <- incidence[incidence$Age.group == '30-39' & incidence$Sex == 'Female' & incidence$Cancer.site %in% c(females_30_39_years_old_most_common_cancers),]
females_40_49_years_old_ts <- incidence[incidence$Age.group == '40-49' & incidence$Sex == 'Female' & incidence$Cancer.site %in% c(females_40_49_years_old_most_common_cancers),]
females_50_59_years_old_ts <- incidence[incidence$Age.group == '50-59' & incidence$Sex == 'Female' & incidence$Cancer.site %in% c(females_50_59_years_old_most_common_cancers),]
females_60_69_years_old_ts <- incidence[incidence$Age.group == '60-69' & incidence$Sex == 'Female' & incidence$Cancer.site %in% c(females_60_69_years_old_most_common_cancers),]
females_70_79_years_old_ts <- incidence[incidence$Age.group == '70-79' & incidence$Sex == 'Female' & incidence$Cancer.site %in% c(females_70_79_years_old_most_common_cancers),]

males_30_39_years_old_ts <- incidence[incidence$Age.group == '30-39' & incidence$Sex == 'Male' & incidence$Cancer.site %in% c(males_30_39_years_old_most_common_cancers),]
males_40_49_years_old_ts <- incidence[incidence$Age.group == '40-49' & incidence$Sex == 'Male' & incidence$Cancer.site %in% c(males_40_49_years_old_most_common_cancers),]
males_50_59_years_old_ts <- incidence[incidence$Age.group == '50-59' & incidence$Sex == 'Male' & incidence$Cancer.site %in% c(males_50_59_years_old_most_common_cancers),]
males_60_69_years_old_ts <- incidence[incidence$Age.group == '60-69' & incidence$Sex == 'Male' & incidence$Cancer.site %in% c(males_60_69_years_old_most_common_cancers),]
males_70_79_years_old_ts <- incidence[incidence$Age.group == '70-79' & incidence$Sex == 'Male' & incidence$Cancer.site %in% c(males_70_79_years_old_most_common_cancers),]

# store the most common cancers data to a CSV-file for the purposes of the cluster analysis

# create the target dataframe structure
df <- data.frame(
  gender = character(),
  age_group = character(),
  year = integer(),
  cancer_id = character(),
  incidence = numeric()
)

# define all the dataframe names
genders <- c('females', 'males')
age_groups <- c('30_39', '40_49', '50_59', '60_69', '70_79')

# create all dataframe names
df_names <- paste0(rep(genders, each = length(age_groups)), '_', 
                   rep(age_groups, times = length(genders)), '_years_old_ts')

# loop through each dataframe and compile
for (df_name in df_names) {
  # check if the dataframe exists
  if (exists(df_name)) {
    # get the current dataframe
    current_df <- get(df_name)
    
    # extract the data
    temp_df <- data.frame(
      gender = ifelse(grepl('^females', df_name), 'female', 'male'),
      age_group = gsub('_', '-', sub('.*_(\\d+_\\d+)_years.*', '\\1', df_name)),
      year = current_df$Year,
      cancer_id = current_df$Cancer.site,
      incidence = current_df$Rate.per.100.000,
      stringsAsFactors = FALSE
    )
    # bind to main dataframe
    df <- rbind(df, temp_df)
  } else {
    warning(paste('Dataframe', df_name, 'not found'))
  }
}

# check the result
head(df)
table(df$gender, df$age_group)

# store the resulting dataframe into a CSV-file
write.csv(df, 'cancer_data_most_common_cancers.csv', row.names = FALSE)

# SUPPLEMENTAL MATERIAL; the following contains an additional functions - for plotting the incidence of the most common cancers
# of a given age group and gender as time series from 1963 to 2023 - that was not used as the basis to produce any material in the
# actual article, but the time series included in the online Supplemental Material.

# function to plot incidence of the most common cancers as time series from 1963 to 2023
# advantage is taken of the color and line style palettes defined above (lines 119-135)

plot_ts <- function(data, title) {
  p <- ggplot(data=data, aes(x=Year, y=Rate.per.100.000, colour = as.factor(Cancer.site), linetype = as.factor(Cancer.site))) +
    geom_line(linewidth = 0.75) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.line = element_line(colour = 'black'),
      legend.position = 'right',
      legend.justification = 'left',
      legend.direction = 'vertical',
      legend.title = element_blank()
    ) + 
    xlab('Year') +
    ylab('Rate per 100,000 person-years') +
    ggtitle(title) +
    scale_color_manual(values = color_palette, name='Cancer site', labels = function(labels) str_wrap(labels, width = 15)) +
    scale_linetype_manual(values = line_style_palette, name='Cancer site', labels = function(labels) str_wrap(labels, width = 15))
  
  return(p)
}

# usage
age_groups <- c('30-39', '40-49', '50-59', '60-69', '70-79')
genders <- c('female', 'male')

for (gender in genders) {
  for (age_group in age_groups) {
  
  final_plot <- plot_ts(data = get(paste0(gender, 's_', gsub('-', '_', age_group), '_years_old_ts')), 
                        title = paste0('Incidence rates per 100,000 person-years of the most commmon cancers\namong females aged ', age_group, ' years'))
  
  filename <- paste0('online_most_common_cancers_ts_', gender, '_', age_group,  '.jpg')
  ggsave(filename, final_plot, width = 15.00, height = 12.50, units = 'in', dpi = 800, device = 'jpg')
  
  }
}

# example: females; age group: 30-39 years
plot_ts(data = females_30_39_years_old_ts, title = 'Age group: 30-39 years')

# ADDITIONAL MATERIAL; the following contains an additional functions - for plotting the moving average 
# graphs of the cancer incidence time series - that was not # used as the basis of any of the material presented in the article, 
# but may be useful for some.

# function to plot the respective moving averages

library('zoo')
plot_ma <- function(data, title, ma_interval_length) {
  p <- ggplot(data=data, aes(x=Year, y=rollmean(Rate.per.100.000, k = ma_interval_length, fill = NA), colour = as.factor(Cancer.site), linetype = as.factor(Cancer.site))) +
    geom_line(linewidth = 0.75) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.line = element_line(colour = 'black'),
      legend.position = 'right',
      legend.justification = 'left',
      legend.direction = 'vertical',
      legend.title = element_blank()
    ) + 
    xlab('Year') +
    ylab(paste0(ma_interval_length, '-year moving average rate per 100,000 person-years')) +
    ggtitle(title) +
    scale_color_manual(values = color_palette, name='Cancer site', labels = function(labels) str_wrap(labels, width = 15)) +
    scale_linetype_manual(values = line_style_palette, name='Cancer site', labels = function(labels) str_wrap(labels, width = 15))
  
  return(p)
}

# example: females; age group: 30-39 years, 10 years moving average
plot_ma(data = females_30_39_years_old_ts, title = 'Age group: 30-39 years', ma_interval_length = 10)
