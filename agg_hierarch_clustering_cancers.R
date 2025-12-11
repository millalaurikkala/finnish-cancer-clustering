
### script for performing the agglomerative hierarchical clustering and analyzing the results ### 

# set working directory

# setwd('your/working/directory')

# read data
df <- read.csv('cancer_data_most_common_cancers.csv', header = T, sep = ',')

# install needed packages
#install.packages('cowplot')
library(cowplot)
#install.packages('ggplot2')
library(ggplot2)
library(stringr)
library(glue)
#install.packages('dendextend')
library(dendextend)
#install.packages('ggdendro')
library(ggdendro)
library(dplyr)

# define auxiliary functions for agglomerative hierarchical clustering

# function that allows compute distance between two curves using mean and 
# variance as the location and scatter measure, respectively
distance_measure_discrete <- function(X, Y) { 
  
  if(length(X) != length(Y)) { # error handling
    stop('X and Y must have the same length')
  }
  
  k <- length(X)        # k observations
  overline_x <- mean(X) # mean of X observations  
  overline_y <- mean(Y) # mean of Y observations
  
  # calculate the discrete proximity measure, including normalization
  D <- (1/k) * sum((X - Y)^2 - (overline_x - overline_y)^2)
  
  return(D)
}

# compute average distance between clusters. Q and R id's are in the df
avg_distance <- function(Q, R, dist_matrix, cluster_structure_df) { 
  
  indices_Q <- cluster_structure_df[cluster_structure_df$cluster_id == Q, 'ts_id']
  indices_R <- cluster_structure_df[cluster_structure_df$cluster_id == R, 'ts_id']
  
  size_Q <- length(indices_Q)
  size_R <- length(indices_R)
  
  d_avg <- 0 # initialize the average distance to be zero
  
  # sum all pairwise distances between clusters Q and R
  for(i in indices_R) {
    for(j in indices_Q) {
      # fetch pre-computed distances from distance matrix
      d_avg <- d_avg + dist_matrix[i, j]
    }
  }
  
  d_avg <- 1/(size_R * size_Q) * d_avg # normalization
  return(d_avg)
}

# standardization using mean and std
standardize_variables <- function(X) { 
  overline_x <- mean(X)
  std_x <- sqrt(sum((X - overline_x)^2) / length(X))
  
  standardized_X <- (X - overline_x) / std_x
  return(standardized_X)
}

# the actual clustering functionality
agg_hierarchical_clust <- function(data, standardization, nof_clusters) {
  
  # some preprocessing of the dataframe
  names(data)[names(data) == 'incidence'] <- 'value'
    
  # standardize the variables (if switch on)
  if(standardization == T) {
    for(cancer in unique(data$cancer_id)) {
      cancer_rows <- data$cancer_id == cancer
      data$value[cancer_rows] <- standardize_variables(data$value[cancer_rows])
    }
  } 
    
  # initialize each object as its own cluster
  
  # start by extracting the 10 most common cancer types
  unique_cancers <- unique(data$cancer_id)
  n_objects <- length(unique_cancers)
  
  # initialize a dataframe for storing the results
  cluster_structure_df <- data.frame(
    cluster_id = 1:n_objects,          # initially each object is its own cluster
    ts_id = 1:n_objects,               # indexing for the dist matrix purposes
    ts = I(vector('list', n_objects)), # allows to store lists into a dataframe
    cancer_id = rep(NA_character_, n_objects)
    )
  
  # populate the dataframe
  time_series_list <- list()
  for (i in 1:n_objects) {
    cancer <- unique_cancers[i]
    
    # extract time series for the cancer at hand
    cancer_data <- data[data$cancer_id == cancer, ]
    cancer_ts <- cancer_data$value[order(cancer_data$year)]  # ensure increasing ordering
    
    # store in list for distance computation
    time_series_list[[i]] <- cancer_ts
    
    # add row to cluster_structure_df
    cluster_structure_df$ts[[i]] <- cancer_ts
    cluster_structure_df$cancer_id[i] <- cancer
  }
  
  # compute initial distance matrix between all objects to avoid unnecessary re-computations later
  dist_matrix <- matrix(0, nrow = n_objects, ncol = n_objects)
  
  for(i in 1:n_objects) {
    for(j in 1:n_objects) {
      if(i != j) {
        dist_matrix[i, j] <- distance_measure_discrete(time_series_list[[i]], time_series_list[[j]])
      }
    }
  }
  
  # update clustering partitioning until there is predefined number of clusters left
  while(length(unique(cluster_structure_df$cluster_id)) > nof_clusters) {
    minimum_avg_dist <- Inf
    closest_clusters <- NULL
    
    # iterate over all clusters
    cluster_pairs <- combn(unique(cluster_structure_df$cluster_id), 2, simplify = FALSE)
    for (cluster_pair in cluster_pairs) {
      avg_dist <- avg_distance(cluster_pair[1], 
                               cluster_pair[2], 
                               dist_matrix, 
                               cluster_structure_df)
      if(avg_dist < minimum_avg_dist) {
        minimum_avg_dist <- avg_dist
        closest_clusters <- cluster_pair
      }
    }
    
    cluster_structure_df$cluster_id[cluster_structure_df$cluster_id == closest_clusters[1]] <- closest_clusters[2]
  }
  
  # finally, adjust the cluster_id's to reflect the actual cancers included in each cluster
  unique_cluster_ids <- unique(cluster_structure_df$cluster_id)
  for(cluster_id in unique_cluster_ids) {
    cancers_in_cluster <- sort(cluster_structure_df$cancer_id[cluster_structure_df$cluster_id == cluster_id])
    new_cluster_label <- paste(cancers_in_cluster, collapse = ', ') # generate comma-separated list of the cancers in a given cluster
    cluster_structure_df$cluster_id[cluster_structure_df$cluster_id == cluster_id] <- new_cluster_label # replace cluster_id with cancer names in the cluster
  }
  
  return(cluster_structure_df)
}

# another auxiliary function to help plotting the results
create_plotting_data <- function(cluster_structure_df) {

  # initialize a dataframe
  plotting_data <- data.frame()
  
  for(i in 1:nrow(cluster_structure_df)) {
    cancer_id  <- cluster_structure_df$cancer_id[i]
    cluster_id <- cluster_structure_df$cluster_id[i]
    year <- c(1963:2023)
    ts_values  <- cluster_structure_df$ts[[i]]
    
    # create dataframe for the cancer at hand
    cancer_data <- data.frame(
      cancer_id = cancer_id,
      cluster_id = cluster_id,
      year = year,
      ts = ts_values
    )
    
    # combine to output dataframe
    plotting_data <- rbind(plotting_data, cancer_data)
  }
  
  return(plotting_data)
}

# automatize color palette creation
create_color_palette <- function(cluster_structure_df, print_version) { 
  if (print_version == F) {
    colors <- c('red', 'black', 'blue', 'purple') # colors for online version
  } else { 
    colors <- c('#BBBBBB', '#000000', '#777777', '#555555') # print version with greyscale
  }
  
  unique_cluster_ids <- sort(unique(cluster_structure_df$cluster_id))
  
  if (print_version == T) {
    # calculate cluster sizes
    cluster_sizes <- cluster_structure_df %>%
      group_by(cluster_id) %>%
      summarise(size = n(), .groups = 'drop') %>%
      arrange(desc(size)) # set largest cluster as the first
    
    # assign lightest color to the largest cluster
    n_clusters <- length(unique_cluster_ids)
    selected_colors <- colors[1:n_clusters]
    
    # create the palette
    color_palette <- setNames(selected_colors, as.character(cluster_sizes$cluster_id))
  } else {
    color_palette <- setNames(colors[1:length(unique_cluster_ids)], as.character(unique_cluster_ids))
  }
  
  return(color_palette)
}

# same for line styles
create_line_style_palette <- function(cluster_structure_df, print_version) { 
  if (print_version == F) {
    line_styles <- c(rep.int('solid', 4)) # line style for online version
  } else {
    line_styles <- c('solid', 'twodash', 'twodash', 'solid') # print version line styles
  }
  
  unique_cluster_ids <- sort(unique(cluster_structure_df$cluster_id))
  
  if (print_version == T) {
    # calculate cluster sizes
    cluster_sizes <- cluster_structure_df %>%
      group_by(cluster_id) %>%
      summarise(size = n(), .groups = 'drop') %>%
      arrange(desc(size)) # set largest cluster as the first
    
    n_clusters <- length(unique_cluster_ids)
    selected_line_styles <- line_styles[1:n_clusters]
    
    # create the palette
    line_style_palette <- setNames(selected_line_styles, as.character(cluster_sizes$cluster_id))
  } else {
    line_style_palette <- setNames(line_styles[1:length(unique_cluster_ids)], as.character(unique_cluster_ids))
  }
  
  return(line_style_palette)
}

# plot clustering results #

# define function for plotting
cluster_and_plot <- function(gender, age_group, standardization, nof_clusters, print_version) {
  res_clust_struct <- create_plotting_data(agg_hierarchical_clust(
    df[df$gender == gender & df$age_group == age_group, ], 
    standardization = standardization, 
    nof_clusters = nof_clusters))
  
  color_palette <- create_color_palette(res_clust_struct, print_version)
  line_style_palette <- create_line_style_palette(res_clust_struct, print_version)
  
  if(standardization == T) {
    ylab <- 'Standardized rate per 100,000 person-years'
  } else {
    ylab <- 'Rate per 100,000 person-years'
  }
  
  ggplot(data=res_clust_struct, aes(x=year, y=ts, colour = as.factor(cluster_id), linetype = as.factor(cluster_id), group = cancer_id)) +
    geom_line(linewidth = 0.75) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.line = element_line(colour = 'black'),
      legend.position = 'right',
      legend.justification = 'left',
      legend.direction = 'vertical',
      legend.title = element_blank(), 
      legend.key.spacing.y = unit(0.5, 'cm') # increase space between legend entries
    ) + guides(fill = guide_legend(byrow = TRUE)) +
    xlab('Year') +
    ylab(ylab) +
    ggtitle(glue('{nof_clusters} clusters')) +
    scale_color_manual(values = color_palette, 
                       name='cluster_id', 
                       labels = function(labels) {
                         wrapped <- str_wrap(labels, width = 15)
                         ifelse(nchar(labels) > 15, 
                                paste0(wrapped),#, '\n'), 
                                wrapped)
                       }) +
    scale_linetype_manual(values = line_style_palette, 
                        name='cluster_id', 
                        labels = function(labels) {
                          wrapped <- str_wrap(labels, width = 15)
                          ifelse(nchar(labels) > 15, 
                                 paste0(wrapped),#, '\n'), 
                                 wrapped)
                        })
}

# define auxiliary function to compute the distance matrix
compute_dist_matrix <- function(data, standardization) {
  
  # some preprocessing of the dataframe
  names(data)[names(data) == 'incidence'] <- 'value'
  
  # standardize the variables (if switch on)
  if(standardization == T) {
    for(cancer in unique(data$cancer_id)) {
      cancer_rows <- data$cancer_id == cancer
      data$value[cancer_rows] <- standardize_variables(data$value[cancer_rows])
    }
  }
  
  # compute the distance matrix
  
  # start by extracting the 10 most common cancer types
  unique_cancers <- sort(unique(data$cancer_id))
  n_objects <- length(unique_cancers)
  
  time_series_list <- list()
  for (i in 1:n_objects) {
    cancer <- unique_cancers[i]
    
    # extract time series for the cancer at hand
    cancer_data <- data[data$cancer_id == cancer, ]
    cancer_ts <- cancer_data$value[order(cancer_data$year)]  # ensure increasing ordering
    
    # store in list for distance computation
    time_series_list[[i]] <- cancer_ts
  }
  
  # populate the distance matrix
  dist_matrix <- matrix(0, nrow = n_objects, ncol = n_objects)
  
  for(i in 1:n_objects) {
    for(j in 1:n_objects) {
      if(i != j) {
        dist_matrix[i, j] <- distance_measure_discrete(time_series_list[[i]], time_series_list[[j]])
      }
    }
  }
  
  # assign cancer types as row and column names
  rownames(dist_matrix) <- unique_cancers
  colnames(dist_matrix) <- unique_cancers
  
  # return the dist matrix
  return(dist_matrix)
}

# create for-loop to automatize the plotting process

# define age groups and genders to iterate
age_groups <- c('30-39', '40-49', '50-59', '60-69', '70-79')
genders <- c('female', 'male')
print_version <- F

for (gender in genders) {
    for (age_group in age_groups) {
    
    # generate plots for 2, 3, and 4 clusters first using unstandardized data
    standardization <- F
    p1 <- cluster_and_plot(gender, age_group, standardization, nof_clusters = 4, print_version)
    p2 <- cluster_and_plot(gender, age_group, standardization, nof_clusters = 3, print_version)
    p3 <- cluster_and_plot(gender, age_group, standardization, nof_clusters = 2, print_version)
    
    # and then the same using standardized data
    standardization <- T
    p4 <- cluster_and_plot(gender, age_group, standardization, nof_clusters = 4, print_version)
    p5 <- cluster_and_plot(gender, age_group, standardization, nof_clusters = 3, print_version)
    p6 <- cluster_and_plot(gender, age_group, standardization, nof_clusters = 2, print_version)
    
    col_titles <- plot_grid(
      ggdraw() + draw_label('Unstandardized data', fontface = 'bold', size = 12),
      ggdraw() + draw_label('Standardized data', fontface = 'bold', size = 12),
      ncol = 2
    )
    
    title <- ggdraw() + 
      draw_label(
        paste0('Agglomerative hierarchical clustering of ', gender, 
               ' incidence rates\nper 100,000 person-years; age group: ', age_group, ' years'),
        fontface = 'bold',
        x = 0.5,
        hjust = 0.5
      ) +
      theme(
        plot.margin = margin(0, 0, 0, 0)
      )
    
    subplots <- plot_grid(p1 + theme(legend.position = 'right'),
                       p4 + theme(legend.position = 'right'),
                       p2 + theme(legend.position = 'right'),
                       p5 + theme(legend.position = 'right'),
                       p3 + theme(legend.position = 'right'),
                       p6 + theme(legend.position = 'right'),
                       nrow = 3, ncol = 2)
    
    final_plot <- plot_grid(title, col_titles, subplots, ncol = 1, rel_heights = c(0.1, 0.05, 1))

    filename <- paste0(ifelse(print_version, 'print_', 'online_'), 'clust_', gender, '_', gsub('_', '_', age_group),  '.eps')
    ggsave(filename, final_plot, width = 8.50, height = 12.50, units = 'in', dpi = 800, device = 'eps')

    }
}    

# ADDITIONAL MATERIAL; the following contains an additional function - for plotting the dendrograms representing
# the agglomerative hierarchical clustering process - that was not used as the basis of any of the material 
# presented in the article, but may be useful for some.

# function to plot the dendrogram
cluster_and_plot_dendogram <- function(gender, age_group, standardization, vjust) {
  
  # compute the dist matrix and assign it to variable
  dist_matrix <- compute_dist_matrix(df[df$gender == gender & df$age_group == age_group, ], 
                                     standardization = standardization)
  
  # perform hierarchical clustering using R's own method and turn into dendrogram object
  dhc <- as.dendrogram(hclust(as.dist(dist_matrix), method = 'average'))
  ddata <- dendro_data(dhc)
  
  # wrap long labels
  ddata$labels$label <- str_wrap(ddata$labels$label, width = 25)
  
  #plot with ggplot2
  ggplot() +
    geom_segment(data = ddata$segments,
                 aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_text(data = ddata$labels,
              aes(x = x, y = y, label = label, vjust = vjust),
              angle = 45, hjust = 1, size = 3) +
    scale_y_continuous(name = 'Average distance') +     
    labs(title = ifelse(standardization, 'Standardized data', 'Unstandardized data')) +
    coord_cartesian(clip = 'off') +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),             
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      plot.title = element_text(hjust = 0.5, face = 'bold'),
      plot.margin = margin(1, 1, 2.5, 1, 'cm')
    )
}

# example usage; similar kind of for-loop than above to plot the clustering results

vjust <- 1.5

for (gender in genders) { # assuming genders and age groups are still defined as above
  for (age_group in age_groups) {
    
    # generate plots using unstandardized and standardized data
    p1 <- cluster_and_plot_dendogram(gender, age_group, F, vjust)
    p2 <- cluster_and_plot_dendogram(gender, age_group, T, vjust)
    
    title <- ggdraw() + 
      draw_label(
        paste0('Agglomerative hierarchical clustering process of ', gender, 
               ' incidence rates\nper 100,000 person-years; age group: ', age_group, ' years'),
        fontface = 'bold',
        x = 0.5,
        hjust = 0.5
      ) +
      theme(
        plot.margin = margin(0, 0, 0, 0)
      )
    
    subplots <- plot_grid(p1 + theme(legend.position = 'right'),
                          p2 + theme(legend.position = 'right'),
                          nrow = 2, ncol = 1)
    
    final_plot <- plot_grid(title, subplots, ncol = 1, rel_heights = c(0.1, 1))
    
    filename <- paste0('ddgram', gender, '_', gsub('_', '_', age_group), '.eps')
    ggsave(filename, final_plot, width = 8.50, height = 12.50, units = 'in', dpi = 800, device = 'eps')
    
  }
}
