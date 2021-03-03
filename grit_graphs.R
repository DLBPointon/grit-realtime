# An R script to take jira_dump data and produce
# descriptive statistics and graphs

# This script is second in like for the grit-realtime project
# preceded by a python script.

# Written by dp24
# Updated January 2020

# Modules Required
require("ggplot2")
require("stringr")
require('dplyr')
require('gridExtra')
library("tidyr")
require("plotly")
library(tidyverse)

filename <- args[1]

getwd()

date <- format(Sys.Date(), "%d%m%y")
jira_data_file <- sprintf("jira_dump_170221.tsv.sorted", date) # jira_dump_%s.tsv.sorted
jira_data <- read.csv(jira_data_file, sep='\t', header=T, row.names=NULL)
detach(jira_data)
attach(jira_data)

jira_data$prefix_v <- str_extract(X.sample_id, '[[:lower:]]+') # pulls first letters for use as categorisers, example 'il' rather than 'i'
jira_data$prefix <- str_extract(X.sample_id, '.') # pulls first letter, example i for insect
jira_data$normalized_by_len <- ((length.after - min(length.after)) / (max(length.after) - min(length.after))) * 1000000
jira_data$manual_interventions_normalised <- (manual_interventions/length.after) * 1000000000 # mi / length = mi per base * 1 *10e9 (million) for per Gb
jira_data$length_in_mb <- length.before/1000000 # Equivilent to length in Gb * 1000 for length in Mb
jira_data$date_in_YMD <- as.Date(jira_data$date_in_YMD, "%Y-%m-%d")

detach(jira_data)

attach(jira_data)

date_graph_ops = list(manual_interventions, manual_interventions_normalised, length.change, scaff_count_per, scaff.n50.change)

date_graphs <- function(dataframe) {
  fig <- plot_ly(jira_data, x=date_in_YMD, y=manual_interventions, type = 'scatter',
                 text = X.sample_id,
                 mode='markers',
                 color=prefix,
                 colors="Set1",
                 hovertemplate = paste('Date: %{x}\n',
                                       'Interventions: %{y}\n',
                                       'Sample: %{text}\n'))
  fig <- fig %>% layout(
    xaxis = list(
      rangeselector = list(
        buttons = list(
          list(
            count = 3,
            label = "3 mo",
            step = "month",
            stepmode = "backward"),
          list(
            count = 6,
            label = "6 mo",
            step = "month",
            stepmode = "backward"),
          list(
            count = 1,
            label = "1 yr",
            step = "year",
            stepmode = "backward"),
          list(
            count = 1,
            label = "YTD",
            step = "year",
            stepmode = "todate"),
          list(step = "all"))),
      
      rangeslider = list(type = "date")),
    yaxis = list(title = "Manual Interventions"))
  
  htmlwidgets::saveWidget(fig, './graphs/date_manual_interventions.html')
  
  
  
}
date_graphs(jira_data)