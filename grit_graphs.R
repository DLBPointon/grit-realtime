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
library(htmlwidgets)


filename <- args[1]

getwd()

date <- format(Sys.Date(), "%d%m%y")
jira_data_file <- sprintf("/Users/dp24/Documents/grit-boot/assets/data/jira_dump_260221.tsv.sorted", date) # jira_dump_%s.tsv.sorted
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

master_dict = list('Amphibia' = 'a',
                   'Bird' = 'b',
                   'Non-vascular plants' = 'c',
                   'Dicotyledons' = 'd',
                   'Echinoderms' = 'e',
                   'Fish' = 'f',
                   'Fungi' = 'g',
                   'Platyhelminths' = 'h',
                   'Insects' = 'i',
                   'Jellyfish and Cnidaria' = 'j',
                   'Other Chordates' = 'k',
                   'Monocotyledons' = 'l',
                   'Mammal' = 'm',
                   'Nematodes' = 'n',
                   'Sponges' = 'o',
                   'Protists' = 'p',
                   'Other Arthropods' = 'q',
                   'Reptile' = 'r',
                   'Shark' = 's',
                   'Other Animal Phyla' = 't',
                   'Algae' = 'u',
                   'Other Vascular Plants' = 'v',
                   'Annelids' = 'w',
                   'Molluscs' = 'x',
                   'Bacteria' = 'y',
                   'Archae' = 'z')

date_graphs_mi <- function(dataframe) {
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
  
  htmlwidgets::saveWidget(as_widget(fig), file='graphs/date_manual_interventions.html', selfcontained = TRUE)
  
  
  
}

plots_length_ch_matrix <- function(dataframe) {
  # Facet wrapped plots for change in length for all grouped by prefix
  ggplot(data=jira_data,
         aes(x=X.sample_id, y=length.change, fill=prefix)) +
    geom_bar(stat = 'identity') +
    theme_minimal() +
    theme(text = element_text(size=10),
          axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = "Sample ID",
         y = "Percentage change in length of genome post-curation",
         title = "A graph to show the change in genome size when comparing pre and post curation") +
    facet_wrap(~prefix , scales = "free", nrow = 3, ncol = 5)
  ggplotly()
  this_plot <- plotly::ggplotly()
  htmlwidgets::saveWidget(as_widget(this_plot), file='graphs/plot_length_all.html', selfcontained = TRUE)
  
}

box_plot <- function(dataframe) {

  fig <- plot_ly(data=jira_data, y=length.change, color = prefix, type="box")
  htmlwidgets::saveWidget(as_widget(fig), 'graphs/boxplot_all_length_change.html', selfcontained = TRUE)
  
}

master_plot <- function(dataframe) {
  fig <- plot_ly(jira_data, x=X.sample_id, y=length.change, color=prefix, type="bar")
  htmlwidgets::saveWidget(as_widget(fig), 'graphs/master_all_length_change.html', )
}

mean_change_all <- function(dataframe) {
  # Plots the average genome length change for all groups
  dfs <- data.frame(x = character(), y = numeric(), stringsAsFactors = FALSE)
  for(i in master_dict) {
    mean <- jira_data[ which(jira_data$prefix==i), ]
    change <- sum(mean$length.change / nrow(mean))
    
    dfs[i,] <- list(i, change)
  }

  fig<-plot_ly(dfs, x=dfs$x, y=dfs$y, type="bar", color = dfs$x)
  htmlwidgets::saveWidget(as_widget(fig), 'graphs/mean_length_change_bar_all.html', selfcontained = TRUE)
}

scatter_change_by_length_all <- function(dataframe) {
  fig <- plot_ly(dataframe,
          x=(normalized_by_len/10000),
          y=length.change,
          type = "scatter",
          mode = "markers",
          color=prefix,
          text = X.sample_id,
          hovertemplate = paste('Percentage through genome: %{x}%\n',
                                'Length Change: %{y}%\n',
                                'Sample: %{text}\n'))
  htmlwidgets::saveWidget(as_widget(fig), 'graphs/scatter_len_ch_by_len_norm.html', selfcontained = TRUE)

}

fig4_mimic <- function(dataframe) {
  fig1 <- plot_ly(jira_data,
                  x=length_in_mb,
                  y=manual_interventions_normalised,
                  color = prefix,
                  type="scatter",
                  mode="markers",
                  legendgroup = prefix,
                  showlegend=TRUE)
  fig2 <- plot_ly(jira_data,
                  x=length_in_mb,
                  y=scaff.n50.change,
                  color = prefix,
                  type="scatter",
                  mode="markers",
                  legendgroup = prefix,
                  showlegend=FALSE)
  fig3 <- plot_ly(jira_data,
                  x=length_in_mb, 
                  y=scaff_count_per,
                  color = prefix,
                  type="scatter",
                  mode="markers",
                  legendgroup = prefix,
                  showlegend=FALSE)
  figure <- subplot(fig1, fig2, fig3, nrows=3, shareX=TRUE)
  htmlwidgets::saveWidget(as_widget(figure), 'graphs/fig4_mimic.html', selfcontained = TRUE)
}

project_pie <- function(dataframe) {
  colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')
  
  fig <- plot_ly(jira_data, labels=project_type,
                 values = nrow(project_type),
                 type = 'pie',
                 textinfo='label+percent',
                 marker = list(colors = colors,
                               line = list(color = '#FFFFFF', width = 1)),
                 showlegend=FALSE)

  htmlwidgets::saveWidget(as_widget(fig), 'graphs/project_pie.html', selfcontained = TRUE)
}

project_graph <- function(dataframe) {
  
  fig <- plot_ly(dataframe, x = X.sample_id, y = manual_interventions_normalised,
          color = project_type,
          type="scatter",
          mode="markers"
          )

  htmlwidgets::saveWidget(as_widget(fig), 'graphs/plot_by_project.html', selfcontained = TRUE)
}

data_table_int <- function(dataframe) {
  fig <- plot_ly(
    type = 'table',
    columnwidth = 200,
    header = list(
      values = c("<b>Jira Dump</b>", names(dataframe)),
      align = c('left', rep('center', ncol(dataframe))),
      line = list(width = 1, color = 'black'),
      fill = list(color = 'rgb(235, 100, 230)'),
      font = list(family = "Arial", size = 14, color = "white")
    ),
    cells = list(
      values = rbind(
        rownames(dataframe), 
        t(as.matrix(unname(dataframe)))
      ),
      align = c('left', rep('center', ncol(dataframe))),
      line = list(color = "black", width = 1),
      fill = list(color = c('rgb(235, 193, 238)', 'rgba(228, 222, 249, 0.65)')),
      font = list(family = "Arial", size = 12, color = c("black")),
      height = 10
    ))
  
  htmlwidgets::saveWidget(as_widget(fig), 'graphs/data_table_full.html', selfcontained = TRUE)
}

main <- function() {
  # Date graphs
  date_graphs_mi(jira_data)
  
  # Sample by Length Change
  plots_length_ch_matrix(jira_data)
  mean_change_all(jira_data)
  
  # Boxplot length all
  box_plot(jira_data)
  
  # Bar length change
  master_plot(jira_data)
  
  # Scatter len change by len normalised
  scatter_change_by_length_all(jira_data)
  
  #Produced a graph which mimics the fig 4 graph from gEVAL paper
  fig4_mimic(jira_data)
  
  project_graph(jira_data)
  
  project_pie(jira_data)
  
  data_table_int(jira_data)
}

main()

