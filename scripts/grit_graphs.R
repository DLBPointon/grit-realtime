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

################################################################################
getwd()

date <- format(Sys.Date(), "%d%m%y")
jira_data_file <- sprintf("../output/jira_dump_%s.tsv.sorted", date) # jira_dump_%s.tsv.sorted
jira_data <- read.csv(jira_data_file, sep='\t', header=T, row.names=NULL)
attach(jira_data)


# These should be moved to the python script
jira_data$manual_interventions_normalised <- (manual_interventions/length.after) * 1000000000 # mi / length = mi per base * 1 *10e9 (million) for per Gb
jira_data$length_in_mb <- length.after/1000000 # Equivilent to length in Gb * 1000 for length in Mb
jira_data$date_in_YMD <- as.Date(jira_data$date_in_YMD, "%Y-%m-%d")
detach(jira_data)
attach(jira_data)
default_save_loc <- '../grit-boot/assets/img/'

################################################################################
# Date Graphs and their outliers- general function
date_graphs <- function(dataframe, yaxis, colour_by, outlier_no) {
  outlier_lim <- sort(manual_interventions_normalised, T)[outlier_no]
  
  fig <- plot_ly(dataframe,
                 x=date_in_YMD,
                 y=yaxis,
                 type = 'scatter',
                 text = X.sample_id,
                 mode='markers',
                 color=colour_by,
                 colors="Set1",
                 hovertemplate = paste('Date: %{x}\n',
                                       'Interventions: %{y}\n',
                                       'Sample: %{text}\n'))
  fig <- fig %>% layout(
    xaxis = list(
      rangeselector = list(
        buttons = list(
          list(
            count = 1,
            label = "1 mo",
            step = "month",
            stepmode = "backward"),
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
          list(step = "all"))),
      
      rangeslider = list(type = "date")),
    yaxis = list(title = "Manual Interventions per 1000Mb",
                 range = c(0,outlier_lim + 50)))
  
  naming_no <- max(str_length(colour_by))
  if (naming_no == 1) {
    name_as <- 'prefix'
  } else if (naming_no == 2) {
    name_as <- 'prefix_v'
  } else if (naming_no > 2){
    name_as <- 'prefix_full'
  } else {
    name_as <- 'UNKNOWN'
  }

  file_name <- sprintf("date_project_manual_interventions_%s.html", name_as)
  full_loc <- paste(default_save_loc, file_name, sep="")
  htmlwidgets::saveWidget(as_widget(fig), full_loc, selfcontained = TRUE)
}
scatter_of_outliers <- function(dataframe, outlier_no) {
  table <- jira_data[order(jira_data$manual_interventions_normalised), ]
  table <- tail(table, outlier_no)
  
  fig <- plot_ly(table,
                 x=table$date_in_YMD,
                 y=table$manual_interventions_normalised,
                 type = 'scatter',
                 text = table$X.sample_id,
                 mode='markers',
                 color=table$prefix_full,
                 colors="Set1",
                 hovertemplate = paste('Date: %{x}\n',
                                       'Interventions: %{y}\n',
                                       'Sample: %{text}\n'))
  fig <- fig %>% layout(
    xaxis = list(
      title = 'Date of Ticket Creation',
      rangeselector = list(
        buttons = list(
          list(
            count = 1,
            label = "1 mo",
            step = "month",
            stepmode = "backward"),
          list(
            count = 3,
            label = "3 mo",
            step = "month",
            stepmode = "backward"))),
      rangeslider = list(type = "date")),
    yaxis = list(title = "Manual Interventions Per 1000Mb"))
  
  file_name <-'scatter_outlier_manual.html'
  full_loc <- paste(default_save_loc, file_name, sep="")
  htmlwidgets::saveWidget(as_widget(fig), full_loc, selfcontained = TRUE)
}
table_of_outliers <- function(dataframe, yaxis, outlier_no) {

  table <- jira_data[order(jira_data$manual_interventions_normalised), ]
  table <- tail(table, outlier_no)

  columns <- c("X.sample_id", "latin_name", "length.after", "manual_interventions", "manual_interventions_normalised")
  table <- table[columns]
  
  fig <- plot_ly(
    type = 'table',
    columnwidth = 200,
    header = list(
      values = c("<b>Outlier row no.</b>", names(table)),
      align = c('left', rep('center', ncol(table))),
      line = list(width = 1, color = 'black'),
      fill = list(color = 'rgb(235, 100, 230)'),
      font = list(family = "Arial", size = 14, color = "white")
    ),
    cells = list(
      values = rbind(
        rownames(table), 
        t(as.matrix(unname(table)))
      ),
      align = c('left', rep('center', ncol(table))),
      line = list(color = "black", width = 1),
      fill = list(color = c('rgb(235, 193, 238)', 'rgba(228, 222, 249, 0.65)')),
      font = list(family = "Arial", size = 12, color = c("black")),
      height = 25
    ))

  file_name <-'outlier_table_manual.html'
  full_loc <- paste(default_save_loc, file_name, sep="")
  htmlwidgets::saveWidget(as_widget(fig), full_loc, selfcontained = TRUE)
} # Outliers from the date graph

# Supplementary box plot for index.html fig1
box_plot <- function(dataframe) {
  
  fig <- plot_ly(data=jira_data, y=length.change, color = prefix, type="box")
  
  file_name <- 'boxplot_all_length_change.html'
  full_loc <- paste(default_save_loc, file_name, sep="")
  htmlwidgets::saveWidget(as_widget(fig), full_loc, selfcontained = TRUE)
}

# Generalized graphs for all data
change_by_length_all_bar <- function(dataframe) {
  dataframe <- jira_data
  bar <- plot_ly(data=dataframe,
                 x=X.sample_id,
                 y=length.change,
                 color=prefix_full,
                 type="bar",
                 text = X.sample_id,
                 hovertemplate = paste('Sample: %{x}\n',
                                       'Length Change: %{y}%\n'),
                 showlegend=TRUE)
  bar <- bar %>% layout(xaxis = list(title = 'Sample ID'),
                        yaxis = list(title = "Change in Genome length (%)"))
  
  file_name <- 'change_by_length_all_bar.html'
  full_loc <- paste(default_save_loc, file_name, sep="")
  htmlwidgets::saveWidget(as_widget(bar), full_loc, selfcontained = TRUE)
}
change_by_length_all_scatter <- function(dataframe) {
  scatter <- plot_ly(dataframe,
                 x=X.sample_id,
                 y=length.change,
                 type = "scatter",
                 mode = "markers",
                 color=prefix_full,
                 text = X.sample_id,
                 hovertemplate = paste('Sample: %{x}\n',
                                       'Length Change: %{y}%\n'),
                 showlegend=TRUE)
  
  scatter <- scatter %>% layout(xaxis = list(title = 'Sample ID'),
                        yaxis = list(title = "Change in Genome length (%)", range = c(-20, 3)))

  file_name <- 'change_by_length_all_scatter.html'
  full_loc <- paste(default_save_loc, file_name, sep="")
  htmlwidgets::saveWidget(as_widget(scatter), full_loc, selfcontained = TRUE)
}

fig3a <- function(dataframe) {
  amp <- jira_data[ which(jira_data$prefix_v=='a' & jira_data$manual_interventions_normalised<1500), ]
  bird <- jira_data[ which(jira_data$prefix_v=='b' & jira_data$manual_interventions_normalised<1500), ]
  non_v_plant <- jira_data[ which(jira_data$prefix_v=='c' & jira_data$manual_interventions_normalised<1500), ]
  dicot <- jira_data[ which(jira_data$prefix_v=='d' & jira_data$manual_interventions_normalised<1500), ]
  echino <- jira_data[ which(jira_data$prefix_v=='e' & jira_data$manual_interventions_normalised<1500), ]
  fish <- jira_data[ which(jira_data$prefix_v=='f' & jira_data$manual_interventions_normalised<1500), ]
  fungi <- jira_data[ which(jira_data$prefix_v=='g' & jira_data$manual_interventions_normalised<1500), ]
  platy <- jira_data[ which(jira_data$prefix_v=='h' & jira_data$manual_interventions_normalised<1500), ]
  insect <- jira_data[ which(jira_data$prefix_v=='i' & jira_data$manual_interventions_normalised<1500), ]
  jelly <- jira_data[ which(jira_data$prefix_v=='j' & jira_data$manual_interventions_normalised<1500), ]
  o_cho <- jira_data[ which(jira_data$prefix_v=='k' & jira_data$manual_interventions_normalised<1500), ]
  mono <- jira_data[ which(jira_data$prefix_v=='l' & jira_data$manual_interventions_normalised<1500), ]
  mammal <- jira_data[ which(jira_data$prefix_v=='m' & jira_data$manual_interventions_normalised<1500), ]
  nema <- jira_data[ which(jira_data$prefix_v=='n' & jira_data$manual_interventions_normalised<1500), ]
  sponge <- jira_data[ which(jira_data$prefix_v=='o' & jira_data$manual_interventions_normalised<1500), ]
  prot <- jira_data[ which(jira_data$prefix_v=='p' & jira_data$manual_interventions_normalised<1500), ]
  o_arthro <- jira_data[ which(jira_data$prefix_v=='q' & jira_data$manual_interventions_normalised<1500), ]
  rep <- jira_data[ which(jira_data$prefix_v=='r' & jira_data$manual_interventions_normalised<1500), ]
  shark <- jira_data[ which(jira_data$prefix_v=='s' & jira_data$manual_interventions_normalised<1500), ]
  o_ani <- jira_data[ which(jira_data$prefix_v=='t' & jira_data$manual_interventions_normalised<1500), ]
  algae <- jira_data[ which(jira_data$prefix_v=='u' & jira_data$manual_interventions_normalised<1500), ]
  o_vas_plant <- jira_data[ which(jira_data$prefix_v=='v' & jira_data$manual_interventions_normalised<1500), ]
  anne <- jira_data[ which(jira_data$prefix_v=='w' & jira_data$manual_interventions_normalised<1500), ]
  moll <- jira_data[ which(jira_data$prefix_v=='x' & jira_data$manual_interventions_normalised<1500), ]
  bact <- jira_data[ which(jira_data$prefix_v=='y' & jira_data$manual_interventions_normalised<1500), ]
  archae <- jira_data[ which(jira_data$prefix_v=='z' & jira_data$manual_interventions_normalised<1500), ]
  leps <- jira_data[ which(jira_data$prefix_v=='il' & jira_data$manual_interventions_normalised<1500), ]
  dipt <- jira_data[ which(jira_data$prefix_v=='id' & jira_data$manual_interventions_normalised<1500), ]
  hymen <- jira_data[ which(jira_data$prefix_v=='iy' & jira_data$manual_interventions_normalised<1500), ]
  inlier <- subset(jira_data, manual_interventions_normalised<1500)
  outlier <- subset(jira_data, manual_interventions_normalised>1500)
  
  fig1 <- plot_ly() %>%
    add_trace(data = outlier, x = outlier$length_in_mb, y = 1450,
              name = 'Outlier', type="scatter", mode="markers",
              marker = list(color = 'black', size = 6), showlegend=TRUE) %>%
    add_trace(data = amp, x = amp$length_in_mb, y = amp$manual_interventions_normalised,
              name = 'Amphibia', type="scatter", mode="markers",
              marker = list(color = '#FFC000', size = 6), showlegend=TRUE) %>%
    add_trace(data = bird, x = bird$length_in_mb, y = bird$manual_interventions_normalised,
              name = 'Bird', type="scatter", mode="markers",
              marker = list(color = '#FFFC00', size = 6), showlegend=TRUE) %>%
    add_trace(data = non_v_plant, x = non_v_plant$length_in_mb, y = non_v_plant$manual_interventions_normalised,
              name = 'Non-Vascular Plants', type="scatter", mode="markers",
              marker = list(color = '#FF0000', size = 6), showlegend=TRUE) %>%
    add_trace(data = dicot, x = dicot$length_in_mb, y = dicot$manual_interventions_normalised,
              name = 'Dicotyledons', type="scatter", mode="markers",
              marker = list(color = 'red', size = 6), showlegend=TRUE) %>%
    add_trace(data = echino, x = echino$length_in_mb, y = echino$manual_interventions_normalised,
              name = 'Echinoderms', type="scatter", mode="markers",
              marker = list(color = '#28b077', size = 6), showlegend=TRUE) %>%
    add_trace(data = fish, x = fish$length_in_mb, y = fish$manual_interventions_normalised,
              name = 'Fish', type="scatter", mode="markers",
              marker = list(color = 'blue', size = 6), showlegend=TRUE) %>%
    add_trace(data = fungi, x = fungi$length_in_mb, y = fungi$manual_interventions_normalised,
              name = 'Fungi', type="scatter", mode="markers",
              marker = list(color = '#e642f5', size = 6), showlegend=TRUE) %>%
    add_trace(data = platy, x = platy$length_in_mb, y = platy$manual_interventions_normalised,
              name = 'Platyhelminths', type="scatter", mode="markers",
              marker = list(color = '#0032e5', size = 6), showlegend=TRUE) %>%
    add_trace(data = insect, x = insect$length_in_mb, y = insect$manual_interventions_normalised,
              name = 'Insect', type="scatter", mode="markers",
              marker = list(color = '#b700ff', size = 6), showlegend=TRUE) %>%
    add_trace(data = jelly, x = jelly$length_in_mb, y = jelly$manual_interventions_normalised,
              name = 'Jellyfish and Cnidaria', type="scatter", mode="markers",
              marker = list(color = '#e500d0', size = 6), showlegend=TRUE) %>%
    add_trace(data = o_cho, x = o_cho$length_in_mb, y = o_cho$manual_interventions_normalised,
              name = 'Other Chordates', type="scatter", mode="markers",
              marker = list(color = '#0687ba', size = 6), showlegend=TRUE) %>%
    add_trace(data = mono, x = mono$length_in_mb, y = mono$manual_interventions_normalised,
              name = 'Monocotyledons', type="scatter", mode="markers",
              marker = list(color = '#e57f00', size = 6), showlegend=TRUE) %>%
    add_trace(data = mammal, x = mammal$length_in_mb, y = mammal$manual_interventions_normalised,
              name = 'Mammals', type="scatter", mode="markers",
              marker = list(color = '#ff00ea', size = 6), showlegend=TRUE) %>%
    add_trace(data = nema, x = nema$length_in_mb, y = nema$manual_interventions_normalised,
              name = 'Nematodes', type="scatter", mode="markers",
              marker = list(color = '#00e5e0', size = 6), showlegend=TRUE) %>%
    add_trace(data = sponge, x = sponge$length_in_mb, y = sponge$manual_interventions_normalised,
              name = 'Sponges', type="scatter", mode="markers",
              marker = list(color = '#e500dc', size = 6), showlegend=TRUE) %>%
    add_trace(data = prot, x = prot$length_in_mb, y = prot$manual_interventions_normalised,
              name = 'Protists', type="scatter", mode="markers",
              marker = list(color = '#f54296', size = 6), showlegend=TRUE) %>%
    add_trace(data = o_arthro, x = o_arthro$length_in_mb, y = o_arthro$manual_interventions_normalised,
              name = 'Other Arthropods', type="scatter", mode="markers",
              marker = list(color = '#58027a', size = 6), showlegend=TRUE) %>%
    add_trace(data = rep, x = rep$length_in_mb, y = rep$manual_interventions_normalised,
              name = 'Reptiles', type="scatter", mode="markers",
              marker = list(color = '#08a826', size = 6), showlegend=TRUE) %>%
    add_trace(data = shark, x = shark$length_in_mb, y = shark$manual_interventions_normalised,
              name = 'Sharks', type="scatter", mode="markers",
              marker = list(color = '#2ae8e8', size = 6), showlegend=TRUE) %>%
    add_trace(data = o_ani, x = o_ani$length_in_mb, y = o_ani$manual_interventions_normalised,
              name = 'Other Animal Phyla', type="scatter", mode="markers",
              marker = list(color = '#ffd24a', size = 6), showlegend=TRUE) %>%
    add_trace(data = algae, x = algae$length_in_mb, y = algae$manual_interventions_normalised,
              name = 'Algae', type="scatter", mode="markers",
              marker = list(color = '#98d437', size = 6), showlegend=TRUE) %>%
    add_trace(data = o_vas_plant, x = o_vas_plant$length_in_mb, y = o_vas_plant$manual_interventions_normalised,
              name = 'Other Vascular Plants', type="scatter", mode="markers",
              marker = list(color = '#92ed77', size = 6), showlegend=TRUE) %>%
    add_trace(data = anne, x = anne$length_in_mb, y = anne$manual_interventions_normalised,
              name = 'Annelids', type="scatter", mode="markers",
              marker = list(color = '#b59d24', size = 6), showlegend=TRUE) %>%
    add_trace(data = moll, x = moll$length_in_mb, y = moll$manual_interventions_normalised,
              name = 'Molluscs', type="scatter", mode="markers",
              marker = list(color = '#a0a197', size = 6), showlegend=TRUE) %>%
    add_trace(data = bact, x = bact$length_in_mb, y = bact$manual_interventions_normalised,
              name = 'Bacteria', type="scatter", mode="markers",
              marker = list(color = '#ff007b', size = 6), showlegend=TRUE) %>%
    add_trace(data = archae, x = archae$length_in_mb, y = archae$manual_interventions_normalised,
              name = 'Archae', type="scatter", mode="markers",
              marker = list(color = '#2bff92', size = 6), showlegend=TRUE) %>%
    add_trace(data = leps, x = leps$length_in_mb, y = leps$manual_interventions_normalised,
              name = 'Lepidoptera', type="scatter", mode="markers",
              marker = list(color = '#4a98ff', size = 6), showlegend=TRUE) %>%
    add_trace(data = dipt, x = dipt$length_in_mb, y = dipt$manual_interventions_normalised,
              name = 'Diptera', type="scatter", mode="markers",
              marker = list(color = '#fcff4a', size = 6), showlegend=TRUE) %>%
    add_trace(data = hymen, x = hymen$length_in_mb, y = hymen$manual_interventions_normalised,
              name = 'Hymenoptera', type="scatter", mode="markers",
              marker = list(color = '#adb000', size = 6), showlegend=TRUE)
  fig1 <- fig1 %>% layout(xaxis = list(title = 'Assembly Length (Mb)'),
                          yaxis = list(title = "Manual Interventions per 1000Mb", range = c(0,1500)))

} # Not in USE
# Main graphs for display
fig3_mimic <- function(dataframe) {
  
  full_pre_colour <- c('#ce782f', '#019053', '#0a75ad', '#553f69',
                      '#e50000', '#e3e500', '#2ae500', '#00e5c8',
                      '#0032e5', '#b500e5', '#e500d0', '#00a6e5',
                      '#e57f00', '#aae500', '#00e5e0', '#e500dc')

  fig1 <- plot_ly(data = jira_data,
                  x=length_in_mb,
                  y=manual_interventions_normalised,
                  color = prefix_full,
                  colors = full_pre_colour,
                  type="scatter",
                  mode="markers",
                  legendgroup = prefix_full,
                  showlegend=TRUE)
  fig1 <- fig1 %>% layout(xaxis = list(title = 'Assembly Length (Mb)'),
                          yaxis = list(title = "Manual Interventions per 1000Mb",
                                       range = c(0,1500)))
  
  fig2 <- plot_ly(data = jira_data,
                  x=length_in_mb,
                  y=scaff.n50.change,
                  color = prefix_full,
                  colors = full_pre_colour,
                  type="scatter",
                  mode="markers",
                  legendgroup = prefix_full,
                  showlegend=FALSE)
  fig2 <- fig2 %>% layout(xaxis = list(title = 'Assembly Length (Mb)'),
                          yaxis = list(title = "Scaffold N50 % Change", range = c(-100,400)))
  
  fig3 <- plot_ly(data = jira_data,
                  x=length_in_mb, 
                  y=scaff_count_per,
                  color = prefix_full,
                  colors = full_pre_colour,
                  type="scatter",
                  mode="markers",
                  legendgroup = prefix_full,
                  showlegend=FALSE)
  fig3 <- fig3 %>% layout(xaxis = list(title = 'Assembly Length (Mb)'),
                          yaxis = list(title = "Scaffold Number Change (%)", range = c(-100, 20)))
  
  figure <- subplot(fig1, fig2, fig3, nrows=3, shareX=TRUE, titleY = TRUE, titleX = TRUE)
  file_name <- 'fig3_mimic.html'
  fn1 <- 'fig1s_mimic.html'
  fn2 <- 'fig2s_mimic.html'
  fn3 <- 'fig3s_mimic.html'
  full_loc <- paste(default_save_loc, file_name, sep="")
  fl1<- paste(default_save_loc, fn1, sep="")
  fl2<- paste(default_save_loc, fn2, sep="")
  fl3<- paste(default_save_loc, fn3, sep="")
  htmlwidgets::saveWidget(as_widget(figure), full_loc, selfcontained = TRUE)
  htmlwidgets::saveWidget(as_widget(fig1), fl1, selfcontained = TRUE)
  htmlwidgets::saveWidget(as_widget(fig2), fl2, selfcontained = TRUE)
  htmlwidgets::saveWidget(as_widget(fig3), fl3, selfcontained = TRUE)
}

assigned_to_chromo <- function(dataframe) {
  
  chromo <- plot_ly(data=jira_data,
                  x=length_in_mb,
                  y=assignment,
                  color = prefix_full,
                  colors = full_pre_colour,
                  type="scatter",
                  mode="markers",
                  legendgroup = prefix_full,
                  showlegend=TRUE)
  chromo <- chromo %>% layout(xaxis = list(title = 'Assembly Length (Mb)'),
                        yaxis = list(title = "Sequence Assigned to Chromosome (%)", range = c(85, 101)))

  file_name <- 'assigned_to_chromosome.html'
  full_loc <- paste(default_save_loc, file_name, sep="")
  htmlwidgets::saveWidget(as_widget(chromo), full_loc, selfcontained = TRUE)
}

project_pie <- function(dataframe) {
  colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')
  
  pie <- plot_ly(jira_data, labels=project_type,
                 values = nrow(project_type),
                 type = 'pie',
                 textinfo='label+percent',
                 marker = list(colors = colors,
                               line = list(color = '#FFFFFF', width = 1)),
                 showlegend=FALSE)
  
  file_name <- 'project_pie.html'
  full_loc <- paste(default_save_loc, file_name, sep="")
  htmlwidgets::saveWidget(as_widget(pie), full_loc, selfcontained = TRUE)
}

project_graph <- function(dataframe) {
  
  proj <- plot_ly(dataframe, x = X.sample_id, y = manual_interventions_normalised,
                 color = project_type,
                 type="scatter",
                 mode="markers"
  )
  file_name <- 'plot_by_project.html'
  full_loc <- paste(default_save_loc, file_name, sep="")
  htmlwidgets::saveWidget(as_widget(proj), full_loc, selfcontained = TRUE)
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
  
  file_name <- 'data_table_full.html'
  full_loc <- paste(default_save_loc, file_name, sep="")
  htmlwidgets::saveWidget(as_widget(fig), full_loc, selfcontained = TRUE)
}

change_by_length_normalised <- function(dataframe) {
  
  dataframe <- jira_data
  normed <- plot_ly(data = dataframe,
                    x = length.after / 1000000,
                    y = length.change,
                    type = 'scatter',
                    mode = 'markers',
                    color = prefix_full,
                    colors = 'Set1')
  normed <- normed %>% layout(xaxis = list(title = 'Length of Genome in 1000Mb'),
                    yaxis = list(title = 'Percent change in Genome Length'))
  normed
}

################################################################################

main <- function() {
  # Date graphs
  date_graphs(jira_data, manual_interventions_normalised, prefix, 3)
  date_graphs(jira_data, manual_interventions_normalised, prefix_full, 3)
  scatter_of_outliers(jira_data, 3)
  table_of_outliers(jira_data, manual_interventions_normalised, 3)
  
  # Boxplot length all
  box_plot(jira_data)
  
  # Length change
  change_by_length_all_bar(jira_data)
  change_by_length_all_scatter(jira_data)
  
  # Main graphs
  fig3_mimic(jira_data)
  assigned_to_chromo(jira_data)

  # Project graph and supplement
  project_graph(jira_data)
  project_pie(jira_data)
  
  # Full data table <- very innefficient
  data_table_int(jira_data)
}

################################################################################

main()

