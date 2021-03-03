# An R script to take jira_dump data and produce
# descriptive statistics and graphs

# Written by dp24
# Updated January 2020

# Modules Required
require("ggplot2")
require("stringr")
require('dplyr')
require('gridExtra')
library("tidyr")
require("plotly")

# File Handling
filename <- args[1]

setwd("../scripts") # Change to save_loc in future


date <- format(Sys.Date(), "%d%m%y")
jira_data_file <- sprintf("jira_dump_170221.tsv.sorted", date) # jira_dump_%s.tsv.sorted
jira_data <- read.csv(jira_data_file, sep='\t', header=T, row.names=NULL)
attach(jira_data)

jira_data$prefix <- str_extract(X.sample_id, '[[:lower:]]+') # pulls first letters for use as categorisers
jira_data$length.change <- as.numeric(as.character(length.change)) # Stop gap measure
jira_data$normalized_by_len <- ((length.after - min(length.after)) / (max(length.after) - min(length.after))) * 1000000
jira_data$test <- (manual_interventions/length.before) * 1000000000
jira_data$mb_len <- length.before/1000000 # Equivilent to length in Gb * 1000 for length in Mb
attach(jira_data)

str(jira_data)
jira_data$date_in_YMD <- as.Date(jira_data$date_in_YMD, "%Y-%m-%d")
max(jira_data$date_in_YMD)
str(jira_data)

ggplot(jira_data, aes(date_in_YMD, nrow(jira_data[, "date_in_YMD"]))) +
  geom_point()

library(tidyverse)



il_data <- jira_data %>% filter(prefix == 'il')
il_data$normalized_mi <- (il_data$manual_interventions/il_data$length.after)*1000000

ggplot(il_data) + geom_point(mapping = aes(date_in_YMD, normalized_mi, labels = X.sample_id, colour = prefix)) + geom_smooth(aes(date_in_YMD, normalized_mi, colour = 'Trend'), method = "lm")
ggplotly()



nrow(jira_data)
names(jira_data)

# Dict which holds all prefixs
master_dict = list('Amphibian' = 'a',
                   'Bird' = 'b',
                   'Dicots' = 'd',
                   'Eudicots' = 'e',
                   'Fish' = 'f',
                   'Insects' = 'i',
                   'Diptera' = 'id',
                   'Lepardoptera' = 'il',
                   'k' = 'k',
                   'Mammal' = 'm',
                   'q' = 'q',
                   'Reptile' = 'r',
                   'Shark' = 's',
                   'x' = 'x')

date <- format(Sys.Date(), "%d%m%y")
Jira_dump_file <- sprintf("./jira_dump_%s.tsv.sorted", date)

main <- function(Jira_dump_file) {
  # File Handling
  setwd("../output") # Change to save_loc in future? That would be for none autonomous usecase
  getwd()

  # Get data - May change to Jira pull in future

  jira_data <- read.csv(Jira_dump_file, sep='\t', header=T, row.names=NULL)
  attach(jira_data)

  # Pull prefix for use downstream
  jira_data$prefix <- str_extract(jira_data$X.sample_id, '[[:lower:]]+') # pulls first letters for use as categorisers
  jira_data$length.change <- as.numeric(as.character(jira_data$length.change)) # Stop gap measure
  return(jira_data)
}

jira_data <- main(Jira_dump_file)

plots_length <- function(dataframe) {
  # Facet wrapped plots for change in length for all grouped by prefix
  ggplot(data=jira_data,
         aes(x=X.sample_id, y=length.change, fill=prefix)) +
    geom_bar(stat = 'identity') +
    theme_minimal() +
    theme(text = element_text(size=10),
          axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = "Project",
         y = "Percentage change in length of genome post-curation",
         title = "A graph to show the change in genome size when comparing pre and post curation") +
    facet_wrap(~prefix , scales = "free", nrow = 3, ncol = 5)
  ggplotly()
  this_plot <- plotly::ggplotly()
  htmlwidgets::saveWidget(as_widget(this_plot), 'plot_length_all.html')

}

fig <- plot_ly(jira_data, x=X.sample_id, y=length.change, type="bar")
fig

plots_length(jira_data)

box_plot <- function(dataframe) {
  ggplot(jira_data,
         aes(prefix, length.change, colour=prefix,fill = prefix))+
    geom_boxplot()+
    facet_wrap(~prefix , scales = "free")
  boxploted <- plotly::ggplotly()
  htmlwidgets::saveWidget(as_widget(boxploted), 'boxplot_all.html')
}

box_plot(jira_data)

plots_master <- function(dataframe) {
  # Shows all tickets and their change in genome length
  con_graph <-
  ggplot(data=jira_data) +
    geom_bar(aes(x=X.sample_id, y=length.change, fill=prefix),
             stat = "identity",
             position = "dodge") +
    theme_minimal()  +
    theme(text = element_text(size=10),
          axis.text.x = element_text(angle = 90,
                                     hjust = 1),
          axis.line = element_blank(),
          axis.ticks = element_blank())
  ggplotly()+
    labs(x = "Project",
         y = "Percentage change in Length of Genome",
         title = "A graph to show the change in genome size pre compared to post curation")
  master_plot <- plotly::ggplotly()
  htmlwidgets::saveWidget(as_widget(master_plot), 'master_plot_len_change.html')
}

plots_master(jira_data)

mean_change_all <- function(datafram) {
  # Plots the average genome length change for all groups
  dfs <- data.frame(x = character(), y = numeric(), stringsAsFactors = FALSE)
  for(i in master_dict) {
    mean <- jira_data[ which(jira_data$prefix==i), ]
    change <- sum(mean$length.change / nrow(mean))

    dfs[i,] <- list(i, change)
  }
  ggplot(dfs,
         aes(x, y, fill=x)) +
    geom_bar(stat='identity') +
    theme_minimal() +
    theme(axis.line.x = element_blank()) +
    labs(x = "Project-average",
         y = "Percentage change in length of genome post-curation",
         title = "A graph to show the average change in genome size grouped by prefix")

  this_plot <- plotly::ggplotly()
  htmlwidgets::saveWidget(as_widget(this_plot), 'mean_change_all.html')
}

mean_change_all(jira_data)

scatter_change_by_length_all <- function(dataframe) {
  ggplot(data = dataframe,
       aes(x=((length.after - min(length.after)) / (max(length.after) - min(length.after))), y=length.change, colour=prefix, label = X.sample_id)) +
  geom_point() +
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle = 90, hjust = 1))
  ggplotly()

}

scatter_change_by_length_all(jira_data)

jira_data$normalized_by_len <- ((length.after - min(length.after)) / (max(length.after) - min(length.after)))
print(tail(jira_data))
print(normalized_by_len)

jira_data$test <- (manual_interventions/length.before) * 1000000000
jira_data$mb_len <- length.before/1000000




ggplotly(ggplot(data = jira_data,
                  aes(x=mb_len, y=test, colour=prefix, label = X.sample_id)) +
             geom_point() +
             theme(text = element_text(size=10),
                   axis.text.x = element_text(angle = 90, hjust = 1)) +
           labs(title = 'Showing number of of manual interventions at various genome lengths') +
           xlab('Assembly Length (Mb)') +
           ylab('Manual Interventions / GB')
)

ggplotly(ggplot(data = jira_data,
                  aes(x=mb_len, y=scaff.n50.change, colour=prefix, label = X.sample_id)) +
             geom_point() +
             theme(text = element_text(size=10),
                   axis.text.x = element_text(angle = 90, hjust = 1)) +
             scale_y_continuous(breaks = seq(-150, max(scaff.n50.change), by = 50)) +
             labs('Showing the increase in N50 length by assembly length') +
             xlab('Assembly Length (Mb)') +
             ylab('Change in Scaffold N50 (%)')
)


ggplotly(ggplot(data = jira_data,
                  aes(x=mb_len, y=scaff_count_per, colour=prefix, label = X.sample_id)) +
             geom_point() +
             theme(text = element_text(size=10),
                   axis.text.x = element_text(angle = 90, hjust = 1)) +
             scale_y_continuous(breaks = seq(-150, max(scaff_count_per), by = 50)) +
             labs('Showing the decrease in scaffold count by assembly length') +
             xlab('Assembly Length (Mb)') +
             ylab('Change in Scaffold number (%)')
)


options  <- list(
  `Basic Information` = c("TolID" = 'X.sample_id',
                          "GRIT Code" = 'key',
                          "Prefix" = 'prefix'),
  `Assembly Length` = c("Length Before Curation" = 'length.before',
                        "Length After Curation" = 'length.after',
                        "Percentage Length Change" = 'length.change',
                        "Normalised Length" = 'normalized_by_len',
                        "Length in 1000 Mb" = 'mb_len'),
  `Date` = c("Date" = 'date_in_YMD'),
  `Manual Interactions` = c("Total Manual Interventions" = 'manual_interventions'),
  `Scaffold Count` = c("Scaffold Count Before Curation" = 'scaff_count_before',
                       "Scaffold Count After Curation" = 'scaff_count_after',
                       "Percentage Change in Scaffold Count" = 'scaff_count_per'),
  `N50 Data` = c("Scaffold N50 Before Curation" = 'scaff.n50.before',
                 "Scaffold N50 After Curation" = 'scaff.n50.after',
                 "Percentage Change in N50" = 'scaff.n50.change'),
  `Other` = c("Chromosome Assignment (TEXT)" = 'chr.assignment',
              "Genome Assigned to Chromosome (%)" = 'assignment')
  )
