# An R script to take jira_dump data and produce 
# descriptive statistics and graphs

# Written by dp24
# Updated January 2020

# Modules Required
require("ggplot2")
require("stringr")
require('dplyr')
require('gridExtra')
library(tidyr)
library(plotly)

# Arg Handling
args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1){
  warning("\nUSAGE: Rscript jira_data.R JIRA_DUMP_FILE SAVE_LOCATION \n")
  stop("Must use both arguments")
}

cat(paste("Arguments are", args[1], args[2], "\n"))

if (!file.exists(args[1])) {
  stop("Can't find one of the input files")
}

# File Handling
filename <- args[1]
save_loc <- args[2]

setwd("~/grit-realtime/output") # Change to save_loc in future
getwd()

# Get data - May change to Jira pull in future
jira_data <- read.csv('../output/jira_dump_250121.tsv.sorted', sep='\t', header=T, row.names=NULL)
jira_data

# Pull prefix for use downstream
attach(jira_data)
jira_data$prefix <- str_extract(jira_data$X.sample_id, '[[:lower:]]+') # pulls first letters for use as categorisers
sapply(jira_data, class)  # to check data types
jira_data$length.change <- as.numeric(as.character(jira_data$length.change)) # Stop gap measure
jira_data <- head(jira_data, -1)


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

# Could make this into a second generator for .change columns?
plots_length <- function(dataframe) {
  for (i in master_dict) {
    print(i)
    #if (nrow(current_df) > 2) {
    ggplot(data=jira_data,
          aes(x=X.sample_id, y=length.after, fill=prefix)) +
      geom_bar(stat = 'identity') +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))

   # } 
    print(graph[[i]])
    ggsave(graph[[i]],
           file=paste0("plot_", i,".png"),
           width = 44.45,
           height = 27.78,
           units = "cm",
           dpi=300)
  }
}
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
  attach(jira_data)
  
  con_graph <- ggplot(data=jira_data) +
    geom_bar(aes(x=X.sample_id, y=length.after, fill=prefix),
             stat = "identity",
             position = "dodge") +
    theme_minimal()  +
    theme(text = element_text(size=10),
          axis.text.x = element_text(angle = 90,
                                     hjust = 1),
          axis.line = element_blank(),
          axis.ticks = element_blank()) +
    labs(x = "Project",
         y = "Change in Length of Genome (bp) log10 scale",
         title = "A graph to show the change in genome size post curation")
  ggplotly()

  log_plot <- ggplot(data=jira_data) +
    geom_bar(aes(x=row.names, y=length.after, fill=prefix),
             stat = "identity",
             position = "dodge") +
    theme_minimal()  +
    theme(text = element_text(size=10),
          axis.text.x = element_text(angle = 90,
                                     hjust = 1),
          axis.line = element_blank(),
          axis.ticks = element_blank()) +
    labs(x = "Project",
         y = "Change in Length of Genome (bp)",
         title = "A graph to show the change in genome size post curation")
  ggplotly()

}

plots_master(jira_data)

master_plot_change <- function() {
  dfs <- data.frame(x = character(), y = numeric(), stringsAsFactors = FALSE)
  for(i in master_dict) {
    mean <- jira_data[ which(jira_data$prefix==i), ]
    change <- sum(mean$length.after / nrow(mean))
    print(change)
    print(i)
    dfs[i,] <- list(i, change)
  }
  ggplot(dfs,
         aes(x, y, fill=x)) +
    geom_bar(stat='identity') +
    theme_minimal() +
    theme(axis.line.x = element_blank())
  
  ggplotly()
}

master_plot_change()

jira_data$length.after <- jira_data$length.after*100
jira_data$length.after <- signif(jira_data$length.after, digits = 2)
ggplot(data=jira_data,
       aes(x=row.names, y=length.after, fill=prefix)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Project",
       y = "Change in Length of Genome (bp) log10 scale",
       title = "A graph to show the change in genome size post curation for") +
  facet_wrap(~prefix , scales = "free")
ggplotly()

ggplot(data = jira_data,
       aes(x=length.change, y=length.before, colour=prefix, fill = prefix)) +
  geom_point() +
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle = 90, hjust = 1))
ggplotly()