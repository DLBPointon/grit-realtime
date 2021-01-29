library(ggplot2)
library(shiny)
library(DT)
library(tidyr)
library(shinycssloaders)
library(shinythemes)
library(shinydashboard)
library(shiny)
library(shinydashboard)

setwd("../output")
getwd()
date <- format(Sys.Date(), "%d%m%y")
jira_data_file <- sprintf("./jira_dump_260121.tsv.sorted", date) # jira_dump_%s.tsv.sorted
jira_data <- read.csv(jira_data_file, sep='\t', header=T, row.names=NULL)
jira_data$prefix <- str_extract(jira_data$X.sample_id, '[[:lower:]]+') # pulls first letters for use as categorisers
jira_data$length.change <- as.numeric(as.character(jira_data$length.change)) # Stop gap measure
jira_data <- head(jira_data, -1)
attach(jira_data)
vars <- colnames(jira_data)

ui <- dashboardPage(skin='green',
  dashboardHeader(title='GRIT-realtime',
                  dropdownMenuOutput("messageMenu")
                  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Dashboard', tabName = "MainDash", icon = icon("dashboard")),
      menuItem("Tab2", tabName = "t2", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    fixedRow(
      column(3,
        infoBox("Rows of Data", nrow(jira_data)-1)
        ),
      column(3,
        infoBox("Data File in Use", jira_data_file)
      )
    ),
    tabItems(
      tabItem(tabName = "MainDash",
              h2("Main Dashboard"),
        fixedRow(
          column(12,
          title = 'Plot 1', background = 'aqua',
           box(background = "aqua",
               width = NULL,
              selectInput('xaxis',
                          'X Variable',
                          vars),
              
              selectInput('yaxis',
                          'Y Variable',
                          vars,
                          selected = vars[5])),
              
            box(background = 'aqua',
                width = NULL,
                plotlyOutput("plot1")))),
        fixedRow(
          column(6,
                 title = 'Plot 2', background = "green",
                 box("plot2",
                     background = "green",
                     plotlyOutput("plot2", height = 800, width=1000),
                     height = 400,
                     width=600)),

          column(6,
                 title = "Plot 3", background = "magenta",
                 box("plot3",
                     background = "orange",
                     plotlyOutput("plot3", height = 800, width=1000),
                     height = 400,
                     width = 600))),
        fixedRow(
          column(6,
                 title = 'Plot 2 - controls', background = "green",
                 box(background = "green",
                     selectInput('p2xaxis',
                                 'X Variable',
                                 vars),
                     
                     selectInput('p2yaxis',
                                 'Y Variable',
                                 vars,
                                 selected = vars[5]))),
          column(6,
                 title = 'Plot 3 - controls', background = "green",
                 box(background = "orange",
                     selectInput('p3xaxis',
                                 'X Variable',
                                 vars),
                     
                     selectInput('p3yaxis',
                                 'Y Variable',
                                 vars,
                                 selected = vars[5]))))
        )
      ),
      tabItem(tabName = "t2")
    )
)


server <- function(input, output) {

  output$plot1 <- renderPlotly({
    
    
    values <- jira_data[, input$yaxis]
    range(values)
    
    ggplotly(
        ggplot(data=jira_data) +
          geom_bar(aes(x= !!as.name(input$xaxis), y=!!as.name(input$yaxis), fill=jira_data$prefix),
                   stat = "identity",
                   position = "dodge") +
          coord_cartesian(ylim=c(min(values) ,max(values))) +
          theme_minimal()  +
          theme(text = element_text(size=10),
          axis.text.x = element_text(angle = 90,
                                       hjust = 1),
          axis.line = element_blank(),
          axis.ticks = element_blank())
        )
    })
  
  output$plot2 <- renderPlotly({
    ggplot(data = dataframe,
           aes(x=((length.after - min(length.after)) / (max(length.after) - min(length.after))), y=length.change, colour=prefix, label = X.sample_id)) +
      geom_point() +
      theme(text = element_text(size=10),
            axis.text.x = element_text(angle = 90, hjust = 1))

  })
  
  output$plot2 <- renderPlotly({
    
  })
    
  output$messageMenu <- renderMenu({
      dropdownMenu(type = 'messages',
                   messageItem(
                     from = "New User",
                     message = "How do I register?",
                     icon = icon("question")
                     )
                   )
      })
}

shinyApp(ui, server)