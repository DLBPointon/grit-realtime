library(ggplot2)
library(plotly)
library(shinythemes)
library(shiny)
library(stringr)
library(shinyWidgets)
library(shinydashboard)

box <- shinydashboard::box

date <- format(Sys.Date(), "%d%m%y")
jira_data_file <- sprintf("jira_dump_%s.tsv.sorted", date) #  line currently must be hard coded to available data sheet
jira_data <- read.csv(jira_data_file, sep='\t', header=T, row.names=NULL)
attach(jira_data)

jira_data$prefix <- str_extract(X.sample_id, '[[:lower:]]+') # pulls first letters for use as categorisers
jira_data$length.change <- as.numeric(as.character(length.change)) # Stop gap measure
jira_data$normalized_by_len <- ((length.after - min(length.after)) / (max(length.after) - min(length.after))) * 1000000
jira_data$test <- (manual_interventions/length.before) * 1000000000
jira_data$mb_len <- length.before/1000000 # Equivilent to length in Gb * 1000 for length in Mb

vars <- colnames(jira_data)

ui <- dashboardPage(skin='green',
  dashboardHeader(title='GRIT-realtime',
                  dropdownMenuOutput("messageMenu") # TO hopefully be used in the future 
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
        infoBox("Rows of Data", nrow(jira_data), width = NULL)
        ),
      column(3,
        infoBox("Data File in Use", jira_data_file, width = NULL)
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
              plotlyOutput("plot1")
              )
          )
          ),
                      
        fixedRow(
          column(4,
                 title = 'Plot 2 - controls',
                 box('Plot 2 - controls',
                     background = "green",
                     selectInput('p2xaxis',
                                 'X Variable',
                                 vars,
                                 selected = vars[-1]),
                     selectInput('p2yaxis',
                                 'Y Variable',
                                 vars,
                                 selected = vars[5])
                     )
                 ),

          column(4,
                 title = 'Plot 3 - controls',
                 box('Plot 3 - controls',
                     background = "orange",
                     selectInput('p3xaxis',
                                 'X Variable',
                                 vars,
                                 selected = vars[-1]),
                     
                     selectInput('p3yaxis',
                                 'Y Variable',
                                 vars,
                                 selected = vars[5])
                     )
                 ),
  
          column(4,
                 title = 'Plot 4 - controls',
                 box('Plot 4 - controls',
                     background = "yellow",
                     selectInput('p3xaxis',
                                 'X Variable',
                                 vars,
                                 selected = vars[-1]),
                     
                     selectInput('p3yaxis',
                                 'Y Variable',
                                 vars,
                                 selected = vars[5])
                     )
                 )
          ),

        fixedRow(
          column(12,
                 title = 'Plot 2', background = "green",
                 box("plot2",
                     width = NULL,
                     background = "green",
                     plotlyOutput("plot2")
                     )
                 )
          ),
        
        fixedRow(
          column(12,
                 title = "Plot 3", background = "magenta",
                 box("plot3",
                     width = NULL,
                     background = "orange",
                     plotlyOutput("plot3")
                     )
                 )
          ),

        fixedRow(
          column(12,
                 title = "Plot 4", background = "yellow",
                 box("plot4",
                     width = NULL,
                     background = "yellow",
                     plotlyOutput("plot4")
                     )
                 )
        ),
        tabItem(tabName = "t2")
        )
      )
  )
)


server <- function(input, output) {

  output$plot1 <- renderPlotly({

    values <- jira_data[, input$yaxis]
    range(values)
    
    ggplotly(
        ggplot(data=jira_data) +
          geom_bar(aes(x= !!as.name(input$xaxis), y=!!as.name(input$yaxis), fill=prefix), # cannot use just prefix here as it will not produce the correct graph
                   stat = "identity",
                   position = "dodge") +
          coord_cartesian(ylim=c(min(values) ,max(values))) +
          theme_minimal()  +
          theme(text = element_text(size=10),
                axis.text.x = element_text(angle = 90,
                                           hjust = 1),
                axis.line = element_blank(),
                axis.ticks = element_blank()) +
          xlab('TolID') +
          ylab('Percentage change in genome lenth')
        )
    })
  
  output$plot2 <- renderPlotly({
    print(input$xaxis2)
    print(input$yaxis2)
    ggplotly(ggplot(data = jira_data,
                    aes(x=mb_len, y=test, colour=prefix, label = X.sample_id)) +
               geom_point() +
               theme(text = element_text(size=10),
                     axis.text.x = element_text(angle = 90, hjust = 1)) +
               xlab('Assembly Length (Mb)') +
               ylab('Manual Interventions / GB')
      )

  })
  
  output$plot3 <- renderPlotly({
    ggplotly(ggplot(data = jira_data,
                    aes(x=mb_len, y=scaff.n50.change, colour=prefix, label = X.sample_id)) +
               geom_point() +
               theme(text = element_text(size=10),
                     axis.text.x = element_text(angle = 90, hjust = 1)) + 
               scale_y_continuous(breaks = seq(-150, max(scaff.n50.change), by = 50)) +
               xlab('Assembly Length (Mb)') +
               ylab('Change in Scaffold N50 (%)')
             )
  })
  
  output$plot4 <- renderPlotly({
    ggplotly(ggplot(data = jira_data,
                    aes(x=mb_len, y=scaff_count_per, colour=prefix, label = X.sample_id)) +
               geom_point() +
               theme(text = element_text(size=10),
                     axis.text.x = element_text(angle = 90, hjust = 1)) + 
               scale_y_continuous(breaks = seq(-150, max(scaff_count_per), by = 50)) +
               xlab('Assembly Length (Mb)') +
               ylab('Change in Scaffold number (%)')
             )
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