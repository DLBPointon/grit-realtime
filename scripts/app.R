library(ggplot2)
library(plotly)
library(shinythemes)
library(shiny)
library(stringr)
library(shinyWidgets)
library(shinydashboard)

box <- shinydashboard::box


date <- format(Sys.Date(), "%d%m%y")
jira_data_file <- sprintf("jira_dump_020221.tsv.sorted", date) #  line currently must be hard coded to available data sheet
jira_data <- read.csv(jira_data_file, sep='\t', header=T, row.names=NULL)
attach(jira_data)

jira_data$prefix <- str_extract(X.sample_id, '[[:lower:]]+') # pulls first letters for use as categorisers
jira_data$length.change <- as.numeric(as.character(length.change)) # Stop gap measure
jira_data$normalized_by_len <- ((length.after - min(length.after)) / (max(length.after) - min(length.after))) * 1000000
jira_data$test <- (manual_interventions/length.before) * 1000000000
jira_data$mb_len <- length.before/1000000 # Equivilent to length in Gb * 1000 for length in Mb
attach(jira_data)

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

dash_header <- dashboardHeader(title='GRIT-realtime',
                               dropdownMenuOutput("messageMenu") # TO hopefully be used in the future
                               )

dashboard <- dashboardSidebar(
  sidebarMenu(
    menuItem('Dashboard', tabName = "MainDash", icon = icon("dashboard")),
    menuItem("Date Dash", tabName = "DateDash", icon = icon("dashboard")),
    menuItem("GitHub Page", icon = icon("file-code-o"),
           href = "https://github.com/DLBPointon/grit-realtime")
  )
)

dash_body <- dashboardBody(
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
                        options),
            selectInput('yaxis',
                        'Y Variable',
                        options,
                        selected = options$`Assembly Length`[3])),

        box(background = 'aqua',
            width = NULL,
            plotlyOutput("plot1")
            )
        )
        ),

      fixedRow(
        column(8,
               title = 'Plot 2', background = "green",
               box("plot2",
                   width = NULL,
                   background = "green",
                   plotlyOutput("plot2")
                   )
               ),
        column(4,
               title = 'Plot 2 - controls',
               box('Plot 2 - controls',
                   background = "green",
                   selectInput('p2xaxis',
                               'X Variable',
                               options,
                               selected = options$`Assembly Length`[5]),
                   selectInput('p2yaxis',
                               'Y Variable',
                               options,
                               selected = options$`Manual Interactions`[1])
                   )
        )
        ),

      fixedRow(
        column(8,
               title = "Plot 3", background = "magenta",
               box("plot3",
                   width = NULL,
                   background = "orange",
                   plotlyOutput("plot3")
                   )
               ),

        column(4,
               title = 'Plot 3 - controls',
               box('Plot 3 - controls',
                   background = "orange",
                   selectInput('p3xaxis',
                               'X Variable',
                               options,
                               selected = options$`Assembly Length`[5]),
                   
                   selectInput('p3yaxis',
                               'Y Variable',
                               options,
                               selected = options$`Scaffold Count`[3])
               )
        )
        ),

      fixedRow(
        column(8,
               title = "Plot 4", background = "yellow",
               box("plot4",
                   width = NULL,
                   background = "fuchsia",
                   plotlyOutput("plot4")
                   )
               ),

        column(4,
               title = 'Plot 4 - controls',
               box('Plot 4 - controls',
                   background = "fuchsia",
                   selectInput('p4xaxis',
                               'X Variable',
                               options,
                               selected = options$`Assembly Length`[5]),
                   
                   selectInput('p4yaxis',
                               'Y Variable',
                               options,
                               selected = options$`N50 Data`[3])
               )
        )
      )
    ),
    tabItem(tabName = "DateDash",
                h2("Dashboard for Date graphs")
                )
  )
)

ui <- dashboardPage(skin='green',
  dash_header,
  dashboard,
  dash_body
)

server <- function(input, output) {

  output$plot1 <- renderPlotly({
    xaxis <- get(input$xaxis)
    yaxis <- get(input$yaxis)

    ggplotly(
        ggplot(data=jira_data) +
          geom_bar(aes(x=xaxis, y=yaxis, fill=prefix), # cannot use just prefix here as it will not produce the correct graph
                   stat = "identity",
                   position = "dodge") +
          #coord_cartesian(ylim=c(signif(min(yaxis), 0), signif(max(xaxis), 0), expand=TRUE)) +
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
    xaxis2 <- get(input$p2xaxis)
    yaxis2 <- get(input$p2yaxis)

    ggplotly(ggplot(data = jira_data,
                    aes(x=xaxis2, y=yaxis2, colour=prefix, labels = X.sample_id)) +
               geom_point() +
               theme(text = element_text(size=10),
                     axis.text.x = element_text(angle = 90, hjust = 1)) +
               xlab('Assembly Length (Mb)') +
               ylab('Manual Interventions / GB')
      )

  })
  
  output$plot3 <- renderPlotly({
    xaxis3 <- get(input$p3xaxis)
    yaxis3 <- get(input$p3yaxis)

    ggplotly(ggplot(data = jira_data,
                    aes(x=xaxis3, y=yaxis3, colour=prefix, labels = X.sample_id)) +
               geom_point() +
               theme(text = element_text(size=10),
                     axis.text.x = element_text(angle = 90, hjust = 1)) + 
               xlab('Assembly Length (Mb)') +
               ylab('Change in Scaffold count (%)')
             )
  })
  
  output$plot4 <- renderPlotly({
    xaxis4 <- get(input$p4xaxis)
    yaxis4 <- get(input$p4yaxis)

    ggplotly(ggplot(data = jira_data,
                    aes(x=xaxis4, y=yaxis4, colour=prefix, labels = X.sample_id)) +
               geom_point() +
               theme(text = element_text(size=10),
                     axis.text.x = element_text(angle = 90, hjust = 1)) + 
               xlab('Assembly Length (Mb)') +
               ylab('Change in Scaffold N50 (%)')
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