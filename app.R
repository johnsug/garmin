
## app.R ##
library(shiny)
library(shinydashboard)
library(markdown)
library(plotly)
library(dygraphs)
library(leaflet)

## user interface #####
ui <- dashboardPage(skin="green", 
  dashboardHeader(title="Garmin Demo"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home",                 tabName="home",          icon=icon("home")), 
      menuItem("Efficient Frontier",   tabName="dea",           icon=icon("signal")), 
      menuItem("Changepoint Analysis", tabName="changepoint",   icon=icon("exchange")), 
      menuItem("Spacial Analysis",     tabName="spacial",       icon=icon("map-o")),
      menuItem("Trends",               tabName="trends",        icon=icon("line-chart")), 
      menuItem("More",                 tabName="more",          icon=icon("th"))
      )
    ),
  dashboardBody(
    tabItems(
      tabItem(tabName="home", includeMarkdown("home.md")),
      tabItem(tabName="dea", 
              fluidRow(box(width=12, collapsible=T, collapsed=T, title="Efficient Frontier", includeMarkdown("dea.md"))),
              fluidRow(plotlyOutput("dea_plot"))),
      tabItem(tabName="changepoint",
              fluidRow(box(
                width=12,
                selectInput("changepoint_list_1", label="Select Activity", choices=paste(dat$Activity.Name, dat$Start), selected=1),
                dygraphOutput("changepoint_plot_1")
                )),
              fluidRow(box(
                width=12,
                selectInput("changepoint_list_2", label="Select Activity", choices=paste(dat$Activity.Name, dat$Start), 
                            selected=paste(dat$Activity.Name, dat$Start)[2]),
                dygraphOutput("changepoint_plot_2")
                ))
              ),
      tabItem(tabName="spacial"),
      tabItem(tabName="trends"),
      tabItem(tabName="more")
    )
  )
)

## server #####
server <- function(input, output) { 
  
  ## load packages
  library(sqldf)         ## data munging
  library(plotly)        ## data viz
  library(ggplot2)       ## data visualization
  library(lubridate)     ## manipulating dates data
  library(changepoint)   ## changepoint
  library(Benchmarking)  ## efficient frontier analysis
  
  ## import forerunner data
  d <- read.csv("forerunner_log.csv", stringsAsFactors=F)
  d <- d[!is.na(d$Distance),]
  d$Pace <- minute(as.POSIXct(strptime(d$Avg.Speed.Avg.Pace, "%M:%S"))) + second(as.POSIXct(strptime(d$Avg.Speed.Avg.Pace, "%M:%S"))) / 60
  d$Mins <- d$Pace * d$Distance
  d$Date <- as.POSIXct(strptime(substr(d$Start, 6, 100), format="%b %d, %Y"))
  d$Avg_MPH <- d$Distance / (d$Mins/60)

  ## DEA analysis #################################################################################
  
  ## build efficient frontier
  dat <- subset(d, Activity.Type=="Running")
  e <- dea(dat$Distance, dat$Avg_MPH)
  dat$Efficiency <- e$eff
  frontier <- data.frame(x=dat$Distance[e$eff==1], y=dat$Avg_MPH[e$eff==1])
  frontier <- frontier[order(frontier$x),]
  frontier <- frontier[!is.na(frontier$x),]
  frontier <- rbind(c(0,0), frontier, c(floor(max(frontier$x)+3),max(frontier$y)))
  
  ## DEA plot
  output$dea_plot <- renderPlotly({
    p <- plot_ly(dat, x=Distance, y=Avg_MPH, name="Activity", hoverinfo="text", 
                 text=paste0(Start, 
                            "<br>Average Pace: ", Avg.Speed.Avg.Pace., 
                            "<br>Distance: ", Distance, 
                            "<br>Efficiency score: ", 
                            round(Efficiency,2)), 
                 mode="markers")
    add_trace(p, x=frontier$x, y=frontier$y, name="Frontier", mode="lines", opacity=.3)
  })
  
  
  ## changepoint analysis #########################################################################
  
  activity_1 <- reactive({dat[paste(dat$Activity.Name, dat$Start)==input$changepoint_list_1,] })
  activity_2 <- reactive({dat[paste(dat$Activity.Name, dat$Start)==input$changepoint_list_2,] })
  full <- read.csv("detailed_forerunner_log.csv", stringsAsFactors=F)
  
  ### steps
  ### 0. need to impute full.csv
  ### 1. need a mapping for dat.csv to full.csv
  ### 2. changepoint analysis
  ### 3. changepoint plots
  
  
  output$changepoint_plot_1 <- renderDygraph({
    activity <- activity_1()
    
    # dygraph(d1, xlab="Time in Hours", ylab="Running Speed (MPH)") %>% 
    #   dyShading(from=time(rr1)[1], to=time(rr1)[90], color="#CCEBD6") %>% 
    #   dyShading(from=time(rr1)[795], to=time(rr1)[1035], color="#FFE6E6") %>%
    #   dyOptions(colors=c("#66C2A5", "red"))
    # 
  })
  
  output$changepoint_plot_2 <- renderDygraph({
    activity <- activity_2()
  })
  
  
  ## trend analysis ###############################################################################
  
  
  
  ## miscellaneous plots ##########################################################################
  
  
  }

## launch app #####
shinyApp(ui, server)

