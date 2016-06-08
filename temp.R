
library(shiny)
library(shinydashboard)

## http://www.r-bloggers.com/change-point-detection-in-time-series-with-r-and-tableau/
library(cpm) # Parametric and Nonparametric Sequential Change Detection in R
library(bcp) # An R Package for Performing a Bayesian Analysis of Change Point Problems
library(ecp) #An R Package for Nonparametric Multiple Change Point Analysis of Multivariate Data

ui <- dashboardPage(
  dashboardHeader(disable=T),
  dashboardSidebar(disable=T),
  dashboardBody(
    h1("Garmin Connect Innovations"), 
    h4("Suggestions by John Sugden"), br(), 
    box(title="Efficient Frontier", status="primary", solidHeader=T, collapsible=T, 
        plotOutput("eda_plot"), 
        radioButtons("eda_months", label="Months", choices=c(1, 3, 6, 12), inline=T)), 
    box(title="Trends", status="primary", solidHeader=T, collapsible=T, 
        selectInput("trend_type", label=NULL, choices=c("Pace", "V02 Max")), 
        plotOutput("trend_plot"), 
        radioButtons("trend_months", label="Months", choices=c(1, 3, 6, 12), inline=T)), 
    box(title="Change Point Analysis", status="primary", solidHeader=T, collapsible=T, width=12, 
        selectInput("individual_run", label="Select run:", choices=1:10), 
        plotOutput("changepoint_plot")), 
    box(title="Twelve Month Activity", status="primary", solidHeader=T, collapsible=F, width=12, 
        plotOutput("activity_plot"))))

server <- shinyServer(function(input, output) {
  ## libraries ####################################################################################
  library(Benchmarking)   ## efficient frontier
  library(changepoint)    ## change point analysis
  library(lubridate)      ## data transformation
  library(ggplot2)        ## data viz
  library(sqldf)          ## data munging
  
  
  ## get data #####################################################################################
  d <- read.csv("forerunner_log.csv", stringsAsFactors=F)
  d$Pace <- minute(as.POSIXct(strptime(d$Avg.Speed.Avg.Pace, "%M:%S"))) + second(as.POSIXct(strptime(d$Avg.Speed.Avg.Pace, "%M:%S"))) / 60
  d$Mins <- d$Pace * d$Distance
  d$Date <- as.POSIXct(strptime(substr(d$Start, 6, 100), format="%b %d, %Y"))


  ## DEA ##########################################################################################
  
  ## query data
  eda_dat <- reactive({
    dat <- d
    date_floor <- as.POSIXct(today() - months(input$eda_months))
    dat <- subset(dat, Date > date_floor)
    dat
  })
  
  frontier <- reactive({
    dat <- eda_dat()
    e <- dea(dat$Mins/60, dat$Distance)
    frontier <- data.frame(x=dat$Mins[e$eff==1], y=dat$Distance[e$eff==1])
    frontier <- sqldf("select * from frontier order by 2")
    frontier <- rbind(c(min(frontier$x),0), frontier, c(300,max(frontier$y)))
    frontier
  })
  
  ## build efficient frontier
  
  
  ## DEA plot
  output$eda_plot <- renderPlot({
    g <- ggplot(frontier(), aes(x=x, y=y)) + 
      geom_line() + 
      xlim(c(-5, max(head(frontier$x,-1)))) + 
      geom_point(dat=eda_dat(), aes(x=dat$Mins, y=dat$Distance), col=5) + 
      labs(x="Time (Minutes)", y="Distance (Miles)")
    plot(g)
  })
  
  
  ## Trends #######################################################################################
  ## perhaps allow options (radios?): 1 month, 3 months, 6 months, 12 months
  
  ## query trend data
  trend_dat <- reactive({
    dat <- d
    date_floor <- as.POSIXct(today() - months(input$trend_months))
    dat <- subset(dat, Date > date_floor)
    dat
  })
  
  ## trend plot
  output$trend_plot <- renderPlot({
    g <- ggplot(trend_dat(), aes(x=Date, y=Pace)) + geom_line() + stat_smooth(fill="steelblue1", method="glm")
    plot(g)
  })
  
  
  ## Changepoint Analysis #########################################################################
  output$changepoint_plot <- renderPlot({
    
    ## need a data frame with all runs combined
    ## then, need to subset data frame, based on selected run
    ## then, display changepoint
  
  ## Twelve Month Activity ########################################################################
  start_date <- mdy(paste0("0", month(today()), "01", year(max_date)-1))
  
  # init vector
  n <- as.numeric(ceiling(as.POSIXct(today()) - start_date))
  annual <- data.frame(date=rep(as.POSIXct(start_date, tz="CST"),n))
  annual$week[1] <- 1
  annual$day[1] <- wday(annual$date[1])
  
  # add days of week and weeks
  for(i in 2:n){
    annual$date[i] <- annual$date[i-1] + 24*60*60
    annual$day[i] <- annual$day[i-1] + 1
    if(annual$day[i]==8) {annual$day[i] <- 1}
    annual$week[i] <- annual$week[i-1]
    if(annual$day[i]==1) {annual$week[i] <- annual$week[i-1]+1}
  }
  
  # join activity
  a <- sqldf("select a.*, d.distance from annual as a left join d on a.date = d.date")
  head(annual, 20)
  head(a, 20)
  annual$date[1]
  d$Date[1]
  
  output$activity_plot <- renderPlot({
    pdat <- sqldf("select Date, Distance from d")
    p <- ggplot(pdat, aes(x=Week, y=Weekday, fill=Pushups)) + geom_tile(colour = "white") + labs(x="", y="") + 
      theme(legend.position="none", axis.ticks=element_blank(), axis.text.x=element_blank()) + coord_fixed()
    plot(p)
    })
})

## run app
shinyApp(ui=ui, server=server)
