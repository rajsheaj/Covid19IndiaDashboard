#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
# run seelcted code - cntl Enter
options(shiny.sanitize.errors = TRUE)

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinycssloaders)
library(rvest)
library(leaflet)
library(htmltools)
library(dplyr)
library(httr)
library(jsonlite)
library(plotly)
library(tidyr)

#webscrapping to get latest data from the Ministry of Health - India
webpage.html <- read_html("https://www.mohfw.gov.in/")

tabledata <- webpage.html %>% html_nodes(xpath = '//*[@id="state-data"]/div/div/div/div/table') %>% html_table(fill = TRUE)

statewisedata.df <- data.frame(tabledata[[1]])
statewisedata.df <- subset(statewisedata.df, select = c(1,2,3,4,5))
colnames(statewisedata.df) <- c("Sl.No", "State", "Active", "Discharged", "Deceased")
tmp <- which(startsWith(statewisedata.df$State, "West Bengal"))
statewisedata.df <- head(statewisedata.df,tmp)

statewisedata.df$Active <- trimws(gsub("[^0-9.-]", "", statewisedata.df$Active))
statewisedata.df$Discharged <- trimws(gsub("[^0-9.-]", "", statewisedata.df$Discharged))
statewisedata.df$Deceased <- trimws(gsub("[^0-9.-]", "", statewisedata.df$Deceased))
statewisedata.df$Sl.No <- trimws(gsub("[^0-9.-]", "", statewisedata.df$Sl.No))
statewisedata.df$State <- trimws(gsub("[^[:alnum:]]", " ", statewisedata.df$State))

statewisedata.df$Sl.No <- as.numeric(statewisedata.df$Sl.No)
statewisedata.df$Active <- as.numeric(statewisedata.df$Active)
statewisedata.df$Discharged <- as.numeric(statewisedata.df$Discharged)
statewisedata.df$Deceased <- as.numeric(statewisedata.df$Deceased)



confirmed.int <- sum(statewisedata.df$Active)
discharged.int <- sum(statewisedata.df$Discharged)
died.int <- sum(statewisedata.df$Deceased)

# created a csv with all latitudes and longditudes of region capitals, which I intend to use in Leaflet Map
state_lat_long.df <- read.csv("State_Capital_Coordinate.csv", stringsAsFactors = FALSE)
colnames(state_lat_long.df)[1] <- "Region"

#merging statewisedata.df into state_lat_long.df
state_lat_long.df1 <- left_join(state_lat_long.df, statewisedata.df, by = setNames("State", "Region"))
state_lat_long.df1 <- na.omit(state_lat_long.df1)
state_lat_long.df1$Label <- paste0(state_lat_long.df1$Region, " -- Total: ", state_lat_long.df1$Confirmed,
                                   " Discharged: ", state_lat_long.df1$Discharged,
                                   " Died: ", state_lat_long.df1$Deceased)

# Getting data from https://api.covid19india.org/

response <- GET("https://api.covid19india.org/data.json")
jsonRespText <- content(response,as="text")
df1 <- fromJSON(jsonRespText)
cases_time_series.df <- df1$cases_time_series
cases_time_series.df$dailyconfirmed <- as.numeric(cases_time_series.df$dailyconfirmed)
cases_time_series.df$dailydeceased <- as.numeric(cases_time_series.df$dailydeceased)
cases_time_series.df$dailyrecovered <- as.numeric(cases_time_series.df$dailyrecovered)
cases_time_series.df$totalconfirmed <- as.numeric(cases_time_series.df$totalconfirmed)
cases_time_series.df$totaldeceased <- as.numeric(cases_time_series.df$totaldeceased)
cases_time_series.df$totalrecovered <- as.numeric(cases_time_series.df$totalrecovered)

testing_rate.df <- df1$tested
testing_rate.df <- subset(testing_rate.df, select = c('totalsamplestested', 'updatetimestamp'))
testing_rate.df$totalsamplestested <- as.numeric(testing_rate.df$totalsamplestested)
testing_rate.df$updatetimestamp <- substr(testing_rate.df$updatetimestamp, 1, 10)
testing_rate.df$totalsamplestested[testing_rate.df$totalsamplestested == ""] <- NA
testing_rate.df <- na.omit(testing_rate.df)

#getting data from second API set
resp1<-GET("https://api.covid19india.org/raw_data1.json")
jsonRespText <- content(resp1,as="text")
rawdata.df1 <- fromJSON(jsonRespText)
rawdata.df1 <- rawdata.df1$raw_data
rawdata.df1 <- subset(rawdata.df1, select = c('agebracket', 'currentstatus'))
rawdata.df1$agebracket <- as.numeric(rawdata.df1$agebracket)
rawdata.df1 <- na.omit(rawdata.df1)

resp2<-GET("https://api.covid19india.org/raw_data2.json")
jsonRespText <- content(resp2,as="text")
rawdata.df2 <- fromJSON(jsonRespText)
rawdata.df2 <- rawdata.df2$raw_data
rawdata.df2 <- subset(rawdata.df2, select = c('agebracket', 'currentstatus'))
rawdata.df2$agebracket <- as.numeric(rawdata.df2$agebracket)
rawdata.df2 <- na.omit(rawdata.df2)

resp3<-GET("https://api.covid19india.org/raw_data3.json")
jsonRespText <- content(resp3,as="text")
rawdata.df3 <- fromJSON(jsonRespText)
rawdata.df3 <- rawdata.df3$raw_data
rawdata.df3 <- subset(rawdata.df3, select = c('agebracket', 'currentstatus'))
rawdata.df3$agebracket <- as.numeric(rawdata.df3$agebracket)
rawdata.df3 <- na.omit(rawdata.df3)

resp4<-GET("https://api.covid19india.org/raw_data4.json")
jsonRespText <- content(resp4,as="text")
rawdata.df4 <- fromJSON(jsonRespText)
rawdata.df4 <- rawdata.df4$raw_data
rawdata.df4 <- subset(rawdata.df4, select = c('agebracket', 'currentstatus'))
rawdata.df4$agebracket <- as.numeric(rawdata.df4$agebracket)
rawdata.df4 <- na.omit(rawdata.df4)

resp5<-GET("https://api.covid19india.org/raw_data5.json")
jsonRespText <- content(resp5,as="text")
rawdata.df5 <- fromJSON(jsonRespText)
rawdata.df5 <- rawdata.df5$raw_data
rawdata.df5 <- subset(rawdata.df5, select = c('agebracket', 'currentstatus'))
rawdata.df5$agebracket <- as.numeric(rawdata.df5$agebracket)
rawdata.df5 <- na.omit(rawdata.df5)

combined.df <- rbind(rawdata.df1, rawdata.df2, rawdata.df3, rawdata.df4, rawdata.df5)
deceased.df <- subset(combined.df, combined.df$currentstatus == "Deceased")
recovered.df <- subset(combined.df, combined.df$currentstatus == "Recovered")

#getting data from third API
respo<-GET("https://api.covid19india.org/v2/state_district_wise.json")
jsonRespText <- content(respo,as="text")
statedistrict.df <- fromJSON(jsonRespText)
states.list <- statedistrict.df$state

# getting data from fourth API
respo <- GET("https://api.covid19india.org/states_daily.json")
jsonRespText <- content(respo,as="text")
statedaily.df <- fromJSON(jsonRespText)
statedaily.df <- statedaily.df$states_daily


# Define the Header
shiny_header <- dashboardHeaderPlus(title = "COVID-19 India Dashboard")

# Define the sidebar
shiny_sidebar <- dashboardSidebar(
  sidebarUserPanel(
    "Welcome Visitor",
    Sys.time()
  ),
  sidebarMenu(id = "left_sidebar",
              menuItem("DashBoard", tabName = "ID_DashBoard", icon = icon("desktop")),
              menuItem("National Level", tabName = "RabbitHole1", icon = icon("bar-chart-o")),
              menuItem("State-District Level", tabName = "RabbitHole2", icon = icon("bar-chart-o")),
              menuItem("About", icon = icon("chalkboard-teacher"), tabName = "About")
  )
)

# Define the body of the shiny app
shiny_body <- dashboardBody(
  tabItems(
    tabItem(tabName = "ID_DashBoard",
      fluidRow(
        valueBoxOutput("total_cases"),
        valueBoxOutput("discharged_cases"),
        valueBoxOutput("died_cases")
        ),
      fluidRow(
        style = "height:20px;"
      ),
      fluidRow(
        column(
          width = 6,
          fluidRow(tags$b("State Distribution Data (https://www.mohfw.gov.in/)"), align = "center", style = "height:25px;background-color: #d1f2ff;font-size: 20px;")
        ),
        column(
          width = 6,
          fluidRow(tags$b("Data by Region (https://www.mohfw.gov.in/)"), align = "center", style = "height:25px; background-color: #d1f2ff;font-size: 20px;")
        )
      ),
      fluidRow(
        column(
          width = 6,
          DT::dataTableOutput("StateDataDisplayTable") %>% withSpinner()
        ),
        column(
          width = 6,
          leafletOutput("mymap", width = "100%", height = 800) %>% withSpinner()
        )
      )
    ),
    tabItem(tabName = "RabbitHole1",
      fluidRow(
        tags$b("National Level Statistics - https://api.covid19india.org/"), align = "center", style = "height:25px;background-color: #d1f2ff;font-size: 20px;"
      ),
      fluidRow(
        style = "height:25px;"
      ),
      fluidRow(
        box(
          title = "Time Series - Daily Data", 
          status = "primary", solidHeader = TRUE,
          collapsible = TRUE,
          plotlyOutput("NL_TS_DD", height = 400) %>% withSpinner()
        ),
        box(
          title = "Time Series - Aggregate Data", 
          status = "primary", solidHeader = TRUE,
          collapsible = TRUE, 
          plotlyOutput("NL_TS_AD", height = 400) %>% withSpinner()
        )
      ),
      fluidRow(
        style = "height:25px;"
      ),
      fluidRow(
        box(
          title = "Deceased by Age (subset of confirmed available data)", 
          status = "primary", solidHeader = TRUE,
          collapsible = TRUE,
          plotOutput(outputId = "histogram_dead", height = 400) %>% withSpinner()
        ),
        box(
          title = "Recovered by Age (subset of confirmed available data)", 
          status = "primary", solidHeader = TRUE,
          collapsible = TRUE,
          plotOutput(outputId = "histogram_recovered", height = 400) %>% withSpinner()
        )
      )
    ),
    tabItem(tabName = "RabbitHole2",
      fluidRow(
        tags$b("State Level Statistics - https://api.covid19india.org/"), align = "center", style = "height:25px;background-color: #d1f2ff;font-size: 20px;"
      ),
      fluidRow(
        style = "height:25px;"
      ),
      fluidRow(
        column(
          width = 5,
          box(
            title = "Select State", width = NULL, status = "primary",
            selectInput("state_selector", "Select State", states.list, selected = "Tamil Nadu")
          ),
          box(
            title = "Time Series Data", width = NULL, status = "primary",
            plotlyOutput("SL_TS_DD", height = 400) %>% withSpinner()
          )
        ),
        column(
          width = 7,
          DT::dataTableOutput("DistrictDataDisplayTable") %>% withSpinner()
        )
      )
    
    ),
    tabItem(tabName = "About",
      fluidRow(
        box(
          title = div("About this project", style = "padding-left: 20px", class = "h2"),
          column(
            h4("This dashboard has been created to track the Covid19 cases in India.
            As a techie and an R enthusiast, it made sense to generate a dashboard
            using Shiny. This is not the official Government Webpage. For all official communication,
            please refer to government data sources."), 
            
            h4("I was able to web-scrap data from the Ministry of Health web portal and consume the 
            awesome APIs exposed by the Covid-19 India team to come up with real time statistics.
            There might some data discrepancy that you might notice, but that is because, I have tried
            to collate data from 2 different sources in the various tabs. This is not to confuse you, but
            to provide a holistic view from the integrated data sources. Again I repeat, for all official
            figures, please utilize the respective government data."),
            
            h3("Data Sources"),
            h4(tags$ul(
              tags$li(tags$b("COVID-19 India data:"), tags$a(href = "https://api.covid19india.org/", "Covid19 India")),
              tags$li(tags$b("COVID-19 India data:"), tags$a(href = "https://www.mohfw.gov.in/", "Ministry of Health and Family Welfare"))
            )),
            h3("Developer"),
            h4("Ajit Rajshekar | Senior Manager @",
            tags$a(href = "https://www.capgemini.com/", "Capgemini"), "|",
            tags$a(href = "https://in.linkedin.com/in/ajit-rajshekar-19b584110", "LinkedIn")),
            style = "padding-left: 20px; padding-right: 20px; padding-bottom: 40px; margin-top: -15px;",
            width = 12
          ),
          width = 8
        ),
        width = 12,
        style = "padding: 15px"
      )
      
    
    )
  )
)

ui <- dashboardPagePlus(
  header = shiny_header,
  sidebar = shiny_sidebar,
  body = shiny_body,
  skin = "black",
  sidebar_background = "light",
  collapse_sidebar = FALSE
)



# Where the "grey matter" resides!
server <- function(input, output) {
  
  # Dashboard valuebox
  output$total_cases <- renderValueBox({
    valueBox(confirmed.int, "Active Cases", icon = icon("align-justify"),color = "aqua")
  })
   
  output$discharged_cases <- renderValueBox({
    valueBox(discharged.int, "Discharged", icon = icon("align-justify"),color = "lime")
  })
  
  output$died_cases <- renderValueBox({
    valueBox(died.int, "Deceased", icon = icon("align-justify"),color = "orange")
  })
  
  output$StateDataDisplayTable <- DT::renderDataTable({
    DT::datatable(statewisedata.df, colnames = c('Sl No', 'State', 'Active Cases', 'Discharged', 'Deceased'), 
                  options = list(searching = FALSE, pageLength = 40, lengthChange = FALSE), 
                  class = 'cell-border stripe',
                  rownames = FALSE)
    
  })
  
  output$mymap <- renderLeaflet({
    m <- leaflet() %>%
      addTiles() %>%
      addMarkers(data = state_lat_long.df1, lat = ~Latitude, lng = ~Longitude, label = ~htmlEscape(Label)) %>%
      setView(lng = 78.50, lat = 23.00, zoom = 5)
  })
  
  # Down the Rabbit Hole
  output$NL_TS_DD <- renderPlotly({
    
    plot_ly(cases_time_series.df, x = ~date, y = ~dailyconfirmed, 
            type = 'scatter', mode = 'lines+markers', line = list(color = 'rgb(51, 51, 255)'),
            hoverinfo = 'text', name = "Daily Confirmed", text = ~paste('Confirmed: ', dailyconfirmed)) %>%
      add_trace(y = ~dailydeceased, name = 'Daily Deceased', mode = 'lines+markers',
                line = list(color = 'rgb(255, 0, 0)'),
                text = ~paste('Deceased: ', dailydeceased)) %>%
      add_trace(y = ~dailyrecovered, name = 'Daily Recovered', mode = 'lines+markers',
                line = list(color = 'rgb(102, 255, 51)'),
                text = ~paste('Recovered: ', dailyrecovered)) %>%
      config(displayModeBar = F)  %>%
      layout(plot_bgcolor='rgba(0,0,0,0)') %>%
      layout(paper_bgcolor='rgba(0,0,0,0)') %>%
      layout(xaxis = list(title = "",
                          showgrid = TRUE,
                          showline = TRUE,
                          showticklabels = TRUE,
                          zeroline = FALSE,
                          categoryorder = "array",
                          categoryarray = "date"),
             yaxis = list(title = "",
                          showgrid = TRUE,
                          showline = TRUE,
                          showticklabels = TRUE,
                          zeroline = FALSE)
      )
          
    
  })
  
  output$NL_TS_AD <- renderPlotly({
    
    plot_ly(cases_time_series.df, x = ~date, y = ~totalconfirmed, 
            type = 'scatter', mode = 'lines+markers', line = list(color = 'rgb(51, 51, 255)'),
            hoverinfo = 'text', name = "Total Confirmed", text = ~paste('Confirmed: ', totalconfirmed)) %>%
      add_trace(y = ~totaldeceased, name = 'Total Deceased', mode = 'lines+markers',
                line = list(color = 'rgb(255, 0, 0)'),
                text = ~paste('Deceased: ', totaldeceased)) %>%
      add_trace(y = ~totalrecovered, name = 'Total Recovered', mode = 'lines+markers',
                line = list(color = 'rgb(102, 255, 51)'),
                text = ~paste('Recovered: ', totalrecovered)) %>%
      config(displayModeBar = F)  %>%
      layout(plot_bgcolor='rgba(0,0,0,0)') %>%
      layout(paper_bgcolor='rgba(0,0,0,0)') %>%
      layout(xaxis = list(title = "",
                          showgrid = TRUE,
                          showline = TRUE,
                          showticklabels = TRUE,
                          zeroline = FALSE,
                          categoryorder = "array",
                          categoryarray = "date"),
             yaxis = list(title = "",
                          showgrid = TRUE,
                          showline = TRUE,
                          showticklabels = TRUE,
                          zeroline = FALSE)
      )
    
    
  })
  
  output$histogram_dead <- renderPlot({
    
    hist(deceased.df$agebracket, col = "#75AADB", border = "white",
         xlab = "Age Distribution",
         ylab = "Count", main = ""
    )  
  })
  
  output$histogram_recovered <- renderPlot({
    
    hist(recovered.df$agebracket, col = "#75AADB", border = "white",
         xlab = "Age Distribution",
         ylab = "Count", main = ""
    )
  })
  
  # Furthur down the Rabbit Hole
  output$DistrictDataDisplayTable <- DT::renderDataTable({
    
    #Localize the data per selected state
    x <- which(statedistrict.df$state == input$state_selector)
    districtdata.df <- statedistrict.df[x,]
    districtdata.df <- districtdata.df$districtData[[1]]
    districtdata.df <- subset(districtdata.df, select = c(1,3,4,5,6))
    
    DT::datatable(districtdata.df, colnames = c('District Name', 'Active Cases', 'Confirmed Cases', 'Deceased', 'Discharged'), 
                  options = list(searching = TRUE, pageLength = 100, lengthChange = FALSE), 
                  class = 'cell-border stripe',
                  rownames = FALSE)
    
  })
  
  
  
  output$SL_TS_DD <- renderPlotly({
    
    #get state data first
    x <- which(statedistrict.df$state == input$state_selector)
    state <- statedistrict.df[x,]
    statecode <- tolower(state$statecode)
    filter.character <- c("date", "status", statecode)
    experiment.df <- subset(statedaily.df, select = filter.character)
    experiment2.df <- experiment.df %>% pivot_wider(names_from = status, values_from = 3)
    experiment2.df$Confirmed <- as.numeric(experiment2.df$Confirmed)
    experiment2.df$Deceased <- as.numeric(experiment2.df$Deceased)
    experiment2.df$Recovered <- as.numeric(experiment2.df$Recovered)
    
    plot_ly(experiment2.df, x = ~date, y = ~Confirmed, 
            type = 'scatter', mode = 'lines+markers', line = list(color = 'rgb(51, 51, 255)'),
            hoverinfo = 'text', name = "Daily Confirmed", text = ~paste('Confirmed: ', Confirmed)) %>%
      add_trace(y = ~Deceased, name = 'Daily Deceased', mode = 'lines+markers',
                line = list(color = 'rgb(255, 0, 0)'),
                text = ~paste('Deceased: ', Deceased)) %>%
      add_trace(y = ~Recovered, name = 'Daily Recovered', mode = 'lines+markers',
                line = list(color = 'rgb(102, 255, 51)'),
                text = ~paste('Recovered: ', Recovered)) %>%
      config(displayModeBar = F)  %>%
      layout(plot_bgcolor='rgba(0,0,0,0)') %>%
      layout(paper_bgcolor='rgba(0,0,0,0)') %>%
      layout(xaxis = list(title = "",
                          showgrid = TRUE,
                          showline = TRUE,
                          showticklabels = TRUE,
                          zeroline = FALSE,
                          categoryorder = "array",
                          categoryarray = "date"),
             yaxis = list(title = "",
                          showgrid = TRUE,
                          showline = TRUE,
                          showticklabels = TRUE,
                          zeroline = FALSE)
      )
    
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

