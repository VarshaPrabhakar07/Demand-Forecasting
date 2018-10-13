#############################################################################
## Use R for Analytics
## Final Project
## Shiny App
#############################################################################
#setwd("/Users/apple/Documents/Master - Purdue Krannert/Using R for Analytics/Final Project/Data")
library(caret)
library(shiny)
library(leaflet)
library(dplyr)
library(plyr)
if(is.null(result)){
  result <- readRDS("result.rds")
}
items <- data.frame(item_nbr = character(), stringsAsFactors = F)
for(i in 1 : 1009){
  items[i, 1] <- as.character(result[[i]][1])
}
df <- read.csv('stores.csv', header = T, sep = "," )
df <- cbind(df, geocode(paste(df$city, df$state)))
df$type <- NULL
df$cluster <- NULL
head(df)
df1 <-count(df, vars=c('city','state','lon','lat'))
df <- df1

oil=read.table(file = "oil_avg.csv", header = TRUE, sep = ",", nrows = 50)
sales=read.table(file = "Avg_sales.csv", header = TRUE, sep = ",", nrows = 50)

ui <- fluidPage(
  theme = shinytheme("slate"),
  tabsetPanel(
    tabPanel("Sale Prediction",(sidebarLayout(
    sidebarPanel(selectInput(inputId = "item",
                label = "Select an item:",
                choices = items),
    selectInput(inputId = "month",
                label = "Select a month:",
                choices = c("01", "02", "03", "04", "05", "06",
                            "07", "08", "09", "10", "11", "12")),
    selectInput(inputId = "Store_nbr",
                label = "Select the store number:",
                choices = c(1:54)),
    numericInput(inputId = "oil_price",
                 label = "Enter expected oil price:",
                 value = 0,
                 min = 0,
                 max = 150),
    selectInput(inputId = "s_type",
                label = "Select the type of the store:",
                choices = c("A", "B", "C", "D", "E")),
    selectInput(inputId = "cluster",
                label = "Select the type of the cluster:",
                choices = c(1:17)),
    selectInput(inputId = "h_type",
                label = "Select the type of the holiday:",
                choices = c("Additional", "Bridge", "Event", "Holiday",
                            "Transfer", "Work day", "NormalDay")),
    selectInput(inputId = "trans",
                label = "Is it transferred holiday?",
                choices = c("Yes", "No")),
    actionButton("pred", "Predict")),
  mainPanel(
    br(), br(), br(), br(), br(),
    h1(textOutput("predictionResult"), align = "center"))  
    )
    )
  ),
  tabPanel("Store Locations",
             mainPanel(
               br(),leafletOutput("map", height="1000px",width="1000px"),br())
           ),
  tabPanel(title = "Oil and Sales Trend",
           br(),
           fluidRow(plotOutput("oilprices",width="1200px",height="250px")),
           br(),
           fluidRow(plotOutput("sales",width="1200px",height="250px"))
  )
)
)


server <- function(input, output){
  observeEvent( input$pred,{
    ii <- which(items$item_nbr == input$item)
    model <- result[[ii]][2][1]
    D <- data.frame(month = factor(),
                    store_nbr = factor(),
                    dcoilwtico = double(),
                    store_type = factor(),
                    cluster = factor(),
                    holiday_type = factor(),
                    transferred = factor())
    levels(D$month) <- c("01", "02", "03", "04", "05", "06",
                         "07", "08", "09", "10", "11", "12")
    levels(D$store_nbr) <- c(1:54)
    levels(D$store_type) <- c("A", "B", "C", "D", "E")
    levels(D$cluster) <- c(1:17)
    levels(D$holiday_type) <- c("Additional", "Bridge", "Event", "Holiday",
                                "Transfer", "Work day", "NormalDay")
    levels(D$transferred) <- c(T, F)
    D[1, "month"] <- input$month
    D[1, "store_nbr"] <- input$Store_nbr
    D[1, "dcoilwtico"] <- input$oil_price
    D[1, "store_type"] <- input$s_type
    D[1, "cluster"] <- input$cluster
    D[1, "holiday_type"] <- input$h_type
    if(input$trans == "Yes"){
      D$transferred <- T
    } else{
      D$transferred <- F
    }
    # create dummy variables
    dummies <- dummyVars(~., data = D)
    ex <- data.frame(predict(dummies, newdata = D))
    names(ex) <- gsub("\\.", "", names(ex))
    D <- cbind(D, ex)
    rm(dummies, ex)
    y <- round(as.numeric(unlist(predict(model, newdata = D))), 2)
    # final output
    output$predictionResult <- renderText({
      paste("The predicted sales is:", y, " units.")
    })
  }# end of "inputPred", 
  )# end of observeEvent
  # create a reactive value that will store the click position
  data_of_click <- reactiveValues(clickedMarker=NULL)
  output$map <- renderLeaflet({
    leaflet(df) %>% 
      setView(lng=-80 , lat =-3, zoom=6.5) %>%
      addTiles(options = providerTileOptions(noWrap = TRUE)) %>%
      addCircleMarkers( ~lon , ~lat, 
                        popup=~paste0("Store Count: ",as.character(freq)), 
                        radius=8 , color="black",  fillColor="red", 
                        stroke = TRUE, fillOpacity = 0.8)
  })
  
  # store the click
  observeEvent(input$map_marker_click,{
    data_of_click$clickedMarker <- input$map_marker_click
  })
  output$oilprices <- renderPlot({
    Month = oil$Month
    g = ggplot(data=oil, aes(x=Month, y=Oil_Price, group=1)) +
      geom_line() + geom_point() + ggtitle("Oil Prices over the year") +  
      theme(plot.title = element_text(hjust=0.5))
    g = g + theme(axis.text=element_text(size=14, colour = "darkblue"),
                  axis.title=element_text(size=13,face="bold", colour = "darkblue"))+
      scale_x_continuous("Month", labels = as.character(Month), breaks = Month)
    g
  })
  output$sales <- renderPlot({
    Month = sales$Month
    g = ggplot(data=sales, aes(x=Month, y=Avg_Unit_Sales, group=1)) +
      geom_line()+
      geom_point() + ggtitle("Sales over the year") + theme(plot.title = element_text(hjust=0.5))
    g = g + theme(axis.text=element_text(size=14, colour = "darkblue"),
                  axis.title=element_text(size=13,face="bold", colour = "darkblue"))+
      scale_x_continuous("Month", labels = as.character(Month), breaks = Month)
    g
  })
  
}
shinyApp(ui = ui, server = server)
