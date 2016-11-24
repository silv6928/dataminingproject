#####Tony Silva
#####Customer Order Analysis Application
#####Data Mining Project

##Note: Please make sure the lt data is loaded into your R workspace. This must be loaded into the R Studio Workspace
##Or else the application will not work.
##Also please make sure to download the libraries listed below.

library(shiny)
library(doBy)
library(ggplot2)
#K-means Clusting Algorithm
kmean<- function(K, d){
  clustersDF<- as.data.frame(d)
  clustersDF$clusters <- sample(1:K, size = nrow(clustersDF), replace = T)
  #Initiate the centroids container
  centroids <- matrix(nrow = K, ncol = dim(clustersDF)[2]-1, data = 0)
  #This is used to check if the centroids change.
  centList <- list()
  
  J<-1
  centList[[J]]<- centroids
  repeat{
    J <- J+1
    centroids <- calculateCentroid(centroids, clustersDF,K)
    centList[[J]]<- centroids
    clustersDF <- measureDist(clustersDF,centroids)
    centroids <- calculateCentroid(centroids, clustersDF,K)
    if (all(centList[[J]] == centList[[J-1]])) {
      break
    }
  }
  #Create a list
  #1.) Final centroids of the clusters
  #2.) Cluster assignments
  #3.) SSE for each cluster
  #4.) SSE Total
  output <- list()
  output[[1]] <- centroids
  output[[2]] <- clustersDF
  output[[3]] <- sseTotal(centroids,clustersDF)
  output[[4]] <- sum(output[[3]])
  return(output)
}

#This will calculate the location of the centroid based on the values in the cluster
calculateCentroid<- function(centroids, clustersDF, K){
  for (i in 1:K) {
    for (j in 1:dim(centroids)[2]) {
      centroids[i,j]<- mean(clustersDF[which(clustersDF$cluster == i),j])
    }
  }
  return(centroids)
}

#Find the distance between points and centroids and reassign the cluster.
#Calculates the distance utilizing the euclidean distance function.
measureDist<- function(clustersDF, centroids){
  distance <- matrix(nrow = nrow(clustersDF), ncol = nrow(centroids))
  for (i in 1:nrow(clustersDF)) {
    for (j in 1:nrow(centroids)) {
      distance[i,j] <- euc.dist(clustersDF[i,1:dim(clustersDF)[2]-1], centroids[j,])
    }
    clustersDF$clusters[i] <- which.min(distance[i,])
  }
  return(clustersDF)
}


# Euclidean Distance Function
euc.dist <- function(x1, x2){
  sqrt(sum((x1 - x2) ^ 2))
}

#Sum of squared Error
sseTotal <- function(c,x){
  distance <- matrix(nrow = nrow(x), ncol = nrow(c))
  SSE <- matrix(nrow = nrow(c))
  for (i in 1:nrow(x)) {
    for (j in 1:nrow(c)) {
      if(x[i, dim(x)[2]] == j){
        distance[i,j] <- euc.dist(x[i,1:dim(x)[2]-1], c[j,])
      }
      SSE[j,] <- sum(distance[,j], na.rm = T)
    }
  }
  return(SSE)
}

# length2 <- function(x, na.rm = FALSE){
#   if(na.rm) sum(!is.na(x))
#   else  length(x)
# }

# Define UI for application.
ui <- shinyUI(fluidPage(
   fluidRow(
   # Application title
   titlePanel("Customer Order Analysis"),
   
   
   # Sidebar to build plots 
   sidebarLayout(
      sidebarPanel(
        h3("Plot Data"),
        #All of the different inputs
        checkboxInput("checkbox1", label = "Histogram?", value = FALSE), 
        selectInput("xVar", "Choose a X coordinate", choices = c("Lead_Time", "Stock_Time", "DATE_ENTERED", "PRIORITY", "PRODUCT_FAMILY","PART_NO"), selected = "PRIORITY"),
        conditionalPanel(condition = "input.checkbox1 == false",
        selectInput("yVar", "Choose a Y coordinate", choices = c("Lead_Time", "Stock_Time", "None"))),
        dateRangeInput("dates",start = "2016-08-01", label = "Date Orders Were Placed")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )),
   #Create a row for data aggregation
   fluidRow(
     sidebarLayout( 
       
       sidebarPanel(
       h3("Aggregate Data"),
        #Check to see if the user wants to sync with the plot inputs
       checkboxInput("checkbox2", label = "Sync Table with the Plot Above?", value = TRUE),
       conditionalPanel(condition = "input.checkbox2 == false",
                        selectInput("xVar2", "Choose a summary value", choices = c("Lead_Time", "Stock_Time", "DATE_ENTERED", "PRIORITY", "PRODUCT_FAMILY", "PART_NO"), selected = "PRIORITY"),
                        selectInput("yVar2", "Choose an aggregation value", choices = c("Lead_Time", "Stock_Time")),
                        dateRangeInput("dates2",start = "2016-08-01", label = "Date Orders Were Placed"))
     ),
     #Main Panel of first row to plot the generic data plots
     mainPanel(
     dataTableOutput("table")
     )
    )
   ),
   #Create another row for the clustering
   fluidRow(
     sidebarLayout(
       sidebarPanel(
         #Inputs for clustering
         h3("Clustering Orders"),
         numericInput("k", "Number of Clusters", value = 2, max = 20, min = 2),
         dateRangeInput("dates3",start = "2016-08-01", end = "2016-08-03",label = "Date Orders Were Placed")),
       #Output the clustering
       mainPanel(
         plotOutput("clusterPlot")
       )
     )
   )
  )
)

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
   
  output$distPlot <- renderPlot({
      # generate a graph of x vs y if not build a histogram
    lt2 <- subset(lt, lt$DATE_ENTERED >= as.POSIXct(input$dates[1]) & as.POSIXct(input$dates[2]) >= lt$DATE_ENTERED)
     x <- lt2[,input$xVar]
     #Check if user wants a histogram
     #if they do not then take inputs and plot vs. each other
     #if they do then plot the histogram.
     if(input$checkbox1 == FALSE)
      {
       #If there is only one input value then plot the single value 
       if (input$yVar != "None") {
          y <- lt2[,input$yVar]
          plot(x = x, y = y, main = paste("Plot of ", input$yVar, " VS ", input$xVar))
        }else{
          plot(x = x, main = paste("Scatter Plot of ", input$xVar))
        }
        
      }else{
        hist(x = x, main = paste("Histogram of ", input$xVar))
      }

   })
  #Aggregation Data Table backend
  output$table <- renderDataTable({
    #Check if the user wants to use the inputs of the plot
    #If yes then use those inputs
    #Else use the new inputs
    if(input$checkbox2){
      lt2 <- subset(lt, lt$DATE_ENTERED >= as.POSIXct(input$dates[1]) & as.POSIXct(input$dates[2]) >= lt$DATE_ENTERED)
      formulaText<- paste(input$yVar, "~", input$xVar)
    }else
    {
      lt2 <- subset(lt, lt$DATE_ENTERED >= as.POSIXct(input$dates2[1]) & as.POSIXct(input$dates2[2]) >= lt$DATE_ENTERED)
      formulaText<- paste(input$yVar2, "~", input$xVar2)
    }
    #output the table
    agg <- summaryBy(as.formula(formulaText), data = lt2, FUN = mean)
    agg
    }, options = list(pageLength = 5))
  
  output$clusterPlot <- renderPlot({
    #subset data for the dates
    lt2 <-subset(lt, lt$DATE_ENTERED >= as.POSIXct(input$dates3[1]) & as.POSIXct(input$dates3[2]) >= lt$DATE_ENTERED)
    #Subset data for just the two columns
    x<- lt2[,c("Lead_Time","Stock_Time")]
    #perform clustering
    cluster <- kmean(as.numeric(input$k), x)
    #Get the cluster assignments
    df <- cluster[[2]]
    #Plot creation that is rendered by inputs
    qplot(x = df[,1], y = df[,2], color = as.factor(df[, 3]), xlab = "Lead_Time", ylab = "Stock_Time", main = "Clustering of Lead_Time and Stock_Time") + labs(color = "Cluster")
  })
  
})

# Run the application 
shinyApp(ui = ui, server = server)

