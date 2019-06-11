##############################################################################################################
# Group 5       : LifeStory Health, Inc Project
# Script        : R code for displaying unlabeled data analysis on RShiny
# Description   : This script will create an interactive Rshiny dashboard to visualize the analysis of unlabeled
#                 data set including the plots, clustering analysis
# Pre-requisite : File should be in csv or text format(preferrable comma delimited)
# Note          : For demo purpose, some of the analysis or charts are geenrated based on filtered data which 
#                 can be changing the sliderInput for Mass and Intensity(the related code has been commented)
##############################################################################################################
#Load required packages and library for building Rshiny dashboard
install.packages("shiny")
install.packages("shinydashboard")
install.packages("semantic.dashboard")
install.packages("NbClust") #This is required for showing clustering analysis
library(shiny)
library(shinydashboard)
library(semantic.dashboard)
library(dplyr) #This is required for showing filtering data
library(ggplot2) #This is required for showing plots
library(NbClust) #This is required to detect the best cluster number
library(corrplot) #This is required for corrplot
# Define UI for data upload app ----
ui <- fluidPage(
  # App title ----
  titlePanel("LifeStory Health: Unlabelled Data File Analysis"),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Select a file ----
      fileInput(
        "file1",
        "Choose CSV File",
        multiple = TRUE,
        accept = c("text/csv",
                   "text/comma-separated-values,text/plain",
                   ".csv")
      ),
      # Horizontal line ----
      tags$hr(),
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),
      # Input: Select separator ----
      radioButtons(
        "sep",
        "Separator",
        choices = c(
          Comma = ",",
          Semicolon = ";",
          Tab = "\t"
        ),
        selected = ","
      ),
      # Input: Select quotes ----
      radioButtons(
        "quote",
        "Quote",
        choices = c(
          None = "",
          "Double Quote" = '"',
          "Single Quote" = "'"
        ),
        selected = '"'
      ),
      # Horizontal line ----
      tags$hr(),
      # Input: Select number of rows to display ----
      radioButtons(
        "disp",
        "Display",
        choices = c("Head(top 6 rows)" = "head",
                    "All" = "all"),
        selected = "head"
      ),
      # Input: Select chart or analysis to analyze ----
      selectInput(
        "chart",
        "Charts/Analysis",
        choices = c(
          "Plot" = "plot",
          "PCA" = "pca",
          "Corrplot" = "corr",
          "Boxplot" = "boxplot",
          "Intensity Histogram" = "hist_int",
          "Mass Histogram" =  "hist_mass",
          "Heatmap" = "heatmap",
          "Hierarchical Clustering" = "clusters",
          "Best Cluster Determination" = "auto_clusters"
        ),
        selected = "plot"
      ),
      # Conditional : Filter Intensity data
      conditionalPanel(condition = "input.chart!='plot'&& input.chart!='corr'",
                       sliderInput("int","Filter Intensity Data",min = 0, max = 100,value = c(30, 40)),
                       conditionalPanel(condition = "input.chart =='hist_mass' | input.chart =='clusters'| input.chart =='auto_clusters' | 
                                        input.chart =='pca'| input.chart =='heatmap'",
                                        sliderInput( "mass","Filter Mass Data", min = 100, max = 200,value = c(100, 200))
                                        
                       )),
      # Conditional : Input Group clusters
      conditionalPanel(condition = "input.chart =='clusters'",
                       sliderInput("k","Choose Groups",min = 0,max = 100,value = 3 )),
      # Conditional : Choose type of plot to display
      conditionalPanel(condition = "input.chart =='plot'",
                       radioButtons("choice", "Choose one to display:",c("IntensityofPeak vs RetentionTime" = "peak",
                                                                         "ObservedMass vs RetentionTime" = "rt"), "peak" ))
    ),#end of sidebarPanel
    # Main panel for displaying outputs ----
    mainPanel(tabsetPanel(
      # Output layout
      tabPanel("Dataset", tableOutput("contents"),
               verbatimTextOutput("data_summary")),
      tabPanel("Plot Analysis",
               plotOutput("plot", click = "plot_click"),
               verbatimTextOutput("info"),
               conditionalPanel(condition = "input.chart =='boxplot' | input.chart =='hist_int' |
                                input.chart =='hist_mass'",verbatimTextOutput("summarize"))
               ),
      tabPanel( "PCA Analysis",
                verbatimTextOutput("pcamean"),
                verbatimTextOutput("pcavar"),
                verbatimTextOutput("pcaoutput"),
                verbatimTextOutput("pcasummary")
      ),
      tabPanel( "Hierarchical Clustering",
                
                
                plotOutput("cluster"),
                radioButtons("summary","Choose to summarize",c("by median" = "median",
                                                               "by mean" = "mean"),"median"),
                tableOutput("clustertable")
      ),
      tabPanel( "Automatic Best Cluster Determination",
                textOutput("clusterText"),
                plotOutput("clustinfo")
      )
      
      )#end of tabsetPanel
  )#end of main Panel
  )#end of sidebarLayout
)#end of ui


# Define server logic to read selected file ----
server <- function(input, output, session) {
  output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    unlabelled <- read.csv(
      input$file1$datapath,
      header = input$header,
      sep = input$sep,
      quote = input$quote
    )
    output$data_summary <- renderPrint({
      count_data<- count(unlabelled)
      print(paste0("Total number of rows: ",count_data))
      print("Summary")
      summary(unlabelled)}) 
    
    #This is advisable that head is chosen, since data large is huge otherwise Rshiny app might hang
    if (input$disp == "head") {
      return(head(unlabelled))
      
    }
    
    else {
      return(unlabelled)
    }
    
  })
  
  #########################################################################################################  
  #Depending on selection of chart input the corresponding event is triggered to display plot and analysis
  #########################################################################################################    
  ##Displaying plot,corrplot on whole data set
  observeEvent(input$chart, {
    if (input$chart == "plot")
     { output$plot <- renderPlot({
        req(input$file1)
        unlabelled <- read.csv(
          input$file1$datapath,
          header = input$header,
          sep = input$sep,
          quote = input$quote
        )
        #Renaming of the variables in the datafile
        names(unlabelled) <- c("ID", "RetentionTime", "Intensity", "MassToChargeRatio", "Charge")
        #Choosing the type of the plot user want to display
        if (input$choice == "peak")
          plot(unlabelled$RetentionTime, unlabelled$Intensity)
        if (input$choice == "rt")
          plot(unlabelled$RetentionTime, unlabelled$MassToChargeRatio)
      })}
    ##Display corrplot
    # else if(input$chart == "corr")
    #   {output$plot<- renderPlot({
    #     req(input$file1)
    #     unlabelled <- read.csv(
    #       input$file1$datapath,
    #       header = input$header,
    #       sep = input$sep,
    #       quote = input$quote)
    #     # col1 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "white",
    #     #                            "cyan", "#007FFF", "blue", "#00007F"))
    #     col3 <- colorRampPalette(c("red", "green", "blue"))
    #     corrplot(cor(unlabelled), method = "number",col = col3(20))
    #      }
    #     )}
      
    else {
      output$plot <- renderPlot({
        req(input$file1)
        unlabelled <- read.csv(
          input$file1$datapath,
          header = input$header,
          sep = input$sep,
          quote = input$quote
        )
        
        ########################################################################################################################
        ##Code in case slider value need to be changed to max and min value of "Intensity of Peak" variable
        #maxvalue_int <- max(unlabelled$Intensity.of.Peak)
        #minvalue_int <- min(unlabelled$Intensity.of.Peak)
        #updateSliderInput(session, "int", min=minvalue_int,max=maxvalue_int, value = c(input$int[1],input$int[2]))
        ########################################################################################################################
        names(unlabelled) <- c("ID", "RetentionTime", "Intensity", "MassToChargeRatio", "Charge")
        ##Filtering Intensity input
        unlb_intensity <-
          filter(unlabelled,
                 Intensity > input$int[1] &
                   Intensity< input$int[2])
        
        ########################################################################################################################
        ##Code in case slider value need to be changed to max and min value of "Mass" variable
        #maxvalue_mass <- max(unlb_intensity$Mass)
        #minvalue_mass <- min(unlb_intensity$Mass)
        #updateSliderInput(session, "mass", min=minvalue_mass,max=maxvalue_mass, value = c(input$mass[1],input$mass[2]))
        ########################################################################################################################
        #Adding new column to datafile to calculate Mass
        unlb_intensity$Mass <-
          unlb_intensity$MassToChargeRatio * unlb_intensity$Charge
        ##Filtering Mass input
        unlb_mass <-
          filter(unlb_intensity, Mass > input$mass[1] & Mass < input$mass[2])
        #####Analysis/charts are based on filtered data(Mass and Intensity)
        ##PCA Anaysis
        if (input$chart == "pca") {
          output$pcamean <-renderPrint({
            print("Mean") 
            apply(unlb_mass,2,mean,trim = .2)})
          output$pcavar <-renderPrint({
            print("Variance")
            apply(unlb_mass,2,var)
          })
          output$pcaoutput <-renderPrint({
            print("PCA Output") 
            pca.out<-prcomp(unlb_mass,scale=TRUE)
            pca.out
          })
          output$pcasummary <-renderPrint({
            print("PCA Summary")
            pca.out<-prcomp(unlb_mass,scale=TRUE)
            summary(pca.out)
          })
          
          pcaout<-prcomp(unlb_mass,scale=TRUE) ##demo purpose
          biplot(pcaout,scale = 0, cex=0.65)                           
        }
        ##Boxplot Analysis
        else if (input$chart == "boxplot")
        {boxplot(unlb_intensity$Intensity,col="gold")
          output$summarize <- renderPrint({
            print("Boxplot Summary")
            summary(unlb_intensity$Intensity)})}
        ##Plotting Intensity Histogram
        else if (input$chart == "hist_int")
        {hist(unlb_intensity$Intensity, breaks = 50,col="green")
          output$summarize <- renderPrint({
            print("Intensity Histogram Summary")
            summary(unlb_intensity$Intensity)})}
        ##Plotting Mass Histogram
        else if (input$chart == "hist_mass")
        {hist(unlb_intensity$Mass,col="blue")
          output$summarize <- renderPrint({
            print("Mass Histogram Summary")
            summary(unlb_intensity$Mass)})}
        ##Plotting Heatmap
        else if (input$chart == "heatmap") {
          unlb_heat <- unlb_mass[, c(2, 4, 5, 6)]
          unlb_dist <-
            dist(unlb_heat, method = 'manhattan') 
          heatmap(as.matrix(unlb_dist),
                  labRow = F,
                  labCol = F)
        }#end of heatmap else
        ##Plotting Hierarical Clustering
        else if (input$chart == "clusters"){
          unlb_heat <- unlb_mass[, c(2, 4, 5, 6)]
          unlb_dist <- dist(unlb_heat, method = 'manhattan')
          model = hclust(unlb_dist , method = 'centroid')
          plot(model) 
          
          output$cluster<- renderPlot({
            #k means - number of clusters
            clusters <- cutree(model, k = input$k)
            ggplot(unlb_mass, aes(unlb_mass$Mass, unlb_mass$RetentionTime)) +
              geom_point(size = 3,
                         alpha = 0.8,
                         aes(colour = factor(clusters)))
          }) ##end of cluster
          #Displaying Hierarical Clustering data by median or mean
          observeEvent(input$summary, {
            output$clustertable <- renderTable({
              
              #k means - number of clusters
              clusters <- cutree(model, k = input$k)
              
              if (input$summary == "median")
                aggregate(unlb_mass, by = list(cluster = clusters), median)
              else
                #(input$summary =="mean")
                aggregate(unlb_mass, by = list(cluster = clusters), mean)
            })
          }) 
        } #end of cluster else
        else if(input$chart == "auto_clusters")
        {
          ###Automatic detetction of best cluster number
          output$clustinfo <- renderPlot({
            unlb_heat <- unlb_mass[, c(2, 4, 5, 6)]
            unlb_dist <- dist(unlb_heat, method = 'manhattan')
            model <- hclust(unlb_dist , method = 'centroid')
            nc<- NbClust(unlb_mass, distance = "manhattan", min.nc=2, max.nc=15, method = "centroid")
            #nc
            output$clusterText <- renderText({
              paste0("Best number of clusters:",max(unlist(nc[4])))})
          })
        } 
        else 
          {#output$plot<- renderPlot({
            # req(input$file1)
            # unlabelled <- read.csv(
            #   input$file1$datapath,
            #   header = input$header,
            #   sep = input$sep,
            #   quote = input$quote)
            # col1 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "white",
            #                            "cyan", "#007FFF", "blue", "#00007F"))
            col3 <- colorRampPalette(c("red", "green", "blue"))
            corrplot(cor(unlabelled), method = "number",col = col3(20))
             }
            #)}
      })
    } 
    ##displaying x and y on plot with mouse click
    output$info <- renderText({
      paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
    })
  })#end of observeEvent - chart
  
}#end  server function


# Run the app ----
shinyApp(ui, server)