##############################################################################################################
# Group 5     : LifeStory Health, Inc Project
# Script      : R code for Finding Visual difference on RShiny
# Description : This script will create an interactive Rshiny dashboard to find the visual differences  
#               between two images(graphs) with two options and provide user an ability to mark differences
#               by drawing rectangles around the difference.Both the images can be made blurred with their 
#               respective sliders.
#               1. Compare with existing: Compare a new graph with sample graphs provided by LSH
#               2. Compare New Images : Compare two new graphs
#############################################################################################################
#Load required packages and library for building Rshiny dashboard
install.packages("shiny")
install.packages("shinydashboard")
install.packages("semantic.dashboard")
install.packages("magick")  #This package is heart of this code, used for overlapping images to compare
library(magick)
library(shiny)
library(shinydashboard)
library(semantic.dashboard)

################################################################################################################
#Set the working directory, change it as per location of the images, here /www is directory where 
# all sample images are downloaded in PNG format and renamed with corresponding Item name like 1201.png etc.

################################################################################################################
setwd("/Users/smitagupta/Documents/MS Analytics/DataMining/Group 5/Data/www")
filepath<-setwd("/Users/smitagupta/Documents/MS Analytics/DataMining/Group 5/Data/www") 

#ui script to create the layout
ui <-     
          fluidPage(
            titlePanel("LifeStory Health:Find visual differences"),
            # Sidebar Layout for inputs ----
            sidebarLayout(
              # Sidebar Panel for inputs ----
              #Select : input a disease
             sidebarPanel ( selectInput("dis", 
                        label = "Choose a Disease",
                        choices = c("Alzheimer", 
                                    "Malaria",
                                    "Cancer", 
                                    "Thyroid"),
                        selected = "Malaria"),
                        #conditional panel : choose compare type
                        conditionalPanel(condition = "input.dis =='Alzheimer'",
                             selectInput("diff",
                                         label ="Choose Difference type",
                                         choices = c("Compare New Images",
                                                     "Compare with Existing"),
                                         selected = "Compare New Images")),
                        #conditional panel :file input for one file when compare with existing
                        conditionalPanel(condition = "input.diff =='Compare with Existing'",
                             fileInput("upload1", "Choose a new Item Name image", accept = c('image/png', 'image/jpeg'))),
                        #conditional panel :file input for two files when compare with new
                        conditionalPanel(condition = "input.diff =='Compare New Images'  && input.dis =='Alzheimer'",
                             fileInput("upload2", "Choose a new Item Name image 1", accept = c('image/png', 'image/jpeg')),
                             fileInput("upload3", "Choose a new Item Name image 2", accept = c('image/png', 'image/jpeg'))),
                        #conditional panel for choosing existing graoh or image
                        conditionalPanel(condition = "input.diff =='Compare with Existing'", 
                             selectInput(
                               "var", 
                               label = "Choose existing Item Name",
                               choices=c(list.files(filepath)),
                               selected = NULL)),
           
                         textInput("size", "Size", value = "500x500!"),
                         sliderInput("blur", "Blur Original Image", 0, 20, 0),
                         sliderInput("blur_new", "Blur New Image", 0, 20, 0),
                         checkboxGroupInput("effects", "Options",
                               choices = list("compare"))
          ), #end of sidebar panel
          #main panel for output
          mainPanel(
            conditionalPanel(condition = "input.dis =='Alzheimer'",
            plotOutput("img",click = "plot_click",
                      dblclick = "plot_dblclick",
                      hover = "plot_hover",
                      brush = "plot_brush")),
          conditionalPanel( condition ="input.effects != 'compare'", imageOutput("img_new"))
          )#end of main panel
        )#end of sidebar layout
 )#end of fluid page

#server script for comparision
server <- function(input, output, session) {
#initialzing variables for rectangle to be created by user on images for comparision
  set.seed(123)
  prev_vals <- NULL
  structures <- reactiveValues(data = data.frame(box_id = numeric(), xmin = numeric(), ymin = numeric(), xmax = numeric(), xmax = numeric()))
  
  #upload file 1 - for Existing comparision
  observeEvent(input$upload1,
               {if (length(input$upload1$datapath))
                 image_x <<- image_convert(image_read(input$upload1$datapath), "jpeg")
               info <- image_info(image_x)
               updateCheckboxGroupInput(session, "effects", selected = "")
               updateTextInput(session, "size", value = "500x300")
               
               output$img_new <- renderImage({
                 tmpfile <- image_x %>%
                   image_resize(input$size) %>%
                   image_blur(input$blur_new, input$blur_new) %>%
                   image_write(tempfile(fileext='jpg'), format = 'jpg')
                 # Return a list
                 list(src = tmpfile, contentType = "image/jpeg")})
               })
  
  #upload file 1 - New Images comparision
  observeEvent(input$upload2,
               {if (length(input$upload2$datapath))
                 #prev_vals <- NULL
                 #structures <- reactiveValues(data = data.frame(box_id = numeric(), xmin = numeric(), ymin = numeric(), xmax = numeric(), xmax = numeric()))
                 image_y <<- image_convert(image_read(input$upload2$datapath), "jpeg")
               info <- image_info(image_y)
               updateCheckboxGroupInput(session, "effects", selected = "")
               updateTextInput(session, "size", value = "500x300")
               
               output$img <- renderImage({
                 tmpfile <- image_y %>%
                   image_resize(input$size) %>%
                   image_blur(input$blur, input$blur) %>%
                   image_write(tempfile(fileext='jpg'), format = 'jpg')
                 # Return a list
                 list(src = tmpfile, contentType = "image/jpeg")})
               })
  
  #upload file 2 - New Images comparision
  observeEvent(input$upload3,
               {if (length(input$upload3$datapath))
                 image_z <<- image_convert(image_read(input$upload3$datapath), "jpeg")
               info <- image_info(image_z)
               updateCheckboxGroupInput(session, "effects", selected = "")
               updateTextInput(session, "size", value = "500x300")
               
               output$img_new <- renderImage({
                 tmpfile <- image_z %>%
                   image_resize(input$size) %>%
                   image_blur(input$blur_new, input$blur_new) %>%
                   image_write(tempfile(fileext='jpg'), format = 'jpg')
                 # Return a list
                 list(src = tmpfile, contentType = "image/jpeg")})
               })  
  
  #load existing files from working directory
  filenames <- list.files(filepath)
##################################################################################################################################### 
#When any chosen on "Compare with Existing" option, the png images avilable in the working directory will be 
#parsed and read until it matches with the existing image selected
######################################################################################################################################   
  observeEvent(input$var, {
    for (i in 1:length(filenames)) {
      length(filenames)
      if (input$var == (filenames[i]))
      {
        image <- image_read((filenames[i]))
        prev_vals <- NULL
        structures <- reactiveValues(data = data.frame(box_id = numeric(), xmin = numeric(), ymin = numeric(), xmax = numeric(), xmax = numeric()))
        break #if selected input matches with filename, come out of loop
      }
      i <- i + 1
    }
    
    #store the retrived image from for loop
    tmpfile <- image
    #display the existing image on UI
    output$img <- renderImage({
      tmpfile <- tmpfile %>%
        image_resize(input$size) %>%
        image_blur(input$blur, input$blur) %>%
        image_write(tempfile(fileext = 'jpg'), format = 'jpg')
      #   # Return a list
      list(src = tmpfile, contentType = "image/jpeg")
    })#end of renderImage
    
    
#######################################################################################################################################
#When compare checkbox is selected, the new and existing images will be overlapped
########################################################################################################################################    
    observeEvent(input$effects,
                 {if (input$effects == 'compare')
                   output$img <- renderPlot({
                     
                     image <- image_flatten(c(image_blur(tmpfile,input$blur,input$blur),image_blur(image_x,input$blur_new,input$blur_new)),"Add")
                     
                     #create a blank plot where overlapped image will be rastered
                     plot(1:500,type='n',axes = FALSE, xlab = "", ylab = "")
                     rasterImage(image,1,1,500,500)
                     #user can click and create red rectangles to mark on the image
                     if (nrow(structures$data) > 0) {
                       r <- structures$data
                       rect(r$xmin, r$ymin, r$xmax, r$ymax, border = "red")
                     }
                     image <- image %>%
                       image_resize(input$size) %>%
                       image_write(tempfile(fileext = 'jpg'), format = 'jpg')
                     #   # Return a list
                     list(src = image, contentType = "image/jpeg")
                   }, height = 500, width = 600)
                 
                 #display the rectangles clicked by user
                 observeEvent(input$plot_brush,{
                   e <- input$plot_brush
                   if (!is.null(e)) {
                     vals <- data.frame(xmin = round(e$xmin, 1), ymin = round(e$ymin, 1), xmax = round(e$xmax, 1), ymax = round(e$ymax, 1))
                     if (identical(vals,prev_vals)) return() #We dont want to change anything if the values havent changed.
                     structures$data <- rbind(structures$data,cbind(data.frame(box_id = nrow(structures$data)+1),vals))
                     prev_vals <<- vals
                   }
                 }) 
                 })
    
  }#end of first if - observEvent - var
)#end of observEvent - var 
  
  
#######################################################################################################################################
#When new file for "Compare New Images" selected, the two new images uploaded are compared by overlapping them
########################################################################################################################################    
  observeEvent(input$upload3,
               {if(!is.null(input$upload2) && !is.null(input$upload3) )
                 observeEvent(input$effects,
                              {if (input$effects == 'compare')
                                output$img <- renderPlot({
   
                                  tmpfile <- image_flatten(c(image_blur(image_y,input$blur,input$blur),image_blur(image_z,input$blur_new,input$blur_new)),"Add")
                                  #create a blank plot where overlapped image will be rastered
                                  plot(1:500,type='n',axes = FALSE, xlab = "", ylab = "")
                                  rasterImage(tmpfile,1,1,500,500)
                                  #user can click and create red rectangles to mark on the image
                                  if (nrow(structures$data) > 0) {
                                    r <- structures$data
                                    rect(r$xmin, r$ymin, r$xmax, r$ymax, border = "red")
                                  }
                                  tmpfile <- tmpfile %>%
                                    image_blur(input$blur, input$blur) %>%
                                    image_write(tempfile(fileext = 'jpg'), format = 'jpg')
                                  #   # Return a list
                                  list(src = tmpfile, contentType = "image/jpeg")
                                  
                                }, height = 500, width = 700)
                              #display the rectangles clicked by user
                              observe({
                                e <- input$plot_brush
                                if (!is.null(e)) {
                                  vals <- data.frame(xmin = round(e$xmin, 1), ymin = round(e$ymin, 1), xmax = round(e$xmax, 1), ymax = round(e$ymax, 1))
                                  if (identical(vals,prev_vals)) return() #We dont want to change anything if the values havent changed.
                                  structures$data <- rbind(structures$data,cbind(data.frame(box_id = nrow(structures$data)+1),vals))
                                  prev_vals <<- vals
                                }
                              })  
                              })#})
                 
               }#end of first if - observEvent - upload3
   )#end of observEvent - upload3
}#end of server

#run App
shinyApp(ui, server)    
