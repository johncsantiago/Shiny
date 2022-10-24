list.of.packages <- c("shiny", "scales","gplots","colourpicker")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)>0){
  if (!require("BiocManager", quietly = TRUE)){
    install.packages("BiocManager")}
  BiocManager::install("scales")
  install.packages('shiny')
  install.packages("gplots")
  install.packages("colourpicker")
  
}

library(shiny)
library(scales)
library(gplots)
library(colourpicker)

# initialize global variable to record selected (clicked) rows
selected_points <- mtcars[0, ]
str(selected_points)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  plotOutput("plot", click = "clicked"),
  tableOutput("data")
)
##server <- function(input, output, session) {
  
  ##xpos = reactiveVal(0)
  ##ypos = reactiveVal(0)
  ##observeEvent(input$plot_click,{
    ##xpos(nearPoints(mtcars, input$plot_click, xvar = "wt", yvar = "mpg")["wt"])
    ##ypos(nearPoints(mtcars, input$plot_click, xvar = "wt", yvar = "mpg")["mpg"])
    ##})
    ##output$plot <- renderPlot({
    ##plot(mtcars$wt, mtcars$mpg)
    ##legend(x = xpos()+.15,
      ##   y = ypos()+3.5,
        ## "hello",
        ## xjust = .5,
        ## x.intersp = -.5,
        ## y.intersp = .1,
        ## bg = "yellow",
        ## xpd=NA)
  ##}, res = 96)
##}

server = shinyServer(function(input, output) {
  
  selected <- reactive({
    # add clicked
    selected_points <<- rbind(selected_points, nearPoints(mtcars, input$clicked))
    # remove _all_ duplicates if any (toggle mode) 
    # http://stackoverflow.com/a/13763299/3817004
    selected_points <<- 
      selected_points[!(duplicated(selected_points) | 
                          duplicated(selected_points, fromLast = TRUE)), ]

    return(selected_points[nrow(selected_points),])
  })
  
  output$plot <- renderPlot({
    ggplot(mtcars, aes(x = mpg, y = wt)) +
      geom_point() +
      geom_point(data = selected(), colour = "red", size = 5)
  })
  
  output$data <- renderTable({
    selected()
  })
  
})



shinyApp(ui = ui, server = server, options)