options(stringsAsFactors = FALSE)

library(shiny)
library(shinydashboard)
library(ggplot2)
library(grid)
library(png)

alex <- readPNG('D:/R_files/shiny/shinyforce/sprites/alex1.png')
ivan <- readPNG('D:/R_files/shiny/shinyforce/sprites/ivan1.png')

fluidPage(
    fluidRow(
        column(width = 4,
               plotOutput("playgrid", height=600, width=600,
                          click = "grid_click"
               )
        )
    ),
    fluidRow(
        column(width = 6,
               h4("Return cell"),
               verbatimTextOutput("cell_click")
        )
    )
)

