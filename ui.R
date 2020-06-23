options(stringsAsFactors = FALSE)
setwd('D:/R_files/shiny/shinyforce/')

source('funcs/mapper.R')

library(shiny)
library(shinydashboard)
library(ggplot2)
library(grid)
library(png)

alex <- readPNG('D:/R_files/shiny/shinyforce/sprites/alex_clear.png')
ivan <- readPNG('D:/R_files/shiny/shinyforce/sprites/ivan_clear.png')


header <- dashboardHeader(title='SHINY FORCE')


sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("", tabName="game", icon = icon("dashboard")),
    menuItem("I Dunno", icon=icon("th"), tabName = "dunno")
  )
)


body <- dashboardBody(
    
  ## suppress all error messages
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  
  tabItems(
    tabItem(tabName='game',
      textOutput('whos_turn'),
      br(),
      br(),
      fluidRow(
        column(width = 4,
          plotOutput("playgrid", height=600, width=600,
                      click = "grid_click"
          )
        )
      ),
      fluidRow(
        column(width = 6,
          h4("Messages"),
          verbatimTextOutput("turn_out"),
          verbatimTextOutput("team_out")
        )
      )
    ),
    tabItem(tabName='dunno',
            h2('meh')
    )        
  )
)
    
suppressWarnings(dashboardPage(header, sidebar, body))                            
    
