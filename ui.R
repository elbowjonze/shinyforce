## git test
options(stringsAsFactors = FALSE)

source('funcs/mapper.R')

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyBS)
library(slickR)
library(ggplot2)
library(grid)
library(png)

alex <- readPNG('./sprites/alex_clear.png')
ivan <- readPNG('./sprites/ivan_clear.png')


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
      actionButton('intro_slides', 'Prologue'),
      br(),
      br(),
      textOutput('whos_turn'),
      br(),
      br(),
      fluidRow(
        column(width = 4,
          bsModal('modal_intro', 'IT BEGINS!!', 'intro_slides', size='large',
                  slickROutput('slick_intro', height='400px', width='800px')
          ),
          plotOutput("playgrid", height=400, width=800,
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
    
