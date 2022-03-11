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
          plotOutput('playgrid', height=400, width=400,
                     click = 'grid_click',
                     hover = 'grid_hover'
          )
        ),
        column(width = 2),
        column(width = 4,
          imageOutput('current_char_icon'),
          verbatimTextOutput('current_char_health')
        )
      ),
      fluidRow(
        column(width = 6,
          h4("Messages"),
          verbatimTextOutput("last_cell_clicked"),
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
    
