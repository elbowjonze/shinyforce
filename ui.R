options(stringsAsFactors = FALSE)

source('funcs/mapper.R')

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyBS)
library(shinyjs)
library(slickR)
library(ggplot2)
library(grid)
library(png)

header <- dashboardHeader(title='SHINY FORCE')

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("game", tabName="game"),
    menuItem("placeholder", icon=icon("th"), tabName = "more_stuff")
  )
)


body <- dashboardBody(
    
  useShinyjs(), 
  
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
      fluidRow(
        column(width = 4,
          bsModal('modal_intro', 'PLACEHOLDER MODAL FOR INTRO SLIDE DECK', 'intro_slides', size='large',
                  slickROutput('slick_intro', height='400px', width='800px')
          ),
          plotOutput('playgrid', height=400, width=400,
                     click = 'grid_click',
                     hover = 'grid_hover'
          ),
          verbatimTextOutput('helper1'),
          verbatimTextOutput('helper2'),
          tableOutput('helper3')
        ),
        column(width = 2),
        column(width = 4,
          textOutput('whos_turn'),
          br(),
          imageOutput('current_char_icon', height='50px', width='50px'),
          br(),
          verbatimTextOutput('current_char_health'),
          br(),
          actionBttn('move_button',
                     label = 'Move',
                     style = 'material-flat'),
          br(),
          br(),
          actionBttn('atk_button',
                      label = 'Attack',
                      style = 'material-flat'),
          verbatimTextOutput('no_atk_msg'),
          br(),
          actionBttn('end_turn',
                     label = 'End Turn',
                     style = 'material-flat')
        )
      )
    ),
    tabItem(tabName='more_stuff',
            h2('more content!')
    )        
  )
)
    
suppressWarnings(dashboardPage(header, sidebar, body))                            
    
