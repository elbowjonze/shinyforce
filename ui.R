options(stringsAsFactors = FALSE)
options(scipen=999)

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
library(LaplacesDemon)  ## for logit-normal distribution

header <- dashboardHeader(title='SHINY FORCE')

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("game", tabName="game"),
    menuItem("placeholder", icon=icon("th"), tabName = "more_stuff")
  )
)


body <- dashboardBody(
    
  useShinyjs(), 
  useSweetAlert(), 
  
  ## suppress all error messages
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  
  tabItems(
    tabItem(tabName='game',
      actionButton('intro_slides', 'Prologue'),
      bsModal('atk_modal', 'TESTING', 'no_trigger', size='large',
              verbatimTextOutput('whos_fighting'),
              plotOutput('atk_plot'),
              actionBttn('atk_roll', 'Attack!'),
              verbatimTextOutput('atk_value')
      ),
      br(),
      br(),
      fluidRow(
        column(width = 5,
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
        column(width = 1),
        column(width = 6,
          textOutput('whos_turn'),
          br(),
          imageOutput('current_char_icon', height='50px', width='50px'),
          br(),
          verbatimTextOutput('current_char_health'),
          verbatimTextOutput('current_char_move'),
          verbatimTextOutput('current_char_atk'),
          br(),
          actionBttn('move_button',
                     label = 'Move',
                     style = 'material-flat'),
          actionBttn('atk_button',
                     label = 'Attack',
                     style = 'material-flat'),
          verbatimTextOutput('no_atk_msg'),
          br(),
          br(),
          # actionBttn('rotate_right_button',
          #            label = 'Rotate',
          #            style = 'material-flat',
          #            icon = icon('redo')),
          # actionBttn('rotate_left_button',
          #            label = 'Rotate',
          #            style = 'material-flat',
          #            icon = icon('undo')),
          actionBttn('flip_vertical_button',
                     label = 'Flip',
                     style = 'material-flat',
                     icon = icon('exchange')),
          actionBttn('flip_horizontal_button',
                     label = 'Invert',
                     style = 'material-flat',
                     icon = icon('retweet')),
          br(),
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
    
