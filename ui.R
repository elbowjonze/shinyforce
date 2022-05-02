options(stringsAsFactors = FALSE)
options(scipen=999)

source('funcs/mapper.R')

library(shiny)
library(shinyWidgets)
library(shinyBS)
library(shinyjs)
library(slickR)
library(ggplot2)
library(grid)
library(png)
library(DT)


shinyUI(
  fluidPage(
    
    # tags$style(
    #   HTML(".tabbable > .nav > li[class=active] > a {
    #        background-color: #000;
    #        color: #FFF;
    #     }")),
    
    tags$style(HTML('thead { display:none; }' )),  ## removes header from narration datatable

    useShinyjs(), 
    useSweetAlert(), 
    
    ## suppress all error messages
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    br(),
    br(),
    actionButton('intro_slides', 'Prologue'),
    # bsModal('atk_modal', 'TESTING', 'no_trigger', size='large',   ## combat modal
    #         verbatimTextOutput('whos_fighting'),
    #         plotOutput('atk_plot'),
    #         actionBttn('flip_vertical_button',
    #                    label = 'Flip',
    #                    style = 'material-flat',
    #                    icon = icon('exchange')),
    #         actionBttn('flip_horizontal_button',
    #                    label = 'Invert',
    #                    style = 'material-flat',
    #                    icon = icon('retweet')),
    #         actionBttn('atk_roll', 'Attack!'),
    #         verbatimTextOutput('atk_value')
    # ),
    
    ## testing UI stuff
    bsModal('atk_modal', 'TESTING', 'no_trigger', size='large',   ## combat modal
            verbatimTextOutput('whos_fighting'),
            plotOutput('atk_plot'),
            actionBttn('flip_vertical_button',
                       label = 'Flip',
                       style = 'material-flat',
                       icon = icon('exchange')),
            actionBttn('flip_horizontal_button',
                       label = 'Invert',
                       style = 'material-flat',
                       icon = icon('retweet')),
            actionBttn('atk_roll', 'Attack!'),
            verbatimTextOutput('atk_value')
    ),    
    
    
    br(),
    br(),
    fluidRow(
      #verbatimTextOutput('helper1'),
      column(width = 3,
        tabsetPanel(id='char_tabs', type='tabs',
          tabPanel('Alex', value='Alex'),
          tabPanel('Tex', value='Tex'),
          tabPanel('Ivan', value='Ivan'),
          tabPanel('Rocko', value='Rocko')
        ),
        br(),
        imageOutput('char_icon', height='50px', width='50px'),
        br(),
        verbatimTextOutput('char_health'),
        verbatimTextOutput('char_move'),
        verbatimTextOutput('char_atk'),
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
        # actionBttn('flip_vertical_button',
        #            label = 'Flip',
        #            style = 'material-flat',
        #            icon = icon('exchange')),
        # actionBttn('flip_horizontal_button',
        #            label = 'Invert',
        #            style = 'material-flat',
        #            icon = icon('retweet')),
        # br(),
        # br(),
        actionBttn('end_turn',
                   label = 'End Turn',
                   style = 'material-flat')
      ),
      column(width = 1),
      column(width = 7,
        bsModal('modal_intro', 'PLACEHOLDER MODAL FOR INTRO SLIDE DECK', 'intro_slides', size='large',
                slickROutput('slick_intro', height='400px', width='800px')
        ),
        plotOutput('playgrid', height=400, width=400,
                   click = 'grid_click',
                   hover = 'grid_hover'
        ),
        br(),
        h3('BATTLE LOG'),
        fluidRow(
          column(width = 7, DTOutput('narrator'))
        )
      )
    )
  )
)
    
    
