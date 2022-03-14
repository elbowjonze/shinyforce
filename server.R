## initial conditions
master_frame <- data.frame('char' = c('Alex', 'Tex', 'Ivan', 'Rocko'),
                           'team' = c('shiny', 'shiny', 'sas', 'sas'),
                           'xloc' = c(1, 2, 4, 5),
                           'yloc' = c(1, 2, 4, 5),
                           'cell' = c(1.1, 2.2, 4.4, 5.5),
                           'move' = c(3, 2, 2, 1),
                           'attk' = c(1, 3, 2, 3),
                           'health' = c(100, 100, 100, 100),
                           'icon' = c('/srv/shiny-server/shinyforce/sprites/alex_clear.png',
                                      '/srv/shiny-server/shinyforce/sprites/tex_clear.png',
                                      '/srv/shiny-server/shinyforce/sprites/ivan_clear.png',
                                      '/srv/shiny-server/shinyforce/sprites/rocko_clear.png')
                           
)


## generate play grid - defined as ggplot polygons
grid_size <- 8  ## number of cells in each row/col
gpoly <- NULL
for(x in 0:(grid_size - 1))
{
  for(y in 0:(grid_size - 1))
  {
    cell <- paste0(x, '.', y)
    
    gpoly <- rbind(gpoly, c(x, y, cell))        ## bottom left
    gpoly <- rbind(gpoly, c(x+1, y, cell))      ## bottom right
    gpoly <- rbind(gpoly, c(x+1, y+1, cell))    ## top right
    gpoly <- rbind(gpoly, c(x, y+1, cell))      ## top left
  }
}
gpoly <- as.data.frame(gpoly)
names(gpoly) <- c('x', 'y', 'cell')    
gpoly$y <- as.integer(gpoly$y)
gpoly$x <- as.integer(gpoly$x)


## initialize vars
char_moved    <<- TRUE ## has the current character been moved AFTER being clicked?
char_attacked <<- TRUE ## has the current character attacked AFTER being clicked?
turn_order    <<- rep(c('Alex', 'Ivan', 'Tex', 'Rocko'), 25)   
turn_index    <<- 1
char_curr     <<- turn_order[turn_index]
char_team     <<- subset(master_frame, char==turn_order[1])$team  
atks          <<- master_frame[0,]

## functionalize ggplot calls
make_plot <- function(grid, frame, scope=NULL, moves=NULL, atks=NULL)
{
  p <- ggplot() +
    geom_polygon(data=grid, mapping=aes(x=x, y=y, group=cell), color='black', fill=NA)
  
  if(!is.null(scope))
  {
    if(scope=='show_move')
    {
      p <- p + geom_polygon(data=subset(grid, cell %in% moves$cell),
                            mapping=aes(x=x, y=y, group=cell),
                            color='black',
                            fill='#ccff99') 
    }
    
    if(scope=='show_atk')
    {
      p <- p + geom_polygon(data=subset(grid, cell %in% atks$cell),
                            mapping=aes(x=x, y=y, group=cell),
                            color='black',
                            fill='#ff6666')
    }
  }
  
  ## loop through all non-dead chars
  for(i in 1:nrow(frame))
  {
    p <- p + annotation_raster(readPNG(master_frame$icon[i]), 
                               xmin=frame$xloc[i], 
                               xmax=frame$xloc[i] + 1, 
                               ymin=frame$yloc[i], 
                               ymax=frame$yloc[i] + 1)
  }
  
  return(p)
}


shinyServer(function(input, output, session) {
  
  ## intro slides
  output$slick_intro <- renderSlickR({
    pngs <- dir('sprites')[2:5]
    pngs <- paste0(getwd(), '/sprites/', pngs)
    slick <- slickR(obj=pngs, height='400px', width='800px')
    slick + settings(infinite=FALSE)
  })
  
  ## --------------------------------------
  ## INITIAL BOARD GENERATION
  ## --------------------------------------
  ## generate initial grid
  output$playgrid <- renderPlot({
    make_plot(gpoly, master_frame)
  })
  
  ## generate initial callout for current chars turn
  output$whos_turn <- renderText({
    paste0(turn_order[turn_index], ", it's your turn")
  })
  
  ## generate initial current char detail panel
  output$current_char_icon <- renderImage({
    image <- subset(master_frame, char == turn_order[turn_index])$icon
    list(src = image,
         width = 50,
         height = 50)
  }, deleteFile = FALSE)
    
  output$current_char_health <- renderText({
    paste0(turn_order[turn_index], ' has ', subset(master_frame, char == turn_order[turn_index])$health, ' health remaining')
  })
  
  
  ## one observer to constantly watch plot clicks?
  observeEvent(input$grid_click$x,{

    x_click <- floor(input$grid_click$x)
    y_click <- floor(input$grid_click$y)
    xy_cell <- paste0(x_click, '.', y_click)
    
    ## current positions of characters
    char_curr <<- turn_order[turn_index]
    char_pos <<- subset(master_frame, char==char_curr)
    char_team <<- char_pos$team
    obstacs <<- subset(master_frame, !char == char_curr)$cell
    moves <- loc_map(char_pos$move, char_pos$xloc, char_pos$yloc, obstacs, focus='blockers', grid_size)
    
    ## move char within valid set of locations
    if(nrow(subset(moves, x==x_click & y==y_click)) > 0 & char_moved==FALSE)
    {
      
      ## update location
      master_frame$xloc[which(master_frame$char == char_curr)] <<- x_click
      master_frame$yloc[which(master_frame$char == char_curr)] <<- y_click
      master_frame$cell[which(master_frame$char == char_curr)] <<- as.numeric(paste0(x_click, '.', y_click))
      
      ## UPDATE PLOT with possible moves
      output$playgrid <- renderPlot({
        make_plot(gpoly, master_frame, scope='show_move', atks=atks)
      })
      
      # output$move_button <- renderUI({
      #   actionBttn('move_button',
      #              label = 'Move Complete',
      #              style = 'material-flat')
      # })
      
      char_moved <<- TRUE
      shinyjs::disable('move_button')
    }
    
    ## ATTACKING!!
    if(nrow(subset(atks, x==x_click & y==y_click)) > 0 & char_attacked==FALSE)
    {
      if(nrow(subset(gpoly, mobs %in% xy_cell)))
      {
        target <- master_frame$char[which(master_frame$cell == xy_cell)]
        master_frame$health[which(master_frame$char == target)] <<- master_frame$health[which(master_frame$char == target)] - 10
        
        ## remove red attack highlights
        output$playgrid <- renderPlot({
          make_plot(gpoly, master_frame)
        })
        
        char_attacked <<- TRUE 
        shinyjs::disable('atk_button')
      }
    }
    
  })
  

  # ## define buttons
  # output$move_button <- renderUI({
  #   if(turn_index==1 | char_moved==FALSE)
  #   {
  #     actionBttn('move_button',
  #                 label = 'Move',
  #                 style = 'material-flat')
  #   }
  # })
  # 
  # output$atk_button <- renderUI({
  #   if(turn_index==1 | char_attacked==FALSE)
  #   {
  #     actionBttn('atk_button',
  #                label = 'Attack',
  #                style = 'material-flat')
  #   }
  # })
  # 
  # 
  # 
  #   
  # output$move_status <- renderPrint({
  #   paste0('char_moved == ', char_moved)
  # })
  # 
  # output$attack_status <- renderPrint({
  #   paste0('char_attacked == ', char_attacked)
  # })  
    
  
  
    
  ## MOVEMENT 
  observeEvent(input$move_button, {
    char_moved <<- FALSE  
    moves <- loc_map(char_pos$move, char_pos$xloc, char_pos$yloc, obstacs, focus='blockers', grid_size)
    
    ## UPDATE PLOT
    output$playgrid <- renderPlot({
      make_plot(gpoly, master_frame, scope='show_move', moves=moves)
    })
  })
    
    
  ## ATTACKING
  observeEvent(input$atk_button, {
    char_attacked <<- FALSE 
    attk_pos <<- subset(master_frame, char==char_curr)
    
    ## check for possible attacks
    mobs <<- subset(master_frame, team != char_team)$cell
    atks <<- loc_map(attk_pos$attk, attk_pos$xloc, attk_pos$yloc, mobs, focus='targets', grid_size)
    
    ## UPDATE PLOT
    output$playgrid <- renderPlot({
      make_plot(gpoly, master_frame, scope='show_atk', atks=atks)
    })
  })    
    
    
  
  ## hit END TURN button, increment turn order
  observeEvent(input$end_turn, {
    
    turn_index <<- turn_index + 1
    char_curr  <<- turn_order[turn_index]    
    char_team  <<- subset(master_frame, team != char_team)$team[1]
    char_pos   <<- subset(master_frame, char==char_curr)
    
    output$whos_turn <- renderText({
      paste0(turn_order[turn_index], ", it's your turn")
    })
    
    ## update current char detail panel
    output$current_char_icon <- renderImage({
      image <- subset(master_frame, char == turn_order[turn_index])$icon
      list(src = image,
           width = '50px',
           height = '50px')
    }, deleteFile = FALSE)
    
    output$current_char_health <- renderText({
      paste0(turn_order[turn_index], ' has ', subset(master_frame, char == turn_order[turn_index])$health, ' health remaining')
    })

    enable('move_button')
    enable('atk_button')
  })   
    
  
  #session$reload()
  
})