## initial conditions
master_frame <- data.frame('char' = c('Alex', 'Tex', 'Ivan', 'Rocko'),
                           'team' = c('shiny', 'shiny', 'sas', 'sas'),
                           'xloc' = c(1, 2, 4, 5),
                           'yloc' = c(1, 2, 4, 5),
                           'cell' = c(1.1, 2.2, 4.4, 5.5),
                           'move' = c(3, 2, 2, 1),
                           'attk' = c(1, 3, 2, 4),
                           'health' = c(100, 100, 100, 100),
                           'icon' = c('/srv/shiny-server/shinyforce/sprites/alex_clear.png',
                                      '/srv/shiny-server/shinyforce/sprites/tex.png',
                                      '/srv/shiny-server/shinyforce/sprites/ivan_clear.png',
                                      '/srv/shiny-server/shinyforce/sprites/rocko.png')
                           
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
char_clicked  <<- FALSE ## has the current character been clicked on yet?
char_moved    <<- FALSE ## has the current character been moved AFTER being clicked?
char_attacked <<- FALSE ## has the current character attacked AFTER being clicked?
turn_order    <<- rep(c('Alex', 'Ivan', 'Tex', 'Rocko'), 5)   
turn_index    <<- 1
char_team     <<- subset(master_frame, char==turn_order[1])$team  


shinyServer(function(input, output, session) {

  ## functionalize ggplot calls
  make_plot <- function(grid, frame, scope, moves=NULL, atks=NULL)
  {
    p <- ggplot() +
      geom_polygon(data=grid, mapping=aes(x=x, y=y, group=cell), color='black', fill=NA)

    if(scope=='pre_move')
    {
      message('PRE MOVE')
      p <- p + geom_polygon(data=subset(grid, cell %in% moves$cell),
                               mapping=aes(x=x, y=y, group=cell),
                               color='black',
                               fill='#ccff99') 
    }
    
    if(scope=='post_move')
    {
      message('POST MOVE')
      p <- p + geom_polygon(data=subset(grid, cell %in% atks$cell),
                            mapping=aes(x=x, y=y, group=cell),
                            color='black',
                            fill='#ff6666')
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
    

  # reload_flag <<- TRUE
  # if(reload_flag)
  # {
  #   session$reload()
  #   reload_flag <<- FALSE
  # }
  
  
  ## intro slide
  output$slick_intro <- renderSlickR({
    pngs <- dir('sprites')[2:5]
    pngs <- paste0(getwd(), '/sprites/', pngs)
    slick <- slickR(obj=pngs, height='400px', width='800px')
    slick + settings(infinite=FALSE)
  })
  
  ## initial grid
  output$playgrid <- renderPlot({
    make_plot(gpoly, master_frame, 'initial_positions')
  })
  

  ## whos turn?
  output$whos_turn <- renderText({
    paste0(turn_order[turn_index], ", it's your turn")
  })
  
  output$current_char_icon <- renderImage({
    image <- subset(master_frame, char == turn_order[turn_index])$icon
    list(src = image,
         width = 300,
         height = 50)
  }, deleteFile = FALSE)
    
  output$current_char_health <- renderText({
    paste0(turn_order[turn_index], ' has ', subset(master_frame, char == turn_order[turn_index])$health, ' remaining')
  })
  
  ## one observer to constantly watch plot clicks?
  observeEvent(input$grid_click$x,{

    x_click <- floor(input$grid_click$x)
    y_click <- floor(input$grid_click$y)
    
    ## whos turn?
    output$whos_turn <- renderText({
      paste0(turn_order[turn_index], ", it's your turn")
    })
    
    ## update current char box
    output$current_char_icon <- renderImage({
      image <- subset(master_frame, char == turn_order[turn_index])$icon
      list(src = image,
           width = 300,
           height = 50)
    }, deleteFile = FALSE)
    
    output$current_char_health <- renderText({
      paste0(turn_order[turn_index], ' has ', subset(master_frame, char == turn_order[turn_index])$health, ' remaining')
    })
    
    output$last_cell_clicked <- renderText({
      print(paste0('Last Cell Clicked:  ', paste0('(', x_click, ',', y_click, ')')))
    })      
      
      
    ## current positions of characters
    char_curr <<- turn_order[turn_index]
    char_pos <<- subset(master_frame, char==char_curr)
    char_team <<- char_pos$team
    obstacs <<- subset(master_frame, !char == char_curr)$cell
    moves <<- loc_map(char_pos$move, char_pos$xloc, char_pos$yloc, obstacs, focus='blockers', grid_size)
    
    print(paste0(char_curr, ' SELECTED IS ', char_clicked))


        
    ## move character, must come before select character
    if(char_clicked & nrow(subset(moves, x==x_click & y==y_click)) > 0)
    {
      print(unique(moves$cell))
      print(paste0("MOVE ", char_curr, " IS TRUE"))
      print(paste0('x_click:  ', x_click))
      print(paste0('y_click:  ', y_click))
      
      ## update location
      master_frame$xloc[which(master_frame$char == char_curr)] <<- x_click
      master_frame$yloc[which(master_frame$char == char_curr)] <<- y_click
      master_frame$cell[which(master_frame$char == char_curr)] <<- as.numeric(paste0(x_click, '.', y_click))
      
      alex_pos <<- subset(master_frame, char=='Alex')
      tex_pos <<- subset(master_frame, char=='Tex')
      ivan_pos <<- subset(master_frame, char=='Ivan')
      rocko_pos <<- subset(master_frame, char=='Rocko')
      attk_pos <<- subset(master_frame, char==char_curr)
      
      ## check for possible attacks
      mobs <- subset(master_frame, team != char_team)$cell
      print(paste("MOBS:  ", mobs))
      
      atks <- loc_map(attk_pos$attk, attk_pos$xloc, attk_pos$yloc, mobs, focus='targets', grid_size)
      
      ## move character
      ## UPDATE PLOT
      output$playgrid <- renderPlot({
        p <- make_plot(gpoly, master_frame, scope='post_move', atks=atks)
        return(p)
      })

      ## PICK UP HERE!!!!    ATTACKING!!  
      if(nrow(atks) > 0)
      {
        observeEvent(input$grid_click$x,{
          
          x_click2 <- floor(input$grid_click$x)
          y_click2 <- floor(input$grid_click$y)
          
          xy_cell <- paste0(x_click2, '.', y_click2)
          
          if(nrow(subset(gpoly, mobs %in% xy_cell)))
          {
            master_frame$health[which(master_frame$char == char_curr)] <<- master_frame$health[which(master_frame$char == char_curr)] - 10
          }
        })
      }
         
      
      ## increment turn order
      turn_index <<- turn_index + 1
      char_curr <<- turn_order[turn_index]    
      char_team <<- subset(master_frame, team != char_team)$team[1]
      char_pos <<- subset(master_frame, char==char_curr)
      
      output$team_out <- renderPrint({
        print(paste0('Current team:  ', char_team))
      }) 
      
      char_clicked <<- FALSE  ## reset after move complete
      print(paste0("AFTER MOVE:   char_curr = ", char_curr))

    } 
      
    ## select CHAR, display valid moves
    if(!char_clicked & x_click==char_pos$xloc & y_click==char_pos$yloc)
    {

      print(paste0("SELECT ", char_curr, " IS TRUE"))
      print(paste0('current position: x=', x_click, '  y=', y_click))
      
      moves <- loc_map(char_pos$move, char_pos$xloc, char_pos$yloc, obstacs, focus='blockers', grid_size)
      print(unique(moves$cell))
      
      alex_pos <<- subset(master_frame, char=='Alex')
      tex_pos <<- subset(master_frame, char=='Tex')
      ivan_pos <<- subset(master_frame, char=='Ivan')
      rocko_pos <<- subset(master_frame, char=='Rocko')

      ## UPDATE PLOT
      output$playgrid <- renderPlot({
        p <- make_plot(gpoly, master_frame, scope='pre_move', moves=moves)
        return(p)
      })
      
      char_clicked <<- TRUE
        
    } 
      
    
    output$turn_out <- renderPrint({
      print(paste0('Turn index:  ', turn_index))
    })
    
  })
  
  

})