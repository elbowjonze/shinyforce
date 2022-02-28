## initial conditions
master_frame <<- data.frame("char" = c('Alex', 'Ivan'),
                            "team" = c('shiny', 'sas'),
                            "xloc" = c(1, 5),
                            "yloc" = c(1, 5),
                            "cell" = c(1.1, 5.5),
                            "move" = c(3, 2),
                            "attk" = c(1, 3)
)

alex <- readPNG('/srv/shiny-server/shinyforce/sprites/alex_clear.png')
ivan <- readPNG('/srv/shiny-server/shinyforce/sprites/ivan_clear.png')

## play grid - defined as ggplot polygons
grid_size <- 16  ## number of cells in each row/col
gpoly <- NULL
for(x in 0:(grid_size - 1))
{
  for(y in 0:(grid_size - 1))
  {
    cell <- paste0(x+1, '.', y+1)
    
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
gpoly <- subset(gpoly, y <= 8)

## initialize vars
char_clicked <<- FALSE ## has the char_curr been clicked on yet?
char_moved   <<- FALSE ## has the char_curr been moved AFTER being clicked?
turn_order <<- rep(c('Alex', 'Ivan'), 5)
turn_index <<- 1
char_team <<- subset(master_frame, char==turn_order[1])$team



shinyServer(function(input, output, session) {

  ## intro slide
  output$slick_intro <- renderSlickR({
    setwd('/srv/shiny-server/shinyforce')
    pngs <- dir('sprites')[2:7]
    pngs <- paste0('/srv/shiny-server/shinyforce/sprites/', pngs)
    slick <- slickR(obj=pngs, height='400px', width='800px')
    slick + settings(infinite=FALSE)
  })
  
  ## initial grid
  output$playgrid <- renderPlot({
    ggplot() +
      geom_polygon(data=gpoly, mapping=aes(x=x, y=y, group=cell), color='black', fill=NA) +
      annotation_raster(alex, xmin=1, xmax=2, ymin=1, ymax=2) +
      annotation_raster(ivan, xmin=5, xmax=6, ymin=5, ymax=6)
  })
  

  ## whos turn?
  output$whos_turn <- renderText({
    paste0(turn_order[turn_index], ", it's your turn")
  })
  
  ## user char selection
  observeEvent(input$grid_click$x,{
 
    ## whos turn?
    output$whos_turn <- renderText({
      paste0(turn_order[turn_index], ", it's your turn")
    })
    
    
    x_click <- floor(input$grid_click$x)
    y_click <- floor(input$grid_click$y)
    
    ## current positions of characters
    char_curr <- turn_order[turn_index]
    char_pos <- subset(master_frame, char==char_curr)
    char_team <- char_pos$team
    obstacs <- subset(master_frame, !char == char_curr)$cell
    moves <- loc_map(char_pos$move, char_pos$xloc, char_pos$yloc, obstacs, focus='blockers', grid_size)
    
    print(paste0(char_curr, ' SELECTED IS ', char_clicked))
    
    ## move character, must come before select character
    if(char_clicked & nrow(subset(moves, x==x_click & y==y_click)) > 0)
    {
      print(paste0("MOVE ", char_curr, " IS TRUE"))
      print(paste0('x_click:  ', x_click))
      print(paste0('y_click:  ', y_click))
      print(loc_map(char_pos$move, char_pos$xloc, char_pos$yloc, obstacs, focus='blockers', grid_size))
      
      ## update location
      master_frame$xloc[which(master_frame$char == char_curr)] <<- x_click
      master_frame$yloc[which(master_frame$char == char_curr)] <<- y_click
      master_frame$cell[which(master_frame$char == char_curr)] <<- as.numeric(paste0(x_click, '.', y_click))
      
      alex_pos <- subset(master_frame, char=='Alex')
      ivan_pos <- subset(master_frame, char=='Ivan')
      attk_pos <- subset(master_frame, char==char_curr)
      
      ## check for possible attacks
      mobs <- subset(master_frame, team != char_team)$cell
      print(paste("MOBS:  ", mobs))
      
      atks <- loc_map(attk_pos$attk, attk_pos$xloc, attk_pos$yloc, mobs, focus='blockers', grid_size)
      
      # print(paste0("ATTACK "))
      # print(loc_map(char_pos$attk, char_pos$xloc, char_pos$yloc, obstacs, focus='targets', grid_size))
      
      
      ## move character
      output$playgrid <- renderPlot({
        ggplot() +
            geom_polygon(data=gpoly, mapping=aes(x=x, y=y, group=cell), color='black', fill=NA) +
            geom_polygon(data=subset(gpoly, cell %in% atks$cell),
                         mapping=aes(x=x, y=y, group=cell),
                         color='black',
                         fill='#ff6666') +          
            annotation_raster(alex, xmin=alex_pos$xloc, xmax=alex_pos$xloc+1, ymin=alex_pos$yloc, ymax=alex_pos$yloc+1) +
            annotation_raster(ivan, xmin=ivan_pos$xloc, xmax=ivan_pos$xloc+1, ymin=ivan_pos$yloc, ymax=ivan_pos$yloc+1)
      })
      
      
      # print(paste0('current position: x=', x_click, '  y=', y_click))
      # print(paste0('char_pos for ', char_curr))
      # print(char_pos)
      # print(subset(master_frame, char==char_curr))
      
      print(paste0(char_curr, ' TURN DONE, NOW ',  turn_order[turn_index + 1], ' TURN'))
      
      turn_index <<- turn_index + 1
      char_curr <<- turn_order[turn_index]    
      char_team <<- subset(master_frame, team != char_team)$team[1]
      char_pos <<- subset(master_frame, char==char_curr)
      
      output$team_out <- renderPrint({
        print(paste0('Current team:  ', char_team))
      }) 
      
      char_clicked <<- FALSE
      print(paste0("AFTER MOVE:   char_curr = ", char_curr))

    } 
      
    print(paste0('INTERMEDIATE CHAR:   ', char_curr))
    print(paste0('INTERMEDIATE CHAR CLICKED:   ', char_clicked))
    print(paste0('INTERMEDIATE INDEX:   ', turn_index))
    
    ## select CHAR, display valid moves
    if(!char_clicked & x_click==char_pos$xloc & y_click==char_pos$yloc)
    {

      print(paste0("SELECT ", char_curr, " IS TRUE"))
      print(paste0('current position: x=', x_click, '  y=', y_click))
      
      moves <- loc_map(char_pos$move, char_pos$xloc, char_pos$yloc, obstacs, focus='blockers', grid_size)
      
      alex_pos <- subset(master_frame, char=='Alex')
      ivan_pos <- subset(master_frame, char=='Ivan')
      
      output$playgrid <- renderPlot({
        ggplot() +
            geom_polygon(data=gpoly, mapping=aes(x=x, y=y, group=cell), color='black', fill=NA) +
            geom_polygon(data=subset(gpoly, cell %in% moves$cell),
                         mapping=aes(x=x, y=y, group=cell),
                         color='black',
                         fill='#ccff99') +
            annotation_raster(alex, xmin=alex_pos$xloc, xmax=alex_pos$xloc+1, ymin=alex_pos$yloc, ymax=alex_pos$yloc+1) +
            annotation_raster(ivan, xmin=ivan_pos$xloc, xmax=ivan_pos$xloc+1, ymin=ivan_pos$yloc, ymax=ivan_pos$yloc+1)
      })   
      
      char_clicked <<- TRUE
        
    } 
      
    # print(paste0('hit flag:  ', hit_flag))
    # if(hit_flag == 1)
    # {
    #   char_hit <<- !char_hit
    #   hit_flag <- 0
    # }
    
    output$turn_out <- renderPrint({
      print(paste0('Turn index:  ', turn_index))
    })
    
  })
  
  

})