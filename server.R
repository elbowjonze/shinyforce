shinyServer(function(input, output, session) {
    
    ## initial conditions
    init_loc <- data.frame("char" = c('Alex', 'Ivan'),
                           "xloc" = c(1, 5),
                           "yloc" = c(1, 5)
    )
    
    alex_pos <<- subset(init_loc, char=='Alex')
    print('alex_pos')
    print(alex_pos)
    
    ivan_pos <<- subset(init_loc, char=='Ivan')

    
    ## calculate valid movement spaces
    move_map <- function(steps, x, y)
    {
        valid_moves <- NULL
        valid_moves <- data.frame('x'=integer(), 'y'=integer(), 'cell'=character())
        for(i in 1:steps)
        {
            ## move orthoginally
            valid_moves <- rbind(valid_moves,
                                 data.frame('x' = c(x+i, x-i, x, x),
                                            'y' = c(y, y, y+i, y-i),
                                            'cell' = c(paste0(x+i, '.', y),
                                                       paste0(x-i, '.', y),
                                                       paste0(x,   '.', y+i),
                                                       paste0(x,   '.', y-i)
                                            )
                                 )
            )
            
            ## one step diagonally
            if(i == 2)
            {
                valid_moves <- rbind(valid_moves,
                                     data.frame('x' = c(x+1, x+1, x-1, x-1),
                                                'y' = c(y+1, y-1, y-1, y+1),
                                                'cell' = c(paste0(x+1, '.', y+1),
                                                           paste0(x+1, '.', y-1),
                                                           paste0(x-1, '.', y-1),
                                                           paste0(x-1, '.', y+1)
                                                )
                                     )
                )
            }
            
            ## move like a knight
            if(i == 3)
            {
                valid_moves <- rbind(valid_moves,
                                     data.frame('x' = c(x+2, x+2, x-2, x-2, x+1, x-1, x+1, x-1),
                                                'y' = c(y+1, y-1, y-1, y+1, y+2, y+2, y-2, y-2),
                                                'cell' = c(paste0(x+2, '.', y+1), 
                                                           paste0(x+2, '.', y-1),
                                                           paste0(x-2, '.', y-1),
                                                           paste0(x-2, '.', y+1),                                 
                                                           paste0(x+1, '.', y+2),
                                                           paste0(x-1, '.', y+2),
                                                           paste0(x+1, '.', y-2),
                                                           paste0(x-1, '.', y-2))
                                     )
                )
            }
        }
        
        valid_moves$x <- as.integer(valid_moves$x)
        valid_moves$y <- as.integer(valid_moves$y)
        valid_moves <- subset(valid_moves, x >=1 & x<= 8 & y >=1 & y<=8)
        
        return(valid_moves)
    }
        
        
    ## polygons
    gpoly <- NULL
    for(x in 0:7)
    {
        for(y in 0:7)
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
    
    ## initial grid
    output$playgrid <- renderPlot({
        ggplot() +
            geom_polygon(data=gpoly, mapping=aes(x=x, y=y, group=cell), color='black', fill=NA) +
            annotation_raster(alex, xmin=1, xmax=2, ymin=1, ymax=2) +
            annotation_raster(ivan, xmin=5, xmax=6, ymin=5, ymax=6)
    })
    
    
    ## -------------------------------
    ## REACTIVE STUFF
    ## -------------------------------
    char_selected <- reactiveValues()
    
    
    
    ## user char selection
    observeEvent(input$grid_click$x,{
        ## select Alex, update plot
        if(floor(input$grid_click$x)==alex_pos$xloc & floor(input$grid_click$y)==alex_pos$xloc)
        {
            ## calculate valid moves
            #curr_loc <- paste0(alex_pos$xloc, '.', alex_pos$yloc)
            moves <- move_map(3, alex_pos$xloc, alex_pos$yloc)
            print(moves)
            
            
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
            
        } 

        ## select Ivan, update plot
        if(floor(input$grid_click$x)==ivan_pos$xloc & floor(input$grid_click$y)==ivan_pos$xloc)
        {
            ## calculate valid moves
            #curr_loc <- paste0(ivan_pos$xloc, '.', ivan_pos$yloc)
            moves <- move_map(2, ivan_pos$xloc, ivan_pos$yloc)
            print(moves)
            
            
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
            
        }                
    })
            
    
    
    
    output$cell_click <- renderPrint({
        xval <- floor(input$grid_click$x)
        yval <- floor(input$grid_click$y)
        
        paste0(xval, ',', yval)
    })
})