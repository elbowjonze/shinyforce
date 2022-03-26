# ## play around with this plotting code
# normal <- function(mu, sigma, x){
#   1/(sigma*sqrt(2*pi))*exp(-((x-mu)/sigma)^2)
# }
# 
# normal_shade <- function(mu, sigma, x, xmax){
#   y <- normal(mu=mu, sigma=sigma, x)
#   y[x < 0 | x > xmax] <- NA
#   return(y)
# }
# 
# xmin <- 0
# xmax <- 100
# mu <- 50
# sigma <- 15
# def_val <- 35
# 
# ggplot(data.frame(x=c(xmin, xmax)), aes(x=x, color=g)) +
#   stat_function(data=data.frame(x=c(xmin, xmax), g=factor(2)), fun=normal, geom='line',
#                 args=list(mu=mu, sigma=sigma)) +
#   stat_function(data=data.frame(x=c(xmin, xmax), g=factor(2)), fun=normal_shade, geom = 'area', fill = 'red', alpha = 0.2,
#                 args=list(mu=mu, sigma=sigma, xmax=def_val)) +
#   scale_x_continuous(breaks=seq(from=xmin, to=xmax, by=10)) +
#   scale_color_manual('',values=c('red', 'red')) +
#   theme(panel.background = element_rect(fill='white')
#   )
# 
# 
# 
# ## FLIP ABOUT X-AXIS
# xmin <- 0
# xmax <- 100
# mu <- 50
# sigma <- 15
# def_val <- 75
# 
# a_formula <- function(mu, sigma, x)
# {
#   -1/(sigma*sqrt(2*pi))*exp(-((x-mu)/sigma)^2)
# }
# 
# ## need to find min
# norm <- expression(-1/(sigma*sqrt(2*pi))*exp(-((root - mu)/sigma)^2))
# d_norm <- D(norm, 'x')
# 
# qq <- function(sigma, mu, x)
# {
#   -(-1/(sigma * sqrt(2 * pi)) * (exp(-((x - mu)/sigma)^2) * (2 * (1/sigma * ((x - mu)/sigma)))))
# }
# 
# root <- uniroot(qq, interval=c(0,100), sigma=sigma, mu=mu)$root   
# cutoff <- eval(norm)
# 
# 
# xs <- seq(xmin, def_val, length.out=100)
# ysmin <- rep(cutoff, length(xs))
# ysmax <- a_formula(mu, sigma, xs)
# 
# df2 <- data.frame(xs, ysmin, ysmax)
# zz <- data.frame(x=c(xmin, xmax), g=factor(2))
# 
# ggplot(data=zz) + 
#   stat_function(fun=a_formula, geom='line', args=list(mu=mu, sigma=sigma)) +
#   geom_ribbon(aes(x=xs, ymin=ysmin, ymax=ysmax), data=df2, fill="#BB000033") +
#   xlim(0, 100)


## -----------------------------------------------------------
## how to generate deviates from inverted distrubtion??
## -----------------------------------------------------------

## approximation may be best
##  - loop through x vals, capture y-val and generate a number of rows in a dataframe proportional to y-vals
##  - then we take random sample from this dataset
# 
# begin <- Sys.time()
# deviate_frame <- data.frame(x=numeric(),
#                             y=numeric()
# )
# 
# for(i in xmin:xmax)
# {
#   prob <- a_formula2(mu, sigma, i, cutoff)
#   
#   ## how many rows to add for this xval?
#   rowsout <- as.integer(round(prob * 10000,0))
#   templine <- c(i, prob)
#   
#   tempframe <- NULL
#   for(j in 1:rowsout)
#   {
#     tempframe <- rbind(tempframe, templine)
#   }
#   
#   deviate_frame <- rbind(deviate_frame, tempframe)
# }
# 
# rownames(deviate_frame) <- NULL
# names(deviate_frame) <- c('roll', 'prob')
# 
# 
# Sys.time() - begin  ## 1.2s - too slow, vectorize
# 
# 
# 
# begin <- Sys.time()
# qq <- a_formula2(mu, sigma, 1:100, cutoff)
# qq <- as.integer(round(qq*10000, 0))
# qq2 <- rep(1:100, qq)
# Sys.time() - begin  ## 0.0034s - bingo!   just sample from this vector
# 
# hist(qq2, breaks=100)
# 
# 
# ggplot(data=zz) + 
#   stat_function(fun=a_formula2, geom='line', args=list(mu=mu, sigma=sigma, cutoff=cutoff)) +
#   geom_ribbon(aes(x=xs, ymin=ysmin, ymax=ysmax), data=df2, fill="#BB000033") +
#   geom_vline(xintercept=sample(qq2, size=1)) +
#   xlim(0, 100)




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
    p <- p + annotation_raster(readPNG(frame$icon[i]), 
                               xmin=frame$xloc[i], 
                               xmax=frame$xloc[i] + 1, 
                               ymin=frame$yloc[i], 
                               ymax=frame$yloc[i] + 1)
  }
  
  return(p)
}


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

shinyServer(function(input, output, session) {

  
  
  ## initial conditions
  master_frame <- data.frame('char' = c('Alex', 'Tex', 'Ivan', 'Rocko'),
                             'team' = c('shiny', 'shiny', 'sas', 'sas'),
                             'xloc' = c(1, 2, 4, 5),
                             'yloc' = c(1, 2, 4, 5),
                             'cell' = c(1.1, 2.2, 4.4, 5.5),
                             'move' = c(3, 2, 2, 1),
                             'atk' = c(1, 3, 2, 3),
                             'health' = c(100, 100, 100, 100),
                             'icon' = c('./sprites/alex_clear.png',
                                        './sprites/tex_clear.png',
                                        './sprites/ivan_clear.png',
                                        './sprites/rocko_clear.png')
  )
  
  ## initialize vars
  char_moved    <- TRUE ## has the current character been moved AFTER being clicked?
  char_attacked <- TRUE ## has the current character attacked AFTER being clicked?
  turn_order    <- rep(c('Alex', 'Ivan', 'Tex', 'Rocko'), 25)   
  turn_index    <- 1
  char_curr     <- turn_order[turn_index]
  char_pos      <- subset(master_frame, char==char_curr)
  char_team     <- char_pos$team
  atks          <- master_frame[0,]
  
  
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
    paste0('Health: ', subset(master_frame, char == turn_order[turn_index])$health)
  })
  
  output$current_char_move <- renderText({
    paste0('Movement Range: ', subset(master_frame, char == turn_order[turn_index])$move)
  })
  
  output$current_char_atk <- renderText({
    paste0('Attack Range: ', subset(master_frame, char == turn_order[turn_index])$atk)
  })  
  
  ## intro slides
  output$slick_intro <- renderSlickR({
    pngs <- dir('sprites')[2:5]
    pngs <- paste0(getwd(), '/sprites/', pngs)
    slick <- slickR(obj=pngs, height='400px', width='800px')
    slick + settings(infinite=FALSE)
  })
  
  
  ## one observer to constantly watch plot clicks
  observeEvent(input$grid_click$x,{

    x_click <- floor(input$grid_click$x)
    y_click <- floor(input$grid_click$y)
    xy_cell <<- paste0(x_click, '.', y_click)
    
    ## current positions of characters
    char_curr <- turn_order[turn_index]
    char_pos  <- subset(master_frame, char==char_curr)
    char_team <- char_pos$team
    obstacs   <- subset(master_frame, !char == char_curr)$cell
    moves     <- loc_map(char_pos$move, char_pos$xloc, char_pos$yloc, obstacs, focus='blockers', grid_size)
    
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
      
      char_moved <<- TRUE
      updateActionButton(session, 'move_button', label='Move Complete')
      shinyjs::disable('move_button')
    }
    
    ## ATTACKING!!
    ## highlight valid targets
    if(nrow(subset(atks, x==x_click & y==y_click)) > 0 & char_attacked==FALSE)
    {
      ## if valid target is selected
      if(nrow(subset(gpoly, mobs %in% xy_cell)))
      {
        toggleModal(session, 'atk_modal')
        
        ## initial attack distribution plot
        output$atk_plot <- renderPlot({
          normal <- function(mu, sigma, x){
            1/(sigma*sqrt(2*pi))*exp(-((x-mu)/sigma)^2)
          }
          
          normal_shade <- function(mu, sigma, x, xmax){
            y <- normal(mu=mu, sigma=sigma, x)
            y[x < 0 | x > xmax] <- NA
            return(y)
          }
          
          xmin <- 0
          xmax <- 100
          mu <- 50
          sigma <- 15
          def_val <- 35
          
          p <- ggplot(data.frame(x=c(xmin, xmax)), aes(x=x, color=g)) +
            stat_function(data=data.frame(x=c(xmin, xmax), g=factor(2)), fun=normal, geom='line',
                          args=list(mu=mu, sigma=sigma)) +
            stat_function(data=data.frame(x=c(xmin, xmax), g=factor(2)), fun=normal_shade, geom = 'area', fill = 'red', alpha = 0.2,
                          args=list(mu=mu, sigma=sigma, xmax=def_val)) +
            scale_x_continuous(breaks=seq(from=xmin, to=xmax, by=10)) +
            scale_color_manual('',values=c('red', 'red')) +
            theme(panel.background = element_rect(fill='white'))
          
          return(p)
        })
      }
    }
  })
  
  observeEvent(input$atk_roll,{
    
    updateActionButton(session, 'atk_roll', label='Attack Complete')
    shinyjs::disable('atk_roll')
    
    ## random roll
    atk_val <- rnorm(1, mean=50, sd=15)
    
    output$atk_plot <- renderPlot({
      normal <- function(mu, sigma, x){
        1/(sigma*sqrt(2*pi))*exp(-((x-mu)/sigma)^2)
      }
      
      normal_shade <- function(mu, sigma, x, xmax){
        y <- normal(mu=mu, sigma=sigma, x)
        y[x < 0 | x > xmax] <- NA
        return(y)
      }
      
      xmin <- 0
      xmax <- 100
      mu <- 50
      sigma <- 15
      def_val <- 35
      
      p <- ggplot(data.frame(x=c(xmin, xmax)), aes(x=x, color=g)) +
        stat_function(data=data.frame(x=c(xmin, xmax), g=factor(2)), fun=normal, geom='line',
                      args=list(mu=mu, sigma=sigma)) +
        stat_function(data=data.frame(x=c(xmin, xmax), g=factor(2)), fun=normal_shade, geom = 'area', fill = 'red', alpha = 0.2,
                      args=list(mu=mu, sigma=sigma, xmax=def_val)) +
        geom_vline(xintercept=atk_val, color='green') +
        scale_x_continuous(breaks=seq(from=xmin, to=xmax, by=10)) +
        scale_color_manual('',values=c('red', 'red')) +
        theme(panel.background = element_rect(fill='white')
        )
      
      return(p)
    })
    
    output$atk_value <- renderPrint({
      atk_val
    })
    
    target <- master_frame$char[which(master_frame$cell == xy_cell)]
    new_health <- master_frame$health[which(master_frame$char == target)] - atk_val
    master_frame$health[which(master_frame$char == target)] <<- new_health
    
    ## murder!
    if(new_health <= 0)
    {
      master_frame <<- subset(master_frame, char != target)     ## remove dead person from master_frame
      
      ## check for win condition
      if(nrow(subset(master_frame, team != char_team)) == 0)
      {
        sendSweetAlert(
          session = session,
          title = paste0('Team ', char_team, ' wins!'),
          type= 'success'
        )
      }
      
      turn_order <<- turn_order[turn_order != target]           ## remove dead person from turn order
      turn_index <<- min(which(turn_order == char_curr, TRUE))  ## reset turn index
    }
    
    ## remove red attack highlights
    output$playgrid <- renderPlot({
      make_plot(gpoly, master_frame)
    })
    
    char_attacked <<- TRUE 
    updateActionButton(session, 'atk_button', label='Attack Complete')
    shinyjs::disable('atk_button')
  })
  

  ## MOVEMENT 
  observeEvent(input$move_button, {
    char_moved <<- FALSE  
    obstacs <- subset(master_frame, !char == char_curr)$cell
    moves <- loc_map(char_pos$move, char_pos$xloc, char_pos$yloc, obstacs, focus='blockers', grid_size)
    
    ## UPDATE PLOT
    output$playgrid <- renderPlot({
      make_plot(gpoly, master_frame, scope='show_move', moves=moves)
    })
  })
    
    
  ## ATTACKING
  observeEvent(input$atk_button, {
    char_attacked <<- FALSE 
    atk_pos <- subset(master_frame, char==char_curr)
    
    output$helper1 <- renderPrint({
      paste0('char_curr: ', char_curr)
    }) 
    
    ## check for possible attacks
    mobs <<- subset(master_frame, team != char_team)$cell
    atks <<- loc_map(atk_pos$atk, atk_pos$xloc, atk_pos$yloc, mobs, focus='targets', grid_size)
    
    ## generic debuggers
    output$helper2 <- renderPrint({
      paste0('curr team: ', char_team)
    })
    
    output$helper3 <- renderTable({
      atks
    })  
    
    ## UPDATE PLOT
    if(nrow(atks) > 0)
    {
      output$playgrid <- renderPlot({
        make_plot(gpoly, master_frame, scope='show_atk', atks=atks)
      })
    }else
    {
      # updateActionButton(session, 'atk_button', label='No Attacks Possible')  ## neither of these are optimal, revisit later
      output$no_atk_msg <- renderPrint({
        'No Attacks Possible'
      })
    }
  })    
    

  ## hit END TURN button, increment turn order
  observeEvent(input$end_turn, {
    turn_index <<- turn_index + 1
    char_curr  <<- turn_order[turn_index]    
    char_pos   <<- subset(master_frame, char==char_curr)
    char_team  <<- char_pos$team
    
    output$whos_turn <- renderText({
      paste0(turn_order[turn_index], ", it's your turn")
    })
    
    ## reset attack modal
    output$atk_value <- renderPrint({
      return(NULL)
    })
    
    ## update current char detail panel
    output$current_char_icon <- renderImage({
      image <- subset(master_frame, char == turn_order[turn_index])$icon
      list(src = image,
           width = '50px',
           height = '50px')
    }, deleteFile = FALSE)
    
    output$current_char_health <- renderText({
      paste0('Health: ', subset(master_frame, char == turn_order[turn_index])$health)
    })
    
    output$current_char_move <- renderText({
      paste0('Movement Range: ', subset(master_frame, char == turn_order[turn_index])$move)
    })
    
    output$current_char_atk <- renderText({
      paste0('Attack Range: ', subset(master_frame, char == turn_order[turn_index])$atk)
    })  
    

    ## reset no available attack helper message
    output$no_atk_msg <- renderPrint({
      ''
    })
    
    updateActionButton(session, 'move_button', label='Move')
    updateActionButton(session, 'atk_button', label='Attack')
    updateActionButton(session, 'atk_roll', label='Attack!')
    
    enable('move_button')
    enable('atk_button')
    enable('atk_roll')
  })   


  
  
})