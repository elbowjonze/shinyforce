
## mapping for all possible characters manipulations
orientation_map <- data.frame('head_start' = c('u', 'l', 'd', 'r', 'u', 'l', 'd', 'r',
                                               'u', 'r', 'd', 'l', 'u', 'r', 'd', 'l',
                                               'u', 'd', 'u', 'd', 'r', 'r', 'l', 'l',
                                               'u', 'u', 'd', 'd', 'r', 'l', 'r', 'l') ,
                              'face_start' = c('r', 'u', 'l', 'd', 'l', 'd', 'r', 'u',
                                               'r', 'd', 'l', 'u', 'l', 'u', 'r', 'd',
                                               'r', 'r', 'l', 'l', 'd', 'u', 'd', 'u',
                                               'r', 'l', 'r', 'l', 'd', 'd', 'u', 'u'),
                              'manipulation' = c('rotate_left', 'rotate_left', 'rotate_left', 'rotate_left', 'rotate_left', 'rotate_left', 'rotate_left', 'rotate_left',
                                                 'rotate_right', 'rotate_right', 'rotate_right', 'rotate_right', 'rotate_right', 'rotate_right', 'rotate_right', 'rotate_right',
                                                 'flip_horizontal', 'flip_horizontal', 'flip_horizontal', 'flip_horizontal', 'flip_horizontal', 'flip_horizontal', 'flip_horizontal', 'flip_horizontal',
                                                 'flip_vertical', 'flip_vertical', 'flip_vertical', 'flip_vertical', 'flip_vertical', 'flip_vertical', 'flip_vertical', 'flip_vertical'),
                              'head_end'   = c('l', 'd', 'r', 'u', 'l', 'd', 'r', 'u',
                                               'r', 'd', 'l', 'u', 'r', 'd', 'l', 'u',
                                               'd', 'u', 'd', 'u', 'r', 'r', 'l', 'l',
                                               'u', 'u', 'd', 'd', 'l', 'r', 'l', 'r'),
                              'face_end'   = c('u', 'l', 'd', 'r', 'd', 'r', 'u', 'l',
                                               'd', 'l', 'u', 'r', 'u', 'r', 'd', 'l',                                                   
                                               'r', 'r', 'l', 'l', 'u', 'd', 'u', 'd',
                                               'l', 'r', 'l', 'r', 'd', 'd', 'u', 'u')
)
                                                                                              
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
                             'atk_range' = c(1, 3, 2, 3),
                             'health' = c(100, 100, 100, 100),
                             'atk_mu' = c(50, 65, 34, 70),
                             'atk_sigma' = c(15, 25, 20, 10),
                             'def' = c(50, 35, 66, 30),
                             'icon' = c('./sprites/alex_u_r.png',   ## icon format:  u/d/l/r stands for up/down/left/right.  First char is head orientation, second char is direction char is facing
                                        './sprites/tex_u_r.png',
                                        './sprites/ivan_u_l.png',
                                        './sprites/rocko_u_l.png'),
                             'head' = c('u', 'u', 'u', 'u'),
                             'face' = c('r', 'r', 'l', 'l')
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
    paste0('Attack Range: ', subset(master_frame, char == turn_order[turn_index])$atk_range)
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
        attacker <<- subset(master_frame, char==char_curr)
        defender <<- subset(master_frame, cell==xy_cell)
        
        toggleModal(session, 'atk_modal')
        
        output$whos_fighting <- renderPrint({
          paste0(char_curr, ' is attacking ', defender$char, ' atk mu = ', attacker$atk_mu, ' atk sigma = ', attacker$atk_sigma, ' def = ', defender$def)  
        })
        
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
          mu <- attacker$atk_mu
          sigma <- attacker$atk_sigma
          def_val <- defender$def
          
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
    
    xmin <- 0
    xmax <- 100
    mu <- attacker$atk_mu
    sigma <- attacker$atk_sigma
    def_val <- defender$def
    
    ## random roll
    atk_val <- rnorm(1, mean=mu, sd=sigma)
    
    output$atk_plot <- renderPlot({
      normal <- function(mu, sigma, x){
        1/(sigma*sqrt(2*pi))*exp(-((x-mu)/sigma)^2)
      }
      
      normal_shade <- function(mu, sigma, x, xmax){
        y <- normal(mu=mu, sigma=sigma, x)
        y[x < 0 | x > xmax] <- NA
        return(y)
      }
      
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
      atk_msg <- paste0('attack value = ', atk_val)
      if(atk_val <= def_val)
      {
        atk_msg <- paste0(atk_msg, ' did not get through defense, no damage done!')
      }
      return(atk_msg)
    })
    
    target <- master_frame$char[which(master_frame$cell == xy_cell)]
    if(atk_val > def_val)
    {
      new_health <- master_frame$health[which(master_frame$char == target)] - atk_val
      master_frame$health[which(master_frame$char == target)] <<- new_health
    }else
    {
      new_health <- master_frame$health[which(master_frame$char == target)]
    }
    
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
    atks <<- loc_map(atk_pos$atk_range, atk_pos$xloc, atk_pos$yloc, mobs, focus='targets', grid_size)
    
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
    
  
  ## ROTATING CHARACTERS
  observeEvent(input$rotate_right_button, {
                 
    curr_head <- subset(master_frame, char==char_curr)$head
    curr_face <- subset(master_frame, char==char_curr)$face
    out_head <- subset(orientation_map, manipulation=='rotate_right' & head_start==curr_head & face_start==curr_face)$head_end
    out_face <- subset(orientation_map, manipulation=='rotate_right' & head_start==curr_head & face_start==curr_face)$face_end
    
    ## update master frame
    master_frame$head[which(master_frame$char == char_curr)] <<- out_head
    master_frame$face[which(master_frame$char == char_curr)] <<- out_face
    master_frame$icon[which(master_frame$char == char_curr)] <<- paste0('./sprites/', tolower(char_curr), '_', out_head, '_', out_face, '.png')
    
    ## update plot
    output$playgrid <- renderPlot({
      make_plot(gpoly, master_frame)
    })
    
    ## disable further rotation
  })
  
  observeEvent(input$rotate_left_button, {
    
    curr_head <- subset(master_frame, char==char_curr)$head
    curr_face <- subset(master_frame, char==char_curr)$face
    out_head <- subset(orientation_map, manipulation=='rotate_left' & head_start==curr_head & face_start==curr_face)$head_end
    out_face <- subset(orientation_map, manipulation=='rotate_left' & head_start==curr_head & face_start==curr_face)$face_end
    
    ## update master frame
    master_frame$head[which(master_frame$char == char_curr)] <<- out_head
    master_frame$face[which(master_frame$char == char_curr)] <<- out_face
    master_frame$icon[which(master_frame$char == char_curr)] <<- paste0('./sprites/', tolower(char_curr), '_', out_head, '_', out_face, '.png')
    
    ## update plot
    output$playgrid <- renderPlot({
      make_plot(gpoly, master_frame)
    })
    
    ## disable further rotation
  })
  
  observeEvent(input$flip_vertical_button, {
    
    curr_head <- subset(master_frame, char==char_curr)$head
    curr_face <- subset(master_frame, char==char_curr)$face
    out_head <- subset(orientation_map, manipulation=='flip_vertical' & head_start==curr_head & face_start==curr_face)$head_end
    out_face <- subset(orientation_map, manipulation=='flip_vertical' & head_start==curr_head & face_start==curr_face)$face_end
    
    ## update master frame
    master_frame$head[which(master_frame$char == char_curr)] <<- out_head
    master_frame$face[which(master_frame$char == char_curr)] <<- out_face
    master_frame$icon[which(master_frame$char == char_curr)] <<- paste0('./sprites/', tolower(char_curr), '_', out_head, '_', out_face, '.png')
    
    ## update plot
    output$playgrid <- renderPlot({
      make_plot(gpoly, master_frame)
    })
    
    ## disable further rotation
  })  
  
  observeEvent(input$flip_horizontal_button, {
    
    curr_head <- subset(master_frame, char==char_curr)$head
    curr_face <- subset(master_frame, char==char_curr)$face
    out_head <- subset(orientation_map, manipulation=='flip_horizontal' & head_start==curr_head & face_start==curr_face)$head_end
    out_face <- subset(orientation_map, manipulation=='flip_horizontal' & head_start==curr_head & face_start==curr_face)$face_end
    
    message('out_head')
    message(out_head)
    message('out_face')
    message(out_face)
    
    ## update master frame
    master_frame$head[which(master_frame$char == char_curr)] <<- out_head
    master_frame$face[which(master_frame$char == char_curr)] <<- out_face
    master_frame$icon[which(master_frame$char == char_curr)] <<- paste0('./sprites/', tolower(char_curr), '_', out_head, '_', out_face, '.png')
    
    ## update plot
    output$playgrid <- renderPlot({
      make_plot(gpoly, master_frame)
    })
    
    ## disable further rotation
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
      paste0('Attack Range: ', subset(master_frame, char == turn_order[turn_index])$atk_range)
    })  
    
    ## wipe plots - does not work, neither did hide('atk_plot') ... probably need to add reactivity
    ## https://stackoverflow.com/questions/49495163/clear-button-in-shiny-app-is-not-clearing-plots
    # output$atk_plot <- renderPlot({
    #   ggplot()
    # })
    
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