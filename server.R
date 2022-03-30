## generalize stat function manipulations
distribution_mangler <- function(atk_dist, atk_state, xmin, xmax, param1, param2)
{
  if(atk_dist=='normal' & atk_state=='standard')
  {
    pdf <- normal_pdf
    expression <- normal_expression
    calc_deriv <- calc_normal_deriv
    cutoff <- 0
  }
  
  if(atk_dist=='normal' & atk_state=='inverted')
  {
    pdf <- normal_inv_pdf
    expression <- normal_inv_expression
    calc_deriv <- calc_normal_inv_deriv
    root <- uniroot(calc_deriv, interval=c(xmin,xmax), param1=param1, param2=param2)$root
    cutoff <- eval(expression)
  }
  
  if(atk_dist=='gamma' & atk_state=='standard')
  {
    pdf <- gamma_pdf
    expression <- gamma_expression
    calc_deriv <- calc_gamma_deriv
    cutoff <- 0
  }
  
  if(atk_dist=='gamma' & atk_state=='inverted')
  {
    pdf <- gamma_inv_pdf
    expression <- gamma_inv_expression
    calc_deriv <- calc_gamma_inv_deriv
    root <- uniroot(calc_deriv, interval=c(xmin,xmax), param1=param1, param2=param2)$root
    cutoff <- abs(eval(expression))
  }
  
  out <- list(pdf, cutoff)
  return(out)
}


deviate_grabber <- function(pdf, param1, param2, xmin, xmax, cutoff)
{
  ## grab random deviate
  dev_probs <- pdf(param1, param2, xmin:xmax, cutoff) ## calc probability for each step along x-axis
  dev_counts <- as.integer(round(dev_probs*10000, 0)) ## convert probs into reasonably sized integer units
  dev_array <- rep(xmin:xmax, dev_counts)             ## create one array to sample from

  atk_val <- sample(dev_array, size=1)
  return(atk_val)
}


  
## -------------------------------------------
## STANDARD attack/defense calculation functions 
## -------------------------------------------

##!! use generic param1, param2 instead of standard variable names to generalize later function calls

## NORMAL distribution
# param1 --> mu
# param2 --> sigma
normal_pdf <- function(param1, param2, x, cutoff){
  1/(param2*sqrt(2*pi))*exp(-((x-param1)/param2)^2) + abs(cutoff)
}

normal_expression <- expression(1/(param2*sqrt(2*pi))*exp(-((root - param1)/param2)^2))
# normal_deriv <- D(normal_expression, 'root')    ## returns function of first derivative, what we define calc_normal_deriv below

calc_normal_deriv <- function(param1, param2, root)
{
  -(1/(param2 * sqrt(2 * pi)) * (exp(-((root - param1)/param2)^2) * (2 * (1/param2 * ((root - param1)/param2)))))
}

## GAMMA distribution
# param1 --> alpha
# param2 --> beta
gamma_pdf <- function(param1, param2, x, cutoff)
{
  ((x^(param1 - 1)*exp(-x/param2)) / ((param2^param1) * gamma(param1))) + abs(cutoff)
}

gamma_expression <- expression((root^(param1 - 1)*exp(-root/param2)) / ((param2^param1) * gamma(param1)))

calc_gamma_deriv <- function(param1, param2, root)
{
  (root^((param1 - 1) - 1) * (param1 - 1) * exp(-root/param2) - root^(param1 - 1) * (exp(-root/param2) * (1/param2)))/((param2^param1) * gamma(param1))
}

## -------------------------------------------
## INVERTED attack/defense calculation functions 
## -------------------------------------------

## NORMAL distribution
# param1 --> mu
# param2 --> sigma
normal_inv_pdf <- function(param1, param2, x, cutoff)
{
  -1/(param2*sqrt(2*pi))*exp(-((x-param1)/param2)^2) + abs(cutoff)
}

normal_inv_expression <- expression(-1/(param2*sqrt(2*pi))*exp(-((root - param1)/param2)^2))

calc_normal_inv_deriv <- function(param1, param2, root)
{
  -(-1/(param2 * sqrt(2 * pi)) * (exp(-((root - param1)/param2)^2) * (2 * (1/param2 * ((root - param1)/param2)))))
}

## GAMMA distribution
gamma_inv_pdf <- function(param1, param2, x, cutoff)
{
  -(x^(param1 - 1)*exp(-x/param2)) / ((param2^param1) * gamma(param1)) + abs(cutoff)
}

gamma_inv_expression <- expression( -(root^(param1 - 1)*exp(-root/param2)) / ((param2^param1) * gamma(param1)) )

calc_gamma_inv_deriv <- function(param1, param2, root)
{
  -((root^((param1 - 1) - 1) * (param1 - 1) * exp(-root/param2) - root^(param1 - 1) * (exp(-root/param2) * (1/param2)))/((param2^param1) * gamma(param1)))
}
  
  
  
  









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
                             'xloc' = c(1, 2, 1, 5),
                             'yloc' = c(1, 2, 2, 5),
                             'cell' = c(1.1, 2.2, 1.2, 5.5),
                             'move' = c(3, 2, 2, 1),
                             'atk_range' = c(1, 3, 2, 3),
                             'health' = c(100, 100, 100, 100),
                             'atk_dist' = c('normal', 'normal', 'gamma', 'gamma'),
                             'atk_param1' = c(45, 55, 2, 2),
                             'atk_param2' = c(15, 25, 14, 11),
                             'def' = c(44, 35, 50, 40),
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
        
        ## determine relative locations of chars ... is attacker facing defender and vice versa?
        if( (attacker$xloc < defender$xloc & attacker$face == 'r') |
            (attacker$xloc > defender$xloc & attacker$face == 'l') |
            (attacker$yloc < defender$yloc & attacker$face == 'u') |
            (attacker$yloc > defender$yloc & attacker$face == 'd') )
        {
          atk_state <<- 'standard'
        }else
        {
          atk_state <<- 'inverted'
        }
        
        toggleModal(session, 'atk_modal')
        
        output$whos_fighting <- renderPrint({
          paste0(char_curr, ' is attacking ', defender$char, ' param1 = ', attacker$atk_param1, ' param2 = ', attacker$atk_param2, ' def = ', defender$def)  
        })
        
        ## initial attack distribution plot
        output$atk_plot <- renderPlot({
          xmin <- 1
          xmax <- 100
          atk_dist <- attacker$atk_dist
          param1 <- attacker$atk_param1
          param2 <- attacker$atk_param2
          def_val <<- defender$def
          
          dist_out <- distribution_mangler(atk_dist, atk_state, xmin, xmax, param1, param2)
          pdf <- dist_out[[1]]
          cutoff <- dist_out[[2]]
          
          xs <- seq(xmin, def_val, length.out=100)  ## x-axis step size
          ysmin <- rep(0, length(xs))
          ysmax <- pdf(param1, param2, xs, cutoff)
          shade_df <- data.frame(xs, ysmin, ysmax)
          dummy_df <- data.frame(x=c(xmin, xmax))


          if(atk_dist=='normal')
          {
            p <- ggplot(dummy_df, aes(x=x)) +
                  stat_function(fun=pdf, geom='line', args=list(param2, param1, cutoff=cutoff)) +
                  geom_ribbon(aes(x=xs, ymin=ysmin, ymax=ysmax), data=shade_df, fill="#BB000033") +
                  xlim(xmin, xmax)
          }
          
          if(atk_dist=='gamma')
          {
            p <- ggplot(dummy_df, aes(x=x)) +
                  stat_function(data=data.frame(x=c(xmin, xmax)), fun=pdf, geom='line', args=list(param1=param1, param2=param2, cutoff=cutoff)) +
                  geom_ribbon(data=shade_df, aes(x=xs, ymin=ysmin, ymax=ysmax), fill="#BB000033") +
                  xlim(xmin, xmax)
          }
            
          return(p)
        })
      }
    }
  })
  
  observeEvent(input$atk_roll,{
    
    updateActionButton(session, 'atk_roll', label='Attack Complete')
    shinyjs::disable('atk_roll')
    
    output$atk_plot <- renderPlot({
   
      xmin <- 1
      xmax <- 100
      atk_dist <- attacker$atk_dist
      param1 <- attacker$atk_param1
      param2 <- attacker$atk_param2
      def_val <<- defender$def
      
      dist_out <- distribution_mangler(atk_dist, atk_state, xmin, xmax, param1, param2)
      pdf <- dist_out[[1]]
      cutoff <- dist_out[[2]]      
      
      xs <- seq(xmin, def_val, length.out=100)  ## x-axis step size
      ysmin <- rep(0, length(xs))
      ysmax <- pdf(param1, param2, xs, cutoff)
      shade_df <- data.frame(xs, ysmin, ysmax)
      dummy_df <- data.frame(x=c(xmin, xmax))      
      
      # random roll
      if(atk_dist=='normal')
      {
        atk_val <<- round(rnorm(1, mean=param1, sd=param2), digits=0)
        
        p <- ggplot(dummy_df, aes(x=x)) +
          stat_function(fun=pdf, geom='line', args=list(param2, param1, cutoff=cutoff)) +
          geom_ribbon(aes(x=xs, ymin=ysmin, ymax=ysmax), data=shade_df, fill="#BB000033") +
          geom_vline(xintercept=atk_val, color='green') +
          xlim(xmin, xmax)
      }
      
      if(atk_dist=='gamma')
      {
        atk_val <<- deviate_grabber(pdf, param1, param2, xmin, xmax, cutoff)
        
        p <- ggplot(dummy_df, aes(x=x)) +
          stat_function(data=data.frame(x=c(xmin, xmax)), fun=pdf, geom='line', args=list(param1=param1, param2=param2, cutoff=cutoff)) +
          geom_ribbon(data=shade_df, aes(x=xs, ymin=ysmin, ymax=ysmax), fill="#BB000033") +
          geom_vline(xintercept=atk_val, color='green') +
          xlim(xmin, xmax)
      }      
      
      return(p)
    })
      
      
    ## resolve attack outcome
    output$atk_value <- renderPrint({
      
      atk_msg <- paste0('attack value = ', atk_val)
      if(atk_val <= def_val)
      {
        atk_msg <- paste0(atk_msg, ' did not get through defense, no damage done!')
      }
      
      ## -----------------------------
      ## update health values
      ## -----------------------------
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
      
      return(atk_msg)
    })
    
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