# brainstorm how to organize this thing

# - need to have individual steps + combine steps
# - generalize grid size, not hard code <= 8
# - stuff I want to support
#   - x incremental steps
#   - ONLY certain steps (like a knight)
#   - cross move (x blocks)
#   - diag
#   - diag + cross
#   - circles?
#   - can move more in one direction than another (fast advance, slow retreat)


## calculate valid movement/attack spaces
loc_map <- function(pattern, x, y, cells, focus, grid_size)
{
  valid_locs <- NULL
  valid_locs <- data.frame('x'=integer(), 
                           'y'=integer(), 
                           'cell'=character())
  
  if(pattern %in% c(1, 2, 3))
  {
    for(i in 1:pattern)
    {
      ## move orthoginally
      valid_locs <- rbind(valid_locs,
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
        valid_locs <- rbind(valid_locs,
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
        valid_locs <- rbind(valid_locs,
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
  }
  
  valid_locs$x <- as.integer(valid_locs$x)
  valid_locs$y <- as.integer(valid_locs$y)
  
  if(focus == 'blockers')
  {
    valid_locs <- subset(valid_locs, x>=0 & x<=grid_size & y>=0 & y<=grid_size & !cell %in% cells)
  }
  
  if(focus == 'targets')
  {
    valid_locs <- subset(valid_locs, x>=0 & x<=grid_size & y>=0 & y<=grid_size & cell %in% cells)
  }
  
  return(valid_locs)
}