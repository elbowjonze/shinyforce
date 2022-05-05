move_ai <- function(char_pos, moves)
{
  ## AI movement - move to cell that minimizes euclidean distance to nearest target
  euclidean <- function(x1, y1, x2, y2) 
  {
    abs(((x1 - x2)^2 + (y1 - y2)^2)^.5)
  }

  char_curr <- char_pos$char
  mobs <- subset(master_frame, team != char_pos$team)
  x1 <- char_pos$xloc
  y1 <- char_pos$yloc
  
  ## find closest mob
  mob_dist <- data.frame('char' = character(),
                         'dist' = numeric())
  for(i in 1:nrow(mobs))
  {
    x2 <- mobs$xloc[i]
    y2 <- mobs$yloc[i]
    dist <- euclidean(x1, y1, x2, y2)
    out <- c(mobs$char[i], dist)
    mob_dist <- rbind(mob_dist, out)
  }
  
  names(mob_dist) <- c('char', 'dist')
  closest_mob <- sample(subset(mob_dist, dist == min(mob_dist$dist))$char, 1)  ## if there's a tie, pick one at random
  
  ## move towards closest mob
  x1 <- subset(master_frame, char==closest_mob)$xloc
  y1 <- subset(master_frame, char==closest_mob)$yloc
  
  cell_dist <- data.frame('x' = integer(),
                          'y' = integer(),
                          'cell' = character(),
                          'dist' = numeric())
  for(i in 1:nrow(moves))
  {
    x2 <- moves$x[i]
    y2 <- moves$y[i]
    dist <- euclidean(x1, y1, x2, y2)
    out <- cbind(moves[i,], dist)
    cell_dist <- rbind(cell_dist, out)
  }
  
  closest_cells <- subset(cell_dist, dist == min(cell_dist$dist))  
  closest_cell  <- closest_cells[sample(nrow(closest_cells), 1), ]  ## if there's a tie, pick one at random
  
  ## update location
  master_frame$xloc[which(master_frame$char == char_curr)] <<- closest_cell$x
  master_frame$yloc[which(master_frame$char == char_curr)] <<- closest_cell$y
  master_frame$cell[which(master_frame$char == char_curr)] <<- closest_cell$cell
  
  return(NULL)
} 
