####################### PRINT ############################
printMaze <- function(maze, rows, cols) {
  for (x in seq(1, rows)) {
    print(maze[((x-1)*cols +1) : (x*cols)])
  }
}
##########################################################

###################### MOVE ##############################
moveUp <- function(position, rows, cols) {
  newPosition <- position - cols
  if (newPosition < 1) {
    return (position)
  } else {
    return (newPosition)
  }
}

moveDown <- function(position, rows, cols) {
  newPosition <- position + cols
  if (newPosition > rows*cols) {
    return (position)
  } else { 
    return (position + cols)
  }
}

moveLeft <- function(position, rows, cols) {
  newPosition <- position - 1
  if ((position - 1) %/% cols != (newPosition - 1) %/% cols) {
    return (position)
  } else {
    return (position - 1)
  }
}

moveRight <- function(position, rows, cols) {
  newPosition <- position + 1
  if ((position - 1) %/% cols != (newPosition - 1) %/% cols) {
    return (position)
  } else { 
    return (position + 1)
  }
}
##########################################################

######################## FITNESS #########################
parsePoint <- function(pos, rows, cols) {
  
  x <- ceiling(pos / rows)
  y <- pos %% cols
  
  if(y == 0) {
    y <- cols
  }
  
  return(c(x, y))
}

simulateSolution <- function(maze, solution, rows, cols, ngen) { # Update this function to serve as a fitness funcition
  # The simplest example is shown here: return 1 if the solution found the exit and 0 if it did not
  kazen <- 0.1
  punish <- 0
  currentPosition <- grep('s', maze)
  endPosition <- grep('e', maze)
  
  for (move in solution) {
    oldPosition <- currentPosition
    
    if (move == 'U') {
      currentPosition <- moveUp(currentPosition, rows, cols)
    } else if (move == 'D') {
      currentPosition <- moveDown(currentPosition, rows, cols)
    } else if (move == 'L') {
      currentPosition <- moveLeft(currentPosition, rows, cols)
    } else if (move == 'R') {
      currentPosition <- moveRight(currentPosition, rows, cols)
    } else {
      print(solution)
      print('Error: Incorrect solution format')
      return(-1)
    }
    if (maze[currentPosition] == '#') {
      punish <- punish + kazen
      currentPosition <- oldPosition
    }
    if (maze[currentPosition] == 'e') {
      # print('Resitev je najdena!')
      # print(solution)
      return(0)
    }
  }
  
  endpos = parsePoint(endPosition, rows, cols)
  currpos = parsePoint(currentPosition, rows, cols)
  
  return(sqrt((endpos[2] - currpos[2])^2 + (endpos[1] - currpos[1])^2))
}
##########################################################

######################## INIT POP ########################
createOneAgent <- function(ngen) {
  
  moves <- c('U', 'D', 'L', 'R')
  
  tmp <- as.integer(runif(ngen, 1, 5))
  
  # print(tmp)
  
  agent <- c(1:ngen)
  
  for (j in c(1:ngen)) {
    agent[j] <- moves[tmp[j]]
  }
  
  return(agent)
}

createPopulateion <- function(maze, rows, cols, size, ngen) {
  
  populacija <- vector(mode="list", length=size)
  
  for (i in c(1:size)) {
    
    populacija[[i]] <- createOneAgent(ngen)
    # print(populacija[[i]])
  }
  
  return(populacija)
}
##########################################################

######################## MUTACIJA ########################
mutacija <- function(sol) {
  
  moves <- c('U', 'D', 'L', 'R')
  
  sol[[1]][runif(1, 1, length(sol))] <- moves[as.integer(runif(1, 1, 5))]
  
  return(sol)
}

mutacija2 <- function(sol) {
  
  moves <- c('U', 'D', 'L', 'R')
  
  for(i in c(1:length(sol))) {
    
    if(as.integer(runif(1, 1, 6)) == 3) {
      
      sol[[1]][i] <- moves[as.integer(runif(1, 1, 5))]
    }
    
  }
  
  return(sol)
}
##########################################################

####################### SELECTION ########################
TOS <- function(pop, N, f, k) {
  
  best <- 0
  
  for (i in c(1:k)) {
    
    ind = as.integer(runif(1, 1, N))
    
    if(best == 0 || f[ind] < f[best]){
      best <- ind
    }
    
  }
  
  return(best)
}

getLoserId <- function(b1, b2) {
  
  # Generate the ID of the losers,
  # who will be replaced by the winners
  # of the TOS.
  
  losers <- c()
  
  for(i in c(1:4)) {
    
    if(i != b1 && i != b2) {
      losers <- c(losers, i)
    }
    
  }
  
  return(losers)
}

selectionTOS <- function(pop, N, f, ngen) {
  
  for (i in seq(from=1, to=N, by=4)) {
    
    subpop <- c(pop[i], pop[i + 1], pop[i + 2], pop[i + 3])
    subf <- c(f[i], f[i + 1], f[i + 2], f[i + 3])
    
    # Select  best 2.
    best1 <- TOS(subpop, 4, subf, 2)
    best2 <- TOS(subpop, 4, subf, 2)
    
    # Quickfix for 2 same candidates.
    if(best1 == best2) {
      
      for (i in c(1:10)) {
        best2 <- TOS(subpop, 4, subf, 2)
        
        if(best1 != best2) {
          break
        }
      }
    }
    
    # Select losers and create children.
    losers <- getLoserId(best1, best2)
  
    otroka <- crossover(subpop[[best1]], subpop[[best2]])
    
    # Replace losers by winners or by diversity token.
    
    # if(as.integer(runif(1, 1, 3)) == 2) {
    #   subpop[losers[1]] <- createOneAgent(ngen)
    # }
    # else {
    #   subpop[losers[1]] <- otroka[1]  
    # }
    # 
    # if(as.integer(runif(1, 1, 3)) == 2) {
    #   subpop[losers[2]] <- createOneAgent(ngen)
    # }
    # else {
    #   subpop[losers[2]] <- otroka[2]
    # }
    subpop[losers[1]] <- otroka[1]
    subpop[losers[2]] <- otroka[2]
    
    # Replace elements in popula
    pop[i] <- subpop[1]
    pop[i + 1] <- subpop[2]
    pop[i + 2] <- subpop[3]
    pop[i + 3] <- subpop[4]
    
  }
  
  # Apply mutateion
  for (i in c(1:N)) {
    pop[i] <- mutacija2(pop[i])
    # print('Mutacija')
    # print(pop[i])
  }
  
  return(pop)
}

SUS <- function(populacija, fitnessVec, N, ngen) {
  
  totalF <- sum(fitnessVec)
  offspring <- N / 2
  distance <- totalF / offspring
  
  pointers <- c(0: as.integer(offspring - 1))
  
  strt <- runif(1, 0, distance)
  
  for (i in pointers) {
    pointers[i] <- strt + i * distance
  }
  
  keep <- c()
  
  for (p in pointers) {
    
    i <- 1
    
    while(sum(fitnessVec[1:i]) < p) {
      i <- i + 1
    }
    
    
    keep <- c(keep, populacija[i])
  }
  
  return(keep) 
}

repopulate <- function(selected, N) {
  
  pop <- list()
  
  n <- length(selected)
  
  # Izvedemo crossover-je
  for (i in seq(from=1, to=N, by=2)) {
    parent1 = as.integer(runif(1, 1, n))
    parent2 = as.integer(runif(1, 1, n))
    
    otroka <- crossover(selected[[parent1]], 
                        selected[[parent2]])
    
    pop <- c(pop, otroka[1])
    pop <- c(pop, otroka[2])
  }
  
  # Podamo se mutacije z majhno verjetnostjo.
  for (i in c(1:N)) {
    
    if(as.integer(runif(1, 1, 10) == 5)) {
      pop[i] <- mutacija2(pop[i]) 
    }
  }
  
  return(pop)
}
##########################################################

###################### GEN ALG. ##########################
geneticAlgorithm <- function(maze, rows, cols, N, ngen, maxItr) {
  # Implement the genetic algorithm in this function
  # You should add additional parameters to the function as needed
  
  # Population params.
  
  # Init pop.
  populacija <- createPopulateion(maze, rows, cols, N, ngen)
  
  # Init fitness vec.
  fitnessVec <- c(1:N)
  
  # We start with gen. 1.
  itr <- 1
  
  while (itr < maxItr) {
    
    # Eval populacijo.
    for (i in c(1:N)) {
      fitnessVec[i] <- simulateSolution(maze, populacija[[i]], rows, cols, ngen)
    }
    
    populacija <- selectionTOS(populacija, N, fitnessVec)
    # populacija <- SUS(populacija, fitnessVec, N, ngen)
    # populacija <- repopulate(populacija, N)
    
    cat("Generacija:", itr, "Najboljsi v generaciji:", min(fitnessVec), "\n")
    
    if(min(fitnessVec) == 0) {
      print('KONEC')
      return(0)
    }
    
    itr <- itr + 1
  }
  
  return(-1)
}

evalGALG <- function(maze, rows, cols, N, ngen, maxItr, evalParam) {
  
  count <- 0
  
  for (i in c(1:evalParam)) {
    
    galg <- geneticAlgorithm(maze, rows, cols, N, ngen, maxItr)
    
    if(galg == 0) {
      count <- count + 1
    }
  }
  
  return(count / evalParam)
}
##########################################################

###################### TEST DATA #########################
maze1 <- c(' ', ' ', ' ', ' ', 'e',
           ' ', '#', '#', '#', '#',
           ' ', ' ', 's', ' ', ' ',
           '#', '#', '#', '#', ' ',
           ' ', ' ', ' ', ' ', ' ')
rows1 <- 5
cols1 <- 5
solution1 <- c('L', 'L','U', 'U', 'R', 'R', 'R', 'R', 'R') 

maze2 <- c('#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#',
           '#', '#', ' ', '#', ' ', '#', ' ', ' ', ' ', ' ', '#', ' ', ' ', '#', ' ', '#', '#',
           '#', '#', 'e', '#', '#', '#', '#', '#', '#', '#', '#', '#', ' ', ' ', ' ', '#', '#',
           '#', '#', ' ', '#', ' ', '#', ' ', '#', ' ', '#', '#', '#', ' ', ' ', ' ', '#', '#',
           '#', '#', ' ', ' ', ' ', '#', ' ', '#', ' ', '#', '#', '#', '#', '#', ' ', ' ', ' ',
           '#', '#', ' ', '#', ' ', '#', ' ', '#', ' ', ' ', ' ', ' ', '#', '#', ' ', '#', ' ',
           '#', ' ', ' ', '#', ' ', ' ', ' ', '#', '#', '#', '#', ' ', '#', '#', ' ', '#', ' ',
           '#', '#', ' ', '#', ' ', '#', ' ', '#', '#', '#', '#', ' ', '#', '#', ' ', '#', ' ',
           '#', '#', ' ', '#', ' ', ' ', ' ', ' ', ' ', '#', '#', ' ', '#', '#', ' ', ' ', ' ',
           '#', ' ', ' ', ' ', '#', ' ', '#', ' ', '#', ' ', ' ', ' ', ' ', ' ', ' ', '#', ' ',
           '#', ' ', '#', ' ', '#', ' ', '#', ' ', '#', ' ', '#', '#', '#', '#', ' ', '#', ' ',
           '#', ' ', '#', ' ', '#', ' ', '#', ' ', '#', ' ', '#', '#', '#', '#', ' ', '#', ' ',
           '#', ' ', '#', ' ', '#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#', ' ', '#', ' ',
           '#', ' ', ' ', ' ', '#', ' ', '#', ' ', '#', '#', '#', '#', ' ', '#', ' ', '#', ' ',
           '#', '#', ' ', '#', '#', '#', '#', ' ', '#', '#', ' ', ' ', ' ', ' ', ' ', '#', 's',
           '#', '#', ' ', ' ', '#', ' ', ' ', ' ', '#', '#', ' ', '#', '#', '#', ' ', '#', ' ',
           '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', ' ', '#', '#',
           '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#')

solution2 <- c('U', 'U', 'U', 'U', 'U', 'U', 'L', 'L', 'D', 'L', 'L', 'L', 'L', 'L', 'D', 'D', 'D', 'L', 'L', 'L', 'L', 'U', 'U', 'U', 'U', 'L', 'U', 'U', 'U', 'U', 'L', 'L', 'U', 'U')
cols2 <- 17
rows2 <- 18

rows3<-11;
cols3<-16;
maze3 <- c('#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#',
           's',' ',' ',' ',' ',' ','#',' ',' ',' ',' ',' ','#',' ',' ','#',
           '#','#','#','#',' ',' ','#','#','#','#',' ',' ','#',' ',' ','#',
           '#',' ',' ',' ',' ',' ','#',' ',' ',' ',' ',' ',' ',' ',' ','#',
           '#',' ',' ','#','#','#','#',' ',' ','#','#','#','#',' ',' ','#',
           '#',' ',' ',' ',' ',' ',' ',' ',' ','#',' ',' ',' ',' ',' ','#',
           '#',' ',' ','#','#','#','#','#','#','#',' ',' ','#','#','#','#',
           '#',' ',' ',' ',' ',' ','#',' ',' ',' ',' ',' ','#',' ',' ','#',
           '#','#','#','#',' ',' ','#',' ',' ','#','#','#','#',' ',' ','#',
           '#',' ',' ',' ',' ',' ','#',' ',' ',' ',' ',' ',' ',' ',' ','e',
           '#','#','#','#','#','#','#','#','#','#','#','#','#','#','#','#')

maze4 <- c('#', '#', '#', '#', '#',
           '#', '#', ' ', ' ', 'e',
           '#', '#', ' ', '#', '#',
           '#', 's', ' ', '#', '#',
           '#', '#', '#', '#', '#')

cols4 <- 5
rows4 <- 5
sol4 <- c('R', 'U', 'U', 'R', 'R')

##########################################################

geneticAlgorithm(maze4, rows4, cols4, 32, 8, 2000)
# print(evalGALG(maze1, rows1, cols1, 32, 16, 5000, 10))

# tpop <- createPopulateion(maze1, rows1, cols1, 16, 8)
# f <- c(1:16)
# 
# for (i in c(1:16)) {
#    f[i] <- simulateSolution(maze1, tpop[i], rows1, cols1)
# }
# 
# tsus <- SUS(tpop, f, 16, 8)
# trepo <- repopulate(tsus, 16)
# 
# 
# fsol1 <- c("R", "R", "U", "U", "U", "D", "U", "L")
# fsol2 <- c("R", "R", "U", "R", "U", "D", "U", "L")
# fsol3 <- c("R", "R", "U", "U", "U", "D", "U", "L")
# 
# sol <- simulateSolution(maze1, fsol1, rows1, cols1)


