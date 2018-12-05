printMaze <- function(maze, rows, cols) {
  for (x in seq(1, rows)) {
    print(maze[((x-1)*cols +1) : (x*cols)])
  }
}


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


parsePoint <- function(pos, rows, cols) {
  
  x <- ceiling(pos / rows)
  y <- pos %% cols
  
  if(y == 0) {
    y <- cols
  }
  
  return(c(x, y))
}


simulateSolution <- function(maze, solution, rows, cols) {
  # Update this function to serve as a fitness funcition
  # The simplest example is shown here: return 1 if the solution found the exit and 0 if it did not
  kazen <- 3
  punish <- 0
  currentPosition <- grep('s', maze)
  endPosition <- grep('e', maze)
  for (move in solution) {
    oldPosition <- currentPosition
    # print(move)
    if (move == 'U') {
      currentPosition <- moveUp(currentPosition, rows, cols)
    } else if (move == 'D') {
      currentPosition <- moveDown(currentPosition, rows, cols)
    } else if (move == 'L') {
      currentPosition <- moveLeft(currentPosition, rows, cols)
    } else if (move == 'R') {
      currentPosition <- moveRight(currentPosition, rows, cols)
    } else {
      print('Error: Incorrect solution format')
      return(-1)
    }
    if (maze[currentPosition] == '#') {
      punish <- punish + kazen
      currentPosition <- oldPosition
    }
    if (maze[currentPosition] == 'e') {
      print('Resitev je najdena!')
      return(0)
    }
  }
  
  endpos = parsePoint(endPosition, rows, cols)
  currpos = parsePoint(currentPosition, rows, cols)
  
  return(sqrt((endpos[2] - currpos[2])^2 + (endpos[1] - currpos[1])^2))
}


createPopulateion <- function(maze, rows, cols, size, ngen) {
  
  populacija = matrix(nrow = size, ncol = ngen)
  
  for (i in c(1:size)) {
    
    populacija[i] <- createOneAgent(ngen)
    print(populacija[i])
  }
  
  return(populacija)
}


createOneAgent <- function(ngen) {
  
  moves <- c('U', 'D', 'L', 'R')
  
  tmp <- as.integer(runif(ngen, 1, 5))
  agent <- c(1:ngen)
  
  for (j in c(1:ngen)) {
    agent[j] <- moves[tmp[j]]
  }
  
  return(agent)
}


mutacija <- function(sol) {
  
  moves <- c('U', 'D', 'L', 'R')
  
  sol[runif(1, 1, length(sol))] <- moves[as.integer(runif(1, 1, 5))]
  
  return(sol)
}


crossover <- function(sol1, sol2) {
  
  # Lenghth of sol and slice param.  
  n = length(sol1)
  slice <- as.integer(runif(1, 1, n))
  
  # Generiramo childa od sol-ov
  child11 <- sol1[1:slice]
  child12 <- sol2[(slice + 1):n]
  
  child1 <- c(child11, child12)
  
  child21 <- sol2[1:slice]
  child22 <- sol1[(slice + 1):n]
  
  child2 <- c(child21, child22)
  
  return(list(child1, child2))
}


geneticAlgorithm <- 
  function(maze, rows, cols) {
  # Implement the genetic algorithm in this function
  # You should add additional parameters to the function as needed
  
  # Velikost populacije in enega agenta.
  N = 100
  ngen = 16
  
  # Purge param and diversity param.
  purge <- as.integer(N / 2)
  diversity <- as.integer(N / 10)
  
  # Inicializiramo populacijo.
  populacija <- createPopulateion(maze, rows, cols, N, ngen)
  
  # Vector of fitness vals.
  fitnessVec <- c(1:N)
  
  # Global fitness.
  globalFitness <- sum(fitnessVec)
  
  itr <- 1
  
  while (globalFitness > 0) {
    
    # Eval populacijo.
    for (i in c(1:N)) {
      fitnessVec[i] <- simulateSolution(maze, populacija[i], rows, cols)
    }
    
    # Dobimo fitness populacije.
    globalFitness <- sum(fitnessVec)
    
    # Pocistimo poulacijo in jo nadomestimo.
    fitnessVec <- sort(fitnessVec)
    
    for (i in c(purge:purge + diversity)) {
      populacija[i] <- createOneAgent(ngen)
    }
    
    # Zacnemo izvajanje crossover-jev.
    for (i in seq(purge + diversity, N, 2)) {
      
      print(i)
      
      # Preprecimo index out of bounds.
      if(i != N){
        
        parent1 = as.integer(runif(1, 1, purge + diversity))
        parent2 = as.integer(runif(1, 1, purge + diversity))
        
        print(populacija[parent1])
        print(populacija[parent2])
        
        otroka = crossover(populacija[parent1], 
                           populacija[parent2])
        
        # print(otroka[1])
        # print(otroka[2])
        
        populacija[i] = otroka[1]
        populacija[i + 1] = otroka[2]
      }
      else{
        populacija[i] = mutacija(populacija[i])
      }
    }
    
    # Podamo se mutacije z majhno verjetnostjo.
    for (i in c(1:N)) {
      
      if(as.integer(runif(1, 1, 10) == 5)) {
        populacija[i] <- mutacija(populacija[i]) 
      }
    }
    
    # print('------------')
    # print(itr)
    # itr <- itr + 1
    # print(fitnessVec)
    # print(globalFitness)
  }
},


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

# geneticAlgorithm(maze1, rows1, cols1)
