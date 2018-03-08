library(ggtern)
library(ggplot2)
library(dplyr)
library(plotly)

rm(list=ls())
rock = c(4,2,1)
paper = c(3,1,3)
scissors = c(5,0,2)
rock_paper_scissors = matrix(c(rock, paper, scissors),3,3, byrow = T) 
rock_paper_scissors

evolve = function(){
  
  random_fr = matrix(c(14/30,5/30,11/30),1,3)

  phi = numeric()  
  
  fitness = function(rps, rf){

    fit = numeric()
    for (i in 1:3){
      fit = c(fit,sum(rps[i,1]*rf[1] + rps[i,2]*rf[2] + rps[i,3]*rf[3]))
    } 
    return(fit)
  }

  
  average_fitness = function(fitness, r_f){
    avg = numeric()
    for (i in 1:3){
      avg = c(avg,r_f[i]* fitness[i])
    }
    return(sum(avg))
  }
  
  play = function(x, fitness, phi){
    dx = x*(fitness - phi)
    return(dx)
  }
  
  change = function(pop, d){
    return(pop + d)
  }
  
  popvalues_x = c()
  popvalues_y = c()
  popvalues_z = c()
  
  dx = c()
  dy = c()
  dz = c()

    
  for (i in 1:100){
    fitnessxyz = fitness(rock_paper_scissors,random_fr) #apply(rock_paper_scissors, MARGIN = 1, FUN = fitness, rf = random_fr) use different fitness function with only one thing
    phi = average_fitness(fitnessxyz, random_fr) #to account for change in fitness   
    
    popvalues_x = c(popvalues_x, random_fr[1])
    popvalues_y = c(popvalues_y, random_fr[2])
    popvalues_z = c(popvalues_z, random_fr[3])
    
    dx = play(random_fr[1], fitnessxyz[1], phi)
    dy = play(random_fr[2], fitnessxyz[2], phi)
    dz = play(random_fr[3], fitnessxyz[3], phi)
    
    #random_fr = apply(random_fr, change, MARGIN = 1, FUN = change, )
    random_fr[1] = change(random_fr[1],dx) #this adds the change in pop value to old pop
    random_fr[2] = change(random_fr[2],dy)
    random_fr[3] = change(random_fr[3],dz)
  }
  
  pop_data = data.frame(x=popvalues_x, #pretty way to set a data frame
                        y=popvalues_y,
                        z=popvalues_z)
  return(pop_data)  
}

  #a = replicate(,play(random_fr[1], fitnessxyz[1], phi))


plot_triangle = function(evo){
  x = evo[,1]
  y = y=evo[,2]
  z = z=evo[,3]
  breaks = seq(0,1,by=0.05)
  ggtern(data=evo,aes(x=x,y=y ,z=z)) + #had to make the x = pop_data[1], couldn't use variables
    geom_point(fill="red",shape=21,size=4)+
    limit_tern(breaks=breaks,labels=breaks)+
    labs(title = 'Evolutionary Dynamics of 3 Strategies')
}

plot_3D_scatterplot = function(evo){
#why not try this 3d scatter plot

  x <- evo[1]
  y <- evo[2]
  z <- evo[3]
  
  p <- plot_ly(evo, x = ~x, y = ~y, z = ~z) %>%
    add_markers(color = ~z) %>%
    layout(title = "Evolutionary Dynamics of 3 Strategies",
           scene = list(
             xaxis = list(title = "X"), 
             yaxis = list(title = "Y"), 
             zaxis = list(title = "Z")))
  p
}

evolution = evolve()
plot_triangle(evolution)
plot_3D_scatterplot(evolution)


