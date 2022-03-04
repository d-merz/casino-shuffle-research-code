#cards guessed as given in dovetail paper
#optimal strategy: guess the next card in succession from the OG 1:52
library(tictoc)
library(ggplot2)

cards_guessed_right_v2 = function(shuffle,sims){
  correc_vec = 0
  for (i in 1:sims){
    og = 1:52
    correct = 1
    cards = shuffle(1:52)
    if (cards[1]==1){
      correct = correct +1
    }
    if(cards[1] == 52){
      top_card = cards[1]
      z = list()
      z[[1]] = og[1:(top_card-1)]
    }else{
      top_card = cards[1]
      z = list()
      z[[1]] = og[1:(top_card-1)]
      z[[2]] = og[(top_card+1):52]
    }

    cards = cards[-1]
    
    for (x in 1:50){
      max = 0
      for(i in 1:length(z)){
        if(length(z[[i]])>max){
          max = length(z[[i]])
          lrg = i
        }
      }
      guess = z[[lrg]][1]
      og = og[-1*(match(top_card,og))]

      if (cards[1]== guess){
        correct = correct +1
      }
      
      top_card = cards[1]
      for (i in 1:length(z)){
        if(any(top_card%in%unlist(z[i]))){
          if(length(unlist(z[[i]])) == 1){
            z[[i]] = NULL
          }else if(z[[i]][1] == top_card){
            z[[i]] = z[[i]][2:length(z[[i]])]
          }else if(z[[i]][length(z[[i]])]==top_card){
            z[[i]] = z[[i]][1:(length(z[[i]])-1)]
          }else{
            index = top_card - z[[i]][1]
            zum = z[[i]]
            z[[i]] = zum[1:index]
            z[[length(z)+1]] = zum[(index+2):(length(zum))]
          }}}
      cards = cards[-1]

    }
    correc_vec = correc_vec + correct
  }
  return(correc_vec/sims)
}
tic()
cards_guessed_right_v2(better_shuffle,1000)
toc()

another_variation_distance_bar_plot = function(sims,shuffle1,shuffle2){
  holdem = cards_guessed_right_v2(shuffle1,sims)
  riffle = cards_guessed_right_v2(shuffle2,sims)
  
  data <- data.frame(values = c(holdem,riffle),
                     subgroup = c("Casino Shuffle","7 riffle shuffles"))
  
  ggplot(data,
         aes(x = reorder(subgroup,1:2),
             y = values,
             fill = reorder(subgroup,1:2),
             width = 0.5,
             size = 5)) + 
    geom_bar(stat = "identity",
             position = "dodge")+
    labs(title="Number of cards guessed correctly after shuffling",
         x ="Shuffle type", y = "Cards guessed correctly",fill = NULL)+ 
    geom_hline(yintercept = 4.54,
               color="black", linetype="dashed")+
    geom_text(aes(label = round(values,2)), vjust = -0.2, size = 7)+
    theme(plot.title = element_text(hjust = .5),
          text = element_text(size=20),
          legend.position = "none")
}
tic()
another_variation_distance_bar_plot(100000,holdem_shuffle,better_shuffle)
toc()
