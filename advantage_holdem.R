#odds compared to what they should be. positive numbers indicate more
#likely than random, negative is less likely than random
#i.e. with 4 players the 3rd card pre-shuffle has a 5% increased chance of being dealt on the flop
library(tictoc)

player_advantage = function(sims,numpl,shuffle){
  calc_odds = deal_holdem_odds(sims,numpl,shuffle)
  
  for (i in 1:numpl){
    vec = unlist(calc_odds[i])
    vec = vec - (200/52)
    calc_odds[i] = list(vec)
  }
  board = calc_odds[[numpl +1]]
  for (i in 1:3){
    z = unlist(board[i])
    names(z) = c()
    if (i == 1){
      z = z - (300/52)
      calc_odds[[1+numpl]][i] = list(z)
    }else{
      z = z - (100/52)
      calc_odds[[1+numpl]][i] = list(z)
    }
  }
  burn = unlist(calc_odds[numpl+2])
  names(burn) = c()
  burn = burn - 300/52
  calc_odds[[2+numpl]] = list("burn cards" = burn)
  names(calc_odds[[numpl+1]]) = c("flop", "turn", "river")
  return(calc_odds)
}
tic()
z = player_advantage(1000,4,holdem_shuffle)
x = player_advantage(1000,4,better_shuffle)
toc()

#percentage gain compared to random for each card. 
#i.e. with 4 players the 3rd card pre-shuffle is 74% more likely to be dealt on the flop than a random card
percentage_advantage = function(sims,numpl,shuffle){
  prob_odds = player_advantage(sims,numpl,shuffle)
  
  for (i in 1:numpl){
    vec = unlist(prob_odds[i])
    vec = vec/(2/52)
    prob_odds[i] = list(vec)
  }
  board = prob_odds[[numpl +1]]
  for (i in 1:3){
    z = unlist(board[i])
    names(z) = c()
    if (i == 1){
      z = z/(3/52)
      prob_odds[[1+numpl]][i] = list(z)
    }else{
      z = z/(1/52)
      prob_odds[[1+numpl]][i] = list(z)
    }
  }
  burn = unlist(prob_odds[numpl+2])
  names(burn) = c()
  burn = burn/(3/52)
  prob_odds[[2+numpl]] = list("burn cards" = burn)
  names(prob_odds[[numpl+1]]) = c("flop", "turn", "river")
  return(prob_odds)
}
tic()
z = percentage_advantage(1000,6,better_shuffle)
toc()
