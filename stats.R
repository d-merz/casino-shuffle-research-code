#holdem_shuffle vs better_shuffle vs completely random
library(tictoc)
library(Metrics)
shuffle_sd = function(sims,numpl,shuffle1){
  shuffle = deal_holdem_odds(sims,numpl,shuffle1)
  for (i in 1:numpl){
    sd_shuffle = sd(unlist(shuffle[i]),na.rm = TRUE)
    shuffle[i] = list(sd_shuffle)
  }
  board = shuffle[[numpl +1]]
  for (i in 1:3){
   sd_board = sd(unlist(board[i]),na.rm = TRUE)
   shuffle[[1+numpl]][i] = list(sd_board)
  }
  burn = shuffle[numpl+2]
  burn = sd(unlist(burn),na.rm = TRUE)
  shuffle[2+numpl] = burn
  names(shuffle[[numpl+1]]) = c("flop", "turn", "river")
  names(shuffle[[numpl+2]]) = c("$burn")
  return(shuffle)
}
tic()
shuffle_sd(1000,4,holdem_shuffle)
toc()


shuffle_mse = function(sims,numpl,shuffle1){
  shuffle = deal_holdem_odds(sims,numpl,shuffle1)
  for (i in 1:numpl){
    odds = unlist(shuffle[i])
    mean = mean(odds)
    mse_shuffle = mse(odds,mean)
    shuffle[i] = list(mse_shuffle)
  }
  board = shuffle[[numpl +1]]
  for (i in 1:3){
    odds = unlist(board[i])
    mean = mean(odds)
    mse_board = mse(odds,mean)
    shuffle[[1+numpl]][i] = list(mse_board)
  }
  burn = shuffle[numpl+2]
  odds = unlist(burn)
  mean = mean(odds)
  burn = mse(odds,mean)
  shuffle[2+numpl] = burn
  names(shuffle[[numpl+1]]) = c("flop", "turn", "river")
  names(shuffle[[numpl+2]]) = c("burn")
  return(shuffle)
}
tic()
x = shuffle_mse(1000,6,holdem_shuffle)
toc()

