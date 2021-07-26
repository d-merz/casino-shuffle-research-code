library(tictoc)
#deals 2 cards to each player and then deals the board + 3 burn cards based off a deck
deal_holdem = function(numpl,deck,shuffle){
  shuffled_deck = shuffle

  hands = matrix(shuffled_deck[1:(numpl*2)],ncol=2)
  
  hands = lapply(seq_len(nrow(hands)), function(i) hands[i,])
  board = c(shuffled_deck[(numpl*2+2):(numpl*2+4)], shuffled_deck[numpl*2+6], shuffled_deck[numpl*2+8])
  hands = append(hands, list("board" = board))
  burn_cards = c(shuffled_deck[(numpl*2+1)], shuffled_deck[numpl*2+5], shuffled_deck[numpl*2+7])
  hands = append(hands,list("burn cards" = burn_cards))
  return(hands)
  }
deal_holdem(6,1:52,better_shuffle(1:52))

#odds of a card going to each player, being on the flop, turn, or river
deal_holdem_odds = function(sims,numpl,shuffle_type){
  flop = integer(52)
  turn = integer(52)
  river = integer(52)
  burned = integer(52)
  empty_vect = integer(52)
  prob_of_card = list()
  for (x in 1:numpl){
    prob_of_card = append(prob_of_card,list(empty_vect))}
  for (x in 1:sims){
    shuffle = shuffle_type(1:52)
    dealt_hand = deal_holdem(numpl,1:52,shuffle)
    z = 0
    for (hand in dealt_hand){
      z = z+1

      if (length(hand)==2){
        card1 = hand[1]
        card2 = hand[2]
        prob_of_card[[z]][card1] = prob_of_card[[z]][card1] +1
        prob_of_card[[z]][card2] = prob_of_card[[z]][card2] +1
      
        }else if(length(hand) == 5){
        #adding one to the specific position of the flop, turn, and river for each time 
        #that number card comes up. I.e. if the board flops a 29, 1 is added to 
        #the 29th position of the vector "flop"
        flop[hand[1]] = flop[hand[1]] +1
        flop[hand[2]] = flop[hand[2]] +1
        flop[hand[3]] = flop[hand[3]] +1
        turn[hand[4]] = turn[hand[4]] +1
        river[hand[5]] = river[hand[5]] +1
        prob_of_card[[z]] = list(flop,turn,river)
        
      }else{
        #same as board cards, except 
        burned[hand[1]] = burned[hand[1]] +1
        burned[hand[2]] = burned[hand[2]] +1
        burned[hand[3]] = burned[hand[3]] +1
        prob_of_card[[z]] = list(burned)
      }
    }}
  #converting the counts to percentages
  for (i in 1:numpl){
    vec = unlist(prob_of_card[i])
    vec = vec/(sims)*100
    prob_of_card[i] = list(vec)
  }
  board = prob_of_card[[numpl +1]]
  for (i in 1:3){
    z = unlist(board[i])
    if (i == 1){
      z = z/(sum(z))*100*3
      prob_of_card[[1+numpl]][i] = list(z)
    }else{
      z = z/(sum(z))*100
      prob_of_card[[1+numpl]][i] = list(z)
    }
    
  }
  
  burn = unlist(prob_of_card[numpl+2])
  burn = burn/sum(burn)*100*3
  prob_of_card[[2+numpl]] = list("burn cards" = burn)
  names(prob_of_card[[numpl+1]]) = c("flop", "turn", "river")
  
  return(prob_of_card)
}
tic()
deal_holdem_odds(10000,6,holdem_shuffle)
toc()

