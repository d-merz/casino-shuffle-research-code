riffle_shuffle = function(deck){
  cut_point = rbinom(n =1, size = length(deck), prob = 1/2)
  left = deck[1:cut_point]
  right = deck[(cut_point+1):length(deck)]
  riffled_deck = c()
  for (i in 1:length(deck)){
    A = length(left)
    B = length(right)
    random = runif(1)
    odds_left_heap_drop = A/(A+B)
    if (random < odds_left_heap_drop){
      card_added = left[A]
      left = left[-A]
      A = A - 1
    }else {
      card_added = right[B]
      right = right[-B]
      B = B -1
    }
    riffled_deck = c(card_added,riffled_deck)
  }
  return(riffled_deck)
}
