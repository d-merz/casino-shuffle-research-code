#confidence intervals for first 5 vals for players and cards for each shuffle

ci_graph = function(sims,numpl,shuffle,name){
  graphs = c()

  n = round(sqrt(numpl+4))
  m = n+1
  par(mfrow = c(n,m))
  shuffle = deal_holdem_odds(sims,numpl,shuffle)
  
  #odds of first five cards
  five_odds = list()
  for (i in 1:numpl){
    odds = unlist(shuffle[[i]][1:5])
    five_odds = append(five_odds,list(odds))
  }
  board = shuffle[[numpl +1]]
  for (i in 1:3){
    odds = unlist(board[[i]][1:5])
    five_odds = append(five_odds,list(odds))
  }
  burn = unlist(shuffle[numpl+2])
  odds = unlist(burn[1:5])
  names(odds) = c()
  five_odds = append(five_odds,list(odds))

  
  
  #mean values
  val = c()
  for(i in 1:(numpl+4)){
    if (i <= numpl){
      val = append(val,200/52)
    }else if (i == numpl+1|i == numpl+4){
      val = append(val,300/52)
    }else{
      val = append(val,100/52)
    }}

  
  cards = 1:5
  cards1 <- paste("card ", cards, sep="")
  
  
  #iterating through each set of odds
  for (i in 1:(numpl+4)){
    if(i <= numpl){
      x_axis <- paste("Player ", i, sep="")
    }else if(i == numpl+1){
      x_axis = "Flop"
    }else if(i == numpl+2){
      x_axis = "Turn"
    }else if(i == numpl+3){
      x_axis = "River"
    }else if(i == numpl+4){
      x_axis = "Burn Cards"}
    odds = unlist(five_odds[i])
    #confidence intervals for specfic set of odds
    CI_uppers = c()
    CI_lowers = c()
    for (x in 1:5){
      p = odds[x]/100
      CI_upper = p + 1.96*(sqrt((p*(1-p))/sims))
      CI_lower = p - 1.96*(sqrt((p*(1-p))/sims))
      CI_uppers = append(CI_uppers,CI_upper)
      CI_lowers = append(CI_lowers,CI_lower)
    }

    data <- data.frame(group = rep(c(cards1)),
                     F =unlist(five_odds[i]),
                     L =CI_lowers*100,
                     U =CI_uppers*100)

    require(ggplot2)
    p = ggplot(data, aes(x = group, y = F)) +
        geom_point(size = 1) +
        labs(title= name,
             x = x_axis, y = "Ci")+ 
        geom_errorbar(aes(ymax = U, ymin = L,width = .5))+ 
        ylim(0,2*val[i])+
        geom_hline(yintercept = val[i], linetype = "dashed", color = "red")+
        theme(plot.title = element_text(hjust = .5))
    graphs[[i]] = p
    }
  ggarrange(graphs[[1]], graphs[[2]], graphs[[3]],graphs[[4]], graphs[[5]], 
            graphs[[6]],graphs[[7]], graphs[[8]], graphs[[9]],graphs[[10]],
            ncol = 3, nrow = 4)
}
tic()
ci_graph(1000,6,holdem_shuffle,"holdem shuffle")
toc()







ci_comparison_graph = function(sims,numpl,shuffle,name,shuffle2){
  graphs = c()
  shuffle = deal_holdem_odds(sims,numpl,shuffle)
  shuffle2 = deal_holdem_odds(sims,numpl,shuffle2)
  #odds of first five cards
  five_odds = list()
  five_odds2 = list()
  for (i in 1:numpl){
    odds = unlist(shuffle[[i]][-47:-6])
    odds2 = unlist(shuffle2[[i]][-47:-6])
    five_odds = append(five_odds,list(odds))
    five_odds2 = append(five_odds2,list(odds2))
  }
  board = shuffle[[numpl +1]]
  board2 = shuffle2[[numpl +1]]
  for (i in 1:3){
    odds = unlist(board[[i]][-47:-6])
    five_odds = append(five_odds,list(odds))
    odds2 = unlist(board2[[i]][-47:-6])
    five_odds2 = append(five_odds2,list(odds2))
  }
  burn = unlist(shuffle[numpl+2])
  odds = unlist(burn[-47:-6])
  names(odds) = c()
  five_odds = append(five_odds,list(odds))
  burn2 = unlist(shuffle2[numpl+2])
  odds2 = unlist(burn2[-47:-6])
  names(odds2) = c()
  five_odds2 = append(five_odds2,list(odds2))
  
  
  #mean values
  val = c()
  for(i in 1:(numpl+4)){
    if (i <= numpl){
      val = append(val,200/52)
    }else if (i == numpl+1|i == numpl+4){
      val = append(val,300/52)
    }else{
      val = append(val,100/52)
    }}
  
  
  cards = c(1:5,48:52)
  cards1 <- paste("card ", cards, sep="")
  
  list_odds = list()
  for (x in 1:(numpl+4)){
    odds3 = c()
    for (i in 1:10){
      odds3 = append(odds3,five_odds[[x]][i])
      odds3 = append(odds3,five_odds2[[x]][i])
      if (i == 10){
        list_odds = append(list_odds,list(odds3))
      }
    }}

  #iterating through each set of odds
  for (i in 1:(numpl+4)){
    if(i <= numpl){
      x_axis <- paste("Player ", i, sep="")
    }else if(i == numpl+1){
      x_axis = "Flop"
    }else if(i == numpl+2){
      x_axis = "Turn"
    }else if(i == numpl+3){
      x_axis = "River"
    }else if(i == numpl+4){
      x_axis = "Burn Cards"}
    odds = unlist(list_odds[i])
    #confidence intervals for specfic set of odds
    CI_uppers = c()
    CI_lowers = c()
    for (x in 1:20){
      p = odds[x]/100
      CI_upper = p + 1.96*(sqrt((p*(1-p))/sims))
      CI_lower = p - 1.96*(sqrt((p*(1-p))/sims))
      CI_uppers = append(CI_uppers,CI_upper)
      CI_lowers = append(CI_lowers,CI_lower)
    }

    data <- data.frame(group = rep(c(cards1),each = 2),
                       F = unlist(list_odds[i]),
                       L = CI_lowers*100,
                       U = CI_uppers*100,
                       subgroup = c("Casino shuffle"," 7 Riffle shuffles"))
    
    require(ggplot2)
    p = ggplot(data, aes(x = reorder(group,1:((numpl+4)*2)), y = F,
                         color = reorder(subgroup,1:((numpl+4)*2)))) +
      geom_point(size = 1) +
      labs(title = name,
           x = x_axis, y = "percentage",color = "Shuffle")+ 
      geom_errorbar(aes(ymax = U, ymin = L,width = .5,color = subgroup))+ 
      ylim(0,2.2*val[i])+
      geom_hline(yintercept = val[i], linetype = "dashed", color = "black")+
      theme(plot.title = element_text(hjust = .5),
            legend.title = element_text(size = 20),
            legend.text = element_text(size = 15))
    graphs[[i]] = p
  }
  ggarrange(graphs[[1]], graphs[[2]], graphs[[3]],graphs[[4]], graphs[[5]], 
            graphs[[6]],graphs[[7]], graphs[[8]], graphs[[9]],graphs[[10]],
            ncol = 2, nrow = 5, common.legend = TRUE)
  
}

tic()
ci_comparison_graph(100000,6,holdem_shuffle,"Casino shuffle vs 7 Riffle shuffles",better_shuffle)
toc()


