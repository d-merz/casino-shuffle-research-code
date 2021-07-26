#holdem graphs

#histogram of deal_holdem_odds. shows how the cards end up in each position

holdem_histogram = function(sims,numpl,shuffle1){
  shuffle = deal_holdem_odds(sims,numpl,shuffle1)
  all_odds = list()
  for (i in 1:numpl){
    odds = unlist(shuffle[[i]])
    all_odds = append(all_odds,list(odds))
  }
  board = shuffle[[numpl +1]]
  for (i in 1:3){
    odds = unlist(board[[i]])
    all_odds = append(all_odds,list(odds))
  }
  burn = unlist(shuffle[numpl+2])
  odds = unlist(burn)
  names(odds) = c()
  all_odds = append(all_odds,list(odds))

  players = c(1:numpl)
  players1 <- paste("player ", players, sep="")
  group = rep(c(players1,
                "flop",
                "turn",
                "river",
                "burn"))
  val = c()
  for(i in 1:(numpl+4)){
    if (i <= numpl){
      val = append(val,200/52)
    }else if (i==numpl+1|i==numpl+4){
      val = append(val,300/52)
    }else{
      val = append(val,100/52)
    }}
  for (i in 1:(numpl+4)){

    df = data.frame(values = unlist(all_odds[i]))
    p= ggplot(df, aes(x=values,y=..density..)) + 
       geom_histogram(color="black", fill="white",bins = 20)+
       geom_vline(xintercept = val[i],color="blue", linetype="dashed")+
       geom_density(alpha=.2,fill = "pink")+
       labs(title= group[i],
           x ="odds")+
       theme(plot.title = element_text(hjust = .5))
    
    graphs[[i]] = p
  }
  
  ggarrange(graphs[[1]], graphs[[2]], graphs[[3]],graphs[[4]], graphs[[5]], 
            graphs[[6]],graphs[[7]], graphs[[8]], graphs[[9]],graphs[[10]],
            ncol = 3, nrow = 4)
}

tic()
holdem_histogram(10000,6,holdem_shuffle)
toc()

#compares mse for holdem shuffle and 7 riffle shuffles
compare_mse_graph = function(sims,numpl,shuffle1,shuffle2){
  holdem = unlist(shuffle_mse(sims,numpl,shuffle1))
  riffle = unlist(shuffle_mse(sims,numpl,shuffle2))
  print(holdem)
  nums = c()
  CI_uppers = c()
  CI_lowers = c()
  for (i in 1:(numpl+4)){
    nums = append(nums,holdem[i])
    nums = append(nums,riffle[i])
    p = holdem[i]/100
    CI_upper = p + 1.96*(sqrt((p*(1-p))/sims))
    CI_lower = p - 1.96*(sqrt((p*(1-p))/sims))
    CI_uppers = append(CI_uppers,CI_upper)
    CI_lowers = append(CI_lowers,CI_lower)
    p = riffle[i]/100
    CI_upper = p + 1.96*(sqrt((p*(1-p))/sims))
    CI_lower = p - 1.96*(sqrt((p*(1-p))/sims))
    CI_uppers = append(CI_uppers,CI_upper)
    CI_lowers = append(CI_lowers,CI_lower)

  }
  players = c(1:numpl)
  players1 <- paste("player ", players, sep="")

  data <- data.frame(values = nums,
                     group = rep(c(players1,
                                   "flop",
                                   "turn",
                                   "river",
                                   "burn"),
                                 each = 2),
                     subgroup = c("holdem shuffle","7 riffles"),
                     L =CI_lowers*100,
                     U =CI_uppers*100)

  ggplot(data,
         aes(x = reorder(group,1:((numpl+4)*2)),
             y = values, fill = reorder(subgroup,1:((numpl+4)*2)))) + 
         geom_bar(stat = "identity",
                  position = "dodge")+
        #geom_errorbar(aes(ymax = U, ymin = L,width = .2),position=position_dodge(.9))+
        labs(title="MSE for players cards & board",
             x ="Players cards + board", y = "MSE",fill = "shuffle")+ 
    theme(plot.title = element_text(hjust = .5))
}
tic()
compare_mse_graph(10000,6,holdem_shuffle,better_shuffle)
toc()

compare_sd_graph = function(sims,numpl,shuffle1,shuffle2){
  holdem = unlist(shuffle_sd(sims,numpl,shuffle1))
  riffle = unlist(shuffle_sd(sims,numpl,shuffle2))
  nums = c()
  for (i in 1:(numpl+4)){
    nums = append(nums,holdem[i])
    nums = append(nums,riffle[i])
  }
  players = c(1:numpl)
  players1 <- paste("player ", players, sep="")
  
  data <- data.frame(values = nums,
                     group = rep(c(players1,
                                   "flop",
                                   "turn",
                                   "river",
                                   "burn"),
                                 each = 2),
                     subgroup = c("holdem shuffle","7 riffles"))
  
  ggplot(data,
         aes(x = reorder(group,1:((numpl+4)*2)),
             y = values, fill = reorder(subgroup,1:((numpl+4)*2)))) + 
    geom_bar(stat = "identity",
             position = "dodge")+
    labs(title="SD for players cards & board",
         x ="Players cards + board", y = "Standard Deviations",fill = "shuffle")+ 
    theme(plot.title = element_text(hjust = .5))
}
tic()
compare_sd_graph(1000,6,holdem_shuffle,better_shuffle)
toc()



#compare histograms
compare_holdem_histogram = function(sims,numpl,shuffle1,shuffle2){
  shuffle = deal_holdem_odds(sims,numpl,shuffle1)
  shuffle2 = deal_holdem_odds(sims,numpl,shuffle2)

  all_odds = list()
  all_odds2 = list()
  for (i in 1:numpl){
    odds = unlist(shuffle[[i]])
    all_odds = append(all_odds,list(odds))
    odds = unlist(shuffle2[[i]])
    all_odds2 = append(all_odds2,list(odds))
  }
  board = shuffle[[numpl +1]]
  board2 = shuffle2[[numpl +1]]
  for (i in 1:3){
    odds = unlist(board[[i]])
    all_odds = append(all_odds,list(odds))
    odds = unlist(board2[[i]])
    all_odds2 = append(all_odds2,list(odds))
  }
  burn = unlist(shuffle[numpl+2])
  odds = unlist(burn)
  names(odds) = c()
  all_odds = append(all_odds,list(odds))
  burn = unlist(shuffle2[numpl+2])
  odds = unlist(burn)
  names(odds) = c()
  all_odds2 = append(all_odds2,list(odds))
  
  list_odds = list()
  for (x in 1:(numpl+4)){
    odds3 = c()
    odds3 = append(odds3,all_odds[[x]])
    odds3 = append(odds3,all_odds2[[x]])
    list_odds = append(list_odds,list(odds3))
    }
  
  players = c(1:numpl)
  players1 <- paste("player ", players, sep="")
  group = rep(c(players1,
                "flop",
                "turn",
                "river",
                "burn"))
  val = c()
  for(i in 1:(numpl+4)){
    if (i <= numpl){
      val = append(val,200/52)
    }else if (i==numpl+1|i==numpl+4){
      val = append(val,300/52)
    }else{
      val = append(val,100/52)
    }}
  for (i in 1:(numpl+4)){
    
    df = data.frame(values = unlist(list_odds[i]),
                    shuffle_type=factor(rep(c("Holdem Shuffle", "Riffle Shuffles"), each=52)))
    p= ggplot(df, aes(x=values,y=..density..,color = shuffle_type,fill = shuffle_type)) + 
      geom_histogram(bins = 20, position="identity",alpha = .5)+
      geom_vline(xintercept = val[i],color="blue", linetype="dashed")+
      geom_density(alpha=.5)+
      labs(title= group[i],
           x ="percentage")+
      theme(plot.title = element_text(hjust = .5))
    
    graphs[[i]] = p
  }
  ggarrange(graphs[[1]], graphs[[2]], graphs[[3]],graphs[[4]], graphs[[5]], 
            graphs[[6]],graphs[[7]], graphs[[8]], graphs[[9]],graphs[[10]],
            ncol = 3, nrow = 4,common.legend = TRUE)
}

tic()
compare_holdem_histogram(10000,6,holdem_shuffle,better_shuffle)
toc()

