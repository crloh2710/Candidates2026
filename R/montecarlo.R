source("tournament.R")

#calc win, draw. loss probabilities for a given white and black rating
#white has 35 first-mover advantage to the elo formula https://en.wikipedia.org/wiki/First-move_advantage_in_chess
#scales w/l probabilities by the draw_probabiliy
#returns w/l/d elements that sum to 1
win_probability = function(white_rating, black_rating) {
  white_adv = 35
  expected_white = 1 / (1 + 10 ^ ((black_rating - white_rating - white_adv) /
                                    400))
  draw = draw_probability(white_rating, black_rating)
  list(
    win = expected_white * (1 - draw),
    draw = draw,
    loss = (1 - expected_white) * (1 - draw)
  )
}

#simulates one full completion of tournament from current game state
#applies points from already played games and simulates the rest in the double round robin
#drawing unplayed game from r~uni(0,1), partitioned by players' chance for w/l/d base on ELO (win_probability)
#if tied on points, winner selected at random from tied players
#returns winner fide id
simulate_once = function(current_games, players) {
  points = setNames(rep(0, nrow(players)), players$fide_id)
  
  for (i in seq_len(nrow(current_games))) {
    g = current_games[i,]
    w = switch(g$result,
               "1-0" = 1,
               "0-1" = 0,
               "1/2-1/2" = 0.5)
    points[as.character(g$white_fide_id)] = points[as.character(g$white_fide_id)] + w
    points[as.character(g$black_fide_id)] = points[as.character(g$black_fide_id)] + (1 - w)
  }
  
  for (wi in seq_len(nrow(players))) {
    for (bi in seq_len(nrow(players))) {
      if (wi == bi)
        next
      wid = players$fide_id[wi]
      bid = players$fide_id[bi]
      already = isTRUE(any(current_games$white_fide_id == wid &
                      current_games$black_fide_id == bid))
      if (!already) {
        prob = win_probability(players$rating[wi], players$rating[bi])
        r = runif(1)
        gain = if (r < prob$win) 1 else if (r < prob$win + prob$draw) 0.5 else 0
        points[as.character(wid)] = points[as.character(wid)] + gain
        points[as.character(bid)] = points[as.character(bid)] + (1 - gain)
      }
    }
  }
  max_pts = max(points)
  top = names(points)[points == max_pts]
  as.integer(sample(top, 1))
}

#runs the mc simulations for 10000 iterations
#each iteration calls simulate_once() to give a winner
#tallies wins per player across the interations
run_monte_carlo = function(current_games, players, iterations = 10000) {
  wins = setNames(rep(0, nrow(players)), players$fide_id)
  
  for (i in seq_len(iterations)) {
    winner = simulate_once(current_games, players)
    wins[as.character(winner)] = wins[as.character(winner)] + 1
  }
  result = players
  result$wins = as.integer(wins[as.character(players$fide_id)])
  result$win_pct = round(100 * result$wins / iterations, 2)
  result[order(-result$win_pct),]
}

#plot for the simulated win probabilities for each player after each round
plot_win_probability = function(results, players, iterations = 10000) {
  player_names = unique(results$name)
  colors = rainbow(length(player_names))
  names(colors) = player_names
  
  plot(
    NULL,
    xlim = c(0, max(rounds)),
    ylim = c(0, 100),
    xlab = "Round",
    ylab = "Win Prob (%)",
    main = "Simulated Win Probability by round",
    xaxt = 'n'
  )
  axis(1, at = rounds)
  
  for (player_name in unique(results$name)) {
    player_data = results[results$name == player_name,]
    player_data = player_data[order(player_data$round), ]
    lines(player_data$round,
          player_data$win_pct,
          col = colors[player_name],
          lwd = 2)
    points(player_data$round,
           player_data$win_pct,
           col = colors[player_name],
           pch = 17)
  }
  
  legend(
    "topright",
    legend = player_names,
    col = colors,
    lwd = 2,
    pch = 16,
    cex = 0.8,
    bty = "n"
  )
}
