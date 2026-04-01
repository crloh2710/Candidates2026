source("draw_probability.R")
# update games with new results, accepts a data frame of the same shape as 'games'
# adds new pairings, errors on duplicates
update_games = function(current_games, new_results, round) {
  stopifnot(all(
    c("white_fide_id", "black_fide_id", "result") %in% names(new_results)
  ))
  
  for (i in seq_len(nrow(new_results))) {
    row = new_results[i,]
    already_played = isTRUE(any(
      current_games$white_fide_id == row$white_fide_id &
        current_games$black_fide_id == row$black_fide_id
    ))
    if (already_played) {
      stop(
        sprintf(
          "Pairing %d vs %d already exists.",
          row$white_fide_id,
          row$black_fide_id
        )
      )
    }
  }
  new_results$round = round
  rbind(current_games, new_results)
}

get_standings = function(current_games, players, up_to_round = NULL) {
  if (!is.null(up_to_round)) {
    current_games = current_games[current_games$round <= up_to_round]
  }
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
  standings = players
  standings$points = as.numeric(points[as.character(players$fide_id)])
  standings$round = ifelse(is.null(up_to_round),
                           max(current_games$round),
                           up_to_round)
  standings[order(-standings$points),]
}
