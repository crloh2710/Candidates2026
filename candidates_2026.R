source("draw_probability.R")
source("tournament.R")
source("montecarlo.R")
library(reshape2)

players = data.frame(
  fide_id = c(2020009, 24116068, 24651516, 14205483, 8603405, 24175439, 25059530, 2016192),
  name    = c("Caruana", "Giri", "Bluebaum", "Sindarov", "Yi", "Esipenko", "Praggnanandhaa", "Nakamura"),
  rating  = c(2795, 2753, 2698, 2745, 2754, 2698, 2741, 2810)
)

games = data.frame(
  white_fide_id = c(2020009, 25059530, 24651516, 14205483, 24175439, 24116068, 8603405, 14205483),
  black_fide_id = c(2016192, 24116068, 8603405, 24175439, 2016192, 2020009, 25059530, 24651516),
  result        = c("1-0", "1-0", "1/2-1/2", "1-0", "1/2-1/2", "1/2-1/2", "1/2-1/2", "1/2-1/2"),
  round         = c(1, 1, 1, 1, 2, 2, 2, 2)
)

games = update_games(games,
                     data.frame(
                       white_fide_id = c(24651516, 25059530, 2020009, 2016192),
                       black_fide_id = c(24175439, 14205483, 8603405, 24116068),
                       result        = c("1/2-1/2", "0-1", "1-0", "1/2-1/2")
                     ), round = 3)

get_standings(games, players)

#final results, including pre-tournament probabilities
build_results = function(games, players, iterations = 10000) {
  rounds = sort(unique(games$round))
  results = data.frame()
  
  no_games = data.frame(
    white_fide_id = integer(),
    black_fide_id = integer(),
    result = character(),
    round = integer()
  )
  mc = run_monte_carlo(no_games, players, iterations)
  mc$round = 0
  results = rbind(results, mc)
  
  for (r in rounds) {
    games_to_round = games[games$round <= r,]
    mc = run_monte_carlo(games_to_round, players, iterations = 10000)
    mc$round = r
    results = rbind(results, mc)
  }
  results
}
results = build_results(games, players)

plot_win_probability(results, players)
#view of tabled mc results
format_standings = function(results) {
  clean = results[, c("round", "name", "win_pct")]
  dcast(clean, round ~ name, value.var = "win_pct")
}

format_standings(results)
