# Candidates 2026 Monte Carlo Simulation

Monte Carlo simulation of the 2026 FIDE Candidates Tournament.
Win probabilities are simulated by modelling each unplayed game
using an Elo-based win/draw/loss probability, where draw rates
are derived from historical chess database data.
Logic is largely based on https://github.com/chessmonitor/chess-monte-carlo-simulation

## Files

- `R/draw_probability.R` — draw probability lookup table and function
- `R/tournament.R` — game state management and standings functions  
- `R/montecarlo.R` — simulation engine and plotting
- `candidates_2026.R` — tournament data and round-by-round results

## Usage

Open `candidates_2026.R` and run the full script to reproduce
the latest standings and win probability plot.

## Dependencies

install.packages("reshape2")

## Workflow

After each round, add the results to `candidates_2026.R`:

new_round <- data.frame(
  white_fide_id = c(...),
  black_fide_id = c(...),
  result        = c(...)
)
games = update_games(games, new_round, round = N)
results = build_results(games, players, iterations = 10000)
format_standings(results, games, players)
plot_win_probability(results, players)
