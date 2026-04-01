#https://web.archive.org/web/20160806071058/http://chess-db.com/public/research/draw_rate.html
draw_table <- list(
  "1400" = c(21,24,25,24,24,22,23,24,22,22,20,20,21,19,18,17),
  "1600" = c(28,29,30,29,27,27,27,26,25,25,23,22,20,20,20,19),
  "1800" = c(31,32,32,32,30,30,28,27,26,25,23,23,22,22,20,20),
  "2000" = c(35,35,34,33,32,31,30,29,27,25,25,24,21,21,19,19),
  "2200" = c(42,42,40,39,37,36,34,32,30,28,25,24,22,20,19,17),
  "2400" = c(54,53,51,50,47,45,41,38,35,33,30,26,24,22,19,18),
  "2600" = c(57,54,54,52,51,50,45,42,40,37,34,31,30,28,29,25)
)

#probability of a draw given two elos based on draw_table above
#possible improvement including the use of candidate tournament results over recent years for the draw_table
#more noisy with the smaller sample size but players approach to the games are more comparable
draw_probability = function(elo1, elo2) {
  avg200 = min(2600, max(1400, floor(((
    elo1 + elo2
  ) / 2) / 200) * 200))
  diff = min(300, max(0, floor(abs(elo1 - elo2) / 20) * 20))
  diff_index = diff / 20 + 1
  draw_table[[as.character(avg200)]][diff_index]/100
}
