### Memory Card Game (match pair) in R

### function to set up the game
set.game.4by4 <- function() {
  game.open <- matrix(sample(c(LETTERS[1:8], LETTERS[1:8])), 4, 4)
  game.hidden <- matrix(1:16, 4, 4,)
  game.list <- list(game.open, game.hidden)
  names(game.list) <- c("open", "hidden")
  print(game.hidden)
  invisible(game.list)
}

### Function for selecting cards
select.cards <- function(game, card.1, card.2) {
  if(card.1 == card.2) {stop("Please choose different cards!")}
  if(card.1 > length(game[["open"]])) {stop("Please choose different cards!")}
  if(card.2 > length(game[["open"]])) {stop("Please choose different cards!")}
  if(game[["open"]][card.1] == "solved") {stop(paste("Card", card.1, "has already been solved. Please choose another card!!"))}
  if(game[["open"]][card.2] == "solved") {stop(paste("Card", card.2, "has already been solved. Please choose another card!!"))}
  show.game <- game[["hidden"]]
  show.game[c(card.1, card.2)] <- game[["open"]][c(card.1, card.2)]
  solve.game <- game[["open"]]
  solve.game[c(card.1, card.2)] <- "solved"
  solve.game.hidden <- game[["hidden"]]
  solve.game.hidden[c(card.1, card.2)] <- "solved"
  if(game[["open"]][card.1] == game[["open"]][card.2]) {game[["open"]] <- solve.game}
  if(game[["open"]][card.1] == game[["open"]][card.2]) {game[["hidden"]] <- solve.game.hidden}
  print(show.game)
  if(game[["open"]][c(card.1)] == game[["open"]][c(card.2)]) {print("It's a match!")}
  if(game[["open"]][c(card.1)] != game[["open"]][c(card.2)]) {print("No match!")}
  if(all(game[["open"]] == rep("solved", length(game[["open"]])))) {print("You win!")}
  invisible(game)
}

### Set up the game
set.seed(1988)
game1 <- set.game.4by4()

### Show cards
game1

### Play the game
game1 <- select.cards(game1, 3, 9)
game1 <- select.cards(game1, 1, 2)
game1 <- select.cards(game1, 3, 12)
game1 <- select.cards(game1, 4, 15)
game1 <- select.cards(game1, 5, 6)
game1 <- select.cards(game1, 7, 16)
game1 <- select.cards(game1, 8, 11)
game1 <- select.cards(game1, 9, 13)
game1 <- select.cards(game1, 10, 14)

game1

