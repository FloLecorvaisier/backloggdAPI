#' Setting the filters for the search
#'
#' Generate the string containing the filters used to retrieve data from
#' backloggd.com. Available filters are 'Type', 'Release Year', 'Genre' and
#' 'Played Status'. Refer to the website for the possible values of these filters.
#'
#' @param type 'Type' filter.
#' @param release_year 'Release Year' filter.
#' @param genre 'Genre' filter.
#' @param game_status 'Played Status' filter.
#'
#' @return A string of the form `field1:value1;field2:value2...`.
#' @export
filters <- function(type = "", release_year = "", genre = "", game_status = "") {

  list_genre <- c("adventure", "arcade", "hack-and-slash-beat-em-up", "card-and-board-game",
                  "fighting", "indie", "moba", "music", "pinball",
                  "platform", "point-and-click", "puzzle", "quiz-trivia", "racing",
                  "real-time-strategy-rts", "role-playing-rpg", "shooter", "simulator", "sport",
                  "strategy", "tactical", "turn-based-strategy-tbs", "visual-novel")

  if (!type %in% c("", "played", "playing", "backlog", "wishlist")) {
    stop("Type should be one of 'played', 'playing', 'backlog' or 'wishlist', or left blank.")
  }
  if (!release_year %in% c("", "upcoming", "released", as.character(1950:2100))) {
    stop("Release year should be one of 'upcoming', 'playing', 'backlog' or 'wishlist', or left blank.")
  }
  if (!genre %in% c("", list_genre)) {
    stop(paste0("Genre should be one of '", paste(list_genre, collapse = "', '"), "' or left blank."))
  }

  if (!game_status %in% c("", "completed", "retired", "shelved", "abandoned", "played")) {
    stop("Game status should be one of 'completed', 'retired', 'shelved', 'abandoned' or 'played', or left blank.")
  }

  string_filters <- paste(collapse = ";",
                          c(
                            if (type == "") NULL else paste0("type:", type),
                            if (release_year == "") NULL else paste0("release_year:", release_year),
                            if (game_status == "") NULL else paste0("game_status:", game_status)
                          )
  )

  return(string_filters)
}

#' Getting the data for a user
#'
#' Retrieve the library of a user according to a specified set of filters.
#'
#' @param user Name of the user to retrieve the library from.
#' @param filters Optional list of filters.
#'
#' @return A list where `$filters` shows the applied filters and their value and `$df` is a data frame with the IDs and names of the games.
#' @export
getdata <- function(user, filters = "") {
  cat("Request is: '", paste("https://backloggd.com/u", user, "games/added", filters, sep = "/"), "'\n", sep = "")

  i = 1
  isOK = FALSE
  list_id = character()
  list_name = character()

  ## Loop to search all the pages (40 games max shown per page)
  while (isOK == FALSE) {
    ## GET the page with the list of games
    games <- httr::GET(paste0("https://backloggd.com/u/", user, "/games/added/", filters, "?page=", i), set_cookies(has_js = "true"))

    ## Extract the content (HTML page)
    code_page = content(games, as = "text")
    if (i == 1) cat(code_page, file = "prout.html")

    ## Split the lines of the
    linesplit = strsplit(x = code_page, split = "\n")

    ## Weird workaround to simplify the next step
    tmp <- tempfile()
    cat(code_page, file = tmp)
    code_page_edited <- readLines(tmp)

    ## Get the lines where games names are stored
    val_name = grep("<div class=\"game-text-centered\"> (.*)</div>", code_page_edited)
    val_id = grep("<div class=\"card mx-auto game-cover \" game_id=\"(.*)\">", code_page_edited)

    if (length(val_id) < 40) {
      isOK = TRUE
    }

    ## Extract the list of IDs (may be more useful for the API)
    list_name <- c(list_name, gsub(".*<div class=\"game-text-centered\"> (.*)</div>.*", "\\1", code_page_edited[val_name]))
    list_id <- c(list_id, gsub(".*<div class=\"card mx-auto game-cover \" game_id=\"(.*)\">.*", "\\1", code_page_edited[val_id]))

    # print(i)
    i = i + 1
  }

  ## Preparing the output
  df <- data.frame(id = as.numeric(list_id),
                   name = list_name)
  df$name <- htm2txt::htm2txt(df$name)

  l = lapply(strsplit(filters, ";")[[1]], strsplit, ":")
  output <- list()
  for (j in 1:length(l)) {
    output$field[[l[[j]][[1]][1]]] <- l[[j]][[1]][2]
  }

  output$df <- df
  return(output)
}

