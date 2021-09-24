# list packages in a vector and load them all
pkgs <- c("readr", "cowsay")
purrr::walk(pkgs, require, character.only = TRUE)

# read quotes from a url
f_read_data <- function(url){
  suppressMessages(
    quotes  <- read_csv(url)  
  )
  return(quotes)
}

# paste the quote to the author
f_preprocess_data <- function(d){
  d$full_quote  <- paste0(d$Quote, " -", d$Author)
  return(d)
}

# print a random animal and a random quote
f_inspire_me <- function(d){
  animals <- names(animals)
  say(sample(d$full_quote, 1), by = sample(animals, 1))
}
