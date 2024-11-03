


get_input = function(){
  choices = c("q"="left", "z"="up", "s"="down", "d"="right")
  i = 0
  repeat {
    choice <- tolower(readline(prompt = "Use QZSD as arrows: "))
    if (choice %in% names(choices)) {
      return(unname(choices[choice]))
    } else {
      i = i+1
      cat("Invalid choice, try again.\n")
    }
    if(i>3) stop("Too much invalid choices")
  }
}
