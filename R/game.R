




#' Play the game
#'
#' @return nothing
#' @export
#'
play_2048 = function(){
  x = new_board() %>%
    add_2()
  print(plot(x))

  i=0
  repeat {
    dir = get_input()
    x = x %>% go(dir)
    print(plot(x))
  }

  invisible(NULL)
}

