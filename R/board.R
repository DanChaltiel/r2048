#' @importFrom tibble tibble
new_board = function(seed=42){
  set.seed(seed)
  x = tibble(id=1:16, row=rep(1:4, each=4), col=rep(1:4, times=4), value=NA)
  class(x) = c("board2048", class(x))
  x
}

#' @importFrom ggplot2 aes coord_fixed geom_text geom_tile ggplot scale_x_continuous scale_y_reverse theme theme_void
plot.board2048 = function(x){
  p = x %>%
    ggplot(aes(x=col, y=row, fill=value, label=value)) +
    geom_tile(color="white") +
    geom_text(na.rm=TRUE, color="white", size=8) +
    # geom_text(aes(x=col-0.4, y=row-0.4, label=id), na.rm=TRUE, color="yellow", size=2) +
    coord_fixed() +
    scale_y_reverse() +
    scale_x_continuous(position = "top") +
    theme_void() +
    theme(legend.position="none")
  p
}

#' @export
#' @importFrom dplyr mutate select
#' @importFrom stringr str_pad
#' @importFrom tidyr pivot_wider replace_na
print.board2048 = function(x, ...){
  a = as.data.frame(x) %>%
    mutate(value=replace_na(as.character(value), "")) %>%
    select(-id) %>%
    pivot_wider(names_from="col", values_from="value", names_prefix="C") %>%
    select(-row) %>%
    as.matrix()
  dimnames(a) = NULL
  w = max(nchar(a))+1
  for(i in seq(nrow(a))){
    v = str_pad(a[i,], width=w)
    cat("  ", v, "", "\n")
  }
  invisible(x)
}
