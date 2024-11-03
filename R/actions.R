
add_2 = function(x){
  random_i = runif(1, min=1, max=sum(is.na(x$value))+1) %>% floor()
  x$value[is.na(x$value)][random_i] = 2
  x
}


apply_func = function(x, dir, f){
  if(dir=="right"){
    x = x %>%
      arrange(row, col) %>%
      f(.by=row)
  }
  if(dir=="left"){
    x = x %>%
      arrange(row, desc(col)) %>%
      f(.by=row)
  }
  if(dir=="down"){
    x = x %>%
      arrange(col, row) %>%
      f(.by=col)
  }
  if(dir=="up"){
    x = x %>%
      arrange(col, desc(row)) %>%
      f(.by=col)
  }
  x
}

move = function(x, dir) apply_func(x, dir, move1)
merge = function(x, dir) apply_func(x, dir, merge1)

move1 = function(x, .by){
  x %>%
    mutate(
      value = c(value[is.na(value)], value[!is.na(value)]),
      .by={{.by}}
    )
}

merge1 = function(x, .by){
  x %>%
    mutate(
      cv = consecutive_id((value)),
      mask = apply_merge_mask(cv),
      i = replace_na(value==lag(value), FALSE),
      value2 = ifelse(i, value*2, value),
      value2 = ifelse(!mask, NA, value2),
      .by={{.by}}
    ) %>%
    select(id, row, col, value=value2) %>%
    arrange(id)
}

apply_merge_mask = function(x){
  stopifnot(length(x)==4)
  lookup_table = lst(
    "1,2,3,4" = c(1,1,1,1),


    "1,2,2,2" = c(1,1,0,1),
    "1,2,1,1" = c(1,1,0,1),
    "1,2,3,3" = c(1,1,0,1),
    "1,1,1,2" = c(1,0,1,1),
    "1,2,2,3" = c(1,0,1,1),

    "1,1,2,1" = c(0,1,1,1),
    "1,1,2,3" = c(0,1,1,1),
    "1,1,1,1" = c(0,1,0,1),
    "1,1,2,2" = c(0,1,0,1),

  )
  key = paste(x, collapse=",")
  if(!key %in% names(lookup_table)) stop(key)
  lookup_table[[key]]
}

go = function(x, dir){
  y = x %>%
    move(dir) %>%
    merge(dir) %>%
    move(dir)

  if(identical(x, y)){
    warning("Didn't move")
    return(x)
  }

  y %>% add_2()
}
