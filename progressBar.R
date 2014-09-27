
load_progress_bar <- function() {
  
  i <- NA
  length <- NA
  bar <- NA
  start <- NA
  title <- NA
  
  init_progress_bar <- function(length, title) {
    
    if(!is.na(i)) {
      warning('Init called on already existing bar. Closing and creating new bar.')
      close(bar)
    }
    
    i <<- 0
    length <<- length
    bar <<- winProgressBar(title= title, min = 0, max = length, width = 300, label='0% done', initial=0) 
    start <<- Sys.time()
    title <<- title
  }
  
  inc_progress_bar <- function() {
    if(is.na(bar)) {
      stop('Calling inc on uninitialized bar.')
    } else {
      i <<- i+1
      label <- paste0(sprintf('%.01f', round(100*i/length, 1)), "% done")
      title <- paste0(title, ': ', round((difftime(Sys.time(), start, units='secs') * (length - i) / i), 0), ' seconds remaining')
      setWinProgressBar(pb = bar, value = i, label=label, title=title)
    }
  }
  
  close_progress_bar <- function() {
    close(bar)
    bar <<- NA
    # returns true if finished
    i == length
  }
  
  c(init = init_progress_bar, inc = inc_progress_bar, close = close_progress_bar)
}