#' Running mean for a Hyfe metric
#'
#' @param x desc
#' @param y desc
#' @param window Window size of running mean, in number of values
#'
#' @return
#' @export
#'
add_running_mean <- function(x,
                             y,
                             window){

  #=============================================================================
  if(FALSE){
    library(hyfer)
    library(dplyr)
    data(hyfe_data)
    ho <- process_hyfe_data(hyfe_data)
    x <- ho$hours$study_hour
    y <- ho$hours$coughs
    window = 24
  }
  #=============================================================================

  crm <- data.frame()
  i=window
  window
  for(i in 1:length(y)){
    #print(i)
    subi <- (i - window):i
    subi[subi < 1] <- 1
    subi <- unique(subi)
    subi
    yi <- y[subi] ; yi
    xi <- x[subi] ; xi
    cri <- mean(yi, na.rm=TRUE) ; cri
    crmi <- data.frame(x = mean(xi,na.rm=TRUE),
                       y = cri)
    crmi
    crm <- rbind(crm,crmi)
  }
  crm %>% head
  crm %>% tail
  return(crm)
}

