library(tidyverse)


corr_trans <- function(trans_frame){
  #' @description corrects the observed false values of the trans:
  #' 
  #' - transforms type('VYBER') to type('PRIJEM')
  
  trans_frame <- trans_frame %>%
    mutate(type = replace(type, type == 'VYBER', 'PRIJEM'))
  
  return(trans_frame)
}