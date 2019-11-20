library(tidyverse)


corr_trans <- function(trans_frame){
  #' @description corrects the observed false values of the trans:
  #' 
  #' - transforms type('VYBER') to type('PRIJEM')
  
  trans_frame <- trans_frame %>%
    mutate(type = replace(type, type == 'VYBER', 'VYDAJ'))
  
  return(trans_frame)
}

round2 = function(x, n) {
  #' @description rounds mathematically correct, e.g. rounds 0.5 up instead of down like base::round
  
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  return(z*posneg)
}