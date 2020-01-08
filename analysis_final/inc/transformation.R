library(tidyverse)


alter_trans <- function(trans_frame){
  #' @description alters trans_frame:
  #' 
  #' - transforms type('VYBER') to type('PRIJEM')
  #' - renames columns 'date', 'type', 'k_symbol' & 'amount' to avoid collisions if one intends to join with other tables
  
  trans_frame <- trans_frame %>%
    dplyr::mutate(type = replace(type, type == 'VYBER', 'VYDAJ')) %>%
    dplyr::mutate(type = replace(type, type == 'VYDAJ', 'abfluss')) %>%
    dplyr::mutate(type = replace(type, type == 'PRIJEM', 'zufluss')) %>%
    dplyr::rename(trans_date = date,
           trans_amount = amount,
           trans_type = type,
           trans_k_symbol = k_symbol)
  
  return(trans_frame)
}

alter_account <- function(account_frame){
  #' @description alters account_frame:
  #' 
  #' - renames column 'date' to avoid collisions if one intends to join with other tables
  
  account_frame <- account_frame %>%
    dplyr::rename(account_date = date)
  
  return(account_frame)
}

alter_disp <- function(disp_frame){
  #' @description alters disp_frame:
  #' 
  #' - renames column 'type' to avoid collisions if one intends to join with other tables
  
  disp_frame <- disp_frame %>%
    dplyr::rename(disp_type = type)
  
  return(disp_frame)
}

alter_card <- function(card_frame){
  #' @description alters card_frame:
  #' 
  #' - renames column 'type' to avoid collisions if one intends to join with other tables
  
  card_frame <- card_frame %>%
    dplyr::rename(card_type = type)
  
  return(card_frame)
}

alter_loan <- function(loan_frame){
  #' @description alters loan_frame:
  #' 
  #' - renames columns 'date' & 'amount' to avoid collisions if one intends to join with other tables
  
  loan_frame <- loan_frame %>%
    dplyr::rename(loan_date = date,
                  loan_amount = amount)
  
  return(loan_frame)
}

alter_orders <- function(orders_frame){
  #' @description alters orders_frame:
  #' 
  #' - renames column 'k_symbol' to avoid collisions if one intends to join with other tables
  
  orders_frame <- orders_frame %>%
    dplyr::rename(orders_k_symbol = k_symbol)
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