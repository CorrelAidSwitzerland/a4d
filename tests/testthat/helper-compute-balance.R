df_balance_product <- data.frame(product= rep("Accu-Chek Performa Test Strips (100s/box)", 10),
                                 product_entry_date= c("2020-12-31", "2020-01-01", "2020-01-11", "2020-01-12", "2020-01-18",
                                                       "2020-01-22", "2020-01-24", "2020-01-28", "2020-01-29", "2020-01-31"),
                                 product_balance= c(20,10,10,10,10,15,5,4,2,6),
                                 product_units_received= c(0,0,0,0,5,0,0,0,0,4),
                                 product_units_released= c(0,10,0,0,0,0,10,1,2,0),
                                 product_units_returned= rep(0,10),
                                 product_balance_status= c('start', rep('change',8), 'end'))


# final balance for each product is the starting balance PLUS all products received MINUS all products releaseds PLUS all products returned
helper_compute_exit_balance <- function(df, year){
    exit_balance <-  df$product_balance[1] + sum(df$product_units_received) - sum(df$product_units_released) + sum(df$product_units_returned)

    return(exit_balance)
}

