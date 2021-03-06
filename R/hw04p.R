#' Sum of n numerics
#'
#'recursive function to find the nth sum of a sequence of values.
#'The first three values must be defined and the nth term must be a integer greater than 0.
#' @param vector of three numerics
#' @param integer must be positive and not zero
#'
#' @return a sum of numerics
#' @export sum_n
#'
#' @examples sum_n(c( 2, 4, 3), 5)
#' \dontrun{ sum_n(c( 2, 4), 5),
#' sum_n(c( 2, 4, 3), 5.5),
#' sum_n(c( 2, 4, 3), -5),
#' sum_n(c( 2, 4, 3), 0)}
sum_n <- function(v,n){
  stopifnot(length(v)==3, n>0, n%%1==0)
  z<-4
  if (n <4){
    return(v[n])
  }
  else{
    while (z <= n){
      v[z] = v[z-1]+( ( v[z-3]-v[z-2])/z )
      z = z + 1
    }
    return(v[z-1])
  }
}

#Test it
sum_n(c(2,4, 3), 5)


#' Graph of Sum of n numerics
#'
#'#Takes a tibble or data frame and using sum_n calculates the nth sum.
#the function then takes the outputted value and plots that against the nth value.
#'
#' @param data frame or tibble
#'
#' @return graph of outputs of sum_n against the value of n
#' @export graph_sum_n
#' @importFrom ggplot2 ggplot aes geom_line xlab ylab
#' @import tibble
#' @importFrom dplyr row_number
#' @examples
#' test <- tibble::tibble(
#' x = 2,
#' y = 4,
#' z = 3,
#' n = 3:12
#' )
#'
#' graph_sum_n(test)
#'
#' error_df1 <- tibble::tibble(
#' x = 2,
#' y = 3,
#' n = 4
#' )
#'
#' error_df2 <- tibble::tibble(
#' x = 2,
#' y = 4,
#' z = 3,
#' n = 0
#' )
#'
#' error_df3 <- tibble::tibble(
#' x = 2,
#' y = 4,
#' z = 3,
#' n = -1
#' )
#'
#' error_df4 <- tibble::tibble(
#' x = 2,
#' y = 4,
#' z = 3,
#' n = 6.5
#' )
#' \dontrun{ graph_sum_n(error_df1),
#' graph_sum_n(error_df2),
#' graph_sum_n(error_df3),
#' graph_sum_n(error_df4)}
graph_sum_n<-function(df){
  stopifnot(length(df)==4, is.numeric( c ( df[[1]], df[[2]], df[[3]] ) ), df[[4]]%%1==0, df[[4]]>0)
  x <- c()
  y <- c()
  for (i in row_number(df[[4]])){
    v <- c( df[[1]][i], df[[2]][i], df[[3]][i] )
    n <- df[[4]][i]
    x[i] <- n
    y[i] <- sum_n(v,n)
  }
  plot_data <-tibble(x, y)
  ggplot(plot_data, aes(x = x, y = y))+
    geom_line()+
    ylab("output")+
    xlab("n")
}

#Test it
 test <- tibble::tibble(
 x = 2,
 y = 4,
 z = 3,
 n = 3:12
 )

graph_sum_n(test)
