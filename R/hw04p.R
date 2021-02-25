#recursive function to find the nth sum of a sequence of values
#The first three values must be defined and the nth term must be a integer greater than 0
func1 <- function(v,n){
  stopifnot(length(v)==3, n>0 && n%%1==0)
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

#Takes a tibble or data frame and using func1 calculates the nth sum
#the function then takes the outputted value and plots that against the nth value
func2<-function(df){
  stopifnot(length(df)==4, is.numeric( c ( df[[1]], df[[2]], df[[3]] ) ), df[[4]]%%1==0 && df[[4]]>0)
  x <- c()
  y <- c()
  for (i in row_number(df[[4]])){
    v <- c( df[[1]][i], df[[2]][i], df[[3]][i] )
    n <- df[[4]][i]
    x[i] <- n
    y[i] <- func1(v,n)
  }
  plot_data <-tibble(x, y)
  ggplot(plot_data, aes(x = x, y = y))+
    geom_line()+
    ylab("output")+
    xlab("n")
}
