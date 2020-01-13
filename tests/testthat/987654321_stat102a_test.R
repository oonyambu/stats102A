gcd <- function(a,b){
  stopifnot(a>0,b>0,as.integer(a)==a,as.integer(b)== b)
  while(b-a) if(a>b) a = a - b  else b = b - a
  a
}

GCD <- function(x,y,z)gcd(gcd(x,y),z)

get_factors <- function(x){
  n <- c()
  i <- 2
  while (x>1){
    if (!x %% i){
      n <- c(n, i)
      x <- x/i
      i <- i - 1
    }
    i <- i + 1
  }
  n
}

