gcd <- function(a,b){
if(a>0 & b>0){
  while(b-a) if(a>b) a = a - b  else b = b - a
  a
}}
GCD <- function(x,y,z) gcd(x,gcd(y,z))
