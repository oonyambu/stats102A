gcd <- function(a, b) {
  stopifnot(a > 0, b > 0, as.integer(a) == a, as.integer(b) == b)
  while (b - a) if (a > b) a <- a - b else b <- b - a
  a
}

GCD <- function(x, y, z) gcd(gcd(x, y), z)
