
gcd <- function(a, b) {
  # Calculates the greatest common divisor (GCD)
  # of two positive integers using the Euclidean algorithm
  # Args:
  # a: positive integer (dimension 1)
  # b: positive integer (dimension 1)
  # Return:
  # positive integer (dimension 1)
  if (!(is.numeric(a + b) & a %% 1 == 0 & b %% 1 == 0 & (a + b) < Inf & a > 1 & b > 1)) {
    warning("The inputs must be positive integers.")
  }
  if (a < b) {
    a <- a + b
    b <- a - b
    a <- a - b
  }
  while (a > 0 & b > 0) {
    temp <- b
    b <- a %% b
    a <- temp
  }
  if (a == 0 | b == 0) {
    max(a, b)
  }
}

gcd_three <- function(a, b, c) {
  # Calculates the greatest common divisor (GCD)
  # of three positive integers using the Euclidean algorithm
  # Args:
  # a: positive integer (dimension 1)
  # b: positive integer (dimension 1)
  # c: positive integer (dimension 1)
  # Return:
  # positive integer (dimension 1)
  gcd(gcd(a, b), c)
}

is_prime <- function(x) {
  # Determines whether or not a positive integer is prime
  # Args:
  # x: positive integer (dimension 1)
  # Return:
  # logical scalar (dimension 1)
  if (!(is.numeric(x) & x %% 1 == 0 & x < Inf & x > 1)) {
    warning("The input must be a positive integer.")
  }
  if (x == 1) {
    FALSE
  } else if (x == 2) {
    TRUE
  } else {
    all(0 != (x / 2:(x - 1)) %% 1)
  }
}

get_factors <- function(x) {
  # Produces a vector of the (not necessarily unique) prime factors of a positive integer
  # Args:
  # x: positive integer (dimension 1)
  # Return:
  # integer vector (dimension 1)
  if (!(is.numeric(x) & x %% 1 == 0 & x < Inf & x > 1)) {
    warning("The input must be a positive integer.")
  }
  (x / x:1)[0 == (x / x:1) %% 1][sapply((x / x:1)[0 == (x / x:1) %% 1], is_prime)]
}
