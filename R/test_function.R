test_function <- function(test_dat, studentFUN, correctFUN, ...) {
  test_dat <- convt2list(test_dat)
  a <- sapply(test_dat, comp, studentFUN, correctFUN, ...)
  b <- suppressWarnings(as.logical(a))
  if (any(is.na(b) | !b)) {
    sprintf(
      "your function differs in %s",
      unlist(toString(test_dat[is.na(b) | !b]))
    )
  } else {
    TRUE
  }
}
