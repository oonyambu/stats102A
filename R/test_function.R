test_function <- function(test_dat, studentFUN, correctFUN, class_value = "numeric"){
  test_dat <- convt2list(test_dat)
  all(sapply(test_dat, comp,studentFUN, correctFUN, class_value))
}
