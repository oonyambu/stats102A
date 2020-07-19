test_function <- function(test_dat, studentFUN, correctFUN) {
  test_dat <- convt2list(test_dat)
  stud_env <- new.env()
  stud_env$stud_fun <- studentFUN
  teacher$teach_fun <- correctFUN
  a <- sapply(test_dat, comp, stud_env,teacher)
  rm("teach_fun", envir = teacher)
  b <- suppressWarnings(as.logical(a))
  if (any(is.na(b) | !b)) {
    if (teacher$no_match)
      sprintf("your function differs in %s",
              unlist(toString(test_dat[is.na(b) | !b])))
    else
      FALSE
  } else {
    TRUE
  }
}
