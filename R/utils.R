teacher <- new.env()

teacher$opts_stats102A <- list(
  time_limit_knit = 4,
  time_limit_compute = 0.5,
  max_space = 30,
  install_missing_packages = TRUE,
  retain_installed_packages = TRUE,
  check_for = list()
)

teacher$opts_stats102A_use <- teacher$opts_stats102A
teacher$no_match <- FALSE



comp <-
  function(x, stud_env, teacher) {

    capture.output(student <- try({
      setTimeLimit(teacher$opts_stats102A_use$time_limit_compute,transient = TRUE)
      suppressMessages(do.call(stud_env$stud_fun, as.list(x)), stud_env)
    },
    silent = TRUE))
    correct <- try(do.call(teacher$teach_fun, as.list(x)), silent = TRUE)
    if (inherits(correct, "try-error"))
      inherits(student, "try-error") | is.null(student)
    else {
      if (inherits(student, "try-error")) {
        return(remark = simpleError(student)$message)
      }
      if (length(correct) == 0) {
        return(length(student) == 0)
      }
      a <- do.call(all.equal, c(list(student, correct), teacher$controls))
      if (is.logical(a))
        a
      else
        FALSE
    }
  }

teacher$sp <-
  sprintf("%%%ds",-teacher$opts_stats102A_use$max_space)




fun_comp <- function(fun_name, stud_env) {
  cat("\t\t", sprintf(teacher$sp, fun_name))
  test_data <- teacher$test_data[[fun_name]]
  fn <-
    match(teacher$fun_dict[[fun_name]], nm_s <- names(stud_env), 0)
  s_f_name <- if (length(nm_s[fn]) == 1)  nm_s[fn]  else    fun_name
  cat("Done!\n")
  if (!exists(s_f_name, stud_env, inherits = FALSE, mode = "function")) {
    return(c(
      grade = 0,
      remark = paste(s_f_name,
                     "0. You are missing the",
                     s_f_name, "function")
    ))
  }

  stud_env$stud_fun <- get0(s_f_name, stud_env, "function", FALSE)
  teacher$teach_fun <- get0(fun_name, teacher, "function", FALSE)


  stopifnot(is.function(stud_env$stud_fun), is.function(teacher$teach_fun))
  .ln = if(is.list(test_data[[1]])) length(test_data[[1]]) else 1
   cat(length(formalArgs(teacher$teach_fun)), .ln)
  if (length(formalArgs(teacher$teach_fun)) != .ln) {
    stop("Incorrect number of arguments for the test data as compared to ",
         fun_name,
         " args")
  }
  val <- sapply(test_data, comp, stud_env, teacher)
  ln <- as.logical(val)
  fin_val <-
    sum(ln, na.rm = TRUE) / length(ln) * teacher$weight[fun_name]
  if (any(grepl("object.*not found", val)))
    fin_val <- 0
  remark <- paste(s_f_name, fin_val)
  if(teacher$no_match){
    if (any(is.na(ln) | !ln)) {
      tss <- test_data[is.na(ln) | !ln]
      ln <- lengths(tss)
      tss1 <- tss[ln <= min(ln) + 3]
      if (min(ln) < 4) {
        not_work <- sapply(sample(tss1, min(c(
          3, length(tss1)
        ))),
        function(x)
          toString(unlist(x)))
      } else {
        not_work <- tss[[which.min(ln)]]
      }
      remark <- paste(
        s_f_name,
        round(fin_val, 2),
        "Your",
        s_f_name,
        "could not work on some data like",
        paste0(not_work, collapse = "; and "),
        toString(unique(val[is.na(ln)]))
      )
    }
  }
  not_to_have <- teacher$opts_stats102A_use[[c('check_for',fun_name)]]
  cc <- sapply(not_to_have, grepl, deparse1(stud_env$stud_fun))
  if(any(cc)) {
    return(c(grade = 0,
              remark = paste("Your function", fun_name, "uses",
                             toString(gsub("\\W+|\\\\b", "", not_to_have[cc])))))
  }
  c(grade = fin_val, remark = remark)
}

compare <- function(student_file) {
  stud_env <- new.env()
  avail_pkgs <- search()
  studentID <- basename(dirname(student_file))
  stud_env$ID <- studentID
  cat("Grading", studentID, "\n")
  if (has_install(student_file)){
    return(cbind(ID = studentID, remark = "Installing a package-cannot grade"))
    }
  scr <- try(source(student_file, stud_env), TRUE)
  if (inherits(scr, "try-error")) {
      if (grepl("no package called", scr)) {
        mes <- trimws(sub(".*no package called", "", scr))
        install.packages(gsub("\\W+", "", mes))
        compare(student_file)
      }
    {
      sapply(setdiff(search(), avail_pkgs), detach, character.only = TRUE)
    return(c(
      ID = studentID,
      grade = 0,
      remark = "Your file could not be sourced!"
    ))
    }
  }
  s <- cbind(ID = studentID,
             t(sapply(
               names(teacher$fun_dict), fun_comp, stud_env
             )))
  sapply(setdiff(search(), avail_pkgs), detach, character.only = TRUE)
  cat("\t\t\t\t\tDone with", studentID, "!!!\n\n")
  s
}


make_teacher <- function(student_dir,
                         teacher_file,
                         function_test_data,
                         weight = 1,
                         keep_par_names = FALSE,
                         fun_dict = NULL,
                         controls = list(), no_match) {
  source(teacher_file, teacher)
  functions_to_test <- names(function_test_data)
  teacher$weight <- set_name(functions_to_test, weight)
  teacher$test_data <- function_test_data
  teacher$fun_dict <- `names<-`(as.list(functions_to_test),
                                functions_to_test)
  if (!is.null(fun_dict)) {
    teacher$fun_dict <- unstack(stack(c(teacher$fun_dict, fun_dict)))
  }
  teacher$keep_par_names <- keep_par_names
  teacher$controls <- controls
  teacher$no_match <- no_match

}

has_install <- function(path) {
  any(grepl("^[^#]*install",
            suppressWarnings(suppressMessages(readLines(path)))))
}



mklst <- function(x, keep_par_names) {
  if (keep_par_names)
    as.list(x)
  else
    unname(as.list(x))
}


agg_fun <- function(x) {
  x <- type.convert(x, as.is = TRUE)
  if (is.numeric(x)) {
    sum(x)
  } else {
    paste0(trimws(x), collapse = "; ")
  }
}

file_write <- function(x, fl,
                       gradeItem = "result_gradeRscripts") {
  assign(gradeItem, x, teacher)
  if (is.null(fl)) {
    return(x)
  }
  write.csv(x, fl, row.names = FALSE)
  cat("The results are in", fl)
}



set_name <- function(funs, weights) {
  len_f <- length(funs)
  len_w <- length(weights)
  nm <- names(weights)
  if (len_w == 1) {
    if (is.null(nm)) {
      weights <- `names<-`(rep(weights, len_f), funs)
    } else {
      weights[setdiff(funs, nm)] <- 1
    }
  }
  else {
    if (is.null(nm)) {
      if (len_f == len_w) {
        names(weights) <- funs
      } else {
        stop("unequal length of weights and number of functions")
      }
    }
    else {
      if (any(nm == "")) {
        weights[setdiff(funs, nm)] <- weights[nm == ""]
      } else {
        weights[setdiff(funs, nm)] <- 1
      }
    }
  }
  weights[names(weights) != ""]
}

knit <- function(path, new_dir, new_file) {
  ID <- basename(dirname(path))
  cat("Knitting", ID, "\n")
  if (has_install(path)) {
    cat(
      ID,
      "Not knittable - Installing a package",
      "\n",
      file = new_file,
      append = TRUE
    )
    return(FALSE)
  }

  tried <- try({
    setTimeLimit(teacher$opts_stats102A_use$time_limit_knit,transient = TRUE)
    rmarkdown::render(
      path,
      "html_document",
      output_dir = new_dir,
      knit_root_dir = dirname(path),
      clean = TRUE,
      quiet = TRUE
    )
  },
  silent = TRUE)
  if (inherits(tried, "try-error")) {
    if (grepl("no package called", tried)) {
      mes <- trimws(sub(".*no package called", "", tried))
      install.packages(gsub("\\W+", "", mes))
      knit(path, new_dir, new_file)
    }
    else {
      cat(ID,
          gsub("\n", "", tried),
          "\n",
          file = new_file,
          append = TRUE)
    }
  }
  else {
    cat(ID, "Successful", "\n", file = new_file, append = TRUE)
  }
  ! inherits(tried, "try-error")
}

my_unzip <- function(zp) {
  path <- dirname(zp)
  unzip(zp, exdir = path)
  unlink(list.files(path, pattern = "_", full.names = TRUE),
         recursive = TRUE, force = TRUE)
  fls <- list.files(sub("\\.zip$", "", zp), full.names = TRUE)
  file.copy(fls, path)
  unlink(dirname(fls[1]), recursive = TRUE, force = TRUE)
  file.remove(zp)
}

opts <- function(..., reset = FALSE) {
  y <- list(...)
  nm <- setdiff(names(y), names(teacher$opts_stats102A_use))
  if (length(nm)) stop("There are no options for ", toString(nm))
  if (length(y) > 0)
    teacher$opts_stats102A_use <- modifyList(teacher$opts_stats102A_use, y)
  if (reset) teacher$opts_stats102A_use <- teacher$opts_stats102A
  if (!length(y) & !reset) teacher$opts_stats102A_use
}




