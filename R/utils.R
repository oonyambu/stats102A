.teacher <- new.env()
.comp <- function(x,studentFUN,correctFUN,class_value = "numeric"){
  student <- try(do.call(studentFUN,as.list(x)),silent = TRUE)
  correct <- try(do.call(correctFUN,as.list(x)),silent = TRUE)
  if(is(correct,"try-error")){
    nl <- is.null(student)
    if(!nl)
      !is(student,class_value)|is.na(student)
    else nl
  }
  else{
    if (is(student,'try-error')){
      return(list(grade = 0,
                  remark = simpleError(student)$message))
    }
    student == correct
  }
}
.compare <- function(student_file, fun_name, test_data, weight,keep_par_names){
  stud_env <- new.env()
  test_data <- convt2list(test_data,keep_par_names)
  studentID <- sub(".*/", "", dirname(student_file))
  scr <- try(source(student_file,stud_env),TRUE)
  if (is(scr,"try-error")){
    return( list(grade=0,
              remark = paste(fun_name,
                             "0. can not source your file")))
  }
  if(!exists(fun_name, stud_env,inherits = FALSE, mode = "function")){
    return (list(ID = studentID, grade = 0,
                 remark = paste(fun_name,
                                "0. You are missing the",
                                fun_name, "function")))
  }
  else{
    stud_fun <-  get0(fun_name,stud_env,'function',FALSE)
    teach_fun <- get0(fun_name,.teacher,'function',FALSE)
    if(length(formalArgs(teach_fun))!=length(test_data[[1]]))
      stop("Incorrect number of arguments for the test data as compared to the function")
    n <- .comp(test_data[[1]],stud_fun,teach_fun)
    if(is.list(n)) return(c(ID = studentID,n))
    val <- c(n,if(length(test_data)>1) sapply(test_data[-1], .comp,stud_fun, teach_fun))
    fin_val <- mean(val) * weight
    remark = paste(fun_name, fin_val)
    if(any(!val)) {
      not_work <- unlist(sample(test_data[!val],1))
      remark <- paste(fun_name, fin_val, "Your", fun_name,
                      "could not work on some data like",
                      toString(not_work))
    }
    list(ID = studentID, grade = fin_val , remark = remark)
  }
}

.mklst <- function(x, keep_par_names) {
  if (keep_par_names) as.list(x) else unname(as.list(x))
}


agg_fun <- function(x){
  x <- type.convert(x,as.is = TRUE)
  if (is.numeric(x)) sum(x)
  else paste0(trimws(x),collapse = "; ")
}

file_write <- function(x, fl = paste0(getwd(),"/result_gradeRscripts.csv"),
                       gradeItem = "result_gradeRscripts"){
  assign(gradeItem,x,.teacher)
  if(is.null(fl)) return(x)
  write.csv(x, fl, row.names = FALSE)
  cat("The results are in", fl)
}



set_name <- function(funs,weights){
  len_f <- length(funs)
  len_w <- length(weights)
  nm <- names(weights)
  if(len_w == 1){
    if(is.null(nm)) weights <- `names<-`(rep(weights,len_f),funs)
    else weights[setdiff(funs,nm)] <- 1
  }
  else {
    if(is.null(nm)){
      if(len_f == len_w)names(weights) <- funs
      else stop("unequal length of weights and number of functions")
    }
    else{
      if (any(nm=="")) weights[setdiff(funs, nm)] <- weights[nm==""]
      else weights[setdiff(funs, nm)] <- 1
    }
  }
  weights[names(weights)!=""]
}

.knit <- function(path,new_dir){
  tried <- try(
    rmarkdown::render(path,"html_document",
                      output_dir = new_dir,clean = TRUE,quiet = TRUE),
    silent = TRUE)
  !is(tried,"try-error")
}


# run_all <- function(){
#   gradable_files <- readline("Do you want to check whether student submitted reduired files? y/n: ")
#   if(c("y","yes")%in%tolower(gradable_files)){
#     gr_files <- readline("Which files do you want to check (use comma/space as the separator)?: ")
#     gr_files <- sub("^(.)",'\\U\\1',tolower(unlist(strsplit(gr,"\\W+"))),perl = TRUE)
#   }
#   name_coform <- readline("Do you want to check the conformity of file names? y/n: ")
#   grade_scripts <- readline("Do you want to grade Rscripts? y/n: ")
#   knitable <- readline("Do you want to check whether the .Rmd is knitable? y/n: ")
#
# }
