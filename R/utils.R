teacher <- new.env()
comp <- function(x,studentFUN,correctFUN,class_value = "numeric"){
  student <- try({setTimeLimit(0.5);suppressMessages(
    suppressWarnings(do.call(studentFUN,as.list(x))))},silent = TRUE)
  correct <- try(do.call(correctFUN,as.list(x)),silent = TRUE)
  if(is(correct,"try-error")){
    nl <- is.null(student)
    if(!nl){
      mes <- simpleError(student)$message
      if(is(student,'try-error')&grepl("CPU time limit",mes)){
          return(mes)
        }
      !is(student,class_value)|is.na(student)
    }
    else TRUE
  }
  else{
    if (is(student,'try-error')){
      return(remark = simpleError(student)$message)
    }
    student == correct
  }
}

fun_comp <- function(fun_name,stud_env){
  test_data <- teacher$test_data[[fun_name]]
  fn <- match(teacher$fun_dict[[fun_name]],nm_s <- names(stud_env),0)
  s_f_name <- if(length(nm_s[fn]) == 1) nm_s[fn]  else fun_name

  if(!exists(s_f_name, stud_env, inherits = FALSE, mode = "function")){
    return (c(grade = 0,
              remark = paste(s_f_name,
                             "0. You are missing the",
                             s_f_name, "function")))
  }

  stud_fun <-  get0(s_f_name,stud_env,'function',FALSE)
  teach_fun <- get0(fun_name,teacher,'function',FALSE)
  if(length(formalArgs(teach_fun))!=length(test_data[[1]]))
    stop("Incorrect number of arguments for the test data as compared to the function")
  val <- sapply(test_data, comp,stud_fun, teach_fun)
  ln <- as.logical(val)
  fin_val <- sum(ln,na.rm = TRUE)/length(ln) * teacher$weight[fun_name]
  if (any(grepl("object.*not found",val))) fin_val <- 0
  remark = paste(s_f_name, fin_val)
  if(any(is.na(ln)|!ln)) {
    tss <- test_data[is.na(ln)|!ln]

    not_work<-sapply(sample(tss,min(c(3,length(tss)))),function(x)toString(unlist(x)))

    remark <- paste(s_f_name, round(fin_val,2), "Your", s_f_name,
                    "could not work on some data like",
                    paste0(not_work,collapse = "; and "),toString(unique(val[is.na(ln)])))
  }

  c (grade = fin_val , remark = remark)
}

compare <- function(student_file){
  stud_env <- new.env()
  studentID <- sub(".*/", "", dirname(student_file))
  stud_env$ID  <- studentID
  scr <- try(source(student_file,stud_env),TRUE)
  if (is(scr,"try-error")){
    return( c(ID = studentID,grade=0,
              remark =simpleError(scr)$message))
  }
  cbind(ID = studentID,
        t(sapply(names(teacher$fun_dict),fun_comp,stud_env)))
}


make_teacher <- function(student_dir,teacher_file,
                         function_test_data,weight = 1,
                         keep_par_names = FALSE,
                         fun_dict = NULL){
  source(teacher_file,teacher)
  functions_to_test <- names(function_test_data)
  teacher$weight <- set_name(functions_to_test,weight)
  teacher$test_data <- function_test_data
  teacher$fun_dict <- `names<-`(as.list(functions_to_test),
                                functions_to_test)
  if (!is.null(fun_dict)){
    teacher$fun_dict <- unstack(stack(c(teacher$fun_dict,fun_dict)))
  }

  teacher$keep_par_names <- keep_par_names
}









mklst <- function(x, keep_par_names) {
  if (keep_par_names) as.list(x) else unname(as.list(x))
}


agg_fun <- function(x){
  x <- type.convert(x,as.is = TRUE)
  if (is.numeric(x)) sum(x)
  else paste0(trimws(x),collapse = "; ")
}

file_write <- function(x, fl,
                       gradeItem = "result_gradeRscripts"){
  assign(gradeItem,x,teacher)
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

knit <- function(path,new_dir){
  tried <- try({setTimeLimit(4);
    rmarkdown::render(path,"html_document",
                      output_dir = new_dir,clean = TRUE,quiet = TRUE)},
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
