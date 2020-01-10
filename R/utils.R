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
.compare <- function(student_file, fun_name, test_data,weight,keep_par_names){
  stud_env <- new.env()
  test_data <- convt2list(test_data,keep_par_names)
  studentID <- sub("\\D+$","",s_base <- basename(student_file))
  if(!nchar(studentID)) studentID <- s_base
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
    remark = " "
    fin_val <- mean(val)
    if(any(!val)) {
      not_work <- unlist(sample(test_data[!val],1))
      remark <- paste(fun_name,fin_val,"Your",fun_name,
                      "could not work on some data like",toString(not_work))
    }
    list(ID = studentID, grade = fin_val ,remark = remark)
  }
}

.mklst <- function(x, keep_par_names) {
  if (keep_par_names) as.list(x) else unname(as.list(x))
}


agg_fun <- function(x){
  x <- type.convert(x,as.is = TRUE)
  if (is.numeric(x)) sum(x)
  else paste0(trimws(x),collapse = "\n")
}

file_write <- function(x, fl){
  if(is.null(fl)) return(x)
  if(fl) write.csv(x,fl,row.names = FALSE)
  else write.csv(x, "result.csv")
}
