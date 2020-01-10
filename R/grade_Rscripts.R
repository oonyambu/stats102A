grade_Rscripts <- function(student_dir,teacher_file,
                    function_test_data,weight = 1,keep_par_names = FALSE){
  source(teacher_file,.teacher)
  student_files <- list.files(student_dir,"\\.R$",ignore.case = TRUE,
                              recursive = TRUE,full.names = TRUE)
  functions_to_test <- names(function_test_data)
  nm <- names(weight)
  weight[setdiff(functions_to_test,nm)] <-
           if(length(nm)==1) weight else 1
  .finalize <- function(f){
    re <-lapply(student_files,.compare,f,
                function_test_data[[f]], weight, keep_par_names)
   do.call(rbind.data.frame, c(stringsAsFactors = FALSE, re))
  }
  result <- do.call(rbind.data.frame,
          c(stringsAsFactors = FALSE,
            lapply(functions_to_test,.finalize)))
  aggregate(.~ID,result,agg_fun)
  }



















