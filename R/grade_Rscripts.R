grade_Rscripts <- function(student_dir,teacher_file,
                    function_test_data,weight = 1,
                    keep_par_names = FALSE,
                    file_name = "",
                    fun_dict = NULL){

  make_teacher(student_dir,teacher_file,
               function_test_data,weight,
               keep_par_names ,
               fun_dict)

  student_files <- list.files(student_dir,"\\.R$",ignore.case = TRUE,
                               recursive = TRUE, full.names = TRUE)
  re <- do.call(rbind,lapply(student_files,compare))
  dat <-data.frame(ID = re[,1],grade = as.numeric(re[,2]),
                   remark=re[,3],stringsAsFactors = FALSE)
  if(!is.null(file_name))  {
    fl_1 <- sub("\\\\[^\\]+\\\\?$", "",normalizePath(student_dir))
    file_name <-normalizePath(file.path(fl_1,"result_gradeRscripts.csv"))
  }
  file_write(aggregate(.~ID,dat,agg_fun), file_name)
}



opts_stats102A <- function(...,reset = FALSE){
  opts(...,reset = reset)
}















