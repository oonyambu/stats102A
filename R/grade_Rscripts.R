grade_Rscripts <- function(student_dir,
                           teacher_file,
                           function_test_data,
                           weight = 1,
                           keep_par_names = FALSE,
                           file_name = "",
                           fun_dict = NULL,
                           controls = list(tolerance = 0, check.attributes = FALSE),
                           no_match = TRUE) {
  make_teacher(
    student_dir,
    teacher_file,
    function_test_data,
    weight,
    keep_par_names,
    fun_dict,
    controls,
    no_match
  )
  options(warn = -1)
  start <- names(.GlobalEnv)
  zipped <- list.files(
    student_dir,
    pattern = "\\.zip$",
    recursive = TRUE,
    full.names = TRUE
  )
  if (length(zipped) > 0) {
    sapply(zipped, my_unzip)
  }
  student_files <- list.files(
    student_dir,
    "\\.R$",
    ignore.case = TRUE,
    recursive = TRUE,
    full.names = TRUE
  )
  re <- do.call(rbind, lapply(student_files, compare))
  dat <- data.frame(
    ID = re[, 1],
    grade = as.numeric(re[, 2]),
    remark = re[, 3],
    stringsAsFactors = FALSE
  )
  if (!is.null(file_name)) {
    fl_1 <- sub("\\\\[^\\]+\\\\?$", "", normalizePath(student_dir))
    file_name <-
      normalizePath(file.path(fl_1, "result_gradeRscripts.csv"))
  }
  options(warn = 0)
  rm(list = setdiff(names(.GlobalEnv), start), envir = .GlobalEnv)
  file_write(aggregate(. ~ ID, dat, agg_fun), file_name)
}



opts_stats102A <- function(..., reset = FALSE) {
  opts(..., reset = reset)
}
