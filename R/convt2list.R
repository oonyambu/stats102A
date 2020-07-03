
convt2list <- function(dat, keep_par_names = FALSE) {
  if (is.data.frame(dat)) {
    unname(by(dat, 1:nrow(dat), mklst, keep_par_names))
  } else {
    dat
  }
}


has_gradable_files <- function(student_dir,
                               conform_Naming = NULL,
                               Rscript = TRUE,
                               Rmd = TRUE,
                               pdf_or_html = TRUE) {
  options(warn = -1)
  files_required <- c(Rscript, Rmd, pdf_or_html)
  regx <- c("R", "Rmd", "pdf|html")[files_required]
  cnt <- sum(files_required) # # of files -> max of 3 ie R, Rmd, html|pdf
  if (!cnt) stop("at least one document must be specified")
  regx <- paste0("\\.(", paste0(regx, collapse = "|"), ")$")
  sfiles <- list.files(student_dir,
    pattern = regx,
    recursive = TRUE, ignore.case = TRUE
  )
  id <- sub("/.*", "", sfiles)
  has_files <- grepl(regx, sfiles, ignore.case = TRUE)
  result <- data.frame(id, has_files,
    row.names = NULL
  )
  if (!is.null(conform_Naming)) {
    result$check_nm <- grepl(
      conform_Naming,
      sub(regx, "", basename(sfiles), ignore.case = TRUE)
    )
  }
  options(warn = 0)
  aggregate(. ~ id, result, function(x) sum(x) == cnt)
}

is_knitable_Rmd <- function(student_dir) {
  student_dir <- gsub("\\\\", "/", normalizePath(student_dir))
  new_dir <- paste0(getwd(), "/Rmd_files_knit")
  new_file <- file.path(getwd(), "is_knitable_results.txt")
  if (file.exists(new_file)) {
    file.remove(new_file)
  }
  file.create(new_file)
  avail_pkgs <- search()
  student_files <- list.files(student_dir, "\\.Rmd",
    recursive = TRUE,
    ignore.case = TRUE, full.names = TRUE
  )
  ID <- sub("/.*", "", sub(paste0(student_dir, "/*"), "", dirname(student_files)))
  is_knitable <- sapply(student_files, knit, new_dir, new_file, USE.NAMES = FALSE)
  sapply(setdiff(search(), avail_pkgs), detach, character.only = TRUE)
  unlink(new_dir, TRUE, TRUE)
  cat("the comments have been written to ", new_file)
  data.frame(ID, is_knitable)
}

no_functions_in_Rmd <- function(student_dir) {
  sfiles <- list.files(student_dir, "\\.Rmd$",
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  )
  ID <- sub(".*/", "", dirname(sfiles))
  data.frame(ID, has_no_functions = sapply(sfiles, has_no_function), row.names = NULL)
}

has_no_function <- function(path) {
  lns <- suppressWarnings(readLines(path))
  idx <- grep("```", lns, fixed = TRUE)
  R_chunks_idx <- match(grep("```\\{r.*?\\}", lns), idx)
  search_idx <-
    unlist(Map(":", idx[R_chunks_idx], idx[R_chunks_idx + 1]))
  ! any(grepl("<-\\s* function\\(.*?\\)", lns[search_idx]))
}

