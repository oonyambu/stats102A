
convt2list <- function(dat, keep_par_names = FALSE){
 if(is.data.frame(dat))
    unname(by(dat,1:nrow(dat),mklst, keep_par_names))
  else
   dat
}


has_gradable_files <- function(student_dir,
                                conform_Naming = NULL,
                                Rscript = TRUE,
                                Rmd = TRUE,
                                pdf_or_html = TRUE){

  files_required <- c(Rscript,Rmd,pdf_or_html)
  regx <- c("R","Rmd","pdf|html")[files_required]
  cnt <- sum(files_required) # # of files -> max of 3 ie R, Rmd, html|pdf
  if (!cnt) stop("at least one document must be specified")
  regx <- paste0("\\.(", paste0(regx, collapse = "|"), ")$")
  sfiles <- list.files(student_dir, pattern = regx,
                       recursive = TRUE,ignore.case = TRUE)
  id <- sub("/.*","", sfiles)
  has_files <- grepl(regx,sfiles,ignore.case = TRUE)
  result <- data.frame(id, has_files,
                       row.names = NULL)
  if(!is.null(conform_Naming))
    result$check_nm <- grepl(conform_Naming,
                    sub(regx,"",basename(sfiles),ignore.case = TRUE))

  aggregate(.~id,result,function(x)sum(x)==cnt)
}

is_knitable_Rmd <- function(student_dir){
  student_dir <- gsub("\\\\","/",normalizePath(student_dir))
  new_dir <- paste0(getwd(),"/Rmd_files_knit")
  avail_pkgs <- search()
  student_files <- list.files(student_dir, "\\.Rmd", recursive = TRUE,
                              ignore.case = TRUE,full.names = TRUE)

  ID <- sub("/.*","",sub(paste0(student_dir,"/*"),"",dirname(student_files)))
  is_knitable <-  sapply(student_files,knit,new_dir,USE.NAMES = FALSE)
  sapply(setdiff(search(),avail_pkgs),detach,character.only = TRUE)
  unlink(new_dir, TRUE, TRUE)
  data.frame(ID, is_knitable)
}

# grade102A <- function(hw,file = paste0(getwd(),"/final.csv"),cache = FALSE){
#   pat <- ls(.teacher,pattern = "^result_")
#   if (length(pat)<0){
#    if(exists(hw,.teacher)& cache)  pat <- hw
#     else stop("You have not run ")
#   }
#   lst <- mget(pat,.teacher)
#   rm(list = pat, envir = .teacher)
#   result <- Reduce(function(x, y)merge(x, y, by = "ID", all = TRUE), lst)
#   file_write(result,file,hw)
# }
#
