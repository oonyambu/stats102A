
convt2list <- function(dat, keep_par_names = FALSE){
  if(is.data.frame(dat))
    unname(by(dat,1:nrow(dat),.mklst, keep_par_names))
  else dat
}
