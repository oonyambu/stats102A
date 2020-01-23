
\name{opts_stats102A}
\alias{opts_stats102A}
\title{set/get the stats102A options}
\usage{
 opts_stats102A(..., reset = FALSE)
 }
\arguments{

  \item{reset}{A logical Parameter on whether to reset the stats102A options}

  \item{...}{ The options to be set. Look at the details below}
}

\details{
\describe{
\item{time_limit_knit:}{A numeric value. Default is 4. The maximum allowable time to knit a file beyond which an error is produced}
\item{time_limit_compute:}{A numeric value. Default is 0.5. The maximum allowable time to a given computation}

\item{max_space:}{The space used to allign the text printed on the screen while grading the scripts}

\item{install_missing_packages:}{A logical value to determine whether to install the missing packages used in an Rmd file and then knit it. Default is TRUE. if FALSE, the packages used in aRMD, if not already installed will not be installed and thus knitting will be unsuccessful}

\item{retain_installed_packages:}{A logical value whether to retain any newly installed packages while knitting. The default is TRUE. will only be considered if install_missing_packages is set to TRUE
}
}

}
\value{
 When called as a get, it returns the list of all the options. When setting, no return value.
}
\description{
  Set or get the stats102A grading options
}

\examples{
opts_stats102A()
opts_stats102A(time_limit_knit = 10)
opts_stats102A()
opts_stats102A(reset = TRUE)
opts_stats102A()
}
