
\name{is_knitable_Rmd}
\alias{is_knitable_Rmd}
\title{Check whether r markdown is knitable}
\usage{
is_knitable_Rmd(student_dir)
}
\arguments{

\item{student_dir}{A character string. The directory that contains student folders/files. (Not the student file itself)
}
}


\value{
The function returns a dataframe with id of the students column and a logical column indicating whether their .Rmd is knitable
}


\description{
Checks whether a student submitted a knitable .Rmd
}

\examples{
## Not run:

library(stats102A)
\dontrun{is_knitable_Rmd("../students")}

## End(Not run)
}
