
\name{uid_as_seed}
\alias{uid_as_seed}
\title{Check whether Student used their own uid as seed}
\usage{
uid_as_seed(student_dir)
}
\arguments{

\item{student_dir}{A character string. The directory that contains student folders/files. (Not the student file itself)
}
}

\value{
The function returns a dataframe with id of the students column and a logical column indicating whether they used their UID as the seed
}


\description{
Desires to reduce copying of code between students
}

\examples{
## Not run:

library(stats102A)
\dontrun{uid_as_seed("../students")}

## End(Not run)
}
