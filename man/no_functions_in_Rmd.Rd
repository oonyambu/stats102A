
\name{no_functions_in_Rmd}
\alias{no_functions_in_Rmd}
\title{Indicates whether a student does not have a function in the markdown}
\usage{
no_functions_in_Rmd(student_dir)
}
\arguments{


\item{student_dir}{A character value of length 1. The directory of the students to be graded}
}

\value{
A dataframe with 2 columns. The ID column and a logical column Indicating whether a student does not have a function in the markdown}

\description{
Determine whether the student included functions in their markdown
}

\examples{
\dontrun{no_functions_in_Rmd(student_dir)}
}
