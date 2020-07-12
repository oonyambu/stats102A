
\name{grade_Rscripts}
\alias{grade_Rscripts}
\title{Grading the Rscripts to produce a dataframe}
\usage{
grade_Rscripts(student_dir, teacher_file, function_test_data, weight
                    = 1, keep_par_names = FALSE, file_name = "", fun_dict
                    = NULL, controls = list(tolerance = 0,
                    check.attributes = FALSE), no_match = TRUE)
}
\arguments{
\item{student_dir}{ The directory that containts the students folders}

\item{teacher_file}{ The file that contains the correct functions}

\item{function_test_data}{ Data that will be used to test the functions. The data must be a named list, with the names corresponding to the functions to be tested. For example If you are testing two functions, then the list should be of length 2 where each element corresponds to the function to be tested. Each element of the list can either be a dataframe or a list of the test cases (The Second Depth).}

\item{weight}{A vector containing the maximum points for each function tested. Defaults to 1 for any function whose weight is not given}

\item{keep_par_names}{ A logical parameter that is used to determine whether the parameter names in the test data and the functions are the same. Default is FALSE}

\item{file_name}{The file to write the results to. if not given, the results will be written to a file in the working directory by the name results.csv. If NULL, the function will return the result dataframe instead}

\item{fun_dict}{A dictionary like named-list that contain the mapping from the teacher's function to the student's function, in case where one function has multiple names. The default is NULL. The names of the list are the teacher's functions which each vector being the various names the students might have named their functions}

\item{controls}{A list of aguments that are passed into `all.equal` function. The default is list(tolerance = 0, check.attributes = FALSE). You could add more arguments in the list}

\item{no_match}{A logical value as to whether to print the data where the teacher and student functions gave conflicting results. The default is TRUE}
}

\value{
The function returns a dataframe with the StudentID, grade and remarks for each tested function
}
\description{
grades students functions.
}

\examples{
# if you are testing two functions add(a,b), multiply(x,y,z)
# test cases for :
#             add:               multiply:
#            x   y                 x   y   z
#           10  20                10  20  30
#            0  -3                -3   2   1
#                                  2  -3   2


add_data <- list(list(10,20),list(0,-3))#wrote each argument since is short

mult_data <- data.frame(x = c(10,-3,2),y = c(20,2,-3),z = c(30,1,2))

test_data <- list(add = add_data, multiply = mult_data) #must be a list

\dontrun{grade_Rscripts("../students/","add_multiply_functions.R",test_data)}

\dontrun{grade_Rscripts("../students/","add_multiply_functions.R"
,test_data, fun_dict = list(multiply = c("mult","multiplication")))}

}
