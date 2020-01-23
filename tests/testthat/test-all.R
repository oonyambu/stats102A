tst <- data.frame(matrix(c(
  1071, 462,
  18678, 120,
  24, 60,
  30, 24,
  98, 99,
  0, 10,
  33, 121,
  432, 1452,
  462, 1452,
  -3, 5
), ncol = 2, byrow = T))
library(stats102A)
tst1 <- list(
  gcd = convt2list(tst),
  gcd_3 = convt2list(cbind(tst, c(42, 30, 100, 36, 77, 13, 69, 18, 54, 4))),
  is_prime = list(100, 1298, 1:100), get_factors = list(32, 564, 100, 98, 97)
)
students <- "D:/Work/school/102A/LEC3_HW1/"
teacher <- "D:/Work/school/102A/homeworks/987654321_stat102a_test.R"
stats102A::grade_Rscripts(students, teacher, tst1,
  fun_dict = list(gcd_3 = c("gcd_three", "gcd3")), file_name = NULL
)
