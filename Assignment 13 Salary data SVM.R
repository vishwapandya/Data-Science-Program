
#simple linear SVM with vanilladot
library(kernlab)
Salary_classifier <- ksvm(Salary ~ ., data = `SalaryData_Train(1)`,
                          kernel = "vanilladot")

# predictions on testing dataset
Salary_predictions <- predict(Salary_classifier, `SalaryData_Test(1)`)

head(Salary_predictions)

table(Salary_predictions,`SalaryData_Test(1)`$Salary)


agreement <- Salary_predictions == `SalaryData_Test(1)`$Salary
table(agreement)
prop.table(table(agreement))

#simple linear SVM with rbfdot
Salary_classifier_rbf <- ksvm(Salary ~ ., data = `SalaryData_Train(1)`, kernel = "rbfdot")

# predictions on testing dataset
Salary_predictions_rbf <- predict(Salary_classifier_rbf, `SalaryData_Test(1)`)

agreement_rbf <- Salary_predictions_rbf == `SalaryData_Test(1)`$Salary
table(agreement_rbf)
prop.table(table(agreement_rbf))
