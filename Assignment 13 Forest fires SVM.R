
#partition the data
intraininglocal<- createDataPartition(forestfires$size_category,p=0.5,list=F)
FF_train<- forestfires[intraininglocal,]
FF_test<- forestfires[-intraininglocal,]

#simple linear SVM with vanilladot
library(kernlab)
FF_classifier <- ksvm(size_category ~ ., data = FF_train,
                          kernel = "vanilladot")

# predictions on testing dataset
FF_predictions <- predict(FF_classifier, FF_test)

head(FF_predictions)

table(FF_predictions,FF_test$size_category)


agreement <- FF_predictions == FF_test$size_category
table(agreement)
prop.table(table(agreement))

#simple linear SVM with rbfdot
FF_classifier_rbf <- ksvm(size_category ~ ., data = FF_train, kernel = "rbfdot")

# predictions on testing dataset
FF_predictions_rbf <- predict(FF_classifier_rbf, FF_test)

agreement_rbf <- FF_predictions_rbf == FF_test$size_category
table(agreement_rbf)
prop.table(table(agreement_rbf))
