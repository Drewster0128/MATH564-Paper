library(VGAM)

#data pre-processing
student_performance <- read.csv("datasets/student_performance.csv")

#convert studentID to rownames
rownames(student_performance) <- student_performance$StudentID
student_performance <- student_performance[, -1]

#scale quantitative predictors
student_performance[, c("Age", "StudyTimeWeekly", "Absences")] <- scale(student_performance[, c("Age", "StudyTimeWeekly", "Absences")])

#drop GPA
student_performance <- student_performance[, !colnames(student_performance) %in% c("GPA")]

#factorize qualitative variables
for(qualitative_variable in colnames(student_performance[, !colnames(student_performance) %in% c("Age", "StudyTimeWeekly", "Absences")]))
{
  student_performance[, qualitative_variable] <- as.factor(student_performance[, qualitative_variable])
  if(qualitative_variable == "Gender")
  {
    levels(student_performance[, qualitative_variable]) <- c("Male", "Female")
  }
  else if(qualitative_variable == "Ethnicity")
  {
    levels(student_performance[, qualitative_variable]) <- c("Caucasian", "African American", "Asian", "Other")
  }
  else if(qualitative_variable == "ParentalEducation")
  {
    levels(student_performance[, qualitative_variable]) <- c("None", "High School", "Some College", "Bachelor's", "Higher")
  }
  else if(qualitative_variable %in% c("Tutoring", "Extracurricular", "Sports", "Music", "Volunteering"))
  {
    levels(student_performance[, qualitative_variable]) <- c("No", "Yes")
  }
  else if(qualitative_variable == "ParentalSupport")
  {
    levels(student_performance[, qualitative_variable]) <- c("None", "Low", "Moderate", "High", "Very High")
  }
  else
  {
    levels(student_performance[, qualitative_variable]) <- c("A", "B", "C", "D", "F")
  }
}

#Visual Exploration

#feature distribution
for(feature in colnames(student_performance))
{
  #make a pie-chart if feature is qualitative, otherwise make a histogram
  if(!feature %in% c("Age", "StudyTimeWeekly", "Absences"))
  {
    pie(sapply(unique(student_performance[, feature]), function(x) {
      return(sum(ifelse(student_performance[, feature] == x, 1, 0))/nrow(student_performance))
    }),
    labels = unique(student_performance[, feature]),
    main = sprintf("Distribution of %s", feature))
  }
  else
  {
    print(feature)
    hist(student_performance[, feature],
         main = sprintf("Distribution of %s", feature),
         xlab = sprintf("Scaled %s", feature))
  }
}

for(predictor in colnames(student_performance[, !colnames(student_performance) %in% c("GradeClass")]))
{
  plot(
    x = student_performance$GradeClass,
    y = student_performance[, predictor],
    xlab = "Grade Class",
    ylab = predictor,
  )
}

#Model fitting

bootstrap_models <- array(dim = c(300))
out_of_sample_probabilities <- array(dim = c(300, 2392, 5), dimnames = list(c(1:300), rownames(student_performance), levels(student_performance$GradeClass)))
for(i in 1:300)
{
  #bootstrap and oob indices
  bootstrap_indices <- sample(1:2392, 2392, replace = TRUE)
  out_of_sample_indices <- c(1:2392)[!c(1:2392) %in% bootstrap_indices]

  #fit multinomial logistic regression model
  model <- vglm(GradeClass ~ .,
                family = multinomial,
                data = student_performance[bootstrap_indices,])
  
  
  
  predictions <- predict(model, student_performance[out_of_sample_indices,], type = "response")
  for(j in 1:nrow(predictions))
  {
    out_of_sample_probabilities[i,out_of_sample_indices[j],] <- predictions[j,]
  }
}

out_of_sample_predictions <- apply(out_of_sample_probabilities, MARGIN = c(1,2), function(x) {
  max_index <- which.max(x)
  return(levels(student_performance$GradeClass)[max_index])
})

prediction_test <- out_of_sample_predictions[1, !sapply(out_of_sample_predictions[1,], function(x) {
  return(identical(x, character(0)))
})]

out_of_sample_accuracy_percentage <- apply(out_of_sample_predictions, MARGIN = 1, function(predictions) {
  correct <- 0
  seen <- 0
  for(i in 1:2392)
  {
    if(!identical(predictions[i][[1]], character(0)))
    {
      seen <- seen + 1
      if(predictions[i] == levels(student_performance$GradeClass)[student_performance$GradeClass[i]])
      {
        correct <- correct + 1
      }
    }
  }
  return(correct/seen)
})

#Confusion matrix for each grade
confusion_matrix <- function(predictions, grade_letter)
{
  tp <- 0
  fp <- 0
  fn <- 0
  tn <- 0
  
  for(i in 1:2392)
  {
    if(!identical(predictions[i][[1]], character(0)))
    {
      if((predictions[i] == grade_letter) && (levels(student_performance$GradeClass)[student_performance$GradeClass[i]] == grade_letter))
      {
        tp <- tp + 1
      }
      else if((predictions[i] != grade_letter) && (levels(student_performance$GradeClass)[student_performance$GradeClass[i]] == grade_letter))
      {
        fn <- fn + 1
      }
      else if((predictions[i] == grade_letter) && (levels(student_performance$GradeClass)[student_performance$GradeClass[i]] != grade_letter))
      {
        fp <- fp + 1
      }
      else
      {
        tn <- tn + 1
      }
    }
  }
  return(matrix(c(tp, fp, fn, tn), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("Prediction:True", "Prediction:False"), c("Actual:True", "Actual:False"))))
}

bootstrap_confusion_matrices <- array(dim = c(300, 5, 2, 2), dimnames = list(c(1:300), levels(student_performance$GradeClass), c("Prediction:True", "Prediction:False"), c("Actual:True", "Actual:False")))

for(i in 1:300)
{
  for(j in 1:5)
  {
    bootstrap_confusion_matrices[i,j,,] <- confusion_matrix(out_of_sample_predictions[i,], levels(student_performance$GradeClass)[j])
  }
  
}

#recall and precision
recall <- function(confusion_matrix)
{
  tp <- confusion_matrix[1,1]
  fn <- confusion_matrix[2,1]
  return(tp/(tp+fn))
}

precision <- function(confusion_matrix)
{
  tp <- confusion_matrix[1,1]
  fp <- confusion_matrix[1,2]
  return(tp/(tp+fp))
}

bootstrap_recall <- matrix(nrow = 300, ncol = 5, dimnames = list(c(1:300), levels(student_performance$GradeClass)))
bootstrap_precision <- matrix(nrow = 300, ncol = 5, dimnames = list(c(1:300), levels(student_performance$GradeClass)))

for(i in 1:300)
{
  for(j in 1:5)
  {
    bootstrap_recall[i,j] <- recall(bootstrap_confusion_matrices[i,j,,])
    bootstrap_precision[i,j] <- precision(bootstrap_confusion_matrices[i,j,,])
  }
}

#tpr and fpr


#anova testing