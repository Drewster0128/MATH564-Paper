library(boot)
library(regclass)
#data pre-processing
student_performance <- read.csv("datasets/student_performance.csv")
rownames(student_performance) <- student_performance$StudentID
student_performance <- student_performance[, -1]

#Categorical features: Gender, Ethnicity, Parental Education, Tutoring, Parental Support, Extracurricular, Sports, Music, Volunteering,

categorical_features <- c("Gender", "Ethnicity", "ParentalEducation", "Tutoring", "ParentalSupport", "Extracurricular", "Sports", "Music", "Volunteering")
numerical_features <- c("Age", "StudyTimeWeekly", "Absences")

#factor categorical features
categorical_data <- student_performance[, colnames(student_performance) %in% categorical_features]
for(column in colnames(categorical_data))
{
  categorical_data[, column] <- as.factor(categorical_data[, column])
}

normalize <- function(lst)
{
  max <- max(lst)
  min <- min(lst)
  return(sapply(lst, function(x) {
    return((x-min)/(max - min))
  }))
}

#scale numerical data
numerical_data <- apply(student_performance[, colnames(student_performance) %in% numerical_features], MARGIN = 2, function(column) {
  return(normalize(column))
})


feature_data <- cbind(categorical_data, numerical_data)
student_performance <- as.data.frame(cbind(feature_data, student_performance[, colnames(student_performance) %in% c("GradeClass")]))
colnames(student_performance)[length(colnames(student_performance))] <- "GradeClass"
student_performance$GradeClass <- as.factor(student_performance$GradeClass)

grade_class <- model.matrix(~ . -1, data = as.data.frame(student_performance$GradeClass))
colnames(grade_class) <- c("A", "B", "C", "D", "F")

student_performance <- cbind(student_performance, grade_class)
student_performance <- student_performance[, !colnames(student_performance) %in% c("GradeClass")]

features <- colnames(student_performance)[!colnames(student_performance) %in% colnames(grade_class)]

anova <- array(dim = c(5, 300, 12, 5),
               dimnames = list(
                 c("A", "B", "C", "D","F"), 
                 c(1:300), 
                 c("Gender", "Ethnicity", "ParentalEducation", "Tutoring", "ParentalSupport", "Extracurricular", "Sports", "Music", "Volunteering", "Age", "StudyTimeWeekly", "Absences"),
                 c("Df", "Deviance", "Resid. Df", "Resid. Dev", "Pr(>Chi)")))


logistic_bootstrapped_probabilites <- array(dim = c(5, 2392, 300))
rownames(logistic_bootstrapped_probabilites) <- c("A", "B", "C", "D", "F")
#boot strap resampling
bootstrap_predictions <- matrix(, nrow = 2392, ncol = 300)
bootstrap_predictions <- as.data.frame(bootstrap_predictions)
colnames(bootstrap_predictions) <- 1:300
for(i in 1:300)
{
  boot_sample_index <- sample(1:nrow(student_performance), size = 2392, replace = TRUE)
  boot_sample <- student_performance[boot_sample_index, ]
  out_of_sample <- c(1:nrow(student_performance))[!1:nrow(student_performance) %in% boot_sample_index]
  
  gradeA.fit <- glm(formula = A ~ .,
                    data = boot_sample[, c(features, "A")],
                    family = 'binomial')
  gradeB.fit <- glm(formula = B ~ .,
                    data = boot_sample[, c(features, "B")],
                    family = 'binomial')
  gradeC.fit <- glm(formula = C ~ .,
                    data = boot_sample[, c(features, "C")],
                    family = "binomial")
  gradeD.fit <- glm(formula = D ~ .,
                    data = boot_sample[, c(features, "D")],
                    family = "binomial")
  gradeF.fit <- glm(formula = F ~ .,
                    data = boot_sample[, c(features, "F")],
                    family = 'binomial')
  
  #anova
  gradeA.anova <- anova(gradeA.fit)[-1,]
  gradeB.anova <- anova(gradeB.fit)[-1,]
  gradeC.anova <- anova(gradeC.fit)[-1,]
  gradeD.anova <- anova(gradeD.fit)[-1,]
  gradeF.anova <- anova(gradeF.fit)[-1,]
  
  anova[1, i,,] = as.matrix(gradeA.anova)
  anova[2, i,,] = as.matrix(gradeB.anova)
  anova[3, i,,] = as.matrix(gradeC.anova)
  anova[4, i,,] = as.matrix(gradeD.anova)
  anova[5, i,,] = as.matrix(gradeF.anova)
  
  gradeA.predictions <- predict(gradeA.fit, student_performance[out_of_sample, ], type = "response")
  gradeB.predictions <- predict(gradeB.fit, student_performance[out_of_sample, ], type = "response")
  gradeC.predictions <- predict(gradeC.fit, student_performance[out_of_sample, ], type = "response")
  gradeD.predictions <- predict(gradeD.fit, student_performance[out_of_sample, ], type = "response")
  gradeF.predictions <- predict(gradeF.fit, student_performance[out_of_sample, ], type = "response")
  
  for(k in 1:length(out_of_sample))
  {
    logistic_bootstrapped_probabilites[1,out_of_sample[k],i] <- gradeA.predictions[k]
    logistic_bootstrapped_probabilites[2, out_of_sample[k], i] <- gradeB.predictions[k]
    logistic_bootstrapped_probabilites[3, out_of_sample[k], i] <- gradeC.predictions[k]
    logistic_bootstrapped_probabilites[4, out_of_sample[k], i] <- gradeD.predictions[k]
    logistic_bootstrapped_probabilites[5, out_of_sample[k], i] <- gradeF.predictions[k]
  }
  
  for(j in 1:length(gradeA.predictions))
  {
    max_index = which.max(c(gradeA.predictions[j], gradeB.predictions[j], gradeC.predictions[j], gradeD.predictions[j], gradeF.predictions[j]))
    if(max_index == 1)
    {
      prediction <- "A"
    }
    else if(max_index == 2)
    {
      prediction <- "B"
    }
    else if(max_index == 3)
    {
      prediction <- "C"
    }
    else if(max_index == 4)
    {
      prediction <- "D"
    }
    else
    {
      prediction <- "F"
    }
    bootstrap_predictions[out_of_sample[j], i] <- prediction
  }
}

#get average p value of each feature for chi-test from Anova
#c("Gender", "Ethnicity", "ParentalEducation", "Tutoring", "ParentalSupport", "Extracurricular", "Sports", "Music", "Volunteering", "Age", "StudyTimeWeekly", "Absences")
p_value_means <- apply(anova, MARGIN = c(1,3), function(x) {
  return(mean(x[, "Pr(>Chi)"]))
})

p_value_sd <- apply(anova, MARGIN = c(1,3), function(x) {
  return(sd(x[, "Pr(>Chi)"]))
})

#t-test
t_test_values <- apply(anova, MARGIN = c(1,3), function(x) {
  return(t.test(x[, "Pr(>Chi)"])$p.value)
})

#For gradeA binomial regression, tutoring, parentalsupport, studytimeweekly, and maybe absences matter
#For gradeB binomial regression, tutoring, parentalsupport, music, studytimeweekly, and maybe absences matter
#For gradeC binomial regression, maybe absences matter
#For gradeD binomial regression, maybe absences matter
#For gradeF binomial regression, tutoring, maybe parentalsupport, studytimeweekly, and maybe absences matter

weightA <- sum(ifelse(boot_sample$A == 1, 1, 0))/nrow(boot_sample)
weightB <- sum(ifelse(boot_sample$B == 1, 1, 0))/nrow(boot_sample)
weightC <- sum(ifelse(boot_sample$C == 1, 1, 0))/nrow(boot_sample)
weightD <- sum(ifelse(boot_sample$D == 1, 1, 0))/nrow(boot_sample)
weightF <- sum(ifelse(boot_sample$F == 1, 1, 0))/nrow(boot_sample)

probs <- apply(student_performance, MARGIN = 1, function(x) {
  if(x["A"] == 1)
  {
    return(1 - weightA)
  }
  else if(x["B"] == 1)
  {
    return(1 - weightB)
  }
  else if(x["C"] == 1)
  {
    return(1 - weightC)
  }
  else if(x["D"] == 1)
  {
    return(1 - weightD)
  }
  else
  {
    return(1 - weightF)
  }
})

remove_redundant_logistic_bootstrapped_probabilities <- array(dim = c(5, 2392, 300))
for(i in 1:300)
{
  boot_sample_index <- sample(1:nrow(student_performance), size = 2392, replace = TRUE, prob = probs)
  boot_sample <- student_performance[boot_sample_index, ]
  out_of_sample <- c(1:nrow(student_performance))[!1:nrow(student_performance) %in% boot_sample_index]
  
  
  weightsA <- ifelse(boot_sample$A == 1, 1 - weightA, weightA)
  
  
  weightsB <- ifelse(boot_sample$B == 1, 1 - weightB, weightB)
  
  
  weightsC <- ifelse(boot_sample$C == 1, 1 - weightC, weightC)
  
  
  weightsD <- ifelse(boot_sample$D == 1, 1 - weightD, weightD)
  
  
  weightsF <- ifelse(boot_sample$F == 1, 1 - weightF, weightF)
  
  remove_redundant_gradeA.fit <- glm(formula = A ~ Tutoring + ParentalSupport + StudyTimeWeekly + Absences,
                                 data = boot_sample,
                                 family = "binomial",
                                 weights = weightsA)
  
  remove_redundant_gradeB.fit <- glm(formula = B ~ Tutoring + ParentalSupport + Music + StudyTimeWeekly + Absences,
                                     data = boot_sample,
                                     family = "binomial",
                                     weights = weightsB)
  
  remove_redundant_gradeC.fit <- glm(formula = C ~ Absences,
                                     data = boot_sample,
                                     family = "binomial",
                                     weights = weightsC)
  
  remove_redundant_gradeD.fit <- glm(formula = D ~ Absences,
                                     data = boot_sample,
                                     family = "binomial",
                                     weights = weightsD)
  
  remove_redundant_gradeF.fit <- glm(formula = F ~ Tutoring + ParentalSupport + StudyTimeWeekly + Absences,
                                     data = boot_sample,
                                     family = "binomial",
                                     weights = weightsF)
  
  remove_redundant_gradeA_predictions <- predict(remove_redundant_gradeA.fit, student_performance[out_of_sample, ], type = "response")
  remove_redundant_gradeB_predictions <- predict(remove_redundant_gradeB.fit, student_performance[out_of_sample, ], type = "response")
  remove_redundant_gradeC_predictions <- predict(remove_redundant_gradeC.fit, student_performance[out_of_sample, ], type = "response")
  remove_redundant_gradeD_predictions <- predict(remove_redundant_gradeD.fit, student_performance[out_of_sample, ], type = "response")
  remove_redundant_gradeF_predictions <- predict(remove_redundant_gradeF.fit, student_performance[out_of_sample, ], type = "response")
  
  for(k in 1:length(out_of_sample))
  {
    remove_redundant_logistic_bootstrapped_probabilities[1, out_of_sample[k], i] <- remove_redundant_gradeA_predictions[k]
    remove_redundant_logistic_bootstrapped_probabilities[2, out_of_sample[k], i] <- remove_redundant_gradeB_predictions[k]
    remove_redundant_logistic_bootstrapped_probabilities[3, out_of_sample[k], i] <- remove_redundant_gradeC_predictions[k]
    remove_redundant_logistic_bootstrapped_probabilities[4, out_of_sample[k], i] <- remove_redundant_gradeD_predictions[k]
    remove_redundant_logistic_bootstrapped_probabilities[5, out_of_sample[k], i] <- remove_redundant_gradeF_predictions[k]
  }
}

remove_redundant_logisitic_mean_probabilities <- apply(remove_redundant_logistic_bootstrapped_probabilities, MARGIN = c(1,2), function(x) {
  x <- na.omit(x)
  return(mean(x))
})

remove_redundant_predictions <- apply(remove_redundant_logisitic_mean_probabilities, MARGIN = 2, function(x) {
  max_index <- which.max(x)
  if(max_index == 1)
  {
    return("A")
  }
  else if(max_index == 2)
  {
    return("B")
  }
  else if(max_index == 3)
  {
    return("C")
  }
  else if(max_index == 4)
  {
    return("D")
  }
  else
  {
    return("F")
  }
})


#get misclassification error for each bootsample
bootstrap_errors <- c()
for(i in 1:ncol(bootstrap_predictions))
{
  error <- 0
  indices <- which(!is.na(bootstrap_predictions[, i]))
  predictions <- na.omit(bootstrap_predictions[, i])
  for(j in 1:length(predictions))
  {
    if(predictions[j] == "A")
    {
      error <- error + ifelse(student_performance[indices[j], "A"] == 1, 0, 1)
    }
    else if(predictions[j] == "B")
    {
      error <- error + ifelse(student_performance[indices[j], "B"] == 1, 0, 1)
    }
    else if(predictions[j] == "C")
    {
      error <- error + ifelse(student_performance[indices[j], "C"] == 1, 0, 1)
    }
    else if(predictions[j] == "D")
    {
      error <- error + ifelse(student_performance[indices[j], "D"] == 1, 0, 1)
    }
    else
    {
      error <- error + ifelse(student_performance[indices[j], "F"] == 1, 0, 1)
    }
  }
  bootstrap_errors <- append(bootstrap_errors, error/length(predictions))
}

predictions <- apply(bootstrap_predictions, MARGIN = 1, function(observations) {
  max <- NULL
  max_grade <- NULL
  temp <- na.omit(observations)
  for(grade in unique(temp))
  {
    if(is.null(max) || sum(temp == grade) > max)
    {
      max <- sum(temp == grade)
      max_grade <- grade
    }
  }
  return(max_grade)
})
bootstrap_average_error <- mean(bootstrap_errors)
bootstrap_variance <- sd(bootstrap_errors)^2

#diagnostic graphs

#grade A
#get mean probability of oob estimates
gradeA_mean_probabilities <- apply(logistic_bootstrapped_probabilites[1,,], MARGIN = 1, function(observations) {
  temp <- na.omit(observations)
  return(mean(temp))
})

gradeA_logit <- sapply(gradeA_mean_probabilities, function(prob) {
  return(-log(prob/(1-prob)))
})

#scatter plot with quanitative data
plot(x = gradeA_logit,
     y = student_performance$Age)

plot(x = gradeA_logit,
     y = student_performance$StudyTimeWeekly)

plot(x = gradeA_logit,
     y = student_performance$Absences)

gradeB_mean_probabilities <- apply(logistic_bootstrapped_probabilites[2,,], MARGIN = 1, function(observations) {
  temp <- na.omit(observations)
  return(mean(temp))
})

gradeB_logit <- sapply(gradeB_mean_probabilities, function(prob) {
  return(-log(prob/(1-prob)))
})

plot(x = gradeB_logit,
     y = student_performance$Age)

plot(x = gradeB_logit,
     y = student_performance$StudyTimeWeekly)

plot(x = gradeB_logit,
     y = student_performance$Absences)

gradeC_mean_probabilities <- apply(logistic_bootstrapped_probabilites[3,,], MARGIN = 1, function(observations) {
  temp <- na.omit(observations)
  return(mean(temp))
})

gradeC_logit <- sapply(gradeC_mean_probabilities, function(prob) {
  return(-log(prob/(1-prob)))
})

plot(x = gradeC_logit,
     y = student_performance$Age)

plot(x = gradeC_logit,
     y = student_performance$StudyTimeWeekly)

plot(x = gradeC_logit,
     y = student_performance$Absences)

gradeD_mean_probabilities <- apply(logistic_bootstrapped_probabilites[4,,], MARGIN = 1, function(observations) {
  temp <- na.omit(observations)
  return(mean(temp))
})

gradeD_logit <- sapply(gradeD_mean_probabilities, function(prob) {
  return(-log(prob/(1-prob)))
})

plot(x = gradeD_logit,
     y = student_performance$Age)

plot(x = gradeD_logit,
     y = student_performance$StudyTimeWeekly)

plot(x = gradeD_logit,
     y = student_performance$Absences)

gradeF_mean_probabilities <- apply(logistic_bootstrapped_probabilites[5,,], MARGIN = 1, function(observations) {
  temp <- na.omit(observations)
  return(mean(temp))
})

gradeF_logit <- sapply(gradeF_mean_probabilities, function(prob) {
  return(-log(prob/(1-prob)))
})

plot(x = gradeF_logit,
     y = student_performance$Age)

plot(x = gradeF_logit,
     y = student_performance$StudyTimeWeekly)

plot(x = gradeF_logit,
     y = student_performance$Absences)


#Confusion matrix

confusion_matrix <- function(predictions, grade)
{
  tp <- 0
  fp <- 0
  fn <- 0
  tn <- 0
  
  for(i in 1:length(predictions))
  {
    if(predictions[i] == grade && student_performance[i, grade] == 1)
    {
      tp <- tp + 1
    }
    else if(predictions[i] == grade && student_performance[i, grade] == 0)
    {
      fp <- fp + 1
    }
    else if(predictions[i] != grade && student_performance[i, grade] == 1)
    {
      fn <- fn + 1
    }
    else
    {
      tn <- tn + 1
    }
  }
  print(fp)
  ret <- matrix(c(tp,fp,fn,tn), byrow = TRUE, nrow = 2, ncol = 2)
  rownames(ret) <- c(1, 0)
  colnames(ret) <- c(1,0)
  return(matrix(c(tp, fp, fn, tn), byrow = TRUE, nrow = 2, ncol =2))
}

gradeA_confusion_matrix <- confusion_matrix(predictions, "A")
gradeB_confusion_matrix <- confusion_matrix(predictions, "B")
gradeC_confusion_matrix <- confusion_matrix(predictions, "C")
gradeD_confusion_matrix <- confusion_matrix(predictions, "D")
gradeF_confusion_matrix <- confusion_matrix(predictions, "F")

heatmap(gradeA_confusion_matrix, Rowv = NA, Colv = NA, ylab = "Prediction", xlab = "Actual", main = "Grade A Confusion Matrix")
heatmap(gradeB_confusion_matrix, Rowv = NA, Colv = NA, ylab = "Prediction", xlab = "Actual", main = "Grade B Confusion Matrix")
heatmap(gradeC_confusion_matrix, Rowv = NA, Colv = NA, ylab = "Prediction", xlab = "Actual", main = "Grade C Confusion Matrix")
heatmap(gradeD_confusion_matrix, Rowv = NA, Colv = NA, ylab = "Prediction", xlab = "Actual", main = "Grade D Confusion Matrix")
heatmap(gradeF_confusion_matrix, Rowv = NA, Colv = NA, ylab = "Prediction", xlab = "Actual", main = "Grade F Confusion Matirx")

remove_redundant_gradeA_confusion_matrix <- confusion_matrix(remove_redundant_predictions, "A")
remove_redundant_gradeB_confusion_matrix <- confusion_matrix(remove_redundant_predictions, "B")
remove_redundant_gradeC_confusion_matrix <- confusion_matrix(remove_redundant_predictions, "C")
remove_redundant_gradeD_confusion_matrix <- confusion_matrix(remove_redundant_predictions, "D")
remove_redundant_gradeF_confusion_matrix <- confusion_matrix(remove_redundant_predictions, "F")

#logistic regression with weights to counteract unbalanced data
weighted_logistic_bootstrapped_probabilites <- array(dim = c(5, 2392, 300))
for(i in 1:300)
{
  boot_sample_index <- sample(1:nrow(student_performance), size = 2392, replace = TRUE)
  boot_sample <- student_performance[boot_sample_index, ]
  out_of_sample <- c(1:nrow(student_performance))[!1:nrow(student_performance) %in% boot_sample_index]
  
  weightA <- sum(ifelse(boot_sample$A == 1, 1, 0))/nrow(boot_sample)
  weightsA <- ifelse(boot_sample$A == 1, 1 - weightA, weightA)
  
  weightB <- sum(ifelse(boot_sample$B == 1, 1, 0))/nrow(boot_sample)
  weightsB <- ifelse(boot_sample$B == 1, 1 - weightB, weightB)
  
  weightC <- sum(ifelse(boot_sample$C == 1, 1, 0))/nrow(boot_sample)
  weightsC <- ifelse(boot_sample$C == 1, 1 - weightC, weightC)
  
  weightD <- sum(ifelse(boot_sample$D == 1, 1, 0))/nrow(boot_sample)
  weightsD <- ifelse(boot_sample$D == 1, 1 - weightD, weightD)
  
  weightF <- sum(ifelse(boot_sample$F == 1, 1, 0))/nrow(boot_sample)
  weightsF <- ifelse(boot_sample$F == 1, 1 - weightF, weightF)
  
  gradeA.fit <- glm(formula = A ~ .,
                    data = boot_sample[, c(features, "A")],
                    family = 'binomial',
                    weights = weightsA)
  
  gradeB.fit <- glm(formula = B ~ .,
                    data = boot_sample[, c(features, "B")],
                    family = 'binomial',
                    weights = weightsB)
  
  gradeC.fit <- glm(formula = C ~ .,
                    data = boot_sample[, c(features, "C")],
                    family = "binomial",
                    weights = weightsC)
  
  gradeD.fit <- glm(formula = D ~ .,
                    data = boot_sample[, c(features, "D")],
                    family = "binomial",
                    weights = weightsD)
  
  gradeF.fit <- glm(formula = F ~ .,
                    data = boot_sample[, c(features, "F")],
                    family = 'binomial',
                    weights = weightsF)
  
  gradeA.predictions <- predict(gradeA.fit, student_performance[out_of_sample, ], type = "response")
  gradeB.predictions <- predict(gradeB.fit, student_performance[out_of_sample, ], type = "response")
  gradeC.predictions <- predict(gradeC.fit, student_performance[out_of_sample, ], type = "response")
  gradeD.predictions <- predict(gradeD.fit, student_performance[out_of_sample, ], type = "response")
  gradeF.predictions <- predict(gradeF.fit, student_performance[out_of_sample, ], type = "response")
  
  for(k in 1:length(out_of_sample))
  {
    weighted_logistic_bootstrapped_probabilites[1, out_of_sample[k], i] <- gradeA.predictions[k]
    weighted_logistic_bootstrapped_probabilites[2, out_of_sample[k], i] <- gradeB.predictions[k]
    weighted_logistic_bootstrapped_probabilites[3, out_of_sample[k], i] <- gradeC.predictions[k]
    weighted_logistic_bootstrapped_probabilites[4, out_of_sample[k], i] <- gradeD.predictions[k]
    weighted_logistic_bootstrapped_probabilites[5, out_of_sample[k], i] <- gradeF.predictions[k]
  }
}

weighted_mean_probabilities <- array(dim = c(5, 2392))
for(i in 1:5)
{
  weighted_means <- apply(weighted_logistic_bootstrapped_probabilites[i,,], MARGIN = 1, FUN = function(observation) {
    temp <- na.omit(observation)
    return(mean(temp))
  })
  weighted_mean_probabilities[i,] <- weighted_means
}

weighted_predictions <- apply(weighted_mean_probabilities, MARGIN = 2, function(probabilities) {
  index <- which.max(probabilities)
  if(index == 1)
  {
    return("A")
  }
  else if(index == 2)
  {
    return("B")
  }
  else if(index == 3)
  {
    return("C")
  }
  else if(index == 4)
  {
    return("D")
  }
  else
  {
    return("F")
  }
})

#get accuracy of weighted predictions
correct <- 0
for(i in 1:length(weighted_predictions))
{
  prediction <- weighted_predictions[i]
  if(student_performance[i, prediction] == 1)
  {
    correct <- correct + 1
  }
}
weighted_accuracy <- correct/length(weighted_predictions)

weighted_gradeA_confusion_matrix <- confusion_matrix(weighted_predictions, "A")
weighted_gradeB_confusion_matrix <- confusion_matrix(weighted_predictions, "B")
weighted_gradeC_confusion_matrix <- confusion_matrix(weighted_predictions, "C")
weighted_gradeD_confusion_matrix <- confusion_matrix(weighted_predictions, "D")
weighted_gradeF_confusion_matrix <- confusion_matrix(weighted_predictions, "F")

heatmap(weighted_gradeA_confusion_matrix, Rowv = NA, Colv = NA, ylab = "Prediction", xlab = "Actual", main = "Weighted Grade A Confusion Matrix")
heatmap(weighted_gradeB_confusion_matrix, Rowv = NA, Colv = NA, ylab = "Prediction", xlab = "Actual", main = "Weighted Grade B Confusion Matrix")
heatmap(weighted_gradeC_confusion_matrix, Rowv = NA, Colv = NA, ylab = "Prediction", xlab = "Actual", main = "Weighted Grade C Confusion Matrix")
heatmap(weighted_gradeD_confusion_matrix, Rowv = NA, Colv = NA, ylab = "Prediction", xlab = "Actual", main = "Weighted Grade D Confusion Matrix")
heatmap(weighted_gradeF_confusion_matrix, Rowv = NA, Colv = NA, ylab = "Prediction", xlab = "Actual", main = "Weighted Grade F Confusion Matirx")

#get tpr
tpr <- function(p, grade)
{
  #get number of true positives
  tp <- sum(sapply(1:length(p), function(i) {
    return(ifelse(student_performance[i, grade] == 1, 1, 0))
  }))
  
  #iterate over predictions to check if said value is a true positive
  ret <- c()
  tp_seen <- 0
  for(i in 1:length(p))
  {
    if(student_performance[i, grade] == 1)
    {
      tp_seen <- tp_seen + 1
    }
    ret <- append(ret, tp_seen/tp)
  }
  return(ret)
}

fpr <- function(p, grade)
{
  #get number of false negatives
  fp <- sum(sapply(1:length(p), function(i) {
    return(ifelse(student_performance[i, grade] == 0, 1, 0))
  }))
  
  #iterate over predictions to check if said value is a false positive
  ret <- c()
  fp_seen <- 0
  for(i in 1:length(p))
  {
    if(student_performance[i, grade] == 0)
    {
      fp_seen <- fp_seen + 1
    }
    ret <- append(ret, fp_seen/fp)
  }
  return(ret)
}

#roc curve for weighted prediction
weighted_gradeA_tpr <- tpr(weighted_predictions, "A")
weighted_gradeA_fpr <- fpr(weighted_predictions, "A")

weighted_gradeB_tpr <- tpr(weighted_predictions, "B")
weighted_gradeB_fpr <- fpr(weighted_predictions, "B")

weighted_gradeC_tpr <- tpr(weighted_predictions, "C")
weighted_gradeC_fpr <- fpr(weighted_predictions, "C")

weighted_gradeD_tpr <- tpr(weighted_predictions, "D")
weighted_gradeD_fpr <- fpr(weighted_predictions, "D")

weighted_gradeF_tpr <- tpr(weighted_predictions, "F")
weighted_gradeF_fpr <- fpr(weighted_predictions, "F")

plot(
  x = weighted_gradeA_fpr,
  y = weighted_gradeA_tpr,
  xlab = "False Positive Rate",
  ylab = "True Positive Rate",
  main = "Weighted ROC Curve",
  type = "l",
  col = "red"
)

lines(
  x = weighted_gradeB_fpr,
  y = weighted_gradeB_tpr,
  col = "blue"
)

lines(
  x = weighted_gradeC_fpr,
  y = weighted_gradeC_tpr,
  col = "green"
)

lines(
  x = weighted_gradeD_fpr,
  y = weighted_gradeD_tpr,
  col = "yellow"
)

lines(
  x = weighted_gradeF_fpr,
  y = weighted_gradeF_tpr,
  col = "purple"
)

legend(
  x = "topleft",
  legend = c("A", "B", "C", "D", "F"),
  lty = 1,
  col = c("red", "blue", "green", "yellow", "purple")
)

#roc curve for non-weighted predictions
gradeA_tpr <- tpr(predictions, "A")
gradeA_fpr <- fpr(predictions, "A")

gradeB_tpr <- tpr(predictions, "B")
gradeB_fpr <- fpr(predictions, "B")

gradeC_tpr <- tpr(predictions, "C")
gradeC_fpr <- fpr(predictions, "C")

gradeD_tpr <- tpr(predictions, "D")
gradeD_fpr <- fpr(predictions, "D")

gradeF_tpr <- tpr(predictions, "F")
gradeF_fpr <- fpr(predictions, "F")

plot(
  x = gradeA_fpr,
  y = gradeA_tpr,
  xlab = "False Positive Rate",
  ylab = "True Positive Rate",
  main = "ROC Curve",
  type = "l",
  col = "red"
)

lines(
  x = gradeB_fpr,
  y = gradeB_tpr,
  col = "blue"
)

lines(
  x = gradeC_fpr,
  y = gradeC_tpr,
  col = "green"
)

lines(
  x = gradeD_fpr,
  y = gradeD_tpr,
  col = "yellow"
)

lines(
  x = gradeF_fpr,
  y = gradeF_tpr,
  col = "purple"
)

legend(
  x = "topleft",
  legend = c("A", "B", "C", "D", "F"),
  lty = 1,
  col = c("red", "blue", "green", "yellow", "purple")
)