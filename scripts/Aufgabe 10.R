library(caret)
library(pROC)
library(ROCR)
#b)
load("../Data/SPECTF.RData")
folds <- createFolds(SPECTF$X0, k = 10, list = TRUE)

predictions <- actuals <- numeric(nrow(SPECTF))

for(i in 1:10) {
  test_idx <- folds[[i]]
  train_idx <- setdiff(seq_len(nrow(SPECTF)), test_idx)

  predictions[test_idx] <- predict(glm(X0 ~ ., data = SPECTF[train_idx, ], family = "binomial"), SPECTF[test_idx, ], type = "response")
  actuals[test_idx] <- SPECTF$X0[test_idx]
}

#c)

# Create a ROC curve
roc_obj <- roc(actuals, predictions)

# Calculate the AUC
auc_obj <- auc(roc_obj)
print(paste("AUC: ", auc_obj))

# Calculate the confidence intervals for the AUC
ci_obj <- ci.auc(roc_obj)
print(paste("95% Confidence Interval for AUC: ", ci_obj))

# Create a prediction object for ROCR
pred_obj <- prediction(predictions, actuals)

# Calculate the performance
perf_obj <- performance(pred_obj, "tpr", "fpr")

# Plot the ROC curve with ROCR
plot(perf_obj, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))


#d)
pdf("../Docs/ROC SPECTF.pdf")
plot(roc_obj, main="ROC Curve")
text(0.8, 0.2, paste("AUC: ", round(auc(roc_obj), 2), "\n95% CI: ", round(ci.auc(roc_obj)[1], 2), "-", round(ci.auc(roc_obj)[3], 2)), adj = c(0, 0))
dev.off()