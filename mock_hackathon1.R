instagram$fake = as.factor(instagram$fake)

train_size = floor(0.7 *nrow(instagram))
train_rows = sample(1:nrow(instagram), size = train_size)

train = instagram[train_rows, ]
test = instagram[-train_rows, ]


baseline_glm = glm(fake ~ profile.pic + nums.length.username + X.followers + X.follows, data = train, family = binomial(link = logit ))
summary(baseline_glm)

predictions_prob = predict(baseline_glm, test, type = "response")
predictions_class = ifelse(predictions_prob > 0.5, 1, 0)
table(predictions_class, test$fake)
accuracy = sum(predictions_class == test$fake) / length(test$fake)

library(tree)
baseline_tree = tree(fake ~ profile.pic + nums.length.username + X.followers + X.follows, data = train)

plot(baseline_tree)
text(baseline_tree)

tree_preds = predict(baseline_tree, newdata = test, type = "class")
table(tree_preds, test$fake)

instagram_improved = instagram

instagram_improved$profile.pic = as.factor(instagram_improved$profile.pic)
instagram_improved$name..username = as.factor(instagram_improved$name..username)
instagram_improved$external.URL = as.factor(instagram_improved$external.URL)
instagram_improved$private = as.factor(instagram_improved$private)

outlier_cols = c(
  "X.posts", 
  "X.followers", 
  "X.follows"
)

for (outlier in outlier_cols){
  cap_value = quantile(instagram_improved[[outlier]], 0.99, na.rm = TRUE)
  instagram_improved[[outlier]] = ifelse(instagram_improved[[outlier]] > cap_value, cap_value, 
                                         instagram_improved[[outlier]])
}

total_missing_values = sum(is.na(instagram_improved))

instagram_improved$follow_ratio = instagram_improved$X.followers / (instagram_improved$X.follows + 1)
instagram_improved$follow_ratio_log = log(instagram_improved$follow_ratio + 1)

train_size = floor(0.7 *nrow(instagram_improved))
train_rows = sample(1:nrow(instagram_improved), size = train_size)

train_improved = instagram_improved[train_rows, ]
test_improved = instagram_improved[-train_rows, ]

improved_glm = glm(fake ~ profile.pic + nums.length.username + X.followers + X.follows, data = train_improved, family = binomial(link = logit ))
summary(improved_glm)

predictions_prob_improved = predict(improved_glm, test_improved, type = "response")
predictions_class_improved = ifelse(predictions_prob_improved > 0.7, 1, 0)
table(predictions_class_improved, test_improved$fake)
accuracy_improved = sum(predictions_class_improved == test_improved$fake) / length(test_improved$fake)


library(tree) 

improved_dt = tree(
  fake ~ profile.pic + nums.length.username + private + external.URL + 
    description.length + follow_ratio_log + fullname.words, 
  data = train_improved
)

summary(improved_dt)
plot(improved_dt)
text(improved_dt)


test_improved$fake = as.factor(test_improved$fake)
predictions_class = predict(improved_dt, newdata = test_improved, type = "class")

confusion_matrix = table(Predicted = predictions_class, Actual = test_improved$fake)

print("Confusion Matrix:")
print(confusion_matrix)

accuracy = sum(diag(confusion_matrix)) / sum(confusion_matrix)

print(paste("Improved Decision Tree Accuracy:", accuracy))
summary(improved_dt)