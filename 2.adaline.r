source("FUN.adaline.r")

#result = read.csv(paste0(category, "_data.csv"))

# config ---------------------
category = "Motorbikes"
convo.size = 18
convo.stride = 7
learning_rate = 0.6
# train ----------------------
train = ada.train.test(category, convo.size, convo.stride, type = "Train", learning_rate)

sum(!xor(train$y.predict, train$y.actual)) / length(train$y.actual)

# test -----------------------
test = x.gather(category, convo.size, convo.stride, type = "Test")

predicted = as.vector(test$xs %*% train$w)

sum(!xor(predicted, test$y.actual)) / length(test$y.actual)
