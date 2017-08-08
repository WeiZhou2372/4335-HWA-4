source("FUN.adaline.r")

#result = read.csv(paste0(category, "_data.csv"))

# config ---------------------
cates = c("airplanes", "Bonsai", "Faces", "Leopards", "Motorbikes")

category = cates[4]
convo.size = 10
convo.stride = 8
learning_rate = 0.6
result = read.csv(paste0(category, "_data.csv"))

train.data = gather.folder.convolute(folderPath = paste0("./homework_data/",category,"/Train"),
                                     convo.size, convo.stride, result)

test.data = gather.folder.convolute(folderPath = paste0("./homework_data/",category,"/Test"),
                                    convo.size, convo.stride, result)

if(!TRUE){
# train ----------------------
train = ada.train.test(category, convo.size, convo.stride, type = "Train", learning_rate)

sum(!xor(train$y.predict, train$y.actual)) / length(train$y.actual)

# test -----------------------
test = x.gather(category, convo.size, convo.stride, type = "Test")

predicted = as.vector(test$xs %*% train$w)

sum(!xor(predicted, test$y.actual)) / length(test$y.actual)
}
# Ver 2.0: Train ---------------------------------------
ttt = cbind(train.data$x, y = train.data$actual.y) %>%
  as.data.frame() %>%
  mutate(y = as.factor(y))
model = adaboost(y~., data = ttt, 50)
train.y = predict(model, train.data$x)$class %>% as.character() %>% as.numeric()

sum(!xor(train.y, train.data$actual.y)) / length(train.y)


# Ver 2.0: Test -----------------------------------
sss = cbind(test.data$x, y = test.data$actual.y)
test.y = predict(model, newdata = sss)$class %>% as.character() %>% as.numeric()

sum(!xor(test.y, test.data$actual.y)) / length(test.y)


