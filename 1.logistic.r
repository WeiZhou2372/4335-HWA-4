source("FUN.logistic.r")

result = read.csv("Leopards_data.csv")

# config ---------------------
convo.size = 20
convo.stride = 10
# train ----------------------
train = read.convolute.and.gather("./homework_data/Leopards/Train", result, size = convo.size, stride = convo.stride)
train.actual.y = train[,dim(train)[2]] %>% as.numeric()

model = glm(y ~.,family=binomial(link='logit'),data=train)
train.model.y = model$linear.predictors
train.model.y[train.model.y > 0] = 1
train.model.y[train.model.y < 0] = 0

sum(!xor(train.model.y, train.actual.y)) / length(train.actual.y)

# test -----------------------
test = read.convolute.and.gather("./homework_data/Leopards/Test", result, size = convo.size, stride = convo.stride)

predicted = predict.glm(model, test[,-dim(test)[2]] %>% as.data.frame())
predicted[predicted > 0] = 1
predicted[predicted < 0] = 0

answer = test[,dim(test)[2]] %>% as.numeric()

sum(!xor(answer, predicted)) / length(answer)
