source("FUN.svm.r")
# config ---------------------------------------
cates = c("airplanes", "Bonsai", "Faces", "Leopards", "Motorbikes")

category = cates[2]
convo.size = 15
convo.stride = 5

# convoluted data ----------------------------------------
result = read.csv(paste0(category, "_data.csv"))

train.data = gather.folder.convolute(folderPath = paste0("./homework_data/",category,"/Train"),
                                    convo.size, convo.stride, result)

test.data = gather.folder.convolute(folderPath = paste0("./homework_data/",category,"/Test"),
                                     convo.size, convo.stride, result)
cat("\014")
# # train ----------------------
ttt = cbind(train.data$x, y = train.data$actual.y)
model = svm(y~., data = ttt, kernel = "linear")
#model = svm(train.data$actual.y ~ train.data$x)

train.y = (predict(model, train.data$x) >= 0.5) %>% as.numeric()

sum(!xor(train.y, train.data$actual.y)) / length(train.y)

# # test -----------------------
sss = cbind(test.data$x, y = test.data$actual.y)
test.y = (predict(model, newdata = sss) >= 0.5) %>% as.numeric()
 
sum(!xor(test.y, test.data$actual.y)) / length(test.y)
