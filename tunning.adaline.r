source("FUN.adaline.r")


# tunning ---------------------------------------------------------------------------
cates = c("airplanes", "Bonsai", "Faces", "Leopards", "Motorbikes")
sizes = 5:20
strides = 3:15
rates = seq(0.1,1, by = 0.1)

foobar = data.frame(category = sample(cates, 200, replace = T),
                    size = sample(sizes, 200, replace = T),
                    stride = sample(strides, 200, replace = T),
                    rate = sample(rates, 200, replace = T)) %>%
  filter(size - stride > 3)
cat(paste("Category", "Size", "Stride", "Rate", "Train", "Test", sep = ","), file = "2.adaline.tunning.csv")
cat("\n", file = "2.adaline.tunning.csv", append = T)
for(i in 1:10){
  # train ----------------------
  train = ada.train.test(foobar$category[i], foobar$size[i], foobar$stride[i], type = "Train", foobar$rate[i])
  
  aa = sum(!xor(train$y.predict, train$y.actual)) / length(train$y.actual)
  
  # test -----------------------
  test = x.gather(foobar$category[i], foobar$size[i], foobar$stride[i], type = "Test")
  
  predicted = as.vector(test$xs %*% train$w)
  
  bb = sum(!xor(predicted, test$y.actual)) / length(test$y.actual)
  
  cat(paste(foobar$category[i], foobar$size[i], foobar$stride[i], foobar$rate[i], aa, bb, sep = ","), file = "2.adaline.tunning.csv", append = T)
  cat("\n", file = "2.adaline.tunning.csv", append = T)
  
}

