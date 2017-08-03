# convo.size = 12
# convo.stride = 6


 test.matrix = matrix(rep(NA, 10*20), nrow = 20)
for(convo.size in 8:20){
  for(convo.stride in 3:10){
    if(convo.size - convo.stride >= 5){
      accuracy = source("1.logistic.r", local = T)$value
      test.matrix[convo.size, convo.stride] = accuracy
      print(paste(convo.size, convo.stride, accuracy))
    }
  }
}