source("FUN.general.r")
needs(fastAdaboost)
# first obeservation ----------------
# input: a convoluted vector[?]

#' o = sig.filter(wtx)
sig.filter = function(wtx){
  as.numeric(wtx > 0)
}

#' w <- w + rate*(o-y)*x
#' w [n+1]
#' o [1] 
#' y [1]
#' x [n+1] <=== c(1,x_1, x_2,...x_n)
ada.w.learn = function(w, o, y, x, rate){
  if(length(w) != length(x) | length(o) > 1 | length(y) >1){
    cat(paste(length(w), length(x), "\n"))
    cat(paste(o, y, rate))
    stop(file_)
  }
  w + rate*(o-y)*x
}

ada.y = function(w,x){
  sig.filter(sum(w*x))
  #sum(w*x)
}

# later be integrated
x.gather = function(category = "Faces", convo.size=20, convo.stride=10, type="Train"){
  folderPath = paste0("./homework_data/",category,"/", type)
  result = read.csv(paste0(category, "_data.csv"))
  all.files = list.files(folderPath)
  
  x1 = readJPEG(paste0(folderPath, "/", all.files[1])) %>%
    jpg.diff.shrink.convolute.vector(convo.size, convo.stride)
  
  y.actual = get.result(all.files[1], result)[1]
  
  xs = matrix(x1, nrow = 1)
  
  print(length(all.files))
  pb = txtProgressBar(max = length(all.files) - 1)
  
  i=0
  for(file_ in all.files[-1]){
    x = readJPEG(paste0(folderPath, "/", file_)) %>%
      jpg.diff.shrink.convolute.vector(convo.size, convo.stride)

    y.actual = c(y.actual, get.result(file_, result)[1])
    xs = rbind(xs, matrix(x, nrow = 1))
    
    i = i + 1
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  xs = cbind(matrix(rep(1, length(all.files))), xs)
  
  return(list(
    xs = xs,
    y.actual = y.actual
  ))
}


# main
ada.train.test = function(category = "Faces", convo.size, convo.stride, type, learning_rate){
  needs(jpeg)
  # if(learning_rate>1 | learning_rate<=0){
  #   stop("leaning rate out of bound, should between 0 and 1.")
  # }
  folderPath = paste0("./homework_data/",category,"/", type)
  result = read.csv(paste0(category, "_data.csv"))
  all.files = list.files(folderPath)

  x1 = readJPEG(paste0(folderPath, "/", all.files[1])) %>%
    jpg.diff.shrink.convolute.vector(convo.size, convo.stride)
  x1 = c(1, x1)
  w = rep(0, length(x1)) # initial value , may change later
  
  xs = matrix(x1, nrow = 1)
  
  y = ada.y(w, x1)
  o = get.result(all.files[1], result)[1]
  y.actual = o
  
  w = ada.w.learn(w, o, y, x1, learning_rate)

  print(length(all.files))
  pb = txtProgressBar(max = length(all.files) - 1)
  
  i=0
  for(file_ in all.files[-1]){
    x = readJPEG(paste0(folderPath, "/", file_)) %>%
      jpg.diff.shrink.convolute.vector(convo.size, convo.stride)
    x = c(1,x)
    y = ada.y(w,x)
    o = get.result(file_, result)[1]
    # print(w)
    w = ada.w.learn(w, o, y, x, learning_rate)
    # if(is.nan(w)){
    #   print(t)
    #   print(x)
    #   print(paste(o,y,learning_rate))
    #   stop(file_)
    # }

    y.actual = c(y.actual, o)
    xs = rbind(xs, matrix(x, nrow = 1))

    i = i + 1
    setTxtProgressBar(pb, i)
  }
  close(pb)
  y.predict = as.vector(xs %*% w) %>%
    sig.filter()
  
  return(list(y.predict = y.predict,
              y.actual = y.actual,
              w = w))
}



