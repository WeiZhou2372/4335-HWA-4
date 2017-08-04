needs(jpeg, biglm, dplyr)
#' filepath %>%           # string
#' readJPG() %>%          # array: [m,n,3]
#' convert.1jpg() %>%     # array:[m-1, n-1, 3] # color difference
#' shrink.3color() %>%    # matrix: [m-1, n-1]
#' convolute() %>%        # matrix: [?,?]
#' as.vector()            # vector: [?]

border.convert = function(a){
  # color -> color difference
  bb1 = a %>% apply(1, diff) %>% abs()
  bb2 = a %>% apply(2, diff) %>% abs()
  frame = (bb2[,-1] + t(bb1)[-1, ])/2
  
  return(frame)
}

convert.1jpg = function(a){
  nnn = dim(a)
  b = array(NA, dim = c(nnn[1]-1, nnn[2]-1, 3))
  b[,,1] = border.convert(a[,,1])
  b[,,2] = border.convert(a[,,2])
  b[,,3] = border.convert(a[,,3])
  
  return(b)
}

shrink.3color = function(jpg.matrix){
  (jpg.matrix[,,1] + jpg.matrix[,,2] + jpg.matrix[,,3])/3
}

append.1jpg.init = function(single.matrix){
    notebook = single.matrix %>%
      as.vector() %>%
      t() %>%
      as.data.frame()

  return(notebook)
}

append.1jpg = function(single.matrix, notebook){
    aaa = as.vector(single.matrix)
    notebook = rbind(notebook, aaa)

  return(notebook)
}

get.result = function(file, result.csv){
  result.csv$final[which(result.csv$name == file)]
}

# gather iamge -------------------------------
read.and.gather = function(folderPath, result){ # no convolution
  needs(jpeg)
  all.files = list.files(folderPath)
  finished = 0
  notes = readJPEG(paste0(folderPath, "/", all.files[1])) %>%
    convert.1jpg() %>%
    shrink.3color() %>%
    append.1jpg.init() %>%
    mutate(y = get.result(all.files[1], result))
  i = 1
  print(length(all.files))
  pb = txtProgressBar(max = length(all.files) - 1)
  for(file in all.files[-1]){
    theImage = readJPEG(paste0(folderPath, "/", file))
    notes = convert.1jpg(theImage) %>%
      shrink.3color() %>%
      c(get.result(file, result)) %>%
      append.1jpg(notebook = notes)
    i = i + 1
    setTxtProgressBar(pb, i)
  }
  close(pb)
  finished = 1
  return(notes)
}

# convolution filter -------------------------------
unit.convolute = function(unitMatrix){
  max(unitMatrix)
}

convolute = function(singleMatrix, size, stride){
  mn = dim(singleMatrix)
  m = floor((mn[1] - size) / stride) + 1
  m_res = mn[1] -(size + (m-1) * stride)
  n = floor((mn[2] - size) / stride) + 1 
  n_res = mn[2] -(size + (n-1) * stride)
  
  newMatrix = matrix(rep(NA, m*n), nrow = m)
  #pb = txtProgressBar(max = m*n)
  ttt = 0
  for(i in 1:m){
    for(j in 1:n){
      unitMatrix = singleMatrix[(1+(i-1)*stride):(size + (i-1)*stride), (1+(j-1)*stride):(size + (j-1)*stride)]
      newMatrix[i,j] = unit.convolute(unitMatrix = unitMatrix)
      ttt = ttt + 1
      #setTxtProgressBar(pb, ttt)
    }
  }
  #close(pb)
  
  return(newMatrix)
}

# main function from jpg to convoluted one, and to vector
jpg.diff.shrink.convolute.vector = function(x, convo.size, convo.stride){
  convert.1jpg(x) %>%     # array:[m-1, n-1, 3] # color difference
    shrink.3color() %>%    # matrix: [m-1, n-1]
    convolute(convo.size, convo.stride) %>%  # matrix: [?,?]
    as.vector()           # vector: [?]
}
# b = ddd %>%
#   shrink.3color() %>%
#   convolute()


convolute.size.check = function(x,y,size,stride){
  a = floor(((x-1)-size)/stride) + 1
  b = floor(((y-1)-size)/stride) + 1
  return(a * b)
}

gather.folder.convolute = function(folderPath, convo.size, convo.stride, result){
  needs(jpeg)
  message(folderPath)
  all.files = list.files(folderPath)
  a = dim(readJPEG(paste0(folderPath, "/", all.files[1])))
  len = convolute.size.check(a[1], a[2], convo.size, convo.stride)
  
  notes = matrix(NA, nrow = length(all.files), ncol = len)
  actual.y = rep(NA, length(all.files))
  i = 1
  print(length(all.files))
  pb = txtProgressBar(max = length(all.files) - 1)
  for(file in all.files){
    b = readJPEG(paste0(folderPath, "/", file))
    notes[i,] = jpg.diff.shrink.convolute.vector(b, convo.size, convo.stride)
    
    actual.y[i] = get.result(file, result)[1]
    
    setTxtProgressBar(pb, i)
    i = i + 1
  }
  close(pb)
  
  return(list(x = notes,
              actual.y = actual.y))
}

#---------------------------
#ddd = readJPEG("./homework_data/Bonsai/Train/image_0052.jpg")


