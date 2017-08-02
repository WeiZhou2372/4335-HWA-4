needs(jpeg, biglm, dplyr)


border.convert = function(a){
  bb1 = a %>% apply(1, diff) %>% abs()
  bb2 = a %>% apply(2, diff) %>% abs()
  frame = (bb2[,-1] + t(bb1)[-1, ])/2
  
  return(frame)
}

convert.1jpg = function(a){
  nnn = dim(a)
  b = array(NA, dim = c(nnn[1]-1, nnn[2]-1,3))
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

read.and.gather = function(folderPath, result){
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
#---------------------------
#ddd = readJPEG("./homework_data/airplanes/Test/image_0013.jpg")


