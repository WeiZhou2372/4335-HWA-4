source("FUN.general.r")

read.convolute.and.gather = function(folderPath, result, size = 10, stride = 5){
  needs(jpeg)
  all.files = list.files(folderPath)
  finished = 0
  notes = readJPEG(paste0(folderPath, "/", all.files[1])) %>%
    convert.1jpg() %>%
    shrink.3color() %>%
    convolute(size, stride) %>%
    append.1jpg.init() %>%
    mutate(y = get.result(all.files[1], result))
  i = 1
  print(length(all.files))
  pb = txtProgressBar(max = length(all.files) - 1)
  for(file in all.files[-1]){
    theImage = readJPEG(paste0(folderPath, "/", file))
    notes = convert.1jpg(theImage) %>%
      shrink.3color() %>%
      convolute(size, stride) %>%
      c(get.result(file, result)) %>%
      append.1jpg(notebook = notes)
    i = i + 1
    setTxtProgressBar(pb, i)
  }
  close(pb)
  finished = 1
  return(notes)
}
