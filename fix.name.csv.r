needs(readr)
airplanes_data <- read_csv("E:/RScript/4335 HWA 4/homework_data/Motorbikes_data.csv")

airplanes_data$name = gsub(".jpg$", "", airplanes_data$name)
airplanes_data$name = gsub("$", ".jpg", airplanes_data$name)
airplanes_data$name = gsub("^Motorbikes_", "", airplanes_data$name)

write.csv(airplanes_data, file = "Motorbikes_data.csv", row.names = F)
