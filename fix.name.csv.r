needs(readr)
airplanes_data <- read_csv("E:/RScript/4335 HWA 4/homework_data/airplanes_data.csv")

airplanes_data$name = gsub(".jpg$", "", airplanes_data$name)
airplanes_data$name = gsub("$", ".jpg", airplanes_data$name)
airplanes_data$name = gsub("^airplanes_", "", airplanes_data$name)

write.csv(airplanes_data, file = "airplanes_data.csv", row.names = F)
