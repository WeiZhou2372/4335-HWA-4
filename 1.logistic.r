source("FUN.general.r")

result = read.csv("airplanes_data.csv")

a = read.and.gather("./homework_data/airplanes/Test", result)

model = glm(y ~.,family=binomial(link='logit'),data=a)
