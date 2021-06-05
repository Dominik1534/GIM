library("neuralnet")

dataset <- read.csv("smartfony.csv")
head(dataset)

## extract a set to train the NN
trainset <- dataset[1:10, ]

## select the test set

testset <- dataset[11:15, ]

## build the neural network (NN)
creditnet <- neuralnet(cena ~ pamiecRAM + pamiecwbudowana + aparat_foto, trainset, hidden = 2,  threshold = 0.1)

plot(creditnet, rep = "best")

## test the resulting output
temp_test <- subset(testset, select = c("pamiecRAM", "aparat_foto", "pamiecwbudowana"))

creditnet.results <- compute(creditnet, temp_test)
head(temp_test)
## test the resulting output

results <- data.frame(actual = testset$cena, prediction = creditnet.results$net.result)
results$prediction <- round(results$prediction)
results[1:5, ]


