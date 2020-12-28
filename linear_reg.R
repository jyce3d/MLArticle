# ML regression

data <- read.csv("c:\\temp\\ArticleML\\immofake.csv", header=TRUE, sep=";")

data <-data[!is.na(data$sqm) & !is.na(data$price) &data$type=="Maison"  ,]

#explorer les data

plot (data$sqm, data$price)

# lié au groupe
#plot (data$sqm, data$price, pch=as.integer(data$type))
#construire le training data set
#construire le test dataset => Attention respecter le random

point.n = nrow(data)
sampling.rate = 0.8 #80 * pour le training set

set.seed(1)
training = sample(1:point.n, sampling.rate*point.n)
testing = setdiff(1:point.n, training)
train_ds = subset(data[training,])
test_ds = subset(data[testing,])

# recherche du prix en fonction de nombre de m^2 
model = lm(price ~ sqm, train_ds)

summary(model)

abline(model)

# on applique le modèle au test dataset et on compare les données prédites par rapport aux données réelles
preds = predict(model, test_ds)

model_eval <- cbind(test_ds$price, preds)
colnames(model_eval)<-c("Actual", "Predicted")

model_eval <- as.data.frame(model_eval)
# mean square error
mse <- sum((model_eval$Actual-model_eval$Predicted)^2)/(nrow(test_ds)-2)
rmse <- sqrt(mse)

# RMSE - trainig=23990
# RMSE- test    =36794
# too different, this model is overfitted (too specific and not easy to generalize)