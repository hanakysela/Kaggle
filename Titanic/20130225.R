titanic<-read.csv("train.csv")
head(titanic)

library(tree)
tree1<-tree(formula = survived ~ pclass + sex + age + sibsp + parch + embarked, data = titanic)
tree1

plot(tree1)
text(tree1)


newdata <- data.frame(Palmitic = 1200, Palmitoleic = 120, Stearic=200,Oleic=7000,Linoleic = 900, Linolenic = 32, Arachidic=60,Eicosenoic=6)
pred1 <- predict(tree1,newdata)
pred1