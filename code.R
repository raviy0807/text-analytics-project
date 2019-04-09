textDataset <-read.csv("D:\\Study\\Masters\\Text analytics\\Project\\TA_Acronyms_Dataset\\TA_Acronyms_Dataset\\Acronym_citations_dataset.csv")
View(textDataset)

# Hypothesis testing for the Journal of Marketing

textDatasetMarketing <- textDataset[which(textDataset$Journal == "Journal of Marketing"),c("Introduced.Acronyms","Assumed.Acronyms","Ratio.I.A.","Citations")]
#colnames(textDatasetMarketing)[3] <- "Ratio.of.Introduced.vs.Assumed.Acronyms"

lmMarketing <-lm(data = textDatasetMarketing,formula = textDatasetMarketing$Citations ~ textDatasetMarketing$Ratio.I.A. )
predicted.intervals  <- predict(lmMarketing,textDatasetMarketing,interval='confidence',level=0.99)

#lines(textDatasetMarketing$Ratio.I.A.,predicted.intervals[,2],col='black',lwd=1)
#lines(textDatasetMarketing$Ratio.I.A.,predicted.intervals[,3],col='black',lwd=1)

summary(lmMarketing)

# plotting the data with regression model
plot(textDatasetMarketing$Ratio.I.A.,textDatasetMarketing$Citations,xlab = 'Ratio of Introduced/Assumend Acronyms',main='Citations vs. ratio of Introduced vs Assumed acronyms',ylab='Number of Citations')
lines(textDatasetMarketing$Ratio.I.A.,predicted.intervals[,1],col='green',lwd=3)

####### IT dataset

textDatasetIT <- textDataset[which(textDataset$Journal == "Journal of Information Technology"),c("Introduced.Acronyms","Assumed.Acronyms","Ratio.I.A.","Citations")]
lmIT <-lm(data = textDatasetIT,formula = textDatasetIT$Citations ~ textDatasetIT$Ratio.I.A. )
summary(lmIT)
predictedLMIT  <- predict(lmIT,textDatasetIT,interval='confidence',level=0.99)

# polynomial regression

polyLmIT <- lm(data = textDatasetIT,formula = Citations ~ poly(Ratio.I.A.,3) )
summary(polyLmIT)
Ratio.I.A. <- seq(0,2.5,by = 0.1)
textDatasetITPred <- data.frame(Ratio.I.A.)
predictedPolyIT  <- predict(polyLmIT,newdata = textDatasetITPred)


plot(textDatasetIT$Ratio.I.A.,textDatasetIT$Citations,xlab = 'Ratio of Introduced/Assumend Acronyms',main='Citations vs. ratio of Introduced vs Assumed acronyms',ylab='Number of Citations')
lines(textDatasetIT$Ratio.I.A.,predictedLMIT[,1],col='green',lwd=3)

lines(textDatasetITPred$Ratio.I.A.,predictedPolyIT,col='blue',lwd=3)

plot(textDatasetIT$Ratio.I.A.,textDatasetIT$Citations)
plot(textDatasetMarketing$Ratio.I.A.,textDatasetMarketing$Citations)  
