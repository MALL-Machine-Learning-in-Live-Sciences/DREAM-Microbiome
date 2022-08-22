setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Give desired becnhmark and know best iter
bch = readRDS("results/fourth-experiment/phylotypes_1e0_28_rf.rds")
iter = 443
  
# Retrive cm from best iter
prediction = bch$resample_results$resample_result[[1]]$predictions()[[iter]]
prob = data.frame(prediction$data$prob,
                  truth = prediction$data$truth,
                  response = prediction$data$response)
prob = data.frame(prediction$data$prob,
                  truth = prediction$data$truth,
                  thold_5 = prediction$data$response,
                  thold_6 =  ifelse(prob$term > 0.6, "term","preterm"),
                  thold_7 =  ifelse(prob$term > 0.7, "term","preterm"),
                  thold_8 =  ifelse(prob$term > 0.81, "term","preterm"),
                  thold_9 =  ifelse(prob$term > 0.9, "term","preterm"))

#Import required library
library(caret)
cms = list()
for (i in c(4:8)) {
  cms[[i-3]] = confusionMatrix( reference= factor(prob$truth), data = factor(prob[,i]), positive = "preterm")
}
names(cms) = colnames(prob[4:8])

sens = list()
spes = list()
for (i in seq_along(cms)) {
  sens[i] = cms[[i]][[4]][[1]] # Retrieve Sensitivity 
  spes[i] =  cms[[i]][[4]][[2]] # Retrieve Specificity
}
names(sens) = names(cms)
names(spes) = names(cms)
df = data.frame(tholds = names(cms),
                Sensitivity = unlist(sens),
                Specificity = unlist(spes))

p1 = ggplot(df, aes(x = tholds, group=1 ) ) + 
  geom_line(aes(y = Sensitivity), color = "darkred") + 
  geom_line(aes(y = Specificity), color="steelblue", linetype="twodash") +
  geom_text(data = subset(df, tholds == "thold_9"),
            aes(label = "Sensitivity", x = "thold_9", y = Sensitivity),
            color= "darkred", hjust = -.1) +
  geom_text(data = subset(df, tholds == "thold_9"),
            aes(label = "Specificity",x = "thold_9", y = Specificity),
            color= "steelblue",  hjust = -.1) +
  labs(x = " ", y = " ")+
  ggtitle(label ="Sensitivity vs Specificity", ) +
  theme(plot.title = element_text(hjust = 0.5))

print(p1)
 
 