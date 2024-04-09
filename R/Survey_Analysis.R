##survey analysis of Indian womens' menstrual experience##
survey<- read.csv("chii_square_india.csv",stringsAsFactors = FALSE)
install.packages("dplyr")
install.packages("tidyverse")

library(tidyverse)

filtered_r <- subset(survey, Product == "Sanitary pad")

View(filtered_r)
table_data <- table(survey$Product, survey$Problems)
chi_square_test <- chisq.test(table_data)
print(chi_square_test)

#Conclusion- As p value is less than our LOS(5%) [2.2e-16] and chi square value [85.667] is high
#we can conclude that there is strong association between use of plastic sanitary pad and facing issues in disposability and comfort 

