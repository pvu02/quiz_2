library(tidyverse)
library(psych)
library(haven)
library(apaTables)
library(dplyr)
bfi_data <- psych::bfi

categorical_variables <- select(bfi_data, gender, education)
categorical_variables$gender <- as.factor(categorical_variables$gender)
categorical_variables$education <- as.factor(categorical_variables$education)
levels(categorical_variables$gender) <- list("Male"=1, "Female"=2)
levels(categorical_variables$education) <- list("HS"=1,"finished HS"=2,"some college"=3,"college graduate"=4,"graduate degree"=5)


agreeableness <- select (bfi_data, A1,A2,A3,A4,A5)
extraversion <- select (bfi_data, E1,E2,E3,E4,E5)
neuroticism <- select (bfi_data, N1,N2,N3,N4,N5)
age <- select (bfi_data, age)

is_bad_value <- agreeableness<1 | agreeableness>6
agreeableness[is_bad_value] <- NA
is_bad_value <- extraversion<1 | extraversion>6
extraversion[is_bad_value] <- NA
is_bad_value <- neuroticism<1 | neuroticism>6
neuroticism[is_bad_value] <- NA

agreeableness <- mutate(agreeableness,A1=7-A1)
extraversion <- mutate(extraversion,E1=7-E1)
extraversion <- mutate(extraversion,E2=7-E2)

psych::describe(agreeableness)
psych::describe(extraversion)
psych::describe(neuroticism)

agreeableness <- psych::alpha(as.data.frame(agreeableness),check.keys=FALSE)$scores
extraversion <- psych::alpha(as.data.frame(extraversion),check.keys=FALSE)$scores
neuroticism <- psych::alpha(as.data.frame(neuroticism),check.keys=FALSE)$scores
analytic_data <- cbind(categorical_variables,age,agreeableness,extraversion,neuroticism)

write_csv(analytic_data,path="analytic_data.csv")
