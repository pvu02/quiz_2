library(tidyverse)
library(psych)
library(haven)
library(apaTables)
library(dplyr)
bfi_data <- psych::bfi

categorical_variables <- select(bfi_data, gender)
categorical_variables$gender <- as.factor(categorical_variables$gender)
levels(categorical_variables$gender) <- list("Male"=1, "Female"=2)

agreeableness <- select (bfi_data, A1,A2,A3,A4,A5)
extraversion <- select (bfi_data, E1,E2,E3,E4,E5)
neuroticism <- select (bfi_data, N1,N2,N3,N4,N5)
age <- select (bfi_data, age)
education <- select (bfi_data, education)

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
analytic_data <- cbind(categorical_variables,education,age,agreeableness,extraversion,neuroticism)

write_csv(analytic_data,path="analytic_data.csv")

apa.cor.table(analytic_data, filename="Table1.doc", table.number=1)

analytic_data_male <- filter (analytic_data, gender=='Male')
analytic_data_male <- select(analytic_data_male, -gender)
analytic_data_male <- filter (analytic_data_male, age>40)

write_csv(analytic_data_male,path="analytic_data_male.csv")

apa.cor.table(analytic_data_male, filename="Table2.doc", table.number=2)

my.plot <- qplot(agreeableness,extraversion, data=analytic_data_male)
my.plot <- my.plot + theme_classic()
my.plot <- my.plot + theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
                           axis.line.y = element_line(colour = 'black', size=0.5, linetype = 'solid'))
my.plot <- my.plot + labs(title="", x="Agreeableness", y="Extraversion")
my.plot <- my.plot + geom_smooth(method = "lm", se = FALSE, color="black")

print(my.plot)

ggsave("Figure1.pdf", plot=my.plot, width=6,height=6)
