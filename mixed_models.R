# ЛЕКЦИЯ
install.packages('lme4')
install.packages('mlmRev')
install.packages('lmerTest')

install.packages('tidyverse')
install.packages('ggplot2')
install.packages('nlme')
install.packages('lmtest')
install.packages('dplyr')
install.packages('merTools')

library(mlmRev)
library(lme4)
library(ggplot2)
library(lmtest)
library(car)
library(tidyverse)
library(nlme)


# Линейность свзяи
qplot(data = mtcars, x = hp, y = mpg) # это я так, развлекаюсь
# Трансформация Тьюки
qplot(data = mtcars, x = -hp^-0.5, y = mpg)
cor.test(mtcars$hp, mtcars$mpg)
# подбирать по коэффициенту корреляции наибольшему по модулю
# степень в которую возводить
# но это плохо интерпретируемо
# логарифмирование гораздо более интерпретируемо
qplot(data = mtcars, x = log(hp), y = log(mpg))
fit1 <- lm(log(mpg) ~ log(hp), mtcars)
summary(fit1)
# ненормально распределенные остатки стали нормальными
qqnorm(fit1$residuals)
qqline(fit1$residuals)
shapiro.test(fit1$residuals)

# гетероскедастичность
library(ggplot2)
str(diamonds)
diamonds_2 <- sample_n(diamonds, 500)
qplot(data =diamonds_2, x = price, y = carat)+
  geom_smooth(method=lm)
fit2 <- lm(price ~ carat, diamonds_2)
summary(fit2)
coefficients(fit2) # средняя температура по больнице
plot(fit2) # look at residuals
# solution with log transformation
qplot(data =diamonds_2, x = log(price), y = log(carat))+
  geom_smooth(method=lm)
fit3 <- lm(log(carat) ~ log(price), diamonds_2)
bptest(fit3) # ошибка не связана с предикторами в модели
bptest(fit2) # ошибка связана с предикторами в модели

# мультиколлинеарность
# give some random data
data <- data_frame(y = rnorm(30),
                   x_1 = rnorm(30),
                   x_2 = x_1,
                   x_3 = rnorm(30))
pairs(data)
# тестим на наличие NA - это типа полная мультиколлинеарность
fit4 <- lm(y~., data)
summary(fit4)
# NA выскакивают из-за мультиколлинеарности
# надо исключить одну из переменных
data(cars)
head(cars)
qplot(x = speed, y = dist, data = cars)
fit5 <- lm(dist~., cars)
summary(fit5)
# вместо трансформации можно ещё и создавать доп предикторы
cars <- mutate(cars, speed_2 = speed^2, speed_3 = speed^3)
pairs(cars) # теперь есть мультиколлинеарность
fit6 <- lm(dist~., cars)
summary(fit6)  # но теперь ни одна из переменных не значима!!
# аааа что произошло?? Но р2 почти не поменялся
# ТО ЕСТЬ МУЛЬТИКОЛЛИНЕАРНОСТЬ МОЖЕТ ЗАТРАГИВАТЬ
# УРОВЕНЬ ЗНАЧИМОСТИ ПЕРЕМЕННЫХ АААААААААААААААА
data(swiss)
head(swiss)
fit7 <- lm(Fertility ~ ., data = swiss)
summary(fit7)
# И вот теперь мы смотрим на этом всё и думаем
# А это вообще хоть что-то значит и зачем вообще это всё??
cor.test(~ Fertility + Examination, swiss)
# Вот теперь в значимо, а в модельке показывало, что
# незначимо
# Качественная проверка - VIF
# для каждого предиктора объясняет насколько хорошо
# каждый предиктор объясняется другими предикторами
vif(fit7)
fit8 <- lm(Fertility ~ ., select(swiss, -Examination))
summary(fit8)
vif(fit8)

######### Mixed models
# Синтаксис для смешанных регрессионных моделей в пакете 'lme4'

lmer(DV ~ IV + (1 + IV | RV), data = my_data)


str(Exam)
help(Exam)

ggplot(data = Exam, aes(x = standLRT, y = normexam)) + 
  geom_point()


ggplot(data = Exam, aes(x = standLRT, y = normexam, col = school)) + 
  geom_point()



# Один главный эффект


Model1 <- lm(normexam ~ standLRT, data=Exam)

Exam$Model1_pred <- predict(Model1)
ggplot(data = Exam, aes(x = standLRT, y = normexam)) + 
  geom_point() + 
  geom_line(data = Exam, aes(x = standLRT, y = Model1_pred), col = 'blue', size = 1)



Model1 <- lmer(normexam ~ standLRT, data=Exam)
# нельзя задавать без рандомного эффекта -- будет ошибка





# Главный эффект + случайный свободный член
Model2 <- lmer(normexam ~ standLRT + (1|school), data=Exam)
summary(Model2)

Exam$Model2_pred <- predict(Model2)
ggplot(data = Exam, aes(x = standLRT, y = normexam)) + 
  geom_point(alpha = 0.2) + 
  geom_line(data = Exam, aes(x = standLRT, y = Model2_pred, col = school))
# Красиво





# Главный эффект + случайный свободный член
# + случайный угловой коэффициент
# Тут мы предполагаем, что коэффициенты скоррелированны
Model3 <- lmer(normexam ~ standLRT + (1 + standLRT|school), data=Exam)
summary(Model3)

Exam$Model3_pred <- predict(Model3)
ggplot(data = Exam, aes(x = standLRT, y = normexam)) + 
  geom_point(alpha = 0.2) + 
  geom_line(data = Exam, aes(x = standLRT, y = Model3_pred, col = school))








# Главный эффект + случайный угловой коэффициент
Model4 <- lmer(normexam ~ standLRT + (0 + standLRT|school), data=Exam)
summary(Model4)

Exam$Model4_pred <- predict(Model4)
ggplot(data = Exam, aes(x = standLRT, y = normexam)) + 
  geom_point(alpha = 0.2) + 
  geom_line(data = Exam, aes(x = standLRT, y = Model4_pred, col = school))








# Нескоррелированные случайные эффекты
# типа intercept и slope отдельно друг от друга заданы
# можно еще через || задавать
Model5 <- lmer(normexam ~ standLRT + (1|school) + (0 + standLRT|school), data=Exam)
summary(Model5)






###################################################################################
# Вернуться здесь на слайды и разобрать вопрос про lm()
# Сравнение моделей


Model2 <- lmer(normexam ~ standLRT + (1|school), REML = FALSE, data=Exam)
summary(Model2)

# исключаем предиктор
# то есть в одной моедли есть интересуемый предиктор,
# а в другой нет
Model0 <- lmer(normexam ~ 1 + (1|school), REML = FALSE, data = Exam)
summary(Model0)

anova(Model0, Model2)  # f-test






# p-значения

library(lmerTest)

Model2 <- lmer(normexam ~ standLRT + (1|school), data=Exam)
summary(Model2)





# Обобщённые смешанные модели

Exam$school_type <- ifelse(Exam$type == 'Mxd', 1, 0)

Model5 <- glmer(school_type ~ normexam + (1|school), family = "binomial", data = Exam)

summary(Model5)






# Предсказания на новых датасетах


predict(Model2, Exam)


new_Exam <- Exam[sample(1:nrow(Exam), 100), ]
new_Exam$school <- sample(101:200)

predict(Model2, new_Exam, allow.new.levels = T)



# Исследование случайных эффектов

fixef(Model3)
ranef(Model3)

