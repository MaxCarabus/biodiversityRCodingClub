# осваиваем применение критерия хи-квадрат на примере данных социологического исследования

# загружаем необходимые библиотеки (функции которых будут использоваться дале в скрипте)
library(ggplot2)
library(corrplot)

# если какой-то из библиотек на компьютере нет - устанавливаем
# install.packages('ggplot2')
# install.packages('corrplot')
# чтобы установить библиотеку - надо раскомментировать (убрат знак #) и запустить соответсвующую строку

# задаём рабочую директорию
setwd('D:/R/codingClub') # задаём свою рабочую директорию, в которой лежит исходная таблица

# загружаем таблицу в data frame
survey = read.csv('social_survey.csv')

# выводим название полей 
names(survey)

# число записей (строк), в данном случае - это сколько было заполнено анкет
nrow(survey)

# поля с которыми будем работать
# survey_year - год опреса
# birth_year - год рождения
# gender - пол
# marital_status - семейное положение 
# often_votes - как часто ходите на выбору
# political_leaning - политическая позиция
# siblings - число братье и сестёр
# religious - вероисповедение

# выведем уникальные значение годов опроса 
unique(survey$survey_year)

# выведем уникальные значение годов рждения, при этом отсортируем по возрастанию
sort(unique(survey$birth_year))

# так как опрос проводился в разные годы, добавил поле "возраст"
# для этого вычтем из года рождения год опроса и запишей в новую переменную в таблице
survey$age = survey$survey_year - survey$birth_year

# посморим какие возраста есть 
sort(unique(survey$age))

# посмотрим распределение респондентов по возрасту 
ggplot() +
  geom_histogram(aes(x = survey$age), stat = 'count') + 
  xlim(32,43) +
  xlab('возраст, лет') + 
  ylab('частота') 

# посмотрим структуру исходных данных
str(survey)

# критерий Хи-квадрат позволяет выявить взаимосвязь между двумя качественными переменными
# в первом примере оценим взаимосвязь семейного положения и политической позиции

# преобразуем переменные в фактор 
survey$marital_status = as.factor(survey$marital_status) # семейное положение
survey$political_leaning = as.factor(survey$political_leaning) # политическая позиция

# значения в таблице закодирована в виде порядкового номера варианта ответа
unique(survey$marital_status)

# зададим категории в более понятном виде
levels(survey$marital_status) = c('married','widowed','divorced','separated',
                                  'never married') # семейное положение
levels(survey$political_leaning) = c('very conserv.','conservative','middle',
                                     'liberal','very liberal') # политические взгляды

# составим таблицу сопряженности классов по данным переменным
table1 = table(survey$marital_status, survey$political_leaning)
table1 # посмотрим содержимое таблицы

result1 = chisq.test(table1) # собствено хи-тест
# обратите внимание на предупреждение - оно из за нулей в некоторых ячейках таблицы сопряженности
result1 # разультат значимый (p < 0.05) -- нулевую гипотезу отвергаем -- взаимосвязь есть!

# посмотрим фактические и ожидаемые значения, которые были бы при отсутсвии взаимосвязи
result1$observed # фактические значения
round(result1$expected) # теоретические значения - округляем, т.к. число человек - значение целое

# построим диаграмму, которая покажет отклонения от ожидаемых частот 
corrplot(result1$residuals, is.corr = F)

# сообщение о возможных ненадёжных результатах из-за нулевых значений в некоторых значениях
table1 # в категории widowed

# удалим записи с этим классом значений из таблицы и проведём анализ заново
survey2 = survey[survey$marital_status != 'widowed',] # значения равные 'widowed' не попадут

# сравним число записей в вариантах таблицы
nrow(survey)
nrow(survey2)

# при этом категория 'widowed' осталась в списке возможных значений
levels(survey2$marital_status)

# сократим этот список только до используемых в таблице (data frame) значений
survey2 = droplevels(survey2)
nrow(survey2) # число записей осталось прежним
levels(survey2$marital_status) # неиспользуемых значений больше нет

# проведём тест ещё раз - с новым вариантом таблицы
table2 = table(survey2$marital_status, survey2$political_leaning)
table2
result2 = chisq.test(table2)
result2 # значение p и без того очень низкое, стало еще меньше, предупреждений больше нет
corrplot(result2$residuals, is.corr = F)

# проведем анализ взаимосвязи пола (gender) и числа братьев и сестёр (siblings)
# преобразуем нужные переменные в факторы
survey$gender = as.factor(survey$gender) # используем исходную таблицу survey
levels(survey$gender) = c('male','female')

survey$siblings = as.factor(survey$siblings) 
# возможно значение не расшифровываем, так как сама цифра обозначение нужное значение

# собственно анализ
table3 = table(survey$gender, survey$siblings)
table3
result3 = chisq.test(table3)
result3 # значимость различий на грани, но всё-таки есть - p < 0.05
corrplot(result3$residuals, is.corr = F)

# категория '997' - это те, кто на вопрос не отвечал, так как указал, 
# что братьев и сестер - нет, то есть единственный ребенок в семье
# что бы вы могли сказать, глядя на эту диаграмму?

# проведем анализ без этой категории
survey3 = survey[survey$siblings != 997,]
table4 = table(survey3$gender, survey3$siblings)
result4 = chisq.test(table4)
result4 # p-значение гораздо больше .05 - нулевую гипотезу подтверждаем
# в семья, где двое и больше детей, пол отращиваемого с число братьев и сестёр не связан
corrplot(result4$residuals, is.corr = F)