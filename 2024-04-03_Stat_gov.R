# R Coding Club 2024-04-03
# демография
# https://stat.gov.kz/ru/industries/social-statistics/demography/dynamic-tables/

# задаём рабочую директорию
setwd('/media/carabus/Carabus_2024/Education/DoJo/R_Coding_Club/2024-04-03/')

# згружаем необходимые пакеты
# install.packages('geodata') # если пакет не загружается, то устанавливаем
library(geodata) # он-лайн ресурсы пространсвенных данных
library(terra)   # работа с пространственными данными

# install.packages('ggplot2')
library(ggplot2)   # продвинутая визуализация данных
# руководство для построения графиков с ggplot2 : https://r-graphics.org/
# https://cran.r-project.org/web/packages/ggplot2/ggplot2.pdf

# install.packages('tidyterra')
library(tidyterra) # расширение пакетов tidyverse и ggplot2 для работы с пространственными данными
# https://cran.r-project.org/web/packages/tidyterra/tidyterra.pdf
# https://dieghernan.github.io/tidyterra/reference/ggspatvector.html
# https://dieghernan.github.io/tidyterra/

# путь к папке для загрузки файлов геоданных
geodata_path('/media/carabus/Carabus_2024/Resources/cache/')

# Global ADMinistrative - GADM: https://gadm.org/download_country.html
KZ_bounds1 = gadm(country = 'KZ', level = 1) # получаем границы областей
KZ_bounds2 = gadm(country = 'KZ', level = 2) # границы районов
class(KZ_bounds1) # класс полученного объекта StatVector пакета terra

ggplot() + 
  geom_spatvector(data = KZ_bounds1) + 
  ggtitle("Административное деление Казахстана") + 
  theme(plot.title = element_text(hjust = 0.5, size = 20))

KZ_bounds1_new = vect('KZ_amd1_new.shp') # загружаем актуальные границы их локального shape-файла

ggplot() + 
  geom_spatvector(data = KZ_bounds2, color = 'blue')+
  geom_spatvector(data = KZ_bounds1_new, fill = NA, lwd = 0.5, color = 'navy') + 
  geom_spatvector_text(data = KZ_bounds1_new, aes(label = NAME_1), color = 'red') + 
  ggtitle("Административное деление Казахстана") + 
    theme(plot.title = element_text(hjust = 0.5, size = 20))

# добавляем поле с площадью в кв. км
KZ_bounds1$regArea = expanse(KZ_bounds1, unit = "km")
sum(KZ_bounds1$regArea) # сумма площадей

KZ_bounds1_new$regArea = expanse(KZ_bounds1_new, unit = "km")
sum(KZ_bounds1_new$regArea) # сумма площадей


# коды регионов
regions = c('Астана','Алматы','Акмолинская область','Актюбинская область',
            'Алматинская область','Атырауская область','Западно-Казахстанская область',
            'Жамбыльская область','Карагандинская область','Костанайская область',
            'Кызылорлинская область','Мангистауская область','Туркестанская область',
            'Павлодарская область','Северо-Казахстанская область','Восточно-Казахстанская область',
            'Шымкент','Абайская область','Жетисуская область','Улытауская область')
regionList = data.frame(code = c(1:20), regionName = regions) # создаём data frame

# добавляем коды регионов в векторный слой
KZ_bounds1_new$NAME_1
oblNums = c(3,4,6,12,15,14,10,11,13,7,8,20,9,19,5,18,16)
regionList[oblNums,]
KZ_bounds1_new$NAME_1
KZ_bounds1_new$NAME_RU = regionList[oblNums,2]
KZ_bounds1_new$CODE = regionList[oblNums,1]

head(KZ_bounds1_new)
values(KZ_bounds1_new)


# таблица с численностью населения
# https://stat.gov.kz/api/iblock/element/6584/csv/file/ru/
population = read.csv('populationDynamics.csv')
names(population)

# указываем разделитель - знак табуляции - \t
population = read.csv('populationDynamics.csv', sep = '\t')
head(population)
population$NAM = NULL     # удаляем ненужные поля
population$PERIOD = NULL
colnames(population)[1] = 'year' # переименовываем заголовок поля

# форматируем значения поля ГОД
unique(population$year) # набор уникальных значений
population$year = substring(population$year,1,4) # оставляем только первые 4 символа
population$year = as.integer(population$year) # преобразуем в целочисленные значения
sort(unique(population$year), decreasing = T)

str(population)
population[,6:9] = NULL # убираем ненужные поля
head(population)
population[,6]

# переименовываем заголовки полей
colnames(population)[2] = 'adminName'
colnames(population)[3] = 'adminCode'
colnames(population)[4] = 'group'
colnames(population)[5] = 'groupCode'
colnames(population)[6] = 'amount'
head(population,10)
population = population[-(1:6),] # убираем первые 6 записей

population$amount
population$amount2 = as.integer(population$amount)

population$amount2 = gsub(" ","",population$amount)
population$amount2 = as.integer(population$amount2)

max(population$amount2)

unique(population$adminName)

population2023 = population[population$year == 2023,] # выбираем данные за 2023 год
population2023all = population2023[population2023$groupCode == 1,] # все данные

nrow(population2023all) # число строк
check = grepl("РАЙОН", population2023all$adminName) # строки, которые содержат текст "РАЙОН"
workTable = population2023all[!check, ] # такие строки удаляем
head(workTable)

workTable = workTable[-1,] # удаляем первую строку
unique(workTable$adminName)

# удаляем записи, содержащие строку " Г.А."
workTable = workTable[!grepl(" Г.А.", workTable$adminName),]
nrow(workTable)
head(workTable, 10)

workTable$amount = workTable$amount2
workTable$amount2 = NULL

# прибавляем значения населения городов к соответствующей области
# Астана
workTable[2,6] = workTable[2,6] + workTable[18,6] 

# Алматы
workTable[4,6] = workTable[4,6] + workTable[19,6]

# Шымкент
workTable[15,6] = workTable[15,6] + workTable[20,6]

# удаляем записи с городами
workTable = workTable[-(18:20),]
workTable = workTable[1:17,] # альтернативный вариант


unique(workTable$adminName)

workTable[workTable$adminName == 'ОБЛАСТЬ АБАЙ',]$adminCode = 18
workTable[workTable$adminName == 'АКМОЛИНСКАЯ ОБЛАСТЬ',]$adminCode = 3
workTable[workTable$adminName == 'АКТЮБИНСКАЯ ОБЛАСТЬ',]$adminCode = 4
workTable[workTable$adminName == 'АЛМАТИНСКАЯ ОБЛАСТЬ',]$adminCode = 5 
workTable[workTable$adminName == 'АТЫРАУСКАЯ ОБЛАСТЬ',]$adminCode = 6
workTable[workTable$adminName == 'ЗАПАДНО-КАЗАХСТАНСКАЯ ОБЛАСТЬ',]$adminCode = 7
workTable[workTable$adminName == 'ЖАМБЫЛСКАЯ ОБЛАСТЬ',]$adminCode = 8
workTable[workTable$adminName == 'ОБЛАСТЬ ЖЕТІСУ',]$adminCode = 19
workTable[workTable$adminName == 'КАРАГАНДИНСКАЯ ОБЛАСТЬ',]$adminCode = 9 
workTable[workTable$adminName == 'КОСТАНАЙСКАЯ ОБЛАСТЬ',]$adminCode = 10
workTable[workTable$adminName == 'КЫЗЫЛОРДИНСКАЯ ОБЛАСТЬ',]$adminCode = 11
workTable[workTable$adminName == 'МАНГИСТАУСКАЯ ОБЛАСТЬ',]$adminCode = 12
workTable[workTable$adminName == 'ПАВЛОДАРСКАЯ ОБЛАСТЬ',]$adminCode = 14
workTable[workTable$adminName == 'СЕВЕРО-КАЗАХСТАНСКАЯ ОБЛАСТЬ',]$adminCode = 15 
workTable[workTable$adminName == 'ТУРКЕСТАНСКАЯ ОБЛАСТЬ',]$adminCode = 13
workTable[workTable$adminName == 'ОБЛАСТЬ ҰЛЫТАУ',]$adminCode = 20
workTable[workTable$adminName == 'ВОСТОЧНО-КАЗАХСТАНСКАЯ ОБЛАСТЬ',]$adminCode = 16
workTable[workTable$adminName == 'Г.АСТАНА',]$adminCode = 1
workTable[workTable$adminName == 'Г.АЛМАТЫ',]$adminCode = 2
workTable[workTable$adminName == 'Г.ШЫМКЕНТ',]$adminCode = 17


head(workTable)
head(KZ_bounds1_new)
str(KZ_bounds1_new)

names(workTable)
names(KZ_bounds1_new)
KZ_bounds1_new$CODE
names(KZ_bounds1_new)[14] = 'adminCode'
values(KZ_bounds1_new)

# объединяем данные векторного слоя и таблицы с численностью населения
workTable
KZ_bounds1_new
values(KZ_bounds1_new)

KZ_bounds1_new = merge(KZ_bounds1_new, workTable, by = 'adminCode')

names(KZ_bounds1_new)
head(KZ_bounds1_new)
names(KZ_bounds1_new)[19]

names(KZ_bounds1_new)[19] = 'population'

values(KZ_bounds1_new)

# численность населения по регионам Казахстана
ggplot(KZ_bounds1_new) + 
  geom_spatvector(aes(fill = population)) + 
  scale_fill_whitebox_c(
    palette = 'arid',
    breaks = c(100000,500000,1000000,2000000),
    direction = -1,
    name = 'население'
  ) +
  labs(title = 'численность населения Казахстана') + 
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        legend.key.size = unit(1.2, 'cm'),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 12))

# разные заливки https://dieghernan.github.io/tidyterra/articles/palettes.html

# плотность населения
KZ_bounds1_new$dencity = KZ_bounds1_new$population / KZ_bounds1_new$regArea

# численность населения по регионам Казахстана
ggplot(KZ_bounds1_new) + 
  geom_spatvector(aes(fill = dencity)) + 
  scale_fill_whitebox_c(
    palette = 'arid',
    breaks = c(2,5,10,20,35),
    direction = -1,
    name = 'чел./км2'
  ) +
  # geom_spatvector_text(aes(label = adminName)) +
  labs(title = 'плотность населения Казахстана') + 
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        legend.key.size = unit(1.2, 'cm'),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 12))