# второй семинар - 8 ноября 2023 года, первый график и ggplot и описательная статистика

# задаём рабочую директорию через меню RStudio
# Session -> Set Working Directory -> Choose Directory

# загружаем библиотеку ggplot2, если её нет - устанавливаем командой install.packages()
library(ggplot2)
# загружает учебную таблицу из рабочей директории
catsData = read.csv('cats.csv')

# составляем график с линейными размерами диких котов
ggplot(catsData, aes(x = bodyLength, y = tailLength, 
                     shape = IUCN, color = IUCN)) + 
  geom_point(aes(size = bodyMass)) + 
  labs(x = 'Длина тела, см', y = 'Длина хвоста, см', title = "Дикие кошки Казахстана") +
  theme(axis.text = element_text(size = 14, face = 'bold', color = 'red'),
        axis.title = element_text(size = 16, face = 'bold', color = 'blue'))

# считаем значения описательной статистики, на примере массы тела
mean(catsData$bodyMass)     # среднее значение
median(catsData$bodyMass)   # медианное значение
quantile(catsData$bodyMass) # квантили
min(catsData$bodyMass)      # минимальное значение
max(catsData$bodyMass)      # максимальное значение 
max(catsData$bodyMass) - min(catsData$bodyMass) # размах значения

var(catsData$bodyMass) # дисперсия - variance 
sd(catsData$bodyMass)  # среднеквадратичное отклонение - standard deviation

cv = sd(catsData$bodyMass) / mean(catsData$bodyMass) * 100 # коэффициент вариации
cv
