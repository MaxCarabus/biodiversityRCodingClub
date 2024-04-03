# задаём путь к рабочей директории
setwd('/media/carabus/Carabus_2024/Education/DoJo/RSpatial/')

# install.packages('geodata')
library(geodata) # загрузка данных из он-лайн ресурсов: GADM, WorldCLim
# https://cran.r-project.org/web/packages/geodata/geodata.pdf
# вместе с geodata загружается пакет terra
# https://cran.r-project.org/web/packages/terra/terra.pdf

# путь к папке для загрузки файлов геоданных
geodata_path('/media/carabus/Carabus_2024/Resources/cache/')

# Global ADMinistrative - GADM: https://gadm.org/download_country.html
KZ_bounds1 = gadm(country = 'KZ', level = 1) # получаем границы областей
class(KZ_bounds1) # класс полученного объекта StatVector пакета terra

# OpenStreetMap - OSM: https://www.openstreetmap.org/
KZ_places = osm(country = 'KZ', var = 'places') # населенные пункты - OSM
KZ_highways = osm(country = 'KZ', var = 'highways') # автомобильные дороги - OSM
KZ_railway = osm('KZ', 'railway') # железные дороги - OSM
# подробнее об объектах карты OSM: https://wiki.openstreetmap.org/wiki/Map_Features

# быстрый способ увидеть что содержит векторный слой - функция базовой графики plot()
plot(KZ_highways)

# install.packages('ggplot2')
library(ggplot2)   # продвинутая визуализация данных
# руководство для построения графиков с ggplot2 : https://r-graphics.org/
# https://cran.r-project.org/web/packages/ggplot2/ggplot2.pdf

# install.packages('tidyterra')
library(tidyterra) # расширение пакетов tidyverse и ggplot2 для работы с пространственными данными
# https://cran.r-project.org/web/packages/tidyterra/tidyterra.pdf
# https://dieghernan.github.io/tidyterra/reference/ggspatvector.html
# https://dieghernan.github.io/tidyterra/

ggplot() + 
  geom_spatvector(data = KZ_bounds1) + 
  geom_spatvector(data = KZ_railway) + 
  ggtitle("Схема железных дорог Казахстана") + 
  theme(plot.title = element_text(hjust = 0.5, size = 20))

# населенные пункты
unique(KZ_places$place)
KZ_places_city = KZ_places[KZ_places$place == 'city',] # выберем крупные города в отдельный слой
KZ_places_towm = KZ_places[KZ_places$place == 'town']  # выберем прочие города

ggplot() + # карта с железными дорогами и крупными населенными пунктами Казахстана
  geom_spatvector(data = KZ_bounds1) +
  geom_spatvector(data = KZ_places_city, size = 3, color = 'blue4') +
  geom_spatvector_text(data = KZ_places_city, aes(label = name), nudge_x = 1.5) + 
  geom_spatvector(data = KZ_places_towm, size = 2, color = 'red4') + 
  geom_spatvector(data = KZ_railway) +
  xlab('долгота') + 
  ylab('широта')

# примеры названия цветов https://bookdown.org/hneth/ds4psy/ds4psy_files/figure-html/apx-color-sample-1.png
# из онлайн книги Data Science for Psychologists https://bookdown.org/hneth/ds4psy/

unique(KZ_highways$highway) # тип дорог
unique(KZ_highways$surface) # покрытие


ggplot() +
  geom_spatvector(data = KZ_bounds1) + 
  geom_spatvector(data = KZ_highways, aes(colour = highway)) + 
  scale_colour_manual(values = c('red','blue','orange'),
                      labels = c('региональные','областные','районные'),
                      name = 'тип дорог') +
  ggtitle('Схема автомобильных дорог Республики Казахстан') +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
                      legend.position = 'bottom',
                      legend.key.size = unit(1.3, 'cm'),
                      legend.text = element_text(size = 14),
                      legend.title = element_text(size = 16))
# https://ggplot2.tidyverse.org/reference/scale_manual.html


# плотность населения: Gridded Population of the World (GPW), v4
ppl = population(2020, res = 2.5)
# https://sedac.ciesin.columbia.edu/data/collection/gpw-v4
# https://sedac.ciesin.columbia.edu/data/collection/gpw-v4/sets/browse

ggplot() + 
  geom_spatraster(data = ppl) # слой на весь мир

ppl_KZ = crop(ppl, KZ_bounds1) # обрежем растер по крайним точкам
ggplot() + 
  geom_spatraster(data = ppl_KZ)

ppl_KZ = mask(ppl_KZ, KZ_bounds1) # удалим все значения за пределами Казахстана
ggplot() + 
  geom_spatraster(data = ppl_KZ)

# цветовая палитра для градиентной заливки
colPallete = c('ghostwhite','cornsilk','moccasin','gold1','goldenrod2','tomato1',
               'sienna3','red4','purple4','navyblue')

popdensity = # записшем график в переменную popdensity
  ggplot() + 
  geom_spatraster(data = ppl_KZ) +
  scale_fill_gradientn(na.value = 'transparent', trans = 'log10', 
      colors = colPallete, name = 'чел. / кв. км',
      breaks = c(0.1,0.5,5,50,500,5000), 
      limits = c(0.1,6000)) +
  theme(legend.key.size = unit(1.3, 'cm')) + 
  ggtitle('Плотность населения Казахстана по данным Gridded Population of the World (GPW)') + 
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        legend.text = element_text(size = 12), # https://www.statology.org/ggplot2-legend-size/
        legend.title = element_text(size = 14))  

popdensity # выведем график

options(scipen=10000) # отключаем научное представление числовых значений
# https://www.geeksforgeeks.org/how-to-prevent-scientific-notation-in-r/

# добавим крупные города
popdensity + geom_spatvector(data = KZ_places_city)

# рельеф - источник данных Shuttle Topography Radar Mission (SRTM)
# на всю страну 
KZ_elev = elevation_30s(country = 'KZ') # разрешение около 1 km
plot(KZ_elev)

# добавим цветовую шкалу
ggplot() + 
  geom_spatraster(data = KZ_elev) + 
  scale_fill_hypso_c(name = 'ВНУМ, м')
  # coord_sf(crs = '+proj=utm +zone=41') # UTM Z41 (38-45)
  # coord_sf(crs = '+proj=lcc +lon_0=70 +lat_1=33 +lat_2=45') # https://proj.org/operations/projections/lcc.html
  # coord_sf(crs = '+proj=leac +lon_0=70') # https://proj.org/operations/projections/leac.html


# сохраним растр в рабочую директорию
writeRaster(KZ_elev, 'KZ_elev.tiff')

# SRTM разрешением 90 м, фрагменты по 5 Х 5 градусов
# https://srtm.csi.cgiar.org v 4.1
QR_elev = elevation_3s(lat = 49.7680, lon = 73.1225) # БГФ КарУ
KR_elev = elevation_3s(lat = 49.4347, lon = 75.4867) # биостанция


# Карагандинская область
sort(KZ_bounds1$NAME_1)
QAR_bounds = KZ_bounds1[KZ_bounds1$NAME_1 == 'Qaraghandy',]
plot(QAR_bounds)

# Каркаралинский район
KZ_bounds2 = gadm('KZ', level = 2) # загрузим границы адм. районов
plot(KZ_bounds2)

sort(KZ_bounds2$NAME_2)
QAR_KR_bounds = KZ_bounds2[KZ_bounds2$NAME_2 == 'Karkaralinskiy' ,]
plot(QAR_bounds)
plot(QAR_KR_bounds, add = T, col = 'green')

QAR_elev = 
ggplot() + 
  geom_spatraster(data = QR_elev) + 
  geom_spatraster(data = KR_elev) +
  scale_fill_hypso_c() +
  geom_spatvector(data = QAR_KR_bounds, fill = NA, color = 'red', lwd = 0.8)

QAR_elev

# дополнительные квадраты, к северу от  уже загруженных
KR_elev_n = elevation_3s(lat = 51, lon = 76)
QR_elev_n = elevation_3s(lat = 51, lon = 74)

QAR_elev + 
  geom_spatraster(data = KR_elev_n) +
  geom_spatraster(data = QR_elev_n) + 
  geom_spatvector(data = QAR_bounds, fill = NA, color = 'green')

# объединим квадраты, которые приходятся на Каркаралинский район в один растровый слой
KR_elev_raster = merge(QR_elev, KR_elev, KR_elev_n)
plot(KR_elev_raster)

# вырежем из большого растрового слоя территорию Каркаралинского района
KR_srtm = mask(crop(KR_elev_raster, QAR_KR_bounds), QAR_KR_bounds)
plot(KR_srtm)

KR_elev_map = 
ggplot() + 
  geom_spatraster(data = KR_srtm) +
  geom_spatvector(data = QAR_KR_bounds, fill = NA, color = 'navy', lwd = 1) 

KR_elev_map # заливка растра цветом - по умолчанию

# добавляем заливку по высотам
KR_elev_map + 
  scale_fill_hypso_c(name = 'ВНУМ, м')

KR_srtm # характеристики слоя 
KR_srtm[1200,1122] # значение одного пикселя растра

KR_elev_map + 
  scale_fill_hypso_c(name = 'ВНУМ, м', limits = c(200,3500)) # подберем диапазон высот

KR_elev_map + 
  scale_fill_hypso_c(name = 'ВНУМ, м', limits = c(500,3500))

# градиенты https://dieghernan.github.io/tidyterra/reference/scale_hypso.html
KR_elev_map + 
  scale_fill_hypso_c(limits = c(200,3500), palette = 'usgs-gswa2')

KR_elev_map + 
  scale_fill_hypso_c(limits = c(200,2500), palette = 'wiki-schwarzwald-cont', 
                     name = 'ВНУМ, м') + 
  labs(title = 'Рельеф Каркаралинского района') + 
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        legend.key.size = unit(1.2, 'cm'),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14))


# загружаем находки из GBIF
# install.packages('rgbif')
# https://cran.r-project.org/web/packages/rgbif/rgbif.pdf
# https://www.gbif.org/tool/81747/rgbif
# install.packages('rgbif')
library(rgbif) # пакет для загрузки данных из информационной системы GBIF

# устанавливаем TimeOut, чтобы сразу не отрубалось
getOption('timeout')
options(timeout = 500000)

# необходимо выяснить идентификатор таксона в системе GBIF - usageKey
name_backbone('Zizifora')  # Zizifora Adans.  status: DOUBTDUL
name_backbone('Ziziphora') # 7308760

# поиск в основном таксономическом списке
name_backbone('Crataégus')$usageKey  # 3013395
name_backbone('Scabiosa')$usageKey   # 2888800
name_backbone('Lonicera') 
taxon = name_backbone('Lonicera L.') # 2888645
taxon$usageKey

taxon = name_lookup('Lonicera') # поиск по всех таксономических списках
nrow(taxon$data) # 100 записей
taxon$data[1,] # первая строка
taxon$data[1,]$scientificName # название из первой строки

taxon = name_lookup('Lonicera', rank = 'genus')
nrow(taxon)
taxon = taxon$data[taxon$data$taxonomicStatus == 'ACCEPTED',]
nrow(taxon)

# также можно taxon GBIF key найти через https://www.gbif.org/species
# чтобы вывести информцию о таксоне https://www.gbif.org/species/2888645

focalTaxon = 2888645 # код рода Lonicera L.

# подсчитываем число находок
numberOccs = occ_count(country = 'KZ', taxonKey = focalTaxon)

# список загружаемых терминов GBIF
terms = c('key','scientificName','taxonKey','decimalLatitude','decimalLongitude',
          'coordinateUncertaintyInMeters','elevation','datasetKey','basisOfRecord',
          'occurrenceStatus','taxonomicStatus','acceptedTaxonKey',
          'acceptedScientificName','taxonRank','iucnRedListCategory',
          'countryCode','stateProvince','county','eventDate','year','month','day',
          'locality','issues','datasetName','collectionCode','recordedBy',
          'verbatimLocality')

# список всех возможных полей - см. руководство Darwin Core Quick Reference Guide
# https://dwc.tdwg.org/terms/

# загружаем находки
occs = occ_search(country = 'KZ', taxonKey = focalTaxon, limit = numberOccs, 
                  fields = terms)

nrow(occs$data) # число находок, полученных из GBIF

# записываем в файл
write.csv(occs$data, paste0(focalTaxon,'_gbif_occurrences.csv'), row.names = F)

# конвертируем векторный формат terra
occurrences = vect(occs$data, geom = c('decimalLongitude', 'decimalLatitude'), crs = 'EPSG:4326')
plot(occurrences)

ggplot() +
  geom_spatvector(data = KZ_bounds1) + 
  geom_spatvector(data = occurrences, aes(color = scientificName)) +
  ggtitle('Распространение видов жимолости на территории Казахстана') + 
  theme(plot.title = element_text(hjust = 0.5, size = 12))

unique(occurrences$scientificName)
sort(table(occurrences$scientificName))

# код вида жимолость татарская
name_backbone('Lonicera tatarica')$usageKey

loniceraTatarica = occurrences[occurrences$acceptedTaxonKey == '5334242',]
nrow(loniceraTatarica)

ggplot() +
  geom_spatvector(data = KZ_bounds1) + 
  geom_spatvector(data = loniceraTatarica, color = 'orange') +
  labs(title = 'Находки вида Lonicera tatarica на территории Казахстана') +
  theme(plot.title = element_text(hjust = 0.5, size = 20))

# биоклиматические слои WorldClim
bioclimatic = worldclim_country('KZ', var = 'bio', res = 10)
bioclimatic
plot(bioclimatic$wc2.1_30s_bio_1)

# краткое описание слоёв (переменных) https://www.worldclim.org/data/bioclim.html
# обрежем слой по границам Казахстана
bioclimKZ = mask(bioclimatic, KZ_bounds1)
plot(bioclimKZ$wc2.1_30s_bio_1)
tmeanKZ = bioclimKZ$wc2.1_30s_bio_1   # среднегодовая температура
precKZ  = bioclimKZ$wc2.1_30s_bio_12  # сумма осадков

# цветовые шкалы
# https://dieghernan.github.io/202212_tidyterra-hillshade-2/
# https://betterfigures.org/2015/06/23/picking-a-colour-scale-for-scientific-graphics/
# https://michaelpaulschramm.com/posts/2022-07-22-drought/
# https://www.climate.gov/news-features/featured-images/new-maps-annual-average-temperature-and-precipitation-us-climate

# среднегодовая температура
ggplot() +
  geom_spatraster(data = tmeanKZ) + 
  scale_fill_whitebox_c(palette = 'muted', limits = c(-20,30), name = 'градусы')

# install.packages('scico') # дополнительные цветовые шкалы
# https://cran.r-project.org/web/packages/scico/scico.pdf
# install.packages('scico')
library(scico)
scico_palette_show()

ggplot() +
  geom_spatraster(data = precKZ) + 
  scale_fill_scico(palette = 'lapaz', name = 'осадки, мм',
                   direction = -1, na.value = 'transparent') +
  geom_spatvector(data = KZ_bounds1, fill = NA) + 
  geom_spatvector(data = KZ_places_city) + 
  labs(title = 'Распределние годовой суммы осадков по территории Казахстана') +
  theme(plot.title = element_text(hjust = 0.5, size = 18))

precipitation = worldclim_country('KZ', var = 'prec', res = 10)
tmean = worldclim_country('KZ', var = 'tavg')
plot(precipitation)
precMonthKZ = mask(precipitation, KZ_bounds1)
plot(precMonthKZ)

ggplot() +
  geom_spatraster(data = precMonthKZ$KAZ_wc2.1_30s_prec_4) +
  scale_fill_scico(palette = 'lapaz', name = 'сумма осадков, мм',
                   direction = -1, na.value = 'transparent', limits = c(1,128)) +
  geom_spatvector(data = KZ_bounds1, fill = NA) +
  geom_spatvector(data = KZ_places_city) + 
  labs(title = 'Распределение месячной суммы осадков по территории Казахстана в апреле') +
  theme(plot.title = element_text(hjust = 0.5, size = 16))

ggplot() +
  geom_spatraster(data = precMonthKZ$KAZ_wc2.1_30s_prec_8) +
  scale_fill_scico(palette = 'lapaz', name = 'сумма осадков, мм',
                   direction = -1, na.value = 'transparent', limits = c(1,128)) +
  geom_spatvector(data = KZ_bounds1, fill = NA) +
  geom_spatvector(data = KZ_places_city) + 
  labs(title = 'Распределение месячной суммы осадков по территории Казахстана в августе') +
  theme(plot.title = element_text(hjust = 0.5, size = 16))

# extract 
tmeanValuesLt = extract(tmeanKZ, loniceraTatarica)

ggplot(data = tmeanValuesLt) +
  geom_histogram(aes(x = wc2.1_30s_bio_1))

ggplot(data = tmeanValuesLt, aes(wc2.1_30s_bio_1)) +
  geom_histogram(bins = 25, color = 'blue', fill = 'red') + 
  labs(x = 'среднегодовая температура', y = 'число находок')

precValuesLt = extract(bioclimatic$wc2.1_30s_bio_12, loniceraTatarica)

ggplot(data = precValuesLt, aes(wc2.1_30s_bio_12)) +
  geom_histogram() +
  labs(x = 'годовая сумма осадков', y = 'число находок')

# высоты над уровнем моря
loniceraTatarica$elevation

# install.packages('elevatr')
library(elevatr) 
# https://cran.r-project.org/web/packages/elevatr/elevatr.pdf
Lt_xy = geom(loniceraTatarica)
class(Lt_xy)
input_xy = data.frame(Lt_xy)

get_elev_point(input_xy, prj = 4326)

input_xy$x[is.na(input_xy$x)] = 0
input_xy$y[is.na(input_xy$y)] = 0

result = get_elev_point(input_xy, prj = 4326, src = 'aws')

# добавляем значения к слою
loniceraTatarica$elevation2 = result$elevation

loniceraTatarica$elevationDiff = loniceraTatarica$elevation2 - loniceraTatarica$elevation

# наиболльшая разница в координатах для точек 71 и 74

# по данным предоставленным в GBIF
ggplot(data = loniceraTatarica) +
  geom_histogram(aes(x = elevation))

# по данным высот взятым по координатам
ggplot(data = loniceraTatarica) +
  geom_histogram(aes(x = elevation2))

min(loniceraTatarica$elevation2) # !  -4931

loniceraTatarica$elevation2[loniceraTatarica$elevation2 == - 4931] = NA

ggplot(data = loniceraTatarica) +
  geom_histogram(aes(x = elevation2))

shapiro.test(precValuesLt$wc2.1_30s_bio_12)

shapiro.test(tmeanValuesLt$wc2.1_30s_bio_1)