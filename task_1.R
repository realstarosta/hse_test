library(dplyr)
library(googlesheets4)

# чтение таблицы
url_data <- "https://docs.google.com/spreadsheets/d/165sp-lWd1L4qWxggw25DJo_njOCvzdUjAd414NSE8co/edit#gid=1439079331"

df_raw <- googlesheets4::read_sheet(
  url_data, 
  col_types = "cicciinn"
)

# поиск дубликатов
df_raw %>% count(area, keyword) %>% filter(n > 1)
# нашлось семь дубликатов; из задания не ясно что с ними сделать, т.к. count, x и y разный у них

# сколько цветов из палитры нужно взять
df_raw %>% count(area, cluster) %>% pull(n) %>% max()


# таблица с цветами и индексами от одного до пяти
df_color <- tibble(
  id = 1:5,
  color = c("#9edae5", "#dbdb8d", "#c7c7c7", "#f7b6d2", "#c49c94")
)

# создаем итоговую таблицу
df_result <- df_raw %>% 
  filter(!is.na(area)) %>% 
  group_by(area, cluster) %>% 
  mutate(
    id = c(1:n()) # проставляем индексы от одного до кол-ва строк в группе
  ) %>% 
  left_join(df_color, by = c("id")) %>%  # джойним с таблицей цветов
  select(- `good (1)`, -id) %>% 
  arrange(area, cluster, cluster_name, -count)

# сохранение финальной таблицы в googlesheets
s_sheet <- gs4_create(name = "test_1", sheets = df_result)

s_sheet # ID: 1NFOWrsB4jGGSaHr7IVvXSGn4PHw-xTuVjmtPokaQEDc

# не нашел как отформатировать таблицу с помощью googlesheets4 (7ой пункт задания)
# первая строка по дефолту закреплена, а вот фильтрацию отсюда добавить не удалось







