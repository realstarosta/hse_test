library(dplyr)
library(googlesheets4)
library(stringr)
library(ggplot2)

# чтение таблицы
url_data <- "https://docs.google.com/spreadsheets/d/1NFOWrsB4jGGSaHr7IVvXSGn4PHw-xTuVjmtPokaQEDc/edit#gid=268319275"

df_raw <- googlesheets4::read_sheet(
  url_data, 
  col_types = "ciccinnc"
)

# отрисовка и сохранние одного графика
areas <- df_raw %>% pull(area) %>% unique()
areas
area_flt <- "twisted" # для примера возьмем этот area

gg_plot <- df_raw %>%
  filter(!is.na(area)) %>% # забыл удалить пустую строку в первой части задания
  filter(area == area_flt) %>%
  mutate(
    # keyword = paste0(str_sub(keyword, 1, 15), "\n", str_sub(keyword, 15, -1)) # перенос строки после 15ого символа в строке
    keyword = str_replace_all(keyword, "\\s", "\\s\n") # перенос строки после каждого пробела
  ) %>%
  ggplot(aes(x, y, color = cluster_name)) +
  geom_point(size = 3) +
  labs(title = paste0("Area - ", area_flt)) +
  geom_text(aes(x, y, label = keyword), colour = "grey", nudge_y = 1, inherit.aes = F)
  # facet_wrap(vars(area), scales = "free")

ggsave("Plot_1.png", plot = gg_plot, scale = 1.7, width = 1500, units = "px")

# функция отрисовки и сохраннения графика
save_plots <- function(area_flt) {
  gg_plot <- df_raw %>%  
    filter(!is.na(area)) %>% 
    filter(area == area_flt) %>%
    mutate(
      # keyword = paste0(str_sub(keyword, 1, 15), "\n", str_sub(keyword, 15, -1)) # перенос строки после 15ого символа в строке
      keyword = str_replace_all(keyword, "\\s", "\\s\n") # перенос строки после каждого пробела
    ) %>% 
    ggplot(aes(x, y, color = cluster_name)) + 
    geom_point(size = 3) +
    labs(title = paste0("Area - ", area_flt)) +
    geom_text(aes(x, y, label = keyword), colour = "grey", nudge_y = 1, inherit.aes = F)
  # facet_wrap(vars(area), scales = "free")
  
  ggsave(filename = paste("Plot_", area_flt,".png" ), plot = gg_plot, scale = 1.7, width = 1500, units = "px")
}

# кидаем в эту функцию вектор имен area и на выходе получаем по графику на каждый area
sapply(areas, save_plots)

