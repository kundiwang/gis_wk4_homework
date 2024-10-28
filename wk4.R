install.packages("countrycode")
library(sf)
library(dplyr)
library(countrycode)
library(here)
library(readr)
install.packages("janitor")
library(janitor)
library(tidyr)

#读取数据
world_data <- st_read(file.choose()) %>%
  clean_names()
gii_data <- read_csv(file.choose()) %>%
  clean_names()

head(gii_data)
str(gii_data)

#筛选2010和2019数据，求差异值
gii_clean <- gii_data %>%
  select(iso3, country, hdi_2010, hdi_2019) %>%  # Select the correct columns
  mutate(gii_diff = hdi_2019 - hdi_2010)
head(gii_clean)

#创建 3 字母 ISO 代码列
world_data <- world_data %>%
  mutate(iso3 = countrycode(iso, origin = "iso2c", destination = "iso3c"))

#合并
merged_data <- world_data %>%
  left_join(gii_clean, by = c("iso3" = "iso3"))

#检查
str(merged_data)
head(merged_data)

library(ggplot2)
install.packages("viridis")
library(viridis)

# 可视化
ggplot(data = merged_data) +
  geom_sf(aes(fill = gii_diff), color = NA) +  # 用于填充颜色
  scale_fill_viridis_c(option = "plasma", na.value = "grey") +  # 使用 Viridis 颜色调色板
  theme_minimal() +  # 使用简约主题
  labs(fill = "Inequality Difference (2019 - 2010)",
       title = "World Inequality Difference") +
  theme(legend.position = "right")  # 图例位置


# 保存图形
ggsave("world_inequality_difference.png", width = 10, height = 6, dpi = 300)
