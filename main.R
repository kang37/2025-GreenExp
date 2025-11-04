pacman::p_load(
  dplyr, readr, sf, mapview, lubridate, ggplot2, showtext
)
showtext_auto()

# 读取原始数据并转化成sf数据。
okinawa_raw <- fs::dir_ls(
  "data_raw/AgoopOkinawa/sophia_PDP_PDP_001_20230801_20230831/",
  regexp = "PDP.+.csv$"
) %>%
  head() %>%
  read_csv(col_select = c(
    dailyid, year, month, day, dayofweek, hour, minute,
    latitude, longitude, home_prefcode, home_citycode, accuracy, age, gender
  )) %>%
  head(10000)

okinawa_gis <- okinawa_raw %>%
  st_as_sf(coords = c("longitude", "latitude")) %>%
  st_set_crs(4326) %>%
  st_transform(6676)

mapview(okinawa_gis)

# 冲绳粗略边界。
okinawa_bbox <- c(
  xmin = 127.68,  # 最西端
  xmax = 128.26,  # 最东端
  ymin = 26.07,   # 最南端
  ymax = 26.88    # 最北端
)
okinawa_boundary <- st_as_sfc(
  st_bbox(okinawa_bbox, crs = st_crs(4326))
) %>%
  st_transform(6676)

okinawa_gis_filtered <- okinawa_gis %>%
  st_filter(okinawa_boundary, .predicate = st_intersects) %>%
  filter(accuracy <= 20)
mapview(okinawa_gis_filtered)

# 数据清洗：每个用户每5分钟保留一个点。
okinawa_gis_cleaned <- okinawa_gis_filtered %>%
  # 创建5分钟时间段标识
  mutate(
    time_5min = floor(minute / 5) * 5,  # 将分钟数归到5分钟段
    datetime = make_datetime(year, month, day, hour, time_5min)
  ) %>%
  # 按用户、日期、5分钟段分组
  group_by(dailyid, year, month, day, hour, time_5min) %>%
  # 先按分钟排序（保留最早），再按精度排序（保留最高精度）
  arrange(minute, accuracy) %>%
  # 每组保留第一条记录
  slice(1) %>%
  ungroup()
# 小结。
cat("原始数据点数:", nrow(okinawa_gis), "\n")
cat("过滤后数据点数:", nrow(okinawa_gis_cleaned), "\n")

# 读取公园数据并创建缓冲区
# 读取公园点数据
parks <- st_read("data_raw/P13-11_47_GML/P13-11_47.shp") %>%
  # 原坐标为JGD2000
  st_set_crs(4612) %>%
  # 转换为目标坐标系 JGD2000 / (B, L) -> EPSG: 6676
  st_transform(6676)  # 转换到与轨迹数据相同的坐标系

# 创建缓冲区
parks_buffer <- parks %>%
  st_buffer(dist = 300)
cat("公园数量:", nrow(parks), "\n")

# 可视化公园缓冲区
mapview(parks_buffer, col.regions = "green", alpha.regions = 0.3,
        layer.name = "公园缓冲区") +
  mapview(okinawa_gis_cleaned %>% head(1000),
          col.regions = "red", cex = 2, layer.name = "轨迹点样本")

# 判断哪些点在公园缓冲区内
# 空间连接：找出在公园缓冲区内的点
points_in_parks <- st_join(
  okinawa_gis_cleaned,
  parks_buffer,
  join = st_within,
  left = FALSE  # 只保留在公园内的点
)
cat("在公园缓冲区内的点数:", nrow(points_in_parks), "\n")

# 计算每个用户每天在公园内的滞留时间
# 为每个用户每天的公园访问计算滞留时间（以5分钟为单位）
daily_park_time <- points_in_parks %>%
  st_drop_geometry() %>%  # 移除几何信息以加快计算
  group_by(dailyid, year, month, day, dayofweek, age, gender) %>%
  # Bug: 非常简化的计算。
  summarise(
    park_time_minutes = n() * 5,  # 每个点代表5分钟
    n_visits = n(),  # 访问次数（数据点数量）
    .groups = "drop"
  ) %>%
  mutate(
    is_weekend = dayofweek %in% c(1, 7),  # 1=周日, 7=周六
    weekday_type = ifelse(is_weekend, "周末", "周中")
  )

# 分析：周中 vs 周末的滞留时间差异
weekday_comparison <- daily_park_time %>%
  group_by(weekday_type) %>%
  summarise(
    mean_time = mean(park_time_minutes, na.rm = TRUE),
    median_time = median(park_time_minutes, na.rm = TRUE),
    sd_time = sd(park_time_minutes, na.rm = TRUE),
    n_users = n(),
    .groups = "drop"
  )
print(weekday_comparison)

# 可视化：周中 vs 周末
ggplot(daily_park_time, aes(
  x = weekday_type, y = park_time_minutes,
  fill = weekday_type)
) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1) +
  labs(
    title = "周中 vs 周末公园滞留时间比较",
    x = "时间类型",
    y = "滞留时间（分钟）",
    fill = "时间类型"
  ) +
  theme_minimal(base_family = "") +
  theme(legend.position = "none")

# 分析不同性别的滞留时间差异
daily_park_time %>%
  filter(!is.na(gender)) %>%
  group_by(gender) %>%
  summarise(
    mean_time = mean(park_time_minutes, na.rm = TRUE),
    median_time = median(park_time_minutes, na.rm = TRUE),
    sd_time = sd(park_time_minutes, na.rm = TRUE),
    n_users = n(),
    .groups = "drop"
  )

# 可视化：性别差异
ggplot(daily_park_time %>% filter(!is.na(gender)),
             aes(x = as.factor(gender), y = park_time_minutes,
                 fill = as.factor(gender))) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1) +
  labs(
    title = "不同性别公园滞留时间比较",
    x = "性别",
    y = "滞留时间（分钟）",
    fill = "性别"
  ) +
  scale_x_discrete(labels = c("1" = "男性", "2" = "女性")) +
  scale_fill_discrete(labels = c("1" = "男性", "2" = "女性")) +
  theme_minimal(base_family = "") +
  theme(legend.position = "bottom")

# 分析:不同年龄的滞留时间差异
# 创建年龄组
daily_park_time %>%
  filter(!is.na(age)) %>%
  mutate(
    age_group = case_when(
      age < 20 ~ "< 20岁",
      age >= 20 & age < 30 ~ "20-29岁",
      age >= 30 & age < 40 ~ "30-39岁",
      age >= 40 & age < 50 ~ "40-49岁",
      age >= 50 & age < 60 ~ "50-59岁",
      age >= 60 ~ "≥ 60岁",
      TRUE ~ "未知"
    ),
    age_group = factor(age_group, levels = c("< 20岁", "20-29岁", "30-39岁",
                                             "40-49岁", "50-59岁", "≥ 60岁"))
  ) %>%
  group_by(age_group) %>%
  summarise(
    mean_time = mean(park_time_minutes, na.rm = TRUE),
    median_time = median(park_time_minutes, na.rm = TRUE),
    sd_time = sd(park_time_minutes, na.rm = TRUE),
    n_users = n(),
    .groups = "drop"
  )

# 可视化：年龄差异
daily_park_time %>%
  filter(!is.na(age)) %>%
  mutate(
    age_group = case_when(
      age < 20 ~ "< 20岁",
      age >= 20 & age < 30 ~ "20-29岁",
      age >= 30 & age < 40 ~ "30-39岁",
      age >= 40 & age < 50 ~ "40-49岁",
      age >= 50 & age < 60 ~ "50-59岁",
      age >= 60 ~ "≥ 60岁",
      TRUE ~ "未知"
    ),
    age_group = factor(age_group, levels = c("< 20岁", "20-29岁", "30-39岁",
                                             "40-49岁", "50-59岁", "≥ 60岁"))
  ) %>%
  ggplot(aes(x = age_group, y = park_time_minutes, fill = age_group)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1) +
  labs(
    title = "不同年龄组公园滞留时间比较",
    x = "年龄组",
    y = "滞留时间（分钟）",
    fill = "年龄组"
  ) +
  theme_minimal(base_family = "") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

