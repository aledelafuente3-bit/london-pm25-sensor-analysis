setwd("~/Downloads/DATA ANALYSIS/Practical_03/Practical_03/FIELD AND LAB ASSIGNMENT")

library(lubridate)
library(dplyr)

df <- read.csv("Airquality.csv") 
head(Airquality)

19
10
10



df <- df %>%
  mutate(datetime = make_datetime(Year, Month, Day, Hour, Min, Sec)) %>%
  mutate(site = case_when(
    datetime >= ymd_hms("2025-10-09 13:18:34") & datetime <= ymd_hms("2025-10-09 13:46:48") ~ "City_Midday",
    datetime >= ymd_hms("2025-10-09 13:49:22") & datetime <= ymd_hms("2025-10-09 14:01:29") ~ "Park_Control",
    datetime >= ymd_hms("2025-10-09 18:36:03") & datetime <= ymd_hms("2025-10-09 19:10:10") ~ "City_Evening",
    TRUE ~ "unknown"
  ))


table(df$site) 

library(ggplot2) 
ggplot(df, aes(datetime, PM2.5)) +
  geom_line() +
  theme_minimal()

df <- df %>%
  mutate(site = case_when(
    datetime >= ymd_hms("2025-10-09 13:18:34") & datetime <= ymd_hms("2025-10-09 13:46:48") ~ "City_Midday",
    datetime >= ymd_hms("2025-10-09 13:49:22") & datetime <= ymd_hms("2025-10-09 14:01:29") ~ "Park_Control",
    datetime >= ymd_hms("2025-10-09 18:36:03") & datetime <= ymd_hms("2025-10-09 19:10:10") ~ "City_Evening",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(site)) 

ggplot(df, aes(datetime, PM2.5)) +
  geom_line() +
  theme_minimal()

summary_tbl <- df %>%
  group_by(site) %>%
  summarise(
    n = n(),
    mean_PM25 = mean(PM2.5, na.rm = TRUE),
    sd_PM25   = sd(PM2.5, na.rm = TRUE),
    mean_PM10 = mean(PM10, na.rm = TRUE),
    sd_PM10   = sd(PM10, na.rm = TRUE),
    duration_min = as.numeric(difftime(max(datetime), min(datetime), units = "mins"))
  )
print(summary_tbl)

print(summary_all)
summary_all <- df %>%
  group_by(site) %>%
  summarise(
    n = n(),
    mean_PM1   = mean(PM01, na.rm = TRUE),
    sd_PM1     = sd(PM01, na.rm = TRUE),
    mean_PM25  = mean(PM2.5, na.rm = TRUE),
    sd_PM25    = sd(PM2.5, na.rm = TRUE),
    mean_PM10  = mean(PM10, na.rm = TRUE),
    sd_PM10    = sd(PM10, na.rm = TRUE),
    mean_Ta    = mean(Ta, na.rm = TRUE),
    sd_Ta      = sd(Ta, na.rm = TRUE),
    mean_rh    = mean(rh, na.rm = TRUE),
    sd_rh      = sd(rh, na.rm = TRUE),
    mean_P     = mean(P, na.rm = TRUE),
    sd_P       = sd(P, na.rm = TRUE)
  )
print(summary_all) 

desc <- df %>%
  group_by(site) %>%
  summarise(
    n = n(),
    PM01_mean = mean(PM01),
    pm2.5_mean = mean(PM2.5),
    pm10_mean = mean(PM10),
    Ta_mean = mean(Ta),
    rh_mean = mean(rh),
    .groups = "drop"
  )
print(desc)

ggplot(df, aes(sample = PM2.5)) +
  stat_qq() + stat_qq_line() + facet_wrap(~site) #not normally distributed for City_Evening and City_Midday, almost normal for Park_control)
# so run Shapiro wilks test 
by(df$PM2.5, df$site, shapiro.test) 
#shapiro show that all data are non-normal 
#ANOVA TEST TO MAKE SURE// actually not relavent at all 
a_PM2.5 <- aov(PM2.5 ~ site, data = df)
summary(a_PM2.5)
TukeyHSD(a_PM2.5)
# all p < 0.05; so we rejecect the null hypothesis and assume that there is a statistically significant difference between at least 2 of group 
# so then run Kruskal test
kruskal.test(PM2.5 ~ site, data = df)
##results: Kruskal-Wallis rank sum test; data:  PM2.5 by site; Kruskal-Wallis chi-squared = 432.19, df = 2, p-value < 2.2e-16

install.packages("FSA")
library(FSA)
dunnTest(PM2.5 ~ site, data = df, method = "bh")
##   Comparison          Z      P.unadj        P.adj
#1  City_Evening - City_Midday 17.5092156 1.218617e-68 3.655851e-68
#2 City_Evening - Park_Control 15.9197682 4.620858e-57 6.931288e-57
#3  City_Midday - Park_Control  0.4825785 6.293950e-01 6.293950e-01


aggregate(PM2.5 ~ site, df,
          function(x) c(Median = median(x), IQR = IQR(x)))
aggregate(PM01 ~ site, df,
          function(x) c(Median = median(x), IQR = IQR(x)))
aggregate(PM10 ~ site, df,
          function(x) c(Median = median(x), IQR = IQR(x)))
aggregate(Ta ~ site, df,
          function(x) c(Median = median(x), IQR = IQR(x)))
aggregate(rh ~ site, df,
          function(x) c(Median = median(x), IQR = IQR(x)))
aggregate(P ~ site, df,
          function(x) c(Median = median(x), IQR = IQR(x)))

library(tidyr)
df %>%
  pivot_longer(cols = c(PM01, PM2.5, PM10), names_to = "ParticleType", values_to = "Concentration") %>%
  ggplot(aes(site, Concentration, fill = ParticleType)) +
  geom_boxplot() +
  labs(x = "Site", y = "Concentration (µg/m³)", title = "Distribution of PM1, PM2.5, and PM10 by Site") +
  theme_minimal()
boxplot(PM2.5 ~ site, data = df,
        main = "PM2.5 by Site",
        ylab = "PM2.5",
        col = "grey90")
cor.test(df$PM2.5, df$rh, method = "spearman")
cor.test(df$PM2.5, df$Ta, method = "spearman")
cor.test(df$PM2.5, df$P, method = "spearman")

#Epsilon2 test 
n <- nrow(df)
n
H <- kruskal_result$statistic
k <- length(unique(df$site))

epsilon2 <- (H - k + 1) / (n - k)
epsilon2

#epilson2 results indicate that site and time of day have a strong practical influence on PM2.5, not just statisically significant 




library(dplyr)
library(tidyr)
library(ggplot2)

# Make sure your df already has the 'site' column created earlier
# and includes Ta, rh, and P columns.

# Create a long-format version for environmental variables
env_vars <- df %>%
  pivot_longer(
    cols = c(Ta, rh, P),
    names_to = "Variable",
    values_to = "Value"
  )

# Plot: boxplots of Ta, rh, and P by site
ggplot(env_vars, aes(x = site, y = Value, fill = site)) +
  geom_boxplot() +
  facet_wrap(~Variable, scales = "free_y") +
  labs(
    x = "Site",
    y = "Value",
    title = "Environmental Conditions (Temperature, Humidity, Pressure) by Site"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

library(GGally) 
df %>%
  select(PM01, PM2.5, PM10, Ta, rh, P) %>%
  ggpairs()

library(tidyverse)
library(lubridate)
library(GGally)
library(ggcorrplot)

# =========================
# 1) LOAD + LABEL DATA
# =========================
df <- read.csv("Airquality.csv")

df <- df %>%
  mutate(datetime = make_datetime(Year, Month, Day, Hour, Min, Sec)) %>%
  # drop warm-up day (3 Oct)
  filter(date(datetime) == as_date("2025-10-09")) %>%
  # label sites using the precise windows we detected
  mutate(site = case_when(
    datetime >= ymd_hms("2025-10-09 13:18:34") & datetime <= ymd_hms("2025-10-09 13:46:48") ~ "City_Midday",
    datetime >= ymd_hms("2025-10-09 13:49:22") & datetime <= ymd_hms("2025-10-09 14:01:29") ~ "Park_Control",
    datetime >= ymd_hms("2025-10-09 18:36:03") & datetime <= ymd_hms("2025-10-09 19:10:10") ~ "City_Evening",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(site)) %>%
  # ORDER sites left→right for easy reading (control → midday → evening)
  mutate(site = factor(site, levels = c("Park_Control","City_Midday","City_Evening")))

# custom colors (consistent across figs)
pm_cols <- c("PM01" = "#b2182b", "PM2.5" = "#2166ac", "PM10" = "#1b7837")
site_cols <- c("Park_Control"="#6baed6","City_Midday"="#9ecae1","City_Evening"="#2171b5")

# =========================
# 2) SUMMARY TABLE (optional for Results)
# =========================
summary_all <- df %>%
  group_by(site) %>%
  summarise(
    n = n(),
    mean_PM1   = mean(PM01),  sd_PM1   = sd(PM01),
    mean_PM25  = mean(PM2.5), sd_PM25  = sd(PM2.5),
    mean_PM10  = mean(PM10),  sd_PM10  = sd(PM10),
    mean_Ta    = mean(Ta),    sd_Ta    = sd(Ta),
    mean_rh    = mean(rh),    sd_rh    = sd(rh),
    mean_P     = mean(P),     sd_P     = sd(P)
  )
print(summary_all)

# =========================
# 3) FIGURE 1 — PM boxplots, better polish
#    - ordered sites
#    - consistent color by particle size
#    - mean markers
#    - trimmed y-range (caps at 99th percentile to reduce extreme outlier stretch)
# =========================
pm_long <- df %>%
  pivot_longer(c(PM01, `PM2.5`, PM10), names_to = "ParticleType", values_to = "Concentration")

upper_cap <- quantile(pm_long$Concentration, 0.99, na.rm = TRUE)

fig1 <- ggplot(pm_long, aes(x = site, y = Concentration, fill = ParticleType)) +
  geom_boxplot(outlier_alpha = 0.25, width = 0.7, position = position_dodge2(preserve = "single")) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3,
               position = position_dodge(width = 0.75), colour = "black") +
  scale_fill_manual(values = pm_cols, name = "ParticleType",
                    breaks = c("PM01","PM2.5","PM10")) +
  coord_cartesian(ylim = c(0, upper_cap)) +
  labs(x = "Site", y = "Concentration (µg/m³)",
       title = "Distribution of PM1, PM2.5, and PM10 by Site") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right",
        plot.title = element_text(face="bold"))

ggsave("Fig1_PM_by_Site_boxplot.png", fig1, width = 7, height = 5, dpi = 300)


# =========================
# 5) FIGURE 2 — Environmental conditions (Ta, rh, P)
#    - facet with clean labels
#    - no legend duplication
# =========================
env_labs <- c(Ta = "Temperature (°C)", rh = "Relative Humidity (%)", P = "Pressure (hPa)")

env_long <- df %>%
  pivot_longer(c(Ta, rh, P), names_to = "Variable", values_to = "Value")

fig2 <- ggplot(env_long, aes(site, Value, fill = site)) +
  geom_boxplot(width = 0.7, outlier_alpha = 0.25) +
  scale_fill_manual(values = site_cols, guide = "none") +
  facet_wrap(~Variable, scales = "free_y", labeller = as_labeller(env_labs)) +
  labs(x = "Site", y = "Value", title = "Environmental Conditions by Site") +
  theme_minimal(base_size = 12) +
  theme(strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold"))
ggsave("Fig2_Environmental_by_Site.png", fig2, width = 7.5, height = 5, dpi = 300)

# =========================
# 6) FIGURE 3 — Correlation heatmap (cleaner than full ggpairs)
#    - round to 2 decimals
#    - show only lower triangle
# =========================
corr_df <- df %>% select(PM01, `PM2.5`, PM10, Ta, rh, P)
corr_mat <- cor(corr_df, use = "pairwise.complete.obs", method = "spearman")

fig3 <- ggcorrplot(
  corr_mat, lab = TRUE, lab_size = 3, type = "lower",
  outline.col = "white", colors = c("#b2182b","#f7f7f7","#2166ac"), # red-white-blue
  ggtheme = theme_minimal(base_size = 12)
) +
  labs(title = "Correlation Among PM and Environmental Variables") +
  theme(plot.title = element_text(face="bold"))
ggsave("Fig3_Correlation_heatmap.png", fig3, width = 6.5, height = 5.5, dpi = 300)
fig3
# =========================
# 7) (Optional) FIGURE 4 — Time series for Methods/SI
#    - shows the three windows colored by site
# =========================
fig4 <- ggplot(df, aes(datetime, `PM2.5`, color = site)) +
  geom_line() +
  scale_color_manual(values = site_cols, name = "Site") +
  labs(x = "Time", y = "PM2.5 (µg/m³)", title = "PM2.5 over Time by Site") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face="bold"))
ggsave("Fig4_PM25_time_series.png", fig4, width = 7.5, height = 4.5, dpi = 300)
fig1
fig2
fig3
fig4
shapiro.test(Air)
cor.test(df$PM2.5, df$rh, method = "spearman")
cor.test(df$PM2.5, df$Ta, method = "spearman")
cor.test(df$PM2.5, df$P,  method = "spearman")

library(dplyr)
library(tidyr)

table1 <- df %>%
  group_by(site) %>%
  summarise(
    n = n(),
    PM01_median = median(PM01, na.rm = TRUE),
    PM01_IQR = IQR(PM01, na.rm = TRUE),
    PM25_median = median(`PM2.5`, na.rm = TRUE),
    PM25_IQR = IQR(`PM2.5`, na.rm = TRUE),
    PM10_median = median(PM10, na.rm = TRUE),
    PM10_IQR = IQR(PM10, na.rm = TRUE),
    Ta_median = median(Ta, na.rm = TRUE),
    Ta_IQR = IQR(Ta, na.rm = TRUE),
    rh_median = median(rh, na.rm = TRUE),
    rh_IQR = IQR(rh, na.rm = TRUE),
    P_median = median(P, na.rm = TRUE),
    P_IQR = IQR(P, na.rm = TRUE)
  )

table1
library(ggplot2)
cols <- c(
  "City_Evening" = "#4575B4",  # blue
  "City_Midday"  = "#FDAE61",  # orange
  "Park_Control" = "#1A9850"   # green
)

fig1.1 <- ggplot(df, aes(x = datetime, y = `PM2.5`, color = site)) +
  geom_line(size = 0.7)  + scale_color_manual(values = cols) + labs(
    title = "PM2.5 Over Time by Site",
    x = "Time",
    y = expression(PM[2.5]~(mu*g/m^3)),
    color = "Site"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right"
  )

ggsave("Fig1.1_PM25_TimeSeries.png", fig1, width = 7, height = 5, dpi = 300)
fig2 <- ggplot(df, aes(x = site, y = `PM2.5`, fill = site)) +
  geom_boxplot(outlier.shape = 16, outlier.size = 1.5, alpha = 0.7) +
  coord_cartesian(ylim = c(0, 40)) +   # <<< ADD THIS
  scale_fill_manual(values = c(
    "City_Evening" = "#4575B4",
    "City_Midday" = "#fc8d59",
    "Park_Control" = "#1a9850"
  )) +
  labs(
    title = "Distribution of PM2.5 by Site",
    x = "Site",
    y = expression(PM[2.5]~(mu*g/m^3))
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none"
  )

ggsave("Fig2_PM25_Boxplot.png", fig2, width = 6.5, height = 5, dpi = 300)

fig1.1
fig2
