library(tidyverse)

dir_factor_development <- NA

dir_factor_development_2016 <- data_modelling %>%
  filter(format(datehour, "%Y") == "2015") %>% # Filtrerer for året 2014
  count(dir_factor) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  as.data.frame()

rownames(dir_factor_development_2015) <- dir_factor_development_2015$dir_factor

dir_factor_development_2015 <- dir_factor_development_2015 %>% 
  select(percentage)

colnames(dir_factor_development_2015)[1] <- "2015"





data_long <- data_modelling %>%
  select(c_NOx_HCAB, hum, speed, pres, temp) %>%
  pivot_longer(cols = c(hum, speed, pres, temp), 
               names_to = "variable", 
               values_to = "value")


s <- lm(c_NOx_HCAB ~ hum + speed + temp + pres, data = data_modelling)

summary(s)

library(dplyr)

# Opret en mapping til de danske navne
variable_names <- c(
  hum = "Fugtighed",
  speed = "Vindhastighed",
  pres = "Atmosfærisk Tryk",
  temp = "Temperatur"
)

# Udfør samlet lineær regression
lm_model <- lm(c_NOx_HCAB ~ hum + speed + pres + temp, data = data_modelling)

# Træk koefficienter og p-værdier
coefficients <- summary(lm_model)$coefficients[-1, ] # Fjern intercept
coefficients_df <- data.frame(
  variable = rownames(coefficients),
  coefficient = coefficients[, 1], # Beta-koefficient
  p_value = coefficients[, 4]      # P-værdi
)

# Tilføj signifikansstjerner
coefficients_df <- coefficients_df %>%
  mutate(
    stars = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      TRUE ~ ""
    ),
    label = paste0(variable_names[variable], "\n(Beta = ", round(coefficient, 2),"), ", "signifikans = ", stars)
  )

# Opret en named vector til labeller
variable_labels <- setNames(coefficients_df$label, coefficients_df$variable)

# Lav plottet med tilpassede facet-titler
ggplot(data_long, aes(x = value, y = c_NOx_HCAB)) +
  geom_point(aes(color = "Datapunkter"), alpha = 0.7) + # Tilføj prikker med en label for legend
  geom_smooth(aes(color = "Lineær regression"), method = "lm", se = FALSE) + # Tilføj linje med label for legend
  facet_wrap(~ variable, scales = "free_x", labeller = labeller(variable = variable_labels)) + # Brug de tilpassede labels
  scale_color_manual(
    values = c("Datapunkter" = "black", "Lineær regression" = "blue"), # Specificér farver
    name = "Forklaring" # Titel til legend
  ) +
  labs(
    title = "Alle opgavens meteorologiske variable og deres regression med NOx",
    x = "Variablenes værdi",
    y = "NOx-koncentration"
  ) +
  theme_minimal()























library(tidyverse)



vejrdata <- data_modelling


vejrdata <- vejrdata %>%
  mutate(datetime = as.POSIXct(datehour, format = "%Y-%m-%d %H:%M:%S"))

# Tilføj en kolonne med året
vejrdata <- vejrdata %>%
  mutate(year = format(datehour, "%Y"))


# Gruppér data efter år og beregn gennemsnitlig trafik
vejrdata <- vejrdata %>%
  group_by(year) %>%
  summarise(mean_hum = mean(hum, na.rm = TRUE),
            mean_temp = mean(temp, na.rm = TRUE),
            mean_speed = mean(speed, na.rm = TRUE),
            mean_pres = mean(pres, na.rm = TRUE)) %>%
  ungroup()# Konverter year til numerisk for korrekt rækkefølge i plot

vejrdata <- vejrdata %>%
  mutate(year = as.numeric(year))

#vejrdata$mean_pres <- scale(vejrdata$mean_pres)
#vejrdata$mean_hum <- scale(vejrdata$mean_hum)
#vejrdata$mean_temp <- scale(vejrdata$mean_temp)
#vejrdata$mean_speed <- scale(vejrdata$mean_speed)
#
#colnames(vejrdata)[2] <- "mean_hum"
#colnames(vejrdata)[3] <- "mean_temp"
#colnames(vejrdata)[4] <- "mean_speed"
#colnames(vejrdata)[5] <- "mean_pres"


vejrdata <- vejrdata %>% 
  filter(year != "2014")

library(ggplot2)
library(dplyr)
library(tidyr)

library(ggplot2)
library(dplyr)
library(tidyr)

# Omstrukturer data til langt format
vejrdata_long <- vejrdata %>%
  pivot_longer(
    cols = c(mean_temp, mean_hum, mean_speed, mean_pres),
    names_to = "Variable",
    values_to = "Value"
  )

# Tilføj manuelle y-akse grænser
y_axis_limits <- tibble(
  Variable = c("mean_temp", "mean_hum", "mean_speed", "mean_pres"),
  y_min = c(0, 50, 0, 1000),   # Minimumværdier for hver variabel
  y_max = c(20, 100, 10, 1030) # Maksimumværdier for hver variabel
)

# Join data med y-akse grænser
vejrdata_long <- vejrdata_long %>%
  left_join(y_axis_limits, by = "Variable")

# Plot med manuelle y-akse grænser
ggplot(vejrdata_long, aes(x = year, y = Value, color = Variable)) +
  geom_line(size = 1) +
  facet_wrap(~ Variable, scales = "free_y", labeller = as_labeller(c(
    mean_temp = "Temperatur (°C)",
    mean_hum = "Fugtighed (%)",
    mean_speed = "Vindhastighed (m/s)",
    mean_pres = "Atmosfærisk pres (hPa)"
  ))) +
  labs(
    title = "Vejrvariablene har ingen større varians i perioden 2015-2022",
    subtitle = "Årligt gennemsnit af vejrvariabler",
    x = "År",
    y = "Værdi"
  ) +
  scale_x_continuous(
    breaks = seq(min(vejrdata$year), max(vejrdata$year), by = 1)
  ) +
  scale_color_manual(
    values = c(
      "mean_temp" = "blue",
      "mean_hum" = "green",
      "mean_speed" = "red",
      "mean_pres" = "purple"
    )
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = "none"
  ) +
  coord_cartesian(
    ylim = NULL # Gør det muligt at bruge y_min og y_max fra data
  ) +
  geom_blank(aes(y = y_min)) +  # Tilføj blank geom for at sætte y-akse minimum
  geom_blank(aes(y = y_max))    # Tilføj blank geom for at sætte y-akse maksimum





library(dplyr)
summary(data_modelling)
