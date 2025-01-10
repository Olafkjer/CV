









library(dplyr)
library(ggplot2)
library(scales)
library(stringr)
library(extrafont)
font_import()
loadfonts()

trafik_data <- calendar_plot_test

# Tilføj kolonner for ugedag og time
trafik_data <- trafik_data %>%
  mutate(
    datetime = as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S"),
    weekday = weekdays(datetime),  # Ugedag
    hour = format(datetime, "%H")  # Time på dagen
  )

# Beregn gennemsnitlig trafik pr. time på dagen
avg_hourly_traffic <- trafik_data %>%
  group_by(hour) %>%
  summarise(mean_traffic = mean(trafik, na.rm = TRUE))

# Sørg for, at time er numerisk for korrekt sortering
avg_hourly_traffic <- avg_hourly_traffic %>%
  mutate(hour = as.numeric(hour))











###Gns. trafik pr. time på dagen
Daglig_trafik <- ggplot(avg_hourly_traffic, aes(x = hour)) +
  geom_line(aes(y = mean_traffic, color = "Gns. trafik pr. time"), size = 1) +  # Linje for timebaseret gennemsnit
  geom_hline(aes(yintercept = overall_mean_traffic, color = "Overordnet gennemsnit"), linetype = "dashed", size = 1) + # Vandret linje for overordnet gennemsnit
  scale_color_manual(
    values = c("Gns. trafik pr. time" = "#2c4c91", "Overordnet gennemsnit" = "black")  # Specificer farver
  ) +
  labs(
    title = "Trafikken falder til under det halve uden for de normale arbejdstider",
    subtitle = "Trafiktallene er sandsynligvis domineret af folk der tager til og fra arbejde mellem 8-18",
    x = "Time på dagen",
    y = "Gennemsnitlig trafik",
    color = "Forklaring",
    caption = "Data fra 1. januar 2015 - 31. december 2022" # Navn på legend
  ) +
  scale_x_continuous(breaks = seq(0, 23, by = 2)) + # Angiv ticks for hver anden time
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
  theme_minimal() +
  theme(
#    plot.title = element_text(hjust = 0.5),
#    plot.subtitle = element_text(hjust = 0.5), # Centrer titel
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title = element_text(face = "bold"),
    legend.title = element_blank(),
    legend.position = "bottom",
    text = element_text(family = "Tahoma")
    # Hold x-labels vandrette
  )


ggsave(filename = "Daglig Trafik.png", plot = Daglig_trafik, width = 10, height = 6, dpi = 300)



?theme()








trafik_data <- calendar_plot_test_uden_2014


trafik_data <- trafik_data %>%
  mutate(datetime = as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S"))

# Tilføj en kolonne med året
trafik_data <- trafik_data %>%
  mutate(year = format(datetime, "%Y"))


# Gruppér data efter år og beregn gennemsnitlig trafik
yearly_traffic <- trafik_data %>%
  group_by(year) %>%
  summarise(mean_traffic = mean(trafik, na.rm = TRUE)) %>%
  ungroup()

# Konverter year til numerisk for korrekt rækkefølge i plot
yearly_traffic <- yearly_traffic %>%
  mutate(year = as.numeric(year))





##gns trafik pr. time pr. år.
Årlig_trafik <- ggplot(yearly_traffic, aes(x = year)) +
  geom_line(aes(y = mean_traffic, color = "Gennemsnit pr. år"), size = 1) +  # Tilføj farve til legend
  geom_hline(aes(yintercept = overall_mean_traffic, color = "Overordnet gennemsnit"), linetype = "dashed", size = 1) +  # Tilføj farve til legend
  labs(
    title = "Den gennemsnitlige trafik pr. time er faldet siden coronaepidemien",
    x = "",
    y = "Trafik pr. time (årligt gns.)",
    color = "Forklaring",
    caption = "Data fra 1. januar 2015 - 31. december 2022"
  ) +
  scale_x_continuous(breaks = seq(min(yearly_traffic$year), max(yearly_traffic$year), by = 1)) +
  scale_y_continuous(
    limits = c(2000, 2200),  # Sæt minimum og maksimum for y-aksen
    breaks = seq(1500, max(yearly_traffic$mean_traffic), by = 50),
    labels = label_number(big.mark = ".", decimal.mark = ",") # Sæt breaks
  ) +
  scale_color_manual(
    values = c("Gennemsnit pr. år" = "#2c4c91", "Overordnet gennemsnit" = "black")  # Definer farver til legend
  ) +
  theme_minimal() +
  theme(
    #    plot.title = element_text(hjust = 0.5),
    #    plot.subtitle = element_text(hjust = 0.5), # Centrer titel
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title = element_text(face = "bold"),
    legend.title = element_blank(),
    legend.position = "bottom",
    text = element_text(family = "Tahoma")
    # Hold x-labels vandrette
  )


ggsave(filename = "Årlig Trafik.png", plot = Årlig_trafik, width = 10, height = 6, dpi = 300)








### Tidslinjeplot 1.-31. januar 2022
trafikkorrelationsplot <- calendar_plot_test[54413:63084,]

rownames(trafikkorrelationsplot) <- NULL

trafikkorrelationsplot <- trafikkorrelationsplot[1:744,]

#trafikkorrelationsplot$date <- as.Date(trafikkorrelationsplot$date)
#trafikkorrelationsplot$date <- as.Date(trafikkorrelationsplot$date, format = "%d-%m-%Y")

Sys.setlocale("LC_TIME", "en_US.UTF-8")

Cykliske_sving <- ggplot(trafikkorrelationsplot, aes(x = date, y = trafik))+
  geom_line(linewidth = 0.7, aes(y = trafik, color = "Daglig Trafik"))+
  geom_hline(aes(yintercept = overall_mean_traffic, color = "Overordnet gennemsnit"), linetype = "dashed", size = 1)+
  theme_minimal()+
  labs(
    title = "Trafikken svinger cyklisk fra dag til nat",
    subtitle = "Der er også mindre trafik i weekenden",
    x = "",
    y = "Daglig Trafik (antal biler)",
    color = "Forklaring",
    caption = "Data fra 1. - 31. januar 2022"
  ) +
  scale_color_manual(
    values = c("Daglig Trafik" = "#2c4c91", "Overordnet gennemsnit" = "black"),
    breaks = c("Daglig Trafik", "Overordnet gennemsnit")
    # Definer farver til legend
  ) +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")
  ) +
  theme(
    #    plot.title = element_text(hjust = 0.5),
    #    plot.subtitle = element_text(hjust = 0.5), # Centrer titel
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title = element_text(face = "bold"),
    legend.title = element_blank(),
    legend.position = "bottom",
    text = element_text(family = "Tahoma")
    # Hold x-labels vandrette
  )

ggsave(filename = "Cykliske Sving.png", plot = Cykliske_sving, width = 10, height = 6, dpi = 300)


library(tidyverse)


trafikkorrelationsplot_år <- calendar_plot_test[54413:63084,]

model <- lm(c_NOx_HCAB ~ trafik, data = calendar_plot_test)
summary(model)
stigning_pr_bil <- coef(model)[2] # Hældningen (NOx stigning pr. bil)
print(paste("NOx stigning pr. bil: ", round(stigning_pr_bil, 6)))

# Plot med regressionslinje og annotation af hældningen
Trafik_korrelation <- ggplot(calendar_plot_test, aes(x = trafik, y = c_NOx_HCAB)) +
  # Punkter med legend-label
  geom_point(aes(color = "Timemålinger for trafik"), alpha = 0.6) +
  # Regressionslinje med legend-label
  geom_smooth(aes(color = "Lineær regression"), method = "lm", se = FALSE, size = 1) +
  scale_y_continuous(limits = c(0, 550),
                     breaks = seq(0, 600, 100)) +
  scale_x_continuous(labels = label_number(big.mark = ".", decimal.mark = ","),
                     limits = c(0, 5285),
                     breaks = seq(0,5285,1000)
  ) +
  # Definer farver og rækkefølge i legend
  scale_color_manual(
    name = "Forklaring", # Titel i legend
    values = c("Timemålinger for trafik" = "#2c4c91", "Lineær regression" = "black")
  ) +
  # Tekst-annotation
  annotate("text", x = min(calendar_plot_test$trafik) + 1000, 
           y = max(calendar_plot_test$c_NOx_HCAB) - 5,
           label = paste("NOx pr bil:", round(stigning_pr_bil, 6)),
           size = 5, color = "black") +
  theme_minimal() +
  labs(
    title = "NOx har positiv korrelation med trafik",
    subtitle = "Hældning = 0.0047, p-værdi = <0.001 (***)",
    x = "Biler pr. time",
    y = "NOx (Timemålinger)",
    caption = "Data fra 1. januar 2015 - 31. december 2022"
  ) +
  theme(
    #    plot.title = element_text(hjust = 0.5),
    #    plot.subtitle = element_text(hjust = 0.5), # Centrer titel
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title = element_text(face = "bold"),
    legend.title = element_blank(),
    legend.position = "bottom",
    text = element_text(family = "Tahoma")
    # Hold x-labels vandrette
  )

ggsave(filename = "Trafik Korrelation.png", plot = Trafik_korrelation, width = 10, height = 6, dpi = 300)








### Tidslinjeplot 1.-31. januar 2022
trafikkorrelationsplot <- calendar_plot_test[54413:63084,]

rownames(trafikkorrelationsplot) <- NULL

trafikkorrelationsplot <- trafikkorrelationsplot[1:744,]

# Eksempel: Omstrukturér data til long format
trafikkorrelationsplot_long <- trafikkorrelationsplot %>%
  pivot_longer(
    cols = c(hum, speed), # Kolonner du vil inkludere i plottet
    names_to = "Variable",        # Kolonnenavn for kategorier (trafik, hum, speed)
    values_to = "Value"           # Kolonnenavn for værdier
  )

# Plot med ggplot
ggplot(trafikkorrelationsplot_long, aes(x = date, y = Value, color = Variable)) +
  geom_line(linewidth = 0.7) +
  theme_minimal() +
  labs(
    title = "Sammenligning af trafik, fugtighed og hastighed",
    subtitle = "Trafik, fugtighed (hum), og gennemsnitshastighed (speed) over tid",
    x = "Dato",
    y = "Værdi",
    color = "Variable",
    caption = "Data fra 1. - 31. Januar 2022"
  ) +
  scale_color_manual(
    values = c(
      "hum" = "blue",
      "speed" = "green",
      "Overordnet gennemsnit" = "red"
    ),
    breaks = c("hum", "speed", "Overordnet gennemsnit") # For at sikre orden i legend
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  )

