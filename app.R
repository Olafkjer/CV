library(shiny)
library(tidyverse)
library(randomForest)
library(lubridate)
library(httr)
library(jsonlite)

background_color <- "#ffffff"
plot_color <- "#3f527b"
line_color <- "#403D39"


avg_traffic <- data_modelling %>%
  mutate(day_month_hour = format(datehour, "%m_%d_%H")) %>%
  group_by(day_month_hour) %>%
  summarise(avg_trafik = mean(trafik, na.rm = TRUE)) %>%
  arrange(day_month_hour)


ui <- navbarPage(
  # App titel
  "NOx Applikation", 
  
  # Første tab
  tabPanel("Forside",
           fluidPage(
             # custom CSS
             tags$head(
               tags$style(HTML("
          body {
            background-color: #f7f7f7;
            font-family: 'Tahoma', sans-serif; /* Change the font globally */
          }
          
          /* Customize Navbar background color */
          .navbar {
            background-color: #2E3B4E; /* Dark blue background for navbar */
            color: white; /* Change text color */
          }
          
          /* Change the color of the navbar links */
          .navbar .navbar-nav > li > a {
            color: white; /* Navbar link text color */
          }
          
          /* Change the color of the navbar links on hover */
          .navbar .navbar-nav > li > a:hover {
            color: #FF6347; /* Color on hover */
          }
          
          /* Customize the panel background */
          .tab-content {
            background-color: #FFFFFF; /* White background for tab content */
            padding: 20px;
            border-radius: 8px;
          }
          
          /* Custom title color */
          .title {
            color: black;
            font-weight: bold;
          }
          
        "))
             ),
             
             
             # Main content
             fluidRow(
               column(12, 
                      h4("Information om NOx Applikationen"),
                      p("Denne applikation giver brugeren mulighed for at foretager analyser og forudsigelser omkring NOx-data."),
                      br(),
                      p("Dataene stammer fra miljømålinger foretaget i Danmark. NOx-koncentrationen er opgjort time for time, og dataene omfatter meteorologiske forhold som vindhastighed, vindretning, temperatur, luftfugtighed og atmosfærisk pres. Samtidig er der benyttet trafikdata i form at optælling af antal køretøjer."),
                      p("Formålet med denne applikation er at visualisere og analysere data samt forudsige NOx-koncentrationen på H.C. Andersens Boulevard."),
                      br(),
                      img(src = "https://media.discordapp.net/attachments/1309165981385363496/1325495935215865946/image.png?ex=677bffc9&is=677aae49&hm=26cf867e1327e8aa867bc26c35c08f27a6bfe7f0aea7687f3b96f43c27861381&=&format=webp&quality=lossless&", alt = "NOx illustration", style = "width: 100%; max-width: 600px; display: block; margin: 0 auto;"),
                      br(),
                      p("Billedet ovenfor er en illustration af luftforureningsdata."),
                      h5("Kilder:"),
                      p(HTML("Luftforureningsdata er hentet fra Instituttet for Miljøvidenskab (Aarhus Universitet). For mere information, se venligst <a href='https://envs.au.dk/' target='_blank'>https://envs.au.dk/</a>")),
                      p(HTML("Trafikdata er hentet fra Vejdirektoratet. For mere information, se venligst <a href='https://www.vejdirektoratet.dk/' target='_blank'>https://www.vejdirektoratet.dk/</a>")),
                      p(HTML("Vejrdata er hentet fra DMI. For mere information, se venligst <a href='https://www.dmi.dk/frie-data' target='_blank'>https://www.dmi.dk/frie-data</a>"))
               )
             )
           )
  ),
  
  # Tab 2 - undersøgelse af variabler
  tabPanel("Undersøgelse af variabler",
           fluidPage(
             h3("Variablers påvirkning på målinger af NOx på H.C. Andersens Boulevard"),
             
             # Sidebar med valg af dato og variable
             sidebarLayout(
               sidebarPanel(
                 dateRangeInput(
                   inputId = "date_range",
                   label = "Vælg dato:",
                   start = as.Date("2022-01-01"),
                   end = as.Date("2022-02-01"),
                   min = min(data_modelling$datehour),
                   max = max(data_modelling$datehour)
                 ),
                 radioButtons(
                   inputId = "line_to_show",
                   label = "Vælg forureningstyper",
                   choices = c("HCAB Nox" = "c_NOx_HCAB",
                               "Trafik" = "trafik",
                               "HCOE NOx" = "c_NOx_HCOE",
                               "Vindhastighed" = "speed",
                               "Luftfugtighed" = "hum",
                               "Temperatur" = "temp",
                               "Atmosfærisk Pres" = "pressure",
                               "Vindretning" = "dir_factor"),
                   selected = c("trafik")
                 ),
                 tags$style(HTML("
          h4 {
            font-weight: bold;
            font-size: 12px;
            margin-top: 20px;
          }
        ")),
                 h4("Statistik af valgte variable:"),
                 tableOutput("summary_stats")
               ),
               
               # Main Panel content
               mainPanel(
                 plotOutput("line_plot"),
                 plotOutput("relationship_plot")
               )
             )
           )
  ),
  
  # Tab 3 - Forudsigelse af NOx
  tabPanel("Forudsigelse af NOx",
           fluidPage(
             h3("Forudsigelse af NOx"),
             p("Forudsigelse af NOx baseret på en random forest machine learning model"),
             
             # Sidebar Layout for Forudsigelse af NOx tab
             sidebarLayout(
               sidebarPanel(
                 numericInput("input_hcoe", "NOx HCOE", value = 10, min = 0, max = 50),
                 numericInput("input_trafik", "Trafik", value = 5000, min = 1, max = 10000),
                 numericInput("input_vindhastighed", "Vindhastighed", value = 7, min = 0, max = 50),
                 numericInput("input_temperatur", "Temperatur", value = 10, min = 0, max = 50),
                 numericInput("input_luftfugtighed", "Luftfugtighed", value = 80, min = 0, max = 100),
                 numericInput("input_pres", "Atmosfærisk Pres", value = 1015, min = 950, max = 1100),
                 selectInput("input_vindretning", "Vindretning", choices = c("N", "NE", "E", "SE", "S", "SW", "W", "NW")),
                 
                 # Knap til at predicte
                 actionButton("predict_button", "Predict NOx")
               ),
               
               # Main panel
               mainPanel(
                 h4("NOx forudsigelse:"),
                 uiOutput("prediction"),
                 br(),
                 h4("Forventede trafik:"),
                 textOutput("avg_trafik_current_time"),
                 br(),
                 h4("Nuværende vejrdata for aktuelle time"),
                 textOutput("speedText"),
                 textOutput("tempText"),
                 textOutput("humText"),
                 textOutput("pressureText"),
                 textOutput("dirFactorText")
               )
             )
           )
  )
)




# Server Logic
server <- function(input, output) {
  # Ændrer df baseret på valgte dato
  filtered_data <- reactive({
    req(input$date_range)
    data_modelling[data_modelling$datehour >= input$date_range[1] & data_modelling$datehour <= input$date_range[2], ]
  })
  
  # Render plot
  output$line_plot <- renderPlot({
    req(input$line_to_show)
    chosen_variable <- input$line_to_show
    
    # Laver subtitle til plot på valgte datoer
    Sys.setlocale("LC_TIME", "da_DK.UTF-8")
    
    subtitle_text <- sprintf(
      "Timelig data fra %s til %s",
      format(input$date_range[1], "%d. %B %Y"),
      format(input$date_range[2], "%d. %B %Y")
    )
    
    # Base plot setup
    p <- ggplot(filtered_data(), aes(x = datehour)) +
      labs(title = "NOx målinger over tid",
           subtitle = subtitle_text,
           x = "", 
           y = chosen_variable) +
      scale_color_manual(
        values = c(
          "c_NOx_HCAB" = plot_color,
          "trafik" = plot_color,
          "c_NOx_HCOE" = plot_color,
          "speed" = plot_color,
          "hum" = plot_color,
          "temp" = plot_color,
          "pressure" = plot_color
        ),
        labels = c(
          "c_NOx_HCAB" = "HCAB Nox",
          "trafik" = "Trafik",
          "c_NOx_HCOE" = "HCOE NOx",
          "speed" = "Vindhastighed",
          "hum" = "Luftfugtighed",
          "temp" = "Temperatur",
          "pressure" = "Atmosfærisk Pres"
        )
      ) +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = background_color, color = background_color),
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(color = "#404040", size = 6),
        legend.title = element_blank(),
        text = element_text(family = "Tahoma"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank()
      )
    
    # Sætter valgte linje på plot
    if ("c_NOx_HCAB" %in% input$line_to_show) {
      p <- p + geom_line(aes(y = c_NOx_HCAB, color = "trafik"), linewidth = 0.7)
    }
    if ("trafik" %in% input$line_to_show) {
      p <- p + geom_line(aes(y = trafik, color = "trafik"), linewidth = 0.7)
    }
    if ("c_NOx_HCOE" %in% input$line_to_show) {
      p <- p + geom_line(aes(y = c_NOx_HCOE, color = "c_NOx_HCOE"), linewidth = 0.7)
    }
    if ("speed" %in% input$line_to_show) {
      p <- p + geom_line(aes(y = speed, color = "speed"), linewidth = 0.7)
    }
    if ("hum" %in% input$line_to_show) {
      p <- p + geom_line(aes(y = hum, color = "hum"), linewidth = 0.7)
    }
    if ("temp" %in% input$line_to_show) {
      p <- p + geom_line(aes(y = temp, color = "temp"), linewidth = 0.7)
    }
    if ("pressure" %in% input$line_to_show) {
      p <- p + geom_line(aes(y = pressure, color = "pressure"), linewidth = 0.7)
    }
    
    # Retuner plot
    p
  })
  
  # Laver lineær forhold plot mellem valgte variable og c_NOx_HCAB
  output$relationship_plot <- renderPlot({
    req(input$line_to_show)
    
    # Laver ny dataframe baseret på valgte variable
    chosen_variable <- input$line_to_show
    data_to_plot <- filtered_data()[, c("datehour", "c_NOx_HCAB", chosen_variable)]
    
    # Fjerner NA værdier for at være sikker
    data_to_plot <- na.omit(data_to_plot)
    
    # Laver linear regression mellem valgt variable og c_NOx_HCAB
    lm_model <- lm(c_NOx_HCAB ~ data_to_plot[[chosen_variable]], data = data_to_plot)
    
    # Finder koefficient fra lm model
    coefficient <- round(coef(lm_model)[2], 4)
    
    # Laver scatterplot med lineær regressionslinje
    p <- ggplot(data_to_plot, aes_string(x = chosen_variable, y = "c_NOx_HCAB")) +
      geom_point(color = plot_color) +
      geom_smooth(method = "lm", se = FALSE, color = line_color) +
      labs(
        title = paste("Lineær forhold mellem", chosen_variable, "og HCAB NOx"),
        x = chosen_variable,
        y = "HCAB NOx",
        caption = paste("Hældning:", coefficient)
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold"),
        text = element_text(family = "Tahoma")
      )
    
    # returnerer plot
    p
  })
  
  # Indlæser random forest model fra environment 
  rf_model <- readRDS("rf_model.rds")
  
  # Laver en reaktiv forudsigelse process - når brugeren trykker på knappen
  prediction <- eventReactive(input$predict_button, {
    
    # Laver dataframe med user inputs 
    wind_direction_levels <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
    
    input_data <- data.frame(c_NOx_HCOE = input$input_hcoe,
                             temp = input$input_temperatur,
                             trafik = input$input_trafik,
                             speed = input$input_vindhastighed,
                             hum = input$input_luftfugtighed,
                             pres = input$input_pres,
                             dir_factor = factor(input$input_vindretning, levels = wind_direction_levels))
    
    
    # Laver forudsigelsen
    pred <- predict(rf_model, newdata = input_data)
    return(pred)
  })
  
  output$prediction <- renderText({
    paste0(
      "Den forventede NOx på H. C. Andersens Boulevard er ",
      "<b>", round(prediction(), 2), "</b>"
    ) %>% HTML()
  })
  
  # Render statistik for valgte variable
  output$summary_stats <- renderTable({
    req(input$line_to_show)
    
    # Vælger den valgte variable
    chosen_variable <- input$line_to_show
    data <- filtered_data()[[chosen_variable]]
    
    # Udregner statistik
    stats <- data.frame(
      Stat = c("Gennemsnit", "Median", "Maksimum Værdi", "Minimum Værdi", "Standard Afvigelse"),
      Værdi = c(
        mean(data, na.rm = TRUE),
        median(data, na.rm = TRUE),
        max(data, na.rm = TRUE),
        min(data, na.rm = TRUE),
        sd(data, na.rm = TRUE)
      )
    )
    stats
  })
  
  # Beregner gennemsnitlige trafik på dagens dato og time
  output$avg_trafik_current_time <- renderText({
    current_time <- Sys.time()
    
    current_day <- day(current_time)
    current_month <- month(current_time)
    current_hour <- hour(current_time)
    
    current_day_month_hour <- paste0(
      sprintf("%02d", current_day), "_", 
      sprintf("%02d", current_month), "_", 
      sprintf("%02d", current_hour)
    )
    
    avg_trafik_value <- avg_traffic %>%
      filter(day_month_hour == current_day_month_hour) %>%
      pull(avg_trafik) %>%
      round(0)
    
    paste0("Forventede antal biler for den aktuelle time: ", avg_trafik_value)
  })
  
  # Får vejrdata
  fetch_weather_data <- reactive({
    # Konfiguration
    api_key <- "&api-key=168a5a3d-61a1-43ab-9c01-4a758c3d65e6"
    base_url <- "https://dmigw.govcloud.dk/v2/metObs/collections/observation/items"
    station_id <- "06180"
    limit <- "1"
    parameters <- c("wind_speed_past1h", "wind_dir_past1h", 
                    "humidity_past1h", "temp_mean_past1h", 
                    "pressure_at_sea")
    
    # lav datetime-query
    current_time <- Sys.time()
    min_date <- floor_date(current_time, unit = "hour")
    date_query <- paste0("&datetime=", 
                         format(min_date, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"), "/", 
                         format(current_time, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"))
    
    # Funktion til at hente og behandle data for et parameter
    fetch_parameter_data <- function(parameter) {
      query <- paste0("?limit=", limit, "&stationId=", station_id, date_query, 
                      "&parameterId=", parameter, api_key)
      url <- paste0(base_url, query)
      response <- GET(url)
      data <- fromJSON(content(response, as = "text"))[["features"]][["properties"]]
      data.frame(data)
    }
    
    # Hent data for alle parametre
    data_list <- lapply(parameters, fetch_parameter_data)
    names(data_list) <- c("speed", "dir", "hum", "temp", "pressure")
    
    # Kombiner data og tilføj vindfaktor
    weather_data <- do.call(cbind, lapply(data_list, function(df) df[5]))  # Vælger 5. kolonne
    colnames(weather_data) <- names(data_list)
    weather_data$dir_factor <- cut(
      as.numeric(weather_data$dir),
      breaks = c(-22.5, 22.5, 67.5, 112.5, 157.5, 202.5, 247.5, 292.5, 337.5, 360),
      labels = c("Nord", "Nordøst", "Øst", "Sydøst", "Syd", "Sydvest", "Vest", "Nordvest", "Nord"),
      include.lowest = TRUE
    )
    
    return(weather_data)
  })
  
  # Render text for hver vejr variable
  output$speedText <- renderText({
    weather_data <- fetch_weather_data()
    paste("Vindhastighed: ", weather_data$speed, "km/h")
  })
  
  output$dirFactorText <- renderText({
    weather_data <- fetch_weather_data()
    paste("Vindretning: ", weather_data$dir_factor)
  })
  
  output$humText <- renderText({
    weather_data <- fetch_weather_data()
    paste("Luftfugtighed: ", weather_data$hum, "%")
  })
  
  output$tempText <- renderText({
    weather_data <- fetch_weather_data()
    paste("Temperatur: ", weather_data$temp, "°C")
  })
  
  output$pressureText <- renderText({
    weather_data <- fetch_weather_data()
    paste("Atmosfærisk Tryk: ", weather_data$pressure, "hPa")
  })
}
  
  
  



# Run the application 
shinyApp(ui = ui, server = server)

### Evt. prøve at estimere NOx i dag i app?
