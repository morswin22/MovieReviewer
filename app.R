library(shiny)
library(shinydashboard)

library(ggplot2)
library(plotly)
library(dplyr)
library(gridExtra)
library(cowplot)
library(stringr)
library(tidyr)
library(grid)
library(lorem)
library(ggsci)

movies <- read.csv2("./imdb-movies-dataset.csv", sep = ",", dec = ".")
movies <- movies[1:1000, ]
flat_movies <- movies %>%
  separate_rows(Genre, sep = ",")

library(DT)
prettyTable <- function(table_df, round_columns_func=is.numeric, round_digits=2) {
  DT::datatable(table_df, style="bootstrap", escape = FALSE, filter = "top", rownames = FALSE, extensions = "Buttons", options = list(dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))) %>%
    formatRound(unlist(lapply(table_df, round_columns_func)), round_digits)
}

genres = flat_movies$Genre %>% unique()
directors = flat_movies$Director %>% na.omit %>% unique()

ui <- dashboardPage(
  dashboardHeader(title = "Movie Reviewer",
                  tags$li(a(href = 'https://www.put.poznan.pl/',
                            img(src = 'logo.png', title = "PUT", height = "52px"),
                            tags$style(".main-header {max-height: 72px}"),
                            tags$style(".main-header .logo {height: 72px; line-height: 72px}"),
                            tags$style(".sidebar-toggle {height: 72px; line-height:72px; padding-top: 0px !important;}"),
                            tags$style(".navbar {min-height:72px !important}"),
                            style = "padding-top:10px; padding-bottom:10px;"),
                          class = "dropdown")),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Datatable", tabName = "page1", icon = icon("dashboard")),
      menuItem("Range of Years", tabName = "page2", icon = icon("th")),
      menuItem("Genres selection", tabName = "page3", icon = icon("dashboard")),
      menuItem("Interactive", tabName = "page4", icon = icon("th")),
      menuItem("Directors", tabName = "page5", icon = icon("dashboard")),
      menuItem("About", tabName = "about", icon = icon("info"))
    ),
    tags$style(".left-side, .main-sidebar {padding-top: 72px}")
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "page1",
        fluidRow(
          box(title = "Datatable", status="primary", width=12,
            prettyTable(movies %>% select(-c(Review, Review.Title)) %>% mutate(Poster = paste('<img src="', Poster, '" style="width: 100%;" />')))
          ),
          tags$style("#shiny-tab-page1 .box-body {overflow-x: scroll;}")
        )
      ),
      tabItem(tabName = "page2",
        fluidRow(
          box(title = "Plot", status="primary", plotOutput("plot2", height = 250)),
          box(title = "Inputs", status="warning",
            sliderInput('years', label = 'Range of Years', step=1, min=min(flat_movies$Year), max = max(flat_movies$Year), value = c(min(flat_movies$Year),max(flat_movies$Year)))
          )
        )
      ),
      tabItem(tabName = "page3",
        fluidRow(
          box(title = "Plot", status="primary", plotOutput("plot3", height = 250)),
          box(title = "Inputs", status="warning",
              selectInput("genres", label = "Selected genres", choices = genres, selected = genres %>% sample(5), multiple = TRUE)
          )
        )
      ),
      tabItem(tabName = "page4",
        fluidRow(
          ggplotly(ggplot(
            flat_movies[sample(nrow(flat_movies), 1000), ],
            aes(x = Year, y = Rating, color = Genre)
          ) +
            geom_point(alpha = 0.5) +
            scale_color_jco() +
            theme_bw() +
            theme(legend.title = element_blank()))
        )
      ),
      tabItem(tabName = "page5",
          fluidRow(
            fluidRow(
              box(title = "Director genres", status="primary", 
                  plotOutput("plot_directors_genres", height = 250)),
              box(title = "Director", status="info",
                  selectInput("single_director_page4", 
                              label = "Select a Director", choices = directors, 
                              selected = directors[1]), height = 250)
            ),
            fluidRow(
              box(title = "Director ratings", status="primary", 
                  plotOutput("plot_director_ratings", height = 250))
            )
            
          )
      ),
      tabItem(tabName = "about",
        fluidRow(
          box(title = "About dataset", status="primary",
            a(href="https://www.kaggle.com/datasets/amanbarthwal/imdb-movies-data", "IMDB Movies Dataset"),
            p("The IMDb Movies Dataset is one of the most extensive collections of movie-related data available. It includes a wide range of attributes such as titles, genres, cast and crew, release dates, ratings, and reviews."),
            tags$ul(
              tags$li("Poster: link of the movie poster."),
              tags$li("Title: name of the movie."),
              tags$li("Year: year the movie was released."),
              tags$li("Certificate: age rating given to the movie (e.g., PG, R)."),
              tags$li("Duration (min): length of the movie in minutes."),
              tags$li("Genre: genre(s) of the movie (e.g., Action, Comedy, Drama)."),
              tags$li("Rating: IMDB user rating for the movie."),
              tags$li("Metascore: score from critics."),
              tags$li("Director: director(s) of the movie."),
              tags$li("Cast: main actors in the movie."),
              tags$li("Votes: number of votes the movie received on IMDB."),
              tags$li("Description: brief summary of the movie's plot."),
              tags$li("Review Count: total count of reviews."),
              tags$li("Review Title: title of the review."),
              tags$li("Review: movie review (review with highest votes on it).")
            ),
            p("This richness allows for in-depth analysis and insights into the film industry and audience preferences. It may be used in various scenarios - for example in recommendation system.")
          ),
          box(title = "About authors",
              tags$ul(
                tags$li("Bernart Mateusz 156072"),
                tags$li("Janiak Patryk 156053")
              )
          ),
          box(title = "Motivation",
              p("We want to get a nice grade 😊")
          )
        )
      )
    )
  )
)


server <- function(input, output) {
  set.seed(156053)
  
  output$plot2 <- renderPlot({
    years = reactive(input$years)
    
    filtered = flat_movies %>% filter(years()[1] < Year & Year < years()[2])
    
    ggplot(
      filtered,
      aes(x = Year, y = Rating, color = Genre)
    ) +
      geom_point(alpha = 0.5) +
      xlim(min(flat_movies$Year), max(flat_movies$Year)) + 
      scale_color_jco() +
      theme_bw() +
      theme(legend.title = element_blank())
  }, res = 96)
  
  output$plot3 <- renderPlot({
    genres = reactive(input$genres)
    
    filtered = flat_movies %>% filter(Genre %in% genres())
    
    ggplot(
      filtered,
      aes(x = Year, y = Rating, color = Genre)
    ) +
      geom_point(alpha = 0.5) +
      xlim(min(flat_movies$Year), max(flat_movies$Year)) + 
      scale_color_jco() +
      theme_bw() +
      theme(legend.title = element_blank())
  }, res = 96)
  
  output$plot_directors_genres <- renderPlot({
    director <- reactive(input$single_director_page4)
    
    filtered <- flat_movies %>% filter(Director == director())

    genre_counts <- table(filtered$Genre)
    
    sorted_genres <- sort(genre_counts, decreasing = TRUE)
    
    top_genres <- names(sorted_genres)[1:5]
    
    other_count <- sum(sorted_genres[-(1:5)])
    
    pie_df <- data.frame(
      Genre = c(top_genres, "Other"),
      Count = c(sorted_genres[1:5], other_count)
    )
    pie_df <- na.omit(pie_df) %>% filter(Count != 0)

    ggplot(pie_df, aes(x = Genre, y = Count, fill = Genre,
                       text = paste(Genre, ": ", Count))) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = paste("Genres Distribution for", director()),
           x = "Gerne",
           y = "Count") +
      theme(legend.position="none")
  }, res = 96)
  
  output$plot_director_ratings <- renderPlot({
    director <- reactive(input$single_director_page4)
    
    filtered <- flat_movies %>% 
      filter(Director == director() & !is.na(Rating)) %>%
      mutate(Ratings = round(Rating, 0)) %>%
      filter(!is.na(Ratings) & !is.na(Ratings))
    
    genre_counts <- as.data.frame(table(filtered$Ratings))
    
    pie_df <- data.frame(
      Rating = genre_counts$Var1,
      Count = genre_counts$Freq
    )
    
    pie_df <- pie_df %>% filter(Count != 0)
    
    custom_palette <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", 
                        "#6a3d9a", "#a6cee3")
    
    ggplot(pie_df, aes(x = Rating, y = Count, fill = Rating, 
                                    text = paste(Rating, ": ", Count))) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = custom_palette) + 
      theme_minimal() +
      labs(title = paste("Ratings Distribution for", director())) +
      theme(legend.position = "none")
  }, res = 96)
  
}

shinyApp(ui, server)