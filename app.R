# INFO 201 Final App
library("shiny")
library("plyr")
library("dplyr")
library("ggplot2")
library("tidyr")
library("maps")
library("DT")
library("XML") 
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("RCurl")

# Gives us access to the data and analysis from a6
source("analysis_a6.R")

page_one <- tabPanel(
  h5("Overview"),
  fluidRow(
    column(12,
      column(6,
        titlePanel(h1("Trump's Tweets and His Approval Rating During 2017", style = "color: #800000; font-weight: bold;")),
        p("by Roshni Sinha, Michelle Ponting, Andy Straavaldson and John Reese -- Group AA2"),
        br(),
        p("Donald Trump often takes to Twitter to share his thoughts with the American public. As president of the United States, his duties and the perception of the American public are often intertwined. Twitter, a social media platform used to share short posts, acts as a means of communication between Trump and both his supporters and rivals, allowing others an insight to his mind and life in the Oval Office. In this assignment, we will analyze the effect Trump's tweets had on his approval rating in 2017. By comparing these two sets of data, we can gain broader knowledge of how social media is used in politics.")),
      column(6, 
        imageOutput("trumpImage")
      )
    )
  )
)

page_three <- tabPanel(
  h5("Tweets With Trending Key Words"),
  titlePanel("Most Commonly Used Words By Trump In His Tweets"),
    sidebarLayout(
      sidebarPanel(
        textInput(
          inputId = "wordcloudinput",
          label = "Trump mentioned:",
          value = "great"
        ),
        br(),
        p("Number of times tweeted:"),
        textOutput("wordcount"),
        br(),
        p("This will show which topics trump spends the most time tweeting about and mentioning,
          will show which issues take priority on social media for him. The larger a word is on the word
          cloud, the more often he uses one of those phrases. You can inmput any word into the widget
          and it will report how many times that word was used in 2017. The word cloud can also show 
          how much he tweets in line with current events and trends in the political world.")
      ),
      mainPanel(
        imageOutput("wordcloud")
      )
   )
)

page_two <- tabPanel(
  h5("Twitter Activity Analysis"),
  titlePanel("Analysis of President Trump's Twitter Activity in 2017"),
  fluidRow(
    column(12, 
      sidebarLayout(
        sidebarPanel(
          sliderInput(
            inputId = "month",
            label = "Month",
            min = 1,
            max = 12,
            value = 1,
            ticks = FALSE
          ),
          h2(textOutput("monthname")),
          p(textOutput("twitterdata")),
          br(),
          p("In 2017, he posted 2,602 tweets total, including retweets, and 2,417 that were not retweets.")
        ),
        mainPanel(
          plotOutput("plotmontlytweets")
        )
      )
    ),
    column(12, 
           tableOutput("tablemonthlytweets")
    )
  )
  
)

page_four <- tabPanel(
  h5("Approval Rating and Tweeting Frequency"), # Label for the tab in the navbar
  titlePanel("How Trump's Approval Rating Affected How Often He Tweeted"),
  sidebarLayout(
    sidebarPanel(
      radioButtons(inputId = "approve", label = "Metric",choices = c("approve","disapprove")),
      textOutput(outputId = "corr"),
      br(),
      p("As can be seen, there exists a weak negative correlation between both Trump's approval 
        rating and tweeting frequency and a weak positive correlation between Trump's disapproval 
        rating and his tweeting frequency. Interestingly, the former correlation is stronger than the latter. 
        Perhaps this indicates that Trump pays more attention to his approval ratings than his dissaproval 
        ratings.")
    ),
    mainPanel(
      plotOutput(outputId = "frequency_plot"),
      plotOutput(outputId = "date_plot"),
      p(""),
      p("- The frequency is the number of times that Donald Trump tweeted in a day"),
      p("- The approval rating is average of all approval ratings released in a day.")
    )
  )
)

page_five <- tabPanel(
  h5("Approval Rating and Tweet Content"),
  titlePanel("Tracking Trump's Tweets"),
  
  sidebarLayout(
    
    sidebarPanel(
      textInput(
        inputId = "query",
        label = "Trump mentioned:",
        value = "covfefe"
      ),
    p("This page allows the user to search any term Trump mentions and view his approval rating at the time he said it. Only approval rating from Gallup are plotted to allow for consistency. Trump's average approval rating was 38.5%, denoted by the black horizontal line. Some common terms mentioned by Trump are 'Russia' and 'Democrats'. As a general trend, Trump's approval rating was either already declining when he tweeted about Russia or declined soon after. Queries are case-sensitive.")
    ),
    
    mainPanel(
      plotOutput("plot_query"),
      tableOutput("searchquery")
    )
    
  )
)

page_six <- tabPanel(
  h5("References"),
  titlePanel("Our Data"),
  p("The data about Donald Trump's approval rating comes from FiveThirtyEight, a website that has 
    statistics on various topics, including politics and sports. This data can be found using the
    following", a("link.", href = "https://projects.fivethirtyeight.com/trump-approval-ratings/")),
  p("The data about Donald Trump's tweets comes from GitHub and was posted by user bpb27, who has posted 
    a number of datasets relating to politics and social media. Our data was retrieved from this",
    a("link.", href = "https://github.com/bpb27/trump-tweet-archive/blob/master/data/realdonaldtrump/2017.json"))
)

my_ui <- navbarPage(
  theme = "stylesheet.css",
  h4("Tweets of Approval", style = "color: #FFFFFF; font-weight: bold;"),
  page_one,
  page_two,
  page_three,
  page_four,
  page_five,
  page_six,
  inverse = TRUE
)

my_server <- function(input,output) {
  # Overview
  
  output$trumpImage <- renderImage({
    list(src = "www/donald_trump.jpg",
         contentType = 'image/png',
         alt = "Donald Trump Image",
         width = "600", 
         height = "auto")
  }, deleteFile = FALSE)
  
  # John's Q
  
  output$frequency_plot <- renderPlot({
    ggplot(data = daily_approval_and_frequency,mapping = aes_string(x = "Frequency", y = input$approve)) +
      geom_point() +
      geom_smooth(method = "lm")
  })
  
  output$date_plot <- renderPlot({
    apr <- toString(input$approve)
    ggplot(data = daily_approval_and_frequency)+
      geom_point(mapping = aes_string(x = "Date", y = apr, size = "Frequency", color = "Frequency")) +
      scale_color_gradient2(mid = "yellow",high="blue", space ="Lab" )
  })
  output$corr <- renderText({
    tex <- ""
    if (input$approve == "approve") {
      x <- as.numeric(daily_approval_and_frequency$approve)
      y <- as.numeric(daily_approval_and_frequency$Frequency)
      freq_cor <- round(cor(x,y),2)
      tex <- paste("The correlation between approval rating and frequency is", paste0(freq_cor, "."))
    } else {
      x <- as.numeric(daily_approval_and_frequency$disapprove)
      y <- as.numeric(daily_approval_and_frequency$Frequency)
      freq_cor <- round(cor(x,y),2)
      tex <- paste("The correlation between disapproval rating and frequency is", paste0(freq_cor, "."))
    }
    tex
  })
  
  # Michelle Q data
  output$plotmontlytweets <- renderPlot({
    data <- twitter_data

    plot.data <- data %>% filter(month == input$month)
    
    ggplot(data = plot.data) +
      geom_point(mapping = aes(x = day, y = time, color = is_retweet)) + 
      scale_y_discrete(breaks = c("00:00:00", "24:00:00")) +
      labs(
        title = paste("All of Donald Trump's Tweets in", month.name[input$month]),
        y = "Time of Day",
        x = "Day of the Month",
        color = "Is it a Retweet?"
      )
  })
  
  output$twitterdata <- renderText({
    twitter_info <- get_monthly_info(input$month)
    twitter_info
  })
  
  output$monthname <- renderText({
    month <- month.name[input$month]
    month
  })
  
  output$tablemonthlytweets <- renderTable({
    table <- monthly_table
    table
  })
  
  #Roshni's data
  
  output$searchquery <- renderTable({
    
    if (input$query == input$query) {
      plot.data <- query_table(input$query) %>% 
        mutate (
          "Text" = text,
          "Retweet Count" = retweet_count,
          "Favorite Count" = favorite_count
        ) %>% 
        select (
          "Text", "Retweet Count", "Favorite Count", "Date"
        )
    } else {
      plot.data <- trump_tweets_date
    }
  })
  
  output$plot_query <- renderPlot({
    if (input$query == input$query) {
      plot.data <- trump_approval_filtered 
    }
    
    ggplot(trump_approval_filtered, aes(x = enddate, y = approve)) + 
      geom_line(group = 1, color = "blue") + geom_hline(yintercept = 38.5, color = "black") +
      geom_vline(xintercept = plotquery(input$query), color = "red") +
      labs(
        x = "Date",
        y = "Approval Rating"
      )
    
  })
  
  output$wordcloud <- renderImage({
    list(src = "www/Trump_Plot.png",
         contentType = 'image/png',
         alt = "Word Cloud of commonly used words in Donald Trump's Tweets",
         width = "800", 
         height = "auto")
  }, deleteFile = FALSE)
  
  output$wordcount <- renderText({
    if (input$wordcloudinput == input$wordcloudinput) {
      count <- word_frequency(input$wordcloudinput)
    } else {
      count <- "Word not found!"
    }
    as.String(count)
  })
}

shinyApp(ui = my_ui, server = my_server)