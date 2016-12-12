#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
# Define UI for application that draws a histogram
ui <- navbarPage(
  title = "Twitter Mining",
  navbarMenu(
    title = "Map",
    tabPanel("United States",
             div("We started to mine 7000 tweets all over the United States"),
             div("Each darkblue point represent each tweet"),
             div("We found that there are some cluster of tweets around Los Angeles, Chicago and Boston, so we would like to pick up these specific locations to process deeper investifation"),
             splitLayout(img(src="us.png", height="299",width="684"))),
    tabPanel("Los Angeles",
             div("This map shows all Tweets that we mined around Los Angeles(the range box of latitude and longitude is (-120,32,-117,35))"),
             hr(),
             splitLayout(img(src="LA.png",height = "500",width="600"))),
    tabPanel("Chicago", 
             div("This map shows all Tweets that we mined around Chicago (the range box of latitude and longitude is (-88,41,-87,42))"),
             hr(),
             splitLayout(img(src="chicago.png",height = "576",width="712"))),
    tabPanel("Boston",
             div("This map shows all Tweets that we mined around Boston (the range box of latitude and longitude is (-72,41,-70,43))",
                 splitLayout(img(src="boston.png",height="585",width="671"))))
  ),
  
  navbarMenu(
    title = "Distribution(number of tweets vs. Hour)",
    tabPanel("Los Angeles",
             div("This graph show that numbers of tweets based on different period of time"),
            
             div("The y-axis of the histogram is numbers of tweets, and the x-axis of the histogram is"),
             div("we can see that most tweets are clustered at 18:00PM"),
             splitLayout(img(src="hourLA.png",height = "500",width="600"))),
    tabPanel("Chicago", 
             div("This graph show that numbers of tweets based on different period of time"),
             
             div("The y-axis of the histogram is numbers of tweets, and the x-axis of the histogram is"),
             div("Compared to the graph of Los Angeles, more tweets after 18:00PM"),
             splitLayout(img(src="hourC.png",height = "500",width="600"))),
    tabPanel("Boston",
             div("Although we mined the same number of tweets in Los Angeles, Chicago and Boston, the range of time is wider in Boston",
                 splitLayout(img(src="hourDB.png",height="600",width="600"))))
    ),
  
  navbarMenu(
    title = "Distribution(words vs. tweet) ",
    
    tabPanel("Los Angeles",
             div("This graph show that the number of word per tweet"),
             
  
           
             splitLayout(img(src="wordDLA.png",height = "500",width="600"))),
    tabPanel("Chicago", 
             div("This graph show that the number of word per tweet"),
             
            
             splitLayout(img(src="wordDC.png",height = "500",width="600"))),
    tabPanel("Boston",
             div("This graph show that the number of word per tweet",
                 splitLayout(img(src="wordB.png",height="600",width="600"))))
  ),
  
  
 
  navbarMenu(
    title = "Polarity",
    tabPanel("Los Angeles",
             div("According to this graph, we can see that there are approximate 2500 users hold neutral attribute to Trump, a litte bit less than 3500 users hold positive attribute to Trump and a little bit more than 1000 users hold negative attributes to Trump among 7000 Tweets that we mine"),
             
             
             
             splitLayout(img(src="polarityLA.png",height = "500",width="600"))),
    tabPanel("Chicago", 
             div("According to this graph, we can see that there are a little bit more than 3000 users hold neutral attribute to Trump, approximate 2600 users hold positive attribute to Trump and a little bit more than 1000 users hold negative attributes to Trump among 7000 Tweets that we mine"),
             
             splitLayout(img(src="polarityDC.png",height = "500",width="600"))),
    tabPanel("Boston",
             div("According to this graph, we can see that there are approximate 1500 users hold neutral attribute to Trump, more than 3500 users hold positive attribute to Trump and 1500 users hold negative attributes to Trump among 7000 Tweets that we mine"),
             
             splitLayout(img(src="polarityB.png",height="600",width="600")))
  ),
  
  navbarMenu(
    title = "Word Cloud",
    tabPanel("Los Angeles",
             splitLayout(img(src="wordcloudLA.png",height="397",width="428"))),
    tabPanel("Chicago",
             splitLayout(img(src="wordcloudC.png",height="349",width="438"))),
    tabPanel("Boston",
             splitLayout(img(src="wordcloudB2.png",height="438",width="476"))),
    tabPanel("Comparison Cloud",
             div("This is the comparion Wordcloud of three cities"),
             div("According to this graph, we can see the frequency of words in tweets(the bigger size of the word, the more times that the word was mentioned in tweets)"),
             splitLayout(img(src="compare.png", height="600",width="1300")))
             
  )
  
  
  
  
)


server <- function(session,input,output){
  
  
}
# Run the application 
shinyApp(ui = ui, server = server)

