rm(list=ls())
library(shiny)
# use the below options code if you wish to increase the file input limit, in this example file input limit is increased from 5MB to 9MB
options(shiny.maxRequestSize = 15*1024^2)

shinyServer(function(input,output){
  
  # This reactive function will take the inputs from UI.R and use them for read.table() to read the data from the file. It returns the dataset in the form of a dataframe.
  # file$datapath -> gives the path of the file
  data <- reactive({
    file1 <- input$file
    if(is.null(file1)){return()} 
    read.table(file=file1$datapath, sep=input$sep)
    
  })
  
  # this reactive output contains the summary of the dataset and display the summary in table format
  output$filedf <- renderTable({
    if(is.null(data())){return ()}
    input$file
  })
  
  
  
  # This reactive output contains the dataset and display the dataset in table format
  output$table <- renderTable({
    if(is.null(data())){return ()}
    df=data()
    df=t(df)
    colnames(df)[1] = "comments"
    id=1:nrow(df)
    d1=data.frame(id)
    df=cbind(d1,df)
    df
  })
  
  output$tdm <- renderTable({
    if(is.null(data())){return ()}
    library(stringr)
    library(tm)
    library(wordcloud)
    library(slam)
    #library(sentiment)
    
    #Load comments/text
    #post = read.csv("C:/Users/DEEKSHA/Desktop/edwisor/PracticeData/Post.csv", header = T)
    #Select only text column
    #post = data.frame(post[1:1000,2])
    #names(post) = "comments"
    df=data()
    df=t(df)
    colnames(df)[1] = "comments"
    a=1:nrow(df)
    d1=data.frame(a)
    df=cbind(d1,df)
    post=df
    post$comments = as.character(post$comments)
    ##Pre-processing
    #convert comments into corpus
    postCorpus = Corpus(VectorSource(post$comments))
    writeLines(as.character(postCorpus[[2]]))
    #postCorpus = tm_map(postCorpus, PlainTextDocument)
    
    #case folding
    postCorpus = tm_map(postCorpus, tolower)
    
    #remove stop words
    postCorpus = tm_map(postCorpus, removeWords, stopwords('english'))
    
    #remove punctuation marks
    postCorpus = tm_map(postCorpus, removePunctuation)
    
    #remove num bers
    postCorpus = tm_map(postCorpus, removeNumbers)
    
    #remove unnecesary spaces
    postCorpus = tm_map(postCorpus, stripWhitespace)
    
    #convert into plain text
    postCorpus = tm_map(postCorpus, PlainTextDocument)
    
    #create corpus
    postCorpus = Corpus(VectorSource(postCorpus))
    tdm = TermDocumentMatrix(postCorpus)
    #calculate the terms frequency
    words_freq = rollup(tdm, 2, na.rm=TRUE, FUN = sum)
    words_freq = as.matrix(words_freq)
    words_freq = data.frame(words_freq)
    words_freq$words = row.names(words_freq)
    row.names(words_freq) = NULL
    words_freq = words_freq[,c(2,1)]
    names(words_freq) = c("Words", "Frequency")
    words_freq
  })
  
  ###wordcloud calculation
  output$word <- renderPlot({
    #load libraries
    library(stringr)
    library(tm)
    library(wordcloud)
    library(slam)
    #library(sentiment)
    
    #Load comments/text
    #post = read.csv("C:/Users/DEEKSHA/Desktop/edwisor/PracticeData/Post.csv", header = T)
    #Select only text column
    #post = data.frame(post[1:1000,2])
    #names(post) = "comments"
    df=data()
    df=t(df)
    colnames(df)[1] = "comments"
    a=1:nrow(df)
    d1=data.frame(a)
    df=cbind(d1,df)
    post=df
    post$comments = as.character(post$comments)
    ##Pre-processing
    #convert comments into corpus
    postCorpus = Corpus(VectorSource(post$comments))
    writeLines(as.character(postCorpus[[2]]))
    #postCorpus = tm_map(postCorpus, PlainTextDocument)
    
    #case folding
    postCorpus = tm_map(postCorpus, tolower)
    
    #remove stop words
    postCorpus = tm_map(postCorpus, removeWords, stopwords('english'))
    
    #remove punctuation marks
    postCorpus = tm_map(postCorpus, removePunctuation)
    
    #remove num bers
    postCorpus = tm_map(postCorpus, removeNumbers)
    
    #remove unnecesary spaces
    postCorpus = tm_map(postCorpus, stripWhitespace)
    
    #convert into plain text
    postCorpus = tm_map(postCorpus, PlainTextDocument)
    
    #create corpus
    postCorpus = Corpus(VectorSource(postCorpus))
    
    #Word cloud
    wordcloud(postCorpus, max.words = 200, scale=c(3, .1), colors=brewer.pal(6, "Dark2"))
    #Remove the defined stop words
  })
  
  ###bar plot
  output$barPlot1 <- renderPlot({
    library(RSentiment)
    #german = read.csv("C:/Users/DEEKSHA/Desktop/edwisor/PracticeData/German.csv", header=T)
    df1=data()
    df1=t(df1)
    colnames(df1)[1] = "comments"
    a=1:nrow(df1)
    d1=data.frame(a)
    df1=cbind(d1,df1)
    post=df1
    #post$comments = as.character(post$comments)
    post = post[complete.cases(post),]
    df = calculate_sentiment(post$comments)
    
    colnames(df)[1] = "comments"
    id=c(1:nrow(df))
    d2=data.frame(id)
    df=cbind(id,df)
         
    library("ggplot2")# to plot graphs
    library("scales")#to set x and y axis limit
    library("psych")###to adjust the fonts
    library("gplots")###for color adjustment 
    ggplot(df, aes_string(x = df$sentiment, y = df$id)) +
      geom_bar(stat="identity",fill= "DarkSlateBlue") + theme_bw() +
      xlab("comments") + scale_y_continuous(breaks=pretty_breaks(n=10)) +
      ggtitle("Sentiment analysis") +  theme(text=element_text(size=15))
    # generate bins based on input$bins from ui.R
    
    
  })
  
  
  output$word_posi <- renderTable({
    #load libraries
    library(stringr)
    library(tm)
    library(wordcloud)
    library(slam)
    library(RSentiment)
    #german = read.csv("C:/Users/DEEKSHA/Desktop/edwisor/PracticeData/German.csv", header=T)
    df1=data()
    df1=t(df1)
    colnames(df1)[1] = "comments"
    a=1:nrow(df1)
    d1=data.frame(a)
    df1=cbind(d1,df1)
    post=df1
    #post$comments = as.character(post$comments)
    post = post[complete.cases(post),]
    df = calculate_sentiment(post$comments)
    
    newdocs = df
    
    #separate the comments based on polarity
    newdocs_positive = newdocs[which(newdocs$sentiment == "Very Positive"),]
    colnames(newdocs_positive)[1] = "Positive comments"
    newdocs_positive
  })
  
  output$word_negi <- renderTable({
    #load libraries
    library(stringr)
    library(tm)
    library(wordcloud)
    library(slam)
    library(RSentiment)
    #german = read.csv("C:/Users/DEEKSHA/Desktop/edwisor/PracticeData/German.csv", header=T)
    df1=data()
    df1=t(df1)
    colnames(df1)[1] = "comments"
    a=1:nrow(df1)
    d1=data.frame(a)
    df1=cbind(d1,df1)
    post=df1
    #post$comments = as.character(post$comments)
    post = post[complete.cases(post),]
    df = calculate_sentiment(post$comments)
    
    newdocs = df
    
    #separate the comments based on polarity
    newdocs_negative = newdocs[which(newdocs$sentiment == "Negative"),]
    colnames(newdocs_negative)[1] = "Negative comments"
    newdocs_negative
  })
  ################################################
  
  
  # the following renderUI is used to dynamically generate the tabsets when the file is loaded. Until the file is loaded, app will not show the tabset.
  output$tb <- renderUI({
    if(is.null(data()))
      #h1("Text Mining And Sentiment Analyser")
      h5(tags$img(src='22.png', heigth=250, width=200), tags$img(src='24.jpg', heigth=250, width=300) , tags$img(src='21.png', heigth=250, width=300), tags$img(src='12.jpg', heigth=250, width=500), tags$img(src='img13.jpg', heigth=150, width=300))
    else
      tabsetPanel(tabPanel("About file", tableOutput("filedf")),tabPanel("Data", tableOutput("table")), tabPanel("Word Frequency", tableOutput("tdm")), tabPanel("wordCloud", plotOutput("word")), tabPanel("Sentiment Analysis", plotOutput("barPlot1")), tabPanel("Positive Views", tableOutput("word_posi")), tabPanel("Negative Views", tableOutput("word_negi")))
  })
})