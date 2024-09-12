server <- function(input, output, session) {
  
  if(require("pacman")==F) install.packages("pacman")
  
  p_load(tidyverse,doParallel, 
         jiebaR, tmcn, tm, widyr,
         ggraph,igraph, 
         ldatuning, topicmodels,
         tidytext,ggwordcloud)
  
  options(shiny.maxRequestSize = 100 *1024^2)   
  
  dtm <- reactive(
    {
      d <- input$"Dictionary"$datapath
      s <- input$"Stop_words"$datapath
      t <- input$"Text"$datapath
      
      validate(
        need(d!="", "Select dictionaries"),
        need(s!="", "Select stop words"),
        need(t!="", "Select text")
      )
      
      d %>% map(read.csv)%>%unlist()%>%write.csv("Dictionaries.csv", row.names = F)
      s %>% map(read.csv)%>%unlist()%>%write.csv("Stopwords.csv", row.names = F)
      
      w <- worker(user = "Dictionaries.csv", stop_word = "Stopwords.csv")
      
      t %>% map( read.table )%>% do.call(rbind,.) %>% apply(., 1, \(x) segment(x, w)) %>%
        createDTM() %>% removeSparseTerms(., .99) %>% .[apply(.,1,sum)>0,]
    }
  )
  
  output$"getwd_out"<-renderPrint(
    {
      paste("Working directory:", getwd())
    }
  )
  
  Triplets <- function(x) {
    df <- data.frame(x$i, x$j, x$v)
    df2 <- x$dimnames$Terms %>% data.frame(x = ., id = 1:length(.))
    left_join(df, df2, by = c("x.j" = "id"))
  }
  
  corr_plot<-reactive(
    {
      set.seed(1)
      p <- dtm() %>% Triplets() %>% pairwise_cor(., x, x.i, sort=T, upper=F) %>% 
        filter(correlation > .4) %>%graph_from_data_frame() %>%ggraph(layout ='fr')
      
      p+geom_edge_link(aes(edge_alpha = correlation), edge_colour = "darkblue") +
        scale_edge_alpha(name = "Corr") + geom_node_point() +
        geom_node_text(aes(label = name), repel = T) + theme_void()
    }
  )
  
  output$"corr_plot_out" <-renderPlot(
    {
      corr_plot ()
    }
  )
  
  output$"corr_plot_DL" <- downloadHandler(
    filename = function() paste("corr_plot", ".png", sep = "") ,
    content = function(x) ggsave(x, corr_plot(), width = 8, height = 8)
  )
  
  lda_tuning <- reactive(
    {
      registerDoParallel(cl<-makeCluster(6))
      ftn<-dtm() %>% FindTopicsNumber(., 2:10, "Griffiths2004", "Gibbs", list(seed = 1), cl) 
      stopCluster(cl)
      ggplot(ftn, aes(topics, Griffiths2004)) +geom_line()
    }
  )
  
  output$"LDA_tuning"<- renderPlot(
    {  
      lda_tuning()
    }
  )
  
  output$"download_LDA_tuning" <- downloadHandler(
    filename = function() paste("LDA tuning plot", ".png", sep = ""),
    content = function(x) ggsave(x, lda_tuning(), width = 8,height = 8)
  )
  
  my_word_clouds<-reactive(
    {
      p<-dtm() %>% LDA(., input$"Number_of_topics","VEM", list(seed = 1)) %>%
        tidy(., "beta") %>% .[ .$beta > mean(.$beta) + sd(.$beta) *input$"Number_of_beta_SD",] %>%
        ggplot(aes(label = term, color = factor(topic), size = beta)) 
      set.seed(1)
      p+ geom_text_wordcloud_area()+scale_size_area(max_size = 20) +theme_minimal()+  
        facet_wrap(~factor(topic))
    }
  )
  
  output$"Word_clouds"<- renderPlot(
    { 
      my_word_clouds()
    }
  )
  
  output$"download_Word_clouds" <- downloadHandler(
    filename = function() paste("Word clouds plot", ".png", sep = ""),
    content = function(x) ggsave(x, my_word_clouds(),width = 8,height = 8)
  )
}

ui <- fluidPage(
  
  fileInput("Dictionary", "Dictionary", buttonLabel = "Upload...", multiple = T),
  fileInput("Stop_words", "Stop words", buttonLabel = "Upload...", multiple = T),
  fileInput("Text", "Text",             buttonLabel = "Upload...", multiple = T),
  
  numericInput("Number_of_topics", "Number of topics",  value = 2, min = 2, max = 15),
  numericInput("Number_of_beta_SD","Number of beta SD", value = 0, min = 0, max = 6 ),
  
  verbatimTextOutput("getwd_out"),
  
  plotOutput("corr_plot_out"),
  downloadButton("corr_plot_DL"),
  
  plotOutput("LDA_tuning"),
  downloadButton("download_LDA_tuning"),
  
  plotOutput("Word_clouds"),
  downloadButton("download_Word_clouds")
  
)
shiny::shinyApp(ui, server)
