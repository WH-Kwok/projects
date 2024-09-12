library(shiny)
server <- function(input,output, session) {
  
  if(require("pacman")==F) install.packages("pacman")
  
  p_load(tidyverse,doParallel,jiebaR, tmcn, tm, widyr, ggraph,
         igraph,ldatuning, topicmodels, tidytext,ggwordcloud)
  
  options(shiny.maxRequestSize = 100 *1024^2)   
  output$"getwd_out"<-renderPrint(    {      paste("Working directory:", getwd())    }  )
  
  dtm <- reactive(    {
    d <- input$"Dictionary"$datapath
    s <- input$"Stop_words"$datapath
    t <- input$"Text"$datapath
    
    validate(
      need(d!="", "Select dictionaries"),need(s!="", "Select stop words"),
      need(t!="", "Select text")
    )
    
    d %>% map(read.csv)%>%unlist()%>%write.csv("Dictionaries.csv", row.names = F)
    s %>% map(read.csv)%>%unlist()%>%write.csv("Stopwords.csv", row.names = F)
    
    w <- worker(user = "Dictionaries.csv", stop_word = "Stopwords.csv")
    
    t %>% map( read.table )%>% do.call(rbind,.) %>% apply(., 1, \(x) segment(x, w)) %>%
      createDTM() %>% removeSparseTerms(., .99) %>% .[apply(.,1,sum)>0,]
  }  )
  
  Triplets <- function(x) {
    df <- data.frame(x$i , x$j , x$v)
    df2 <- x$dimnames$Terms %>% data.frame(x = ., id = 1:length(.))
    left_join(df, df2, by = c("x.j" = "id"))
    
  }
  
  output $ "download_Triplets_out" <- downloadHandler(
    filename = function() paste("Triplets_out", ".csv", sep = ""),
    content = function(file)  dtm() %>% Triplets() %>% write.csv(file, row.names = F)
  )
  
  Pairwise_cor<-    reactive({
    dtm() %>% Triplets() %>% pairwise_cor(., x, x.i, sort=T, upper=F) %>%
      filter(correlation > .4)
    })
  
  output $ "download_Pairwise_cor" <- downloadHandler(
    filename = function() paste("Pairwise_cor_out", ".csv", sep = ""),
    content = function(file)  Pairwise_cor() %>% write.csv(file, row.names = F)
  )
  
  corr_plot <- reactive({
    
    x <- input $ "Pairwise_cor_in" $ datapath
    my_colour <-  input $ "colour_name"
    
    validate(
      need(x !="", "Select translated file"),
      need(my_colour !="", "Select colour: \"darkblue\", \"darkgreen\" etc.")
    )
    
    set.seed(1)
    
    p <-x %>% read.csv() %>% graph_from_data_frame() %>% ggraph(layout ='fr')
    
    p + geom_edge_link(aes(edge_alpha = correlation),  edge_colour = my_colour) +
      scale_edge_alpha( name = "Corr") + geom_node_point() +
      geom_node_text(aes(label = name), repel = T) + theme_void()
  }  )
  
  output $ "corr_plot_out" <-renderPlot(    {     corr_plot()    }  )
  
  output$"Download_corr_plot" <- downloadHandler(
    filename = function() paste("corr_plot", ".png", sep = "") ,
    content = function(x) ggsave(x, corr_plot(), width = 8, height = 8)
  )
  
  lda_tuning <- reactive(    {
    registerDoParallel(cl<-makeCluster(6))
    dtm() %>%
      FindTopicsNumber(., 2:10, c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
                       "Gibbs", list(seed = 1), cl) %>% FindTopicsNumber_plot()
    x<-recordPlot()
    
    stopCluster(cl)
    x
    replayPlot (x)
    # ftn%>% gather("ke","va",-topics)%>%       ggplot( aes(topics, va,lty=ke)) +geom_line()
  }  )
  
  output $ "LDA_tuning"<- renderPlot(    {        lda_tuning()    }  )
  
  output $ "download_LDA_tuning" <- downloadHandler(
    filename = function() paste("LDA tuning plot", ".png", sep = ""),
    content = 
       function(file) {
       png(file)
      lda_tuning()
       dev.off()
       } 
    ,
    contentType = 'image/png'
    
  )
  
  Beta<-    reactive( {
      
      nt<-input$"Number_of_topics"
      nb<-input$"Number_of_beta_SD"
      validate(
        need(nt!="","input num of topics."),need(nb!="","input num of beta sd.")
      )
      
      dtm() %>% LDA(., nt ,"VEM", list(seed = 1)) %>% tidy(., "beta") %>%
        .[ .$beta > mean(.$beta) + sd(.$beta) * nb,]
      
    }  )

  output $ "download_Beta" <- downloadHandler(
    filename = function() paste("Beta_out", ".csv", sep = ""),
    content = function(file)  Beta() %>% write.csv(file, row.names = F)
  )
  
  my_word_clouds<-reactive(    {
    
    bi <- input$"Beta_in"$datapath
    validate(need(bi!="","pls upload beta translated."))
    p <- bi %>% read.csv()  %>%
      ggplot( aes( label = term, color = factor(topic), size = beta) ) 
    
    set.seed(1)
    p+ geom_text_wordcloud_area()+scale_size_area(max_size = 20) +
      theme_minimal()+facet_wrap(~factor(topic))
  }  )
  
  output $ "Word_clouds" <- renderPlot(    {       my_word_clouds()    }  )
  
  output$"download_Word_clouds" <- downloadHandler(
    filename = function() paste("Word clouds plot", ".png", sep = ""),
    content = function(x) ggsave(x, my_word_clouds(),width = 8,height = 8)
  )
}

ui <- fluidPage(
  
  fileInput("Dictionary", "Dictionary",  T),
  fileInput("Stop_words", "Stop words",  T),
  fileInput("Text", "Text",              T),
 
  fileInput(  "Pairwise_cor_in", "Pairwise corr translated",T),
  textInput("colour_name", "Colour name", value = "darkblue"),
  
  numericInput("Number_of_topics", "Number of topics",  value = 2, min = 2, max = 15),
  numericInput("Number_of_beta_SD","Number of beta SD", value = 0, min = 0, max = 6 ),
  
  fileInput(  "Beta_in", "Beta translated",T),
  
  verbatimTextOutput("getwd_out"),
  
  downloadButton("download_Triplets_out","download_Triplets_out"),
  downloadButton("download_Pairwise_cor","download_Pairwise_cor"),
  
  plotOutput("corr_plot_out"),
  downloadButton("Download_corr_plot","Download_corr_plot"),
  
  plotOutput("LDA_tuning"),
  downloadButton("download_LDA_tuning","download_LDA_tuning"),
  
  downloadButton("download_Beta","download_Beta"),
  
  plotOutput("Word_clouds"),
  downloadButton("download_Word_clouds","download_Word_clouds")
  
)
shiny::shinyApp(ui, server)
