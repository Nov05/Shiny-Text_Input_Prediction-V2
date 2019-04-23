
# https://stackoverflow.com/questions/35265920/auto-complete-and-selection-of-multiple-values-in-text-box-shiny

# CAUTION: Full 1-gram dataset contains too many objects,
#          which makes it impossible for the app to load.
# unigram <- readRDS("1gram_en.rds")

library(shiny)
library(shinysky)
library(ggplot2)
library(ggrepel)
library(stringr)

# remove all objects
rm(list = ls())

unigram <- readRDS("1gram_90_en.rds")
unilist <- as.character((unigram[,1])); rm(unigram)
trigram <- readRDS("3gram_notail_en.rds")
text <- ""
text_all <- list()

ui <- shinyUI(
  fluidPage(tags$style(type="text/css",".shiny-output-error { visibility: hidden; }",".shiny-output-error:before { visibility: hidden; }"),
            tags$style(type="text/css","#search { top: 50% !important;left: 50% !important;margin-top: -100px !important;margin-left: -250px 
                       !important; color: blue;font-size: 20px;font-style: italic;}"),         
            
            titlePanel("Text Input Prediction"),
      
            sidebarLayout(
              sidebarPanel(
                helpText("Please type one word a time in the input box below."),
                textInput.typeahead(id="typeahead01",
                                    placeholder="Type a word here",
                                    local=data.frame(name=c(unilist)),
                                    valueKey="name",
                                    tokens=c(1:length(unilist)),
                                    template = HTML("<p class='repo-language'>{{info}}</p> <p class='repo-name'>{{name}}</p>"),
                                    limit=10
                ), # end of textInput.typeahead()
                shinyalert("shinyalert01"),
                textInput("textInput01", label="Input History", value = "", width = NULL,
                          placeholder='Display words here'),
                tableOutput('tableOutput01'),
                actionButton("actionButton01", "Clear Input")
              ),
              mainPanel(
                plotOutput("barplot01")
              )
            ) # end of sidebarLayout()
  ) # end of fluidPage()
)

server <- function(input, output, session){
  # typeahead
  observe({
    input$typeahead01 # read-only
    
    text <- str_trim(input$typeahead01)
    if (text != ""){
      # append input to list
      text_all[length(text_all)+1] <<- str_trim(gsub("\\s+", " ", text))
      
      # show message
      showshinyalert(session=session, 
                     id="shinyalert01", 
                     HTMLtext=paste0("You just inputted \"", text, "\"."), 
                     styleclass="success")
      
      # search 3-gram dictionary
      # match with two words
      pred <- head(trigram[grep(paste0("^", str_trim(paste0(text_all[length(text_all)-1], " ",
                                                            text_all[length(text_all)])), " "), 
                                trigram[,'ngrams']),], 20)
      # match with the last word
      if (length(text_all)>1 & dim(pred)[1]==0){
        pred <- head(trigram[grep(paste0("^", text_all[length(text_all)], " "), 
                                  trigram[,'ngrams']),], 20)}
      # update text input
      updateTextInput.typeahead(session, "typeahead01",
                                placeholder="Type a word here",
                                dataset=data.frame(name=c(unilist)),
                                valueKey="name",
                                tokens=c(1:length(unilist)),
                                template = HTML("<p class='repo-language'>{{info}}</p> <p class='repo-name'>{{name}}</p>"),
                                limit=10)
      # update input history
      updateTextInput(session, "textInput01", value=paste(text_all, collapse=" "))
      
      # show predicted words table
      output$tableOutput01 <- renderTable(pred[1:5,])
      
      # plot bar chart
      output$barplot01 <- renderPlot({
        p01 <- ggplot(data=pred, aes(x=reorder(ngrams, -prob), y=prob)) +
               geom_bar(stat="identity", fill="grey") +
               geom_label_repel(aes(label=ngrams), size=4) +
               labs(title="Predicted 3-gram Probability",
                    x="", y="Probability") +
               theme(plot.title=element_text(size=30, face="bold"),
                     axis.text.x=element_blank(),
                     axis.text.y=element_text(angle=90))
        print(p01)
      }) # end of renderPlot()    
    }
  }) # end of observe input$typeahead01
  
  observeEvent(input$actionButton01, {
    # clear variants
    text <<- ""
    text_all <<- list()
    
    # update text input
    updateTextInput.typeahead(session, "typeahead01",
                              placeholder="Type a word here",
                              dataset=data.frame(name=c(unilist)),
                              valueKey="name",
                              tokens=c(1:length(unilist)),
                              template = HTML("<p class='repo-language'>{{info}}</p> <p class='repo-name'>{{name}}</p>"),
                              limit=10)
    
    # update input history
    updateTextInput(session, "textInput01", value="")
    output$tableOutput01 <- renderTable(NULL)
    output$barplot01 <- renderPlot(NULL)
    
    # show message
    showshinyalert(session=session, 
                   id="shinyalert01", 
                   HTMLtext=paste0("All inputs have been cleared."), 
                   styleclass="success")
  }) # end of observeEvent input$actionButton01
  
} # end of server function

shinyApp(ui=ui, server=server)