#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Install: install.packages("gmailr")
# Install:install.packages("ellipsis")
#install.packages("shinycustomloader") https://emitanaka.org/shinycustomloader/
#install.packages("shinycssloaders")
#install.packages("dplyr")
#install.packages("shinyFeedback")
#install.packages("shinyjs")
#install.packages("future")


Sys.setenv(PATH = paste("C:/rtools40/usr/bin", Sys.getenv("PATH"), sep=";")) # NECESSARIO
library(shiny)
library(autoresampling)
library(gmailr)
library(dplyr)
library(shinycssloaders)
library(DT)
library(shinyFeedback)
library(shinyjs)

clientId<-"example.apps.googleusercontent.com"
consumerScret<-"XXXXXXX"
gm_auth_configure(key=clientId,secret = consumerScret)

#use_secret_file("autoresampling.json")
#read.csv("PimaIndiansDiabetes.csv",stringsAsFactors=TRUE)
#sapply(my.data, class)

gm_oauth_app()



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Auto ML"),
    helpText("Upload a csv file."),

    # Sidebar with a slider input for number of bins 
    navbarPage("Auto ML",
               tabPanel("ATOMIC",
                    sidebarLayout(
                        sidebarPanel(
                          useShinyFeedback(),
                            fileInput("file", "Choose CSV File", accept = ".csv"),
                          #  textInput("target", "Put the target"),
                            uiOutput("secondSelection"),
                            sliderInput("slider", h3("Number of models"), min = 2, max = 469, value = 20),
                            checkboxInput("donate", "Donate data set?", value = FALSE, width = NULL),
                            checkboxInput("sendemail", "Send results to your email?", value = FALSE, width = NULL),
                            conditionalPanel(
                              condition = "input.sendemail",
                              textInput("email", "Put a valid email"),
                              ),
                          checkboxInput("predictions", "Show predictions?", value = FALSE, width = NULL),
                          
                              conditionalPanel(
                                  condition = "input.predictions",
                                  useShinyFeedback(),
                                  fileInput("testfile", "Input CSV File for test", accept = ".csv"),
                              ),
                         
                          useShinyjs(),
                          fluidRow(column(6, align="center", offset = 3, actionButton("click", "Click me")))
                        
                           
                            
                            
                        ),
                
                        # Show a plot of the generated distribution
                        mainPanel(
                          conditionalPanel(
                            condition = "input.click",
                            withSpinner(
                              tableOutput("table")
                            )
                          ),

                            textOutput("textemail")
                        )
                    )
               ),
                tabPanel("Predctions",
                         conditionalPanel(
                           condition="input.click",
                           uiOutput("dowloadPredctions"),
                           withSpinner(
                             tableOutput("predictionsTable")
                           )
                          
                         )
                        
                ),
               tabPanel("About",
                        p("About Us")
                        
               )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
                   
                    train_set_is_valid<-FALSE
                    
                    train_set<-eventReactive(input$file,{
                   
                      
                      file <- input$file
                      ext <- tools::file_ext(file$datapath)
                     
                      req(file)
                      isvalid<-ext == "csv"
                      feedbackWarning("file", !isvalid, "It's not a CSV")
                      if(!isvalid){
                        train_set_is_valid<<-FALSE
                        return()
                      }
                      req(isvalid, cancelOutput = TRUE)
                      train_set_is_valid<<-TRUE
                      read.csv(file$datapath)
                    })
      
                #     train_set<-function(){
                #    
                #     file <- input$file
                #     ext <- tools::file_ext(file$datapath)
                #     req(file)
                #     validate(need(ext == "csv", "Please upload a csv file"))
                # 
                #     read.csv(file$datapath,stringsAsFactors=TRUE)
                # }
                   

               output$secondSelection <- renderUI({if(!length(names(train_set()))==0){selectInput("target", "Target:", choices = names(train_set()))}
                 })
               
               
               target<-FALSE
               train_set_new<-NULL
     
               observeEvent(input$target,{
                  req(input$target)
                  isbinary<-length(table(train_set()[[input$target]]))==2
                  feedbackWarning("target", !isbinary, "It is not a binary variable, try again")
                  if(!isbinary){
                    target<<-FALSE
                    return()
                  }
                  req(isbinary,cancelOutput = TRUE)
                  target<<-TRUE
                  train_set_new<<-train_set()
                  train_set_new[[input$target]]<<-as.factor(train_set_new[[input$target]])
                 
               })
              

               
                test_set<-NULL
                test_set_is_valid<-TRUE
                observeEvent(input$predictions,{
                 
                  if(input$predictions){test_set_is_valid<<-FALSE}
                  else{test_set_is_valid<<-TRUE}
                  
                })

                observeEvent(c(input$testfile,input$file),{
                
                file <- input$testfile
                ext <- tools::file_ext(file$datapath)
               
                req(file)
                feedbackWarning("testfile", !train_set_is_valid, "Put a valid train set")
                
                req(train_set_is_valid,cancelOutput = TRUE)
                
                isvalid<-ext == "csv"
                feedbackWarning("testfile", !isvalid, "It's not a CSV")
                
              
                req(isvalid, cancelOutput = TRUE)
                test_set<<- read.csv(file$datapath)
                datas_match<-pred()
                feedbackWarning("testfile", !datas_match, "The test set and train set don't match")
                
                
                req(datas_match, cancelOutput = TRUE)
                test_set_is_valid<<-TRUE
              })
                observe({
                  
                  shinyjs::hide("click")
                  
                })
              
                observeEvent(c(input$target,input$file,input$testfile,input$predictions),{
                  valid<-(train_set_is_valid && target && test_set_is_valid)

            
                  shinyjs::hide("click")
                  
                  if(valid){
                    shinyjs::show("click")
                  }
           
                })


              form<-NULL
              button<-eventReactive(input$click,
                           {  
                                 
                                 form<<-formula(paste(input$target,"~."))
                                 
                                 atomic.m <- ATOMIC(form,train_set_new,nmodels = input$slider, modelPath="sysdata.rda")
                                 
                                 atomic.m
                                 
                               # ind <- sample(1:nrow(train_set), 0.7*nrow(train_set))
                               # train <- train_set[ind,]
                               # test <- train_set[-ind,]

  
                    })
              
              test_set_aux<-NULL
              pred<-function(){
                train_set_aux<-train_set_new
                train_set_aux[[input$target]]<-NULL
                test_set_aux<<-test_set
                test_set_aux[[input$target]]<<-NULL
                return(identical(sapply(test_set_aux, class),sapply(train_set_aux, class)))
              
              }
              pred2<-eventReactive(button(),{
                req(input$predictions)
                req(input$testfile)
                test_set_aux<<-test_set
                print( "aqui")
                test_set_aux[[input$target]]<<-predict(button(), test_set_aux)
                print("aqui")
                test_set_aux
              })
              
       
             
             output$dowloadPredctions <- renderUI({
               
               if(input$predictions){
                 
                   downloadButton("dowloadPred","Dowload predctions")
               }

                 
               })
             output$dowloadPred <- downloadHandler(
              
               filename = "Predctions.csv",
               content = function(filename) {
                 write.csv(pred2(), filename, row.names = FALSE)
               }
             )

            

             results<-eventReactive(button(),{
                 results<-button()$cv.results
                 results$ntrees <-as.integer(results$ntrees)
                 results$k <-as.integer(results$k)
                 results$RStrategy<-sapply(results$RStrategy, sub,pattern="rs.",replacement="")

                 write.csv(results,"ResultsAtomic.csv",row.names = FALSE)
                 
                 results
             })
             
             email<-eventReactive(results(),{
               msg<-NULL
               if(input$sendemail){
                 if(input$predictions){
                   msg <- gm_mime() %>%
                     gm_subject("Autoresampling results") %>%
                     gm_to(input$email) %>%
                     gm_from("autoresampling@gmail.com") %>%
                     gm_text_body("Autoresampling resuts") %>%
                     gm_attach_file("ResultsAtomic.csv")%>%
                     gm_attach_file("Predctions.csv")
                 }
                 else{
                   msg <- gm_mime() %>%
                     gm_subject("Autoresampling results") %>%
                     gm_to(input$email) %>%
                     gm_from("autoresampling@gmail.com") %>%
                     gm_text_body("Autoresampling resuts") %>%
                     gm_attach_file("ResultsAtomic.csv")
                 }

                 gm_send_message(msg)
                 "If the email is valid the csv was sent"
                 
               }else{""}
             })

     
     
   
     output$table <- renderTable({ input$click 
                                    Sys.sleep(1.5) 
                                    results()})
     output$textemail <- renderText({ email() })
     output$predictionsTable<-renderTable({pred2()})
     
     
     observeEvent(input$click,{
       req(input$donate)
       dataset<-list()
       dataset[["data"]]<-train_set_new
       dataset[["formula"]]<-form
       saveRDS(dataset,file="fileNew.RData")
       system("Rscript runModels.R fileNew.RData")
     })
     
    
  

}

# Run the application 
shinyApp(ui = ui, server = server)


