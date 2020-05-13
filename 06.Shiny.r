library(shiny)
library(rlist)
library(DT)
library(shinyWidgets)
library(shinythemes)

ui<- fluidPage(theme = shinytheme("slate"),
  
  
  pageWithSidebar(
    headerPanel("Query Search"),
    sidebarPanel(
      searchInput("query",label ="Enter your query",btnSearch = icon("search"),btnReset = icon("remove"),width = "450px")
      
    ),
    mainPanel(
      h4("Search Results"),
      #verbatimTextOutput("results")
      #textOutput("selected_var")
      dataTableOutput("results")
    )
  )
)

server<-function(input, output) {
  
  x<-reactive({
                  y<-QuestionSearch(input$query)
                  if(nrow(y)!=0){
                   
                                    op<-list()
                                    for (i in 1:nrow(y)){
                                      op<-list.append(op,paste0(y[i,1]))
                                      op<-list.append(op,paste0(y[i,2]))
                                      i=i+1
                                    }
                                    op<-as.character(op)
                                    
                                    iter=length(op)
                                    output <- matrix(ncol=2, nrow = iter)
                                    
                                    
                                    for (i in 1:iter){
                                      if((i %% 2) == 0) {
                                        
                                        output[i,1]<- paste("Answer",i/2)
                                        output[i,2]<- paste(op[i])
                                        
                                      } 
                                      else {
                                        if(round(i/3)!=0){
                                          if(i>=7){
                                            output[i,1]<- paste("Question",round(i/3)+2)
                                            output[i,2]<- paste(op[i])
                                          }
                                          else{
                                            output[i,1]<- paste("Question",round(i/3)+1)
                                            output[i,2]<- paste(op[i])
                                          }
                                          
                                        }
                                        else{
                                          output[i,1]<- paste("Question",i)
                                          output[i,2]<- paste(op[i])
                                        }
                                      }
                                      
                                      i=i+1
                                      
                                    }
                              }
                          else{
                            output <- matrix(ncol=2, nrow = 1)
                            output[1,1]<- paste(".")
                            output[1,2]<- paste("No Result Found")
                          }
                          output<-data.frame(output)
                      #    datatable(head(output), rownames = FALSE)
                          
                          return(output)
    
                })
  
    output$results<-renderDataTable({
                                      validate(
                                                need(input$query, 'need to enter a question')
                                              )
    
                                      datatable(x(),rownames = FALSE,colnames = "",
                                                options = list(dom = 't',autoWidth = TRUE,
                                                               columnDefs = list(list(width = '90%', targets = 1)))
                                              )
  
    
                                  })
  
}

runApp(
  launch.browser= T,shinyApp(ui = ui, server = server)
  )

