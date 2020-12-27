require(dplyr)
if (!require('shiny')){install.packages("shiny");require(shiny)}
if (!require('ggplot2')) install.packages("ggplot2")
if (!require('caret')) install.packages("caret")
if (!require('tidyverse')) install.packages("tidyverse")

load('LoanStatsrf.Rdata')

ui = fluidPage(
  titlePanel("Check your Loan Approval Here"),
  withMathJax(),
  
  sidebarLayout(
    sidebarPanel(
    
      numericInput("loan_amnt", "Expected Loan Amount", value = NULL),
      
      selectInput("loanStatus", "Current Status of the Loan",
                   c('Fully Paid','Charged Off', 'Does not meet the credit policy. Status:Fully Paid',
                                    'Does not meet the credit policy. Status:Charged Off','Current',
                                    'Late (16-30 days)', 'In Grace Period', 'Late (31-120 days)','Default'),
                  c('FP','CO','DNMSFP','DNMSCO','C','L1630','GP','L31120','DEF')),
      
      sliderInput("inq_last_6mths", "The number of inquiries in past 6 months (excluding auto and mortgage inquiries)", 5, min = 0, max = 35),
      
      selectInput( "grade", "LC assigned loan grade", c('A','B','C','D','E','F','G'), c('a','b','c','d','e','f','g')),
      
      selectInput("home_ownership", 'The home ownership status provided by the borrower during registration', c('OWN','RENT','MORTGAGE','OTHER','NONE'),
                  c('Own','Rent','Mortgage','Other','None')),
        
      selectInput("verification_status", "Indicate if income was verified by LC, not verified, or if the income source was verified",
                  c('Not Verified', 'Verified', 'Source Verified'),c("NV","V",'SV')),
      
      radioButtons("term","The number of payments on the loan, 36 or 60 months", c(36, 60))
    ),
    
    # this directs where to send the user input to
    #  -> Send user input to "main panel" on site
    mainPanel(
      uiOutput('data'),
      uiOutput('interpretation')
    )
  )
)

server <- function(input, output){
  
loanstat = function(input, df){
  if (input$loanStatus == 'FP'){df$loan_status = 1}
  else if (input$loanStatus == 'CO'){df$loan_status = 2}
  else if (input$loanStatus == 'DNMSFP'){df$loan_status = 3}
  else if (input$loanStatus == 'DNMSCO'){df$loan_status = 4}
  else if (input$loanStatus == 'C'){df$loan_status = 5}
  else if (input$loanStatus == 'L1630'){df$loan_status = 6}
  else if (input$loanStatus == 'GP'){df$loan_status = 7}
  else if (input$loanStatus == 'L31120'){df$loan_status = 8}
  else if (input$loanStatus == 'DEF'){df$loan_status = 9}
}
grad = function(input, df){
  if (input$grade == 'a'){df$grade = 1}
  else if (input$grade == 'b'){df$grade = 2}
  else if (input$grade == 'c'){df$grade = 3}
  else if (input$grade == 'd'){df$grade = 4}
  else if (input$grade == 'e'){df$grade = 5}
  else if (input$grade == 'f'){df$grade = 7}
  else if (input$grade == 'g'){df$grade = 6}
}
homeowner = function(input, df){
  if (input$home_ownership == 'Own'){df$home_ownership = 1}
  else if (input$home_ownership == 'Rent'){df$home_ownership = 2}
  else if (input$home_ownership == 'Mortgage'){df$home_ownership = 3}
  else if (input$home_ownership == 'Other'){df$home_ownership = 4}
  else if (input$home_ownership == 'None'){df$home_ownership = 5}
}
verification = function(input, df){
  if (input$verification_status == 'NV'){df$verification_status = 1}
  else if (input$verification_status == 'V'){df$verification_status = 2}
  else if (input$verification_status == 'SV'){df$verification_status = 3}
}

output$data = renderUI(
  {
    df <- data.frame(loan_amnt=integer(),
                     loan_status=factor(),
                     grade=factor(),
                     inq_last_6mths = integer(),
                     home_ownership=factor(),
                     verification_status=factor(),
                     term=integer())
    
    df$loan_amnt = as.numeric(input$loan_amnt)
    df$loan_status = loanstat(input$loanStatus)
    df$grade = grad(input$grade)
    df$inq_last_6mths = as.numeric(input$inq_last_6mths)
    df$home_ownership = homeowner(input$home_ownership)
    df$verification_status = verification(input$verification_status)
    df$term = as.numeric(input$term)
     }
)

tuneGrid = data.frame('mtry' = 2,
                      'splitrule' = 'gini',
                      'min.node.size' = 1)

trainControl = trainControl(method = 'cv')

rfOutD   = train(x = XtestD, y = Ytest,
                 method = "ranger", 
                 tuneGrid = tuneGrid,
                 importance = 'permutation',
                 preProcess = 'scale',
                 trControl = trainControl)

output$interpretation = renderUI({
  
  pr <- predict(rfOutD, newdata=df)
  pr<-as.numeric(pr)
  if (pr == 1){
    withMathJax(
      paste0('Congrats, your loan is approved')
    )
  }
  else {
    withMathJax(
      paste0('Sorry, your loan is not approved at this moment. Please, call the customer service to know more about your application')
    )
  }
})
}

# Run the application 
shinyApp(ui = ui, server = server)
