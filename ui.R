ui <- shinyUI(fluidPage(
  
  #includeCSS("/srv/connect/apps/kaggle_shiny/kaggle_shiny.css"),
  includeCSS("kaggle_shiny.css"),
  
  title = '통계분석',
  navbarPage("",
  tabPanel("데이터 선택",
    sidebarLayout(
     sidebarPanel(
       selectInput("domainData", "데이터 선택", choices = domainData),
       actionButton("analysis", "분석 시작")
       ),
     mainPanel( 
     tabsetPanel(
       tabPanel('설명', uiOutput("introduction")),
       tabPanel('train 데이터', DT::dataTableOutput('train_review')),
       tabPanel('test 데이터', DT::dataTableOutput('test_review')),
       tabPanel('answer 데이터',  DT::dataTableOutput('answer_review'))
       ))
  )), 
  
  tabPanel("이얼",        
  
    mainPanel( 
      tabsetPanel(
                  tabPanel(' ', DT::dataTableOutput('yeal'))
               
                  )
            )
     
    ),
  
  tabPanel("김태훈",        
          
             mainPanel( 
               tabsetPanel(
                 tabPanel('titanic', 
                          tabsetPanel(tabPanel("1", plotOutput('taehun1')),
                                      tabPanel("2", verbatimTextOutput('taehun2'))
                          )
                 )
                 )
             )
          
  ),
  
  tabPanel("김태인",        
          
             mainPanel( 
               tabsetPanel(
                 tabPanel('titanic', 
                        tabsetPanel(tabPanel("1", tableOutput('taein1')),
                                    tabPanel("2", plotOutput('taein2')),
                                    tabPanel("3", plotOutput('taein3')),
                                    tabPanel("4", plotOutput('taein4')),
                                    tabPanel("5", plotOutput('taein5')),
                                    tabPanel("6", plotOutput('taein6')),
                                    tabPanel("7", plotOutput('taein7')),
                                    tabPanel("8", plotOutput('taein8')),
                                    tabPanel("9", plotOutput('taein9')),
                                    tabPanel("10", tableOutput('taein10')),
                                    tabPanel("11", verbatimTextOutput('taein11'))
                                    )
                          ),
                 tabPanel('animal', 
                          tabsetPanel(tabPanel("1", plotOutput('taein12')),
                                      tabPanel("2", plotOutput('taein13')),
                                      tabPanel("3", plotOutput('taein14')),
                                      tabPanel("4", tableOutput('taein15'))
                          )
                 )
               )
             )
          
  ),
  
  tabPanel("노원두",        
           
             mainPanel( 
               tabsetPanel(
                 tabPanel('animal', 
                          tabsetPanel(tabPanel("1", plotOutput('wondoo1')),
                                      tabPanel("2", plotOutput('wondoo2')),
                                      tabPanel("3", plotOutput('wondoo3')),
                                      tabPanel("4", plotOutput('wondoo4')),
                                      tabPanel("5", plotOutput('wondoo5')),
                                      tabPanel("6", tableOutput('wondoo6'))
                          )
                 )
               )
             )
           
  ),
  
  tabPanel("유세현",        
           
              mainPanel( 
               tabsetPanel(
                 tabPanel(' ', DT::dataTableOutput('you'))
               )
             )
          
  )
)  
)
)


