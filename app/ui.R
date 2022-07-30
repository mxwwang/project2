#tagList(
fluidPage( theme = shinytheme("flatly"),
  #shinythemes::themeSelector(),
  
  navbarPage(
    #theme = "cerulean",  # <--- To use a theme, uncomment this
    
    title = "Empire State of Mind",
    
    
    ###### TAB - 1 ######
    tabPanel(#":)", 
             "",icon = icon("home", lib = "glyphicon"),
             
             h4("How NYC is moving around differently since the onset of the COVID pandemic"),
             # h5("This App inspect NYC mobility based on Google data, and compared them to
             # covid trend to find out reletinship by borough and category."),
             h1(""),
             
             img(id="homeimg", src = "./image/ap_gatherings_covid.jpeg", style = "width: 100%; padding: 0;")
    ),
    
    
    
    ###### TAB - 2 ######
    tabPanel("Category",
             sidebarPanel(
               # fileInput("file", "File input:"),
               # textInput("txt", "Text input:", "general"),
               # sliderInput("slider", "Slider input:", 1, 100, 30),
               # tags$h5("Default actionButton:"),
               # actionButton("action", "Search"),
               
               selectizeInput(
                 inputId='category',
                 label='Category',
                 choices=categories)
                #choices=unique(mobility_nyc$origin))
                 
                
                 #tags$h5("actionButton with CSS class:"),
                 #actionButton("action2", "Action button", class = "btn-primary")
             ),
             
             mainPanel(
               plotOutput("cat_plots",width = "100%", height = "600px")
               
               
             )
    ),
    
    
    ###### TAB - 3 ######
    tabPanel("Borough",
             
             sidebarPanel(
               selectizeInput(
                 inputId='boro',
                 label='Borough',
                 choices=unique(mobility_nyc$borough)),
               
               selectizeInput(
                 inputId='stat',
                 label='COVID Stat',
                 choices=unique(covid_stats))
               
          
             ),
             
             mainPanel(
               
               tabsetPanel(
                 ###### tab - 1
                 tabPanel( "Mobility Trends vs. COVID Stats",
                           plotOutput("boro_plots",width = "100%", height = "900px")
                 ),
                 
                 ###### tab - 2
                 tabPanel("Correlation", 
                          
                          h2(""),
                          dateInput('sdate',
                                      label = 'Select cut-off date: yyyy-mm-dd',
                                      value = Sys.Date()
                          ),
                          
                          plotOutput("corr_matrix",width = "100%", height = "900px")
                          
                 )
                 
                 
               )
             )
    ),
    
    
    ###### TAB - 4 ######
    tabPanel("Comparison",
             
             sidebarPanel(
               selectizeInput(
                 inputId='boro1',
                 label='Borough 1',
                 choices=unique(mobility_nyc$borough)),
               
               selectizeInput(
                 inputId='boro2',
                 label='Borough 2',
                 choices=unique(mobility_nyc$borough)),
               
               selectizeInput(
                 inputId='cat',
                 label='Category',
                 choices=categories),
               
               
               selectizeInput(
                 inputId='cstat',
                 label='COVID Stat',
                 choices=covid_stats)
              
             ),
             
             mainPanel(
                 plotOutput("covid_comp",width = "100%", height = "400px"),
                 plotOutput("boro_comp",width = "100%", height = "400px")
                 
             )
    )
  )
)
