# library
library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)
library(plotly)
library(tidyverse)
library(lubridate)
library(forcats)
library(RColorBrewer)
library(ggrepel)
library(tm)
library(wordcloud)
#library(syuzhet)
#library(SnowballC)


drivers <- read.csv("drivers.csv")
drivers <- drivers %>%
  mutate(name = paste(forename, surname))
driver_standings <- read.csv("driver_standings.csv")
races <- read.csv("races.csv")
results <- read.csv("results.csv")
constructors <- read.csv("constructors.csv")
constructor_results = read_csv("constructor_results.csv")
constructor_standings = read_csv("constructor_standings.csv")
constructor_info = read_csv("constructor_info.csv")
circuits <- read_csv("circuits.csv")


allData = list(
  races,
  results,
  drivers,
  constructors,
  constructor_results,
  constructor_standings,
  constructor_info,
  circuits
)

temp = left_join(x = results, y = races, by = "raceId")
constructors_mu = left_join(x = temp, y = constructors, by = "constructorId")
constructors_mu = constructors_mu %>%
  select(2, 3, 4, 6, 7, 9, 10, 19, 22, 35, 36, 37, 38)
driver_mu = left_join(x = constructors_mu, y = drivers, by = "driverId")
driver_mu = driver_mu %>%
  mutate(name = paste0(forename, " ", surname))


cons_22_names = constructors_mu %>%
  filter(year == "2022") %>%
  group_by(name.y) %>%
  summarise(total_points = sum(points)) %>%
  arrange(desc(total_points)) %>%
  head(10) %>%
  pull(name.y)

circuits_Z1 <- circuits %>%
  left_join(races, by = "circuitId") %>%
  filter(year >= 2017 & year <= 2021) %>%
  subset(select = c(1:15))

circuits_Z2 = circuits_Z1 %>%
  mutate(
    popusText = paste(
      strong("Year:"),
      year,
      "<br>",
      strong("Country:"),
      country,
      "<br>",
      strong("Circuit name:"),
      name.x,
      "<br>",
      strong("Introduction:"),
      url.x
    )
  )

ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "Welcome to F1 World"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "page1", icon = icon("home")),
      menuItem(
        "Global Info",
        tabName = "page2",
        icon = icon("flag-checkered")
      ),
      menuItem("Constructor", tabName = "page3", icon = icon("car")),
      menuItem("Driver", tabName = "page4", icon = icon("user")),
      menuItem("Circuit", tabName = "page5", icon =
                 icon("road")),
      menuItem(
        "Data Source",
        tabName = "page6",
        icon = icon("database")
      )
    )
  ),
  dashboardBody(
    #######
    #Page 1
    #######
    tabItems(
      tabItem(
        tabName = "page1",
        h2(strong("Home")),
        fluidRow(
          box(
            title = "F1 overview",
            solidHeader = TRUE,
            status = "warning",
            width = 12,
            collapsible = TRUE,
            column(12,
                   tags$div(fluidRow(
                     column(
                       8,
                       "Formula 1 racing began in 1950 and is the world’s most prestigious motor racing competition, as well as the world’s most popular annual sporting series: The 2019 FIA Formula One World Championship runs from March to December and spans 21 races in 21 countries across four continents. Formula One World Championship Limited is part of Formula 1 and holds the exclusive commercial rights to the FIA Formula One World Championship.",
                       br(),
                       a(h5("Source", href = 'https://corp.formula1.com/about-f1'))
                     ),
                     
                     column(
                       4,
                       img(
                         src = "F1_logo.png ",
                         length = 200 ,
                         width =
                           200,
                         style = "display: block;margin-left: auto;margin-right: auto;"
                       )
                     )
                   )))
          )
        ),
        fluidRow(
          box(
            title = "User Guide",
            solidHeader = TRUE,
            status = "primary",
            width = 12,
            collapsible = TRUE,
            column(
              12,
              strong("Welcome to the world of Formula 1."),
              "This website is aiming to help F1 fans and researchers acquire information by using data visualization tools.",
              br(),
              br(),
              "The",
              strong("Global Info"),
              "page will perform a overview of the Formula One history from year 1950 to 2022.",
              br(),
              br(),
              "The",
              strong("Constructor, Driver, and Circuit"),
              "pages will provide more detailed information on things you would like to know about F1."
            )
          )
        ),
        
        fluidRow(
          box(
            title = "About Us",
            solidHeader = TRUE,
            status = "success",
            width = 12,
            collapsible = TRUE,
            column(
              12,
              "This website is developed for the final project of the Data Visualization courses of Johns Hopkins Carey Business School. All team members are graduate students from Johns Hopkins University who majoring in Business Analytics and Risk Management.",
              br(),
              br()
            ),
            column(12,
                   tags$div(fluidRow(
                     column(
                       12,
                       strong("Jiahan Wu:"),
                       "Jiahan comes from a strong mathematic and analysis background who mainly focusing on delivering data-driven solutions. She likes all kinds of outdoor activities and she is now a dance instructor in the DMV area.",
                       br(),
                       br(),
                       strong("Jingyi Zheng:"),
                       "She is a 2022 NBA CHAMPION & FINALs MVP fan. She loves watching F1 tire changing videos.",
                       br(),
                       br(),
                       strong("Jingyi Ye:"),
                       "Jingyi graduated from UC San Diego, majored in Economics. She is keen to work as a project manager in a professional technical solution firm after graduation. Jingyi enjoyed traveling and has been to 10 countries. She also practices yoga.",
                       br(),
                       br(),
                       strong("Menghan Fan:"),
                       "Menghan is experienced in business analysis and trading for renewable energy sector. Her hobby is traveling, reading, and playing piano. She also enjoys going out with friends and trying new experience.",
                       br(),
                       br(),
                       strong("Junru Mu:"),
                       "Junru graduated from Colgate University majored in Mathematical Economics. She is passionate in using data analytics to help businesses develop new possibilities. She loves music and books and is a fan of adventure and detective novels."
                     )
                   )))
          )
        )
      ),
      #######
      #Page 2
      #######
      tabItem(
        tabName = "page2",
        h2(strong(
          "An Overview of the History of Formula 1 (1950-2022)"
        )),
        br(),
        fluidRow(
          column(
            4,
            h3('The Grand Prix'),
            align = 'center',
            h1(strong('1068'),
               style = "color:red; font-size:80px",
               align = 'center'),
            h6(
              'Total Grand Prix 1950-2022',
              style = 'color:Grey; font-size:15px',
              align = 'center'
            ),
            br(),
            h6(
              '1st Grand Prix: British Grand Prix @ May 13th, 1950',
              style = 'color:Grey; font-size:15px',
              align = 'center'
            ),
            h6(
              'Lastest Grand Prix: Austrian Grand Prix @ Jul 10th, 2022',
              style = 'color:Grey; font-size:15px',
              align = 'center'
            )
          ),
          column(
            4,
            h3('Most Successful Constructor', align = 'center'),
            h1(strong('Ferrari'),
               style = "color:red; font-size:80px",
               align = 'center'),
            br(),
            h4(
              '16',
              style = 'color:Grey; font-size:60px',
              align = 'center',
              "Constructors' Championships",
              style = 'color:Grey; font-size:15px'
            ),
            h6('242 Grand Prix Wins',
               style = 'color:Grey; font-size:15px',
               align = 'center'),
            h6(
              "15 Drivers'Championships",
              style = 'color:Grey; font-size:15px',
              align = 'center'
            )
          ),
          column(
            4,
            h3('Most Successful Driver', align = 'center'),
            h1(
              strong('Michael Schumacher'),
              style = "color:red; font-size:40px",
              align = 'center'
            ),
            h6("7 World Championships",
               style = 'color:Grey; font-size:15px',
               align = 'center'),
            h6("91 Grand Prix Wins",
               style = 'color:Grey; font-size:15px',
               align = 'center'),
            h1(strong('Lewis Hamilton'),
               style = 'color:red; font-size:40px',
               align = 'center'),
            h6("7 World Championships",
               style = 'color:Grey; font-size:15px',
               align = 'center'),
            h6("103 Grand Prix Wins",
               style = 'color:Grey; font-size:15px',
               align = 'center')
          )
        ),
        hr(style = "border-color: red"),
        
        fluidRow(
          column(
            8,
            h3('World Championship', align = 'center'),
            br(),
            tabsetPanel(tabPanel(
              h5('World Championship Titles by Driver', align = 'center'),
              plotOutput("pg2_plot1")
            ),
            tabPanel(
              h5('World Championship Titles by Constructor', align = 'center'),
              plotOutput("pg2_plot2")
            ))
            
            
          ),
          column(
            4,
            h3('Distribution of Driver Nationalities'),
            br(),
            br(),
            br(),
            br(),
            plotOutput("pg2_plot3")
          )
        )
      ),
      
      #######
      #Page 3
      #######
      tabItem(
        tabName = "page3",
        h2(strong("Constructors")),
        box(
          h5(
            "This page visualizes the constructors total points earned by country and top 10 constructors performance in the selected year of",
            em(strong("last 5 years (2017-2021)")),
            ". You can use",
            em(strong("Current Constructors")),
            "drop down box below to filter the constructors in 2021. ",
            "This page also introduces the",
            em(strong("profile")),
            "of each contructor that users choose",
            em(strong("basic information")),
            "of each constructor and",
            em(strong("ranks")),
            "of constructors over years."
          ),
          title = "User Guide",
          solidHeader = TRUE,
          status = "warning",
          width = 12,
          collapsible = TRUE
        ),
        h3("Constructor Points by Country--Country Performance"),
        plotOutput("plot_mu1"),
        
        h3("Top 10 highest points Constructors"),
        sliderInput(
          "ConstructorYear",
          "Select Year:",
          min = 2017,
          max = 2021,
          value = 2017,
          step = 1,
          animate = animationOptions(interval = 1500, loop = FALSE)
        ),
        plotOutput("plot_mu2"),
        
        h2("Constructors Overview"),
        tabsetPanel(
          tabPanel(
            "Constructors Info",
            icon = icon("info"),
            selectInput(
              inputId = "ConstructorsM",
              label = "Current Constructors",
              choices = cons_22_names
            ),
            
            fluidRow(column(
              6,
              box(
                title = "Profile",
                status = "primary",
                solidHeader = TRUE,
                h4("Nationality:"),
                textOutput("text_mu1"),
                
                hr(),
                h4("Logo:"),
                uiOutput("imageLogo1"),
                
                hr(),
                h4("Current Drivers:"),
                textOutput("text_mu2"),
                textOutput("text_mu3")
              ),
              box(
                title = "About",
                status = "success",
                solidHeader = TRUE,
                textOutput("text_mu4")
              )
            ),
            
            column(
              6,
              box(
                title = "Ranks",
                background = "yellow",
                solidHeader = TRUE,
                width = 600,
                plotlyOutput("plot_mu5", height = 300)
              )
            ))
            
          ),
          ###word cloud
          
          tabPanel("Word Cloud",
                   icon = icon("cloud"),
                   
                   fluidRow(
                     column(
                       3,
                       strong("Settings:"),
                       br(),
                       br(),
                       sliderInput(
                         "slider1",
                         "Minimum Frequency:",
                         min = 1,
                         max = 50,
                         value = 5
                       ),
                       
                       sliderInput(
                         "slider3",
                         "Rotation:",
                         min = 0.0,
                         max = 1.0,
                         value = 0.35
                       ),
                       
                       sliderInput(
                         "slider2",
                         "Max words:",
                         min = 10,
                         max = 50,
                         value = 10
                       )
                     ),
                     column(9, mainPanel(plotOutput("wordcloud")))
                   )),
          
          
          tabPanel(
            "Video",
            icon = icon("video"),
            h4("F1 Dramatic Moment"),
            HTML(
              '<iframe width="50%" height="400" src="https://www.youtube.com/embed/7YMjw2sjXqU" frameborder="0" allowfullscreen></iframe>'
            )
          )
        )
      ),
      
      #####
      #Page4
      #####
      tabItem(
        tabName = "page4",
        h2(strong("Driver")),
        fluidRow(column(
          12,
          h4(""),
          
          box(
            h5(
              "This page visualizes the best driver, the most successful drivers and the drivers' championship winner
                           in the",
              em(strong("last 5 years (2017-2021)")),
              ". You can use",
              strong("Select Year"),
              "function below to filter. ",
              strong("Best Driver"),
              "is the driver that has won the most races of all the drivers.",
              strong("Most successful drivers"),
              "is a top 5 based on the number of races won by a driver.",
              strong("Drivers'championship winner"),
              "is based on the sum of drivers' points won by a driver.",
              "This page also introduces the profile of each driver",
              ". You can use",
              strong("Select Driver"),
              "function below to retrieve each driver's profile."
            ),
            title = "User Guide",
            status = "warning",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            width = "100%"
          )
          
        )),
        hr(),
        fluidRow(
          box(
            title = "Drivers of the Year",
            solidHeader = TRUE,
            status = "primary",
            width = 12,
            collapsible = TRUE,
            sliderInput(
              "year",
              "Select Year:",
              min = 2017,
              max = 2021,
              value = 2017,
              step = 1,
              animate = animationOptions(interval = 1500, loop = FALSE)
            ),
            fluidRow(
              column(
                3,
                strong("Best driver"),
                style = "font-size:18px;",
                align = "center",
                color = 'red',
                verbatimTextOutput("text1_F"),
                imageOutput("img1_F")
              ),
              column(
                4,
                strong("Most successful drivers"),
                style = "font-size:18px;",
                align = "center",
                plotOutput("plot1_F")
              ),
              column(
                5,
                strong("Drivers' championship winner"),
                style = "font-size:18px;",
                align = "center",
                plotOutput("plot2_F")
              )
              
            )
          ),
          
          box(
            title = "Drivers Profile",
            solidHeader = TRUE,
            status = "success",
            width = 12,
            collapsible = TRUE,
            selectInput(
              inputId = "Driver",
              label = "Select Driver",
              choices = drivers$name,
              width = "200px"
            ),
            fluidRow(
              infoBoxOutput("Age"),
              infoBoxOutput("Nationality"),
              infoBoxOutput("CurrentTeam"),
              infoBoxOutput("NumberofPoles"),
              infoBoxOutput("NumberofRaceWins"),
              infoBoxOutput("NumberofWDC")
            )
          )
        )
      ),
      
      
      #######
      #Page 5
      #######
      tabItem(tabName = "page5",
              h2(strong("The Circuits")),
              fluidRow(
                column(
                  12,
                  h4(""),
                  
                  box(
                    h5(
                      "The map below visualizes the circuit details of F1 in the last five years ",
                      em(strong(" (2017-2021)")),
                      "in different countries. You can first use the",
                      strong("Select Country"),
                      "function below to filter different circuits in different countries.",
                      " And then, you can use the",
                      strong("Select Year"),
                      "button below to see the circuit changes during these years."
                      ,
                      " Besides, you can click on the",
                      strong("icon"),
                      em(strong("(car icon)")),
                      "to see the information of this circuit.",
                      br(),
                      br(),
                      "At the bottom of the page, you can see",
                      strong("the top 3 classic F1 circuits to experience in 2022"),
                      "guidance."
                    ),
                    title = "User Guide",
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = FALSE,
                    width = "100%"
                  ),
                  hr(),
                  
                  fluidRow(
                    box(
                      title = "Circuit Details Map",
                      solidHeader = TRUE,
                      status = "primary",
                      width = 12,
                      collapsible = TRUE,
                      
                      column(
                        4,
                        selectInput(
                          "country",
                          "Select Country:",
                          choices = circuits_Z2$country,
                          multiple = FALSE
                        )
                      ),
                      
                      column(
                        4,
                        sliderInput(
                          "year_circuit",
                          "Select Year:",
                          min = 2017,
                          max = 2021,
                          value = 2017,
                          step = 1,
                          animate = animationOptions(1500, loop = FALSE)
                        )
                      ),
                      column(12, leafletOutput("circuits_map"))
                    )
                  ),
                  
                  hr(),
                  
                  fluidRow(
                    box(
                      title = "RANKED: Top 3 classic F1 circuits to experience in 2022",
                      solidHeader = TRUE,
                      status = "success",
                      width = 12,
                      collapsible = TRUE,
                      column(
                        4,
                        h6(
                          strong("1. Autodromo Nazionale di Monza"),
                          br(),
                          em("Hosted 71 World Championship races"),
                          br(),
                          strong("Why go?"),
                          "Electric atmosphere; stunning location in the heart of a large city park with easy access to the food, culture and history of Milan.",
                          a(
                            "Celebrate Monza's centenary at the 2022 Italian Grand Prix on 8-11 September.",
                            href = "https://f1experiences.com/2022-italian-grand-prix"
                          )
                        )
                      ),
                      
                      column(
                        4,
                        h6(
                          strong("2. Circuit de Monaco"),
                          br(),
                          em("Hosted 67 World Championship races"),
                          br(),
                          strong("Why go?"),
                          "Glamour, super yachts, people-watching and the chance to see modern F1 cars on the ragged edge at very close quarters."
                          ,
                          a(
                            "Secure an Official Ticket Package for the 2022 Monaco Grand Prix at Circuit de Monaco on 26-29 May.",
                            href = "https://f1experiences.com/2023-monaco-grand-prix"
                          )
                        )
                      ),
                      
                      column(
                        4,
                        h6(
                          strong("3. Silverstone"),
                          br(),
                          em("Hosted 56 World Championship races"),
                          br(),
                          strong("Why go?"),
                          "Passionate and knowledgeable local fans, friendly atmosphere and an action-packed event schedule, on and off the track."
                          ,
                          a(
                            "Official Ticket Packages are selling fast for the 2022 British Grand Prix at Silverstone on 30 June-3 July.",
                            href = "https://f1experiences.com/2022-british-grand-prix"
                          )
                        )
                      ),
                      
                      br(),
                      
                      column(4,
                             img(
                               src = "Italian_GP.jpg",
                               length = 250,
                               width = 180
                             )),
                      
                      column(4,
                             img(
                               src = "Monaco_F1.jpg",
                               length = 250,
                               width = 180
                             )),
                      
                      
                      column(4,
                             img(
                               src = "Brazil-2019.jpg",
                               length = 250,
                               width = 180
                             ))
                      
                    )
                  )
                )
              )),
      
      
      ######
      #Page6
      ######
      tabItem(
        tabName = "page6",
        h2(strong("Data Source")),
        fluidRow(column(
          12,
          h4(
            "Below are the data tables our team used in building this website.
                              The original data source is based on the website ",
            strong("Ergast Developer API"),
            "which provides historical record of motor racing data for non-commercial purposes.
                              You can access the source by visiting this",
            a("link.", href = "http://ergast.com/mrd/db/#csv ")
          )
        )),
        
        selectInput(
          "Data",
          "Select Data:",
          choices = c(
            "races" = 1,
            "results" = 2,
            "drivers" = 3,
            "constructors" = 4,
            "constructor_results" = 5,
            "constructor_standings" = 6,
            "constructor_info" = 7,
            "circuits" = 8
          ),
          multiple = FALSE
        ),
        dataTableOutput("myTable"),
      )
    )
  )
)


server <- function(input, output, session) {
  #page2
  results_ye <- subset(results, select = c(2:5, 7))
  wins_ye <- results_ye %>%
    filter(position == 1)
  
  final_cons_ye <- wins_ye %>%
    group_by(constructorId) %>%
    summarise(WinCount = n()) %>%
    filter(WinCount > 22) %>%
    left_join(constructors, by = "constructorId")
  
  
  final_dri_ye <- wins_ye %>%
    group_by(driverId) %>%
    summarise(WinCount = n()) %>%
    filter(WinCount >= 25) %>%
    left_join(drivers, by = "driverId")
  
  
  driver_ye <- subset(drivers, select = c(1:2, 5:6, 8))
  
  driver1_ye <- driver_ye %>%
    group_by(nationality) %>%
    summarise(Count = n()) %>%
    filter(Count >= 23)
  
  
  driver1_ye$fraction = driver1_ye$Count / sum(driver1_ye$Count)
  driver1_ye$ymax = cumsum(driver1_ye$fraction)
  driver1_ye$ymin = c(0, head(driver1_ye$ymax, n = -1))
  
  output$pg2_plot1 <- renderPlot({
    ggplot(final_dri_ye , aes(x = fct_rev(fct_reorder(
      driverRef, WinCount
    )), y = WinCount), color = y) +
      geom_col() +
      labs(title = 'Top 11 Drivers',
           y = "Number of Championships") +
      scale_fill_brewer(palette = 'Blues') +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()
      ) +
      geom_text(aes(label = WinCount), vjust = -0.5) +
      scale_x_discrete(
        'Driver Name',
        labels = c(
          'Lewis Hamilton',
          'Michael Schumacher',
          'Sebastian Vettel',
          'Alain Prost',
          'Ayrton Senna',
          'Fernando Alons',
          'Nigel Mansell',
          'Jackie Stewart',
          'Max Verstappen',
          'Niki Lauda',
          'Jim Clark'
        )
      )
  })
  
  output$pg2_plot2 = renderPlot({
    ggplot(final_cons_ye , aes(x = fct_rev(fct_reorder(name, WinCount)), y =
                                 WinCount), color = y) +
      geom_col() +
      labs(title = 'Top 10 Constructors',
           y = "Number of Championships",
           x = "Constructor") +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()
      ) +
      geom_text(aes(label = WinCount), vjust = -0.5) +
      scale_color_gradient(low = "red", high = "white")
    
  })
  
  output$pg2_plot3 = renderPlot({
    ggplot(driver1_ye,
           aes(
             ymax = ymax,
             ymin = ymin,
             xmax = 4,
             xmin = 3,
             fill = nationality
           )) +
      geom_rect() +
      coord_polar(theta = "y") + # Try to remove that to understand how the chart is built initially
      xlim(c(2, 4)) + # Try to remove that to see how to make a pie chart
      theme(
        panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank()
      ) +
      scale_fill_brewer(palette = "Reds") +
      geom_label_repel(
        aes(
          label = paste(round(fraction * 100, 0), "%"),
          x = 4,
          y = (ymin + ymax) / 2
        ),
        inherit.aes = F,
        show.legend = F,
        size = 3
      )
    
  })
  
  #page3
  
  #wordcloud
  v = constructors$name %>%
    VectorSource %>%
    Corpus %>%
    TermDocumentMatrix %>%
    as.matrix %>%
    rowSums %>%
    sort(decreasing = TRUE)
  
  d <- data.frame(word = names(v),
                  freq = v,
                  stringsAsFactors = FALSE)
  
  output$wordcloud <- renderPlot({
    wordcloud(
      names(v),
      v,
      scale = c(5, 0.5),
      min.freq = input$slider1,
      max.words = input$slider2,
      rot.per = input$slider3,
      random.order = F,
      colors = brewer.pal(8, "Dark2")
    )
  })
  
  
  output$plot_mu1 = renderPlot({
    nation_points <- constructors_mu %>%
      filter(points > 0) %>%
      group_by(nationality) %>%
      summarize(points_country = sum(points))
    
    ggplot(data = nation_points) +
      geom_col(mapping = aes(y = points_country, x = nationality, fill = nationality)) +
      labs(
        title = "Total Points Earned by Constructors across Countries",
        x = "Nationality",
        y = "Total Points Earned",
        fill = "Nationality"
      ) +
      theme_classic()
    
  })
  
  output$plot_mu2 = renderPlot({
    # points for each constructor
    constructor_points = constructors_mu %>%
      filter(year == input$ConstructorYear) %>%
      group_by(name.y) %>%
      summarise(total_points = sum(points)) %>%
      arrange(desc(total_points)) %>%
      head(10)
    ggplot(data = constructor_points) +
      geom_bar(mapping = aes(
        x = total_points,
        y = reorder(name.y, total_points),
        fill = name.y
      ),
      stat = 'Identity') +
      labs(
        title = "Top 10 Construtors' Performance",
        x = "Total Points Earned",
        y = "Constructors",
        fill = "Constructors",
        caption = "legend color matches constructor's color
        color reference from reddit"
      ) +
      scale_fill_manual(
        "Constructors",
        values = c(
          "Red Bull" = "#0600EF",
          "Ferrari" = "#DC0000",
          "Mercedes" = "#00D2BE",
          "Alpine F1 Team" = "#0090FF",
          "McLaren" = "#FF8700",
          "Alfa Romeo" = "#900000",
          "Haas F1 Team" = "#F62039",
          "Alpha Tauri" = "#2B4562",
          "Aston Martin" = "#006F62",
          "Force India" = "#FF80C7",
          "Williams" = "#7CCCED",
          "Renault" = "#FFE800",
          "Toro Rosso" = "#D0D2D3",
          "Sauber" = "#006EFF",
          "Racing Point" = "#FF69B4"
        )
      ) +
      theme_minimal()
    
    
  })
  
  output$text_mu1 = renderText({
    constructors_mu %>%
      filter(name.y == input$ConstructorsM) %>%
      distinct(nationality) %>%
      pull(nationality)
  })
  
  logo = reactive({
    constructors_mu %>%
      filter(name.y == input$ConstructorsM)
  })
  
  output$imageLogo1 = renderUI({
    champion = logo()$name.y[1]
    
    img(
      src = paste0(champion, ".png"),
      height = 100,
      width = 200,
      align = "center"
    )
    
    
  })
  
  output$text_mu2 = renderText({
    driver_mu %>%
      filter(year == "2021") %>%
      filter(name.y == input$ConstructorsM) %>%
      distinct(name) %>%
      slice(1) %>%
      pull()
    
    
  })
  
  output$text_mu3 = renderText({
    driver_mu %>%
      filter(year == "2021") %>%
      filter(name.y == input$ConstructorsM) %>%
      distinct(name) %>%
      slice(2) %>%
      pull()
    
  })
  
  output$plot_mu5 = renderPlotly({
    g = constructors_mu %>%
      group_by(year, name.y) %>%
      summarise(total_points = sum(points)) %>%
      group_by(year) %>%
      mutate(ranks = order(order(total_points, decreasing = TRUE))) %>%
      filter(name.y == input$ConstructorsM) %>%
      ggplot(mapping = aes(x = year, y = ranks)) +
      geom_line(size = 1.5, color = "red") +
      labs(title = "Constructor ranks over years",
           x = "year",
           y = "ranks") +
      theme_classic()
    ggplotly(g)
    
  })
  
  output$text_mu4 = renderText({
    constructor_info %>%
      filter(name == input$ConstructorsM) %>%
      pull(Info)
    
  })
  
  #page4
  driverstandings_in_year <- reactive({
    race_in_year <- races %>%
      filter(year == input$year) %>%
      pull(raceId)
    
    driver_standings %>%
      filter(raceId %in% race_in_year)
  })
  
  output$text1_F <- renderPrint({
    data4bestdriver <- driverstandings_in_year() %>%
      group_by(driverId) %>%
      summarise(wincount = sum(wins)) %>%
      left_join(drivers, by = "driverId") %>%
      slice_max(order_by = wincount, n = 1)
    
    data4bestdriver_wdc <- driver_standings %>%
      left_join(races, by = "raceId") %>%
      group_by(year, driverId) %>%
      summarise(sumpoints = sum(points)) %>%
      filter(year <= input$year) %>%
      group_by(year) %>%
      slice_max(order_by = sumpoints, n = 1) %>%
      ungroup() %>%
      count(driverId) %>%
      filter(driverId == data4bestdriver$driverId) %>%
      pull(n)
    
    cat(
      "",
      data4bestdriver$forename,
      "\n",
      data4bestdriver$surname,
      "\n",
      paste(data4bestdriver$wincount, "victories"),
      "\n",
      paste(data4bestdriver_wdc, "world championships")
    )
  })
  
  output$img1_F <- renderImage({
    data4bestdriver <- driverstandings_in_year() %>%
      group_by(driverId) %>%
      summarise(wincount = sum(wins)) %>%
      left_join(drivers, by = "driverId") %>%
      slice_max(order_by = wincount, n = 1)
    
    filename <-
      normalizePath(file.path('./www', paste0(data4bestdriver$name, '.jpg', sep =
                                                '')))
    list(src = filename,
         width = 275,
         height = 350)
  }, deleteFile = FALSE)
  
  output$plot1_F <- renderPlot({
    driverstandings_in_year() %>%
      group_by(driverId) %>%
      summarise(wincount = sum(wins)) %>%
      left_join(drivers, by = "driverId") %>%
      slice_max(order_by = wincount, n = 10) %>%
      filter(wincount != 0) %>%
      ggplot(aes(
        x = as_factor(name),
        y = wincount,
        fill = name
      )) +
      scale_fill_grey(5) +
      geom_col(show.legend = F) +
      labs(x = "", y = "") +
      theme_minimal() +
      theme(axis.text.x = element_text(
        angle = 90,
        vjust = 0.5,
        size = 15
      ))
  })
  
  output$plot2_F <- renderPlot({
    data4mostpoints <- driverstandings_in_year() %>%
      group_by(driverId) %>%
      summarise(sumpoints = sum(points)) %>%
      left_join(drivers, by = "driverId") %>%
      slice_max(order_by = sumpoints, n = 10) %>%
      filter(sumpoints != 0)
    
    dri_col <- brewer.pal(10, "OrRd")
    
    pie(
      data4mostpoints$sumpoints,
      labels = paste(data4mostpoints$name,
                     data4mostpoints$sumpoints),
      textinfo = "data4mostpoints$sumpoints",
      radius = 0.9,
      border = F,
      col = dri_col
    )
  })
  
  thedriver <- reactive({
    drivers %>%
      filter(name == input$Driver)
  })
  
  output$Age <- renderInfoBox({
    infoBox(
      "Age",
      year(Sys.Date()) - year(as.Date(thedriver()$dob)),
      icon = icon("calendar"),
      fill = TRUE,
      color = 'black'
    )
  })
  
  output$Nationality <- renderInfoBox({
    infoBox(
      "Nationality",
      thedriver()$nationality,
      icon = icon("globe"),
      color = 'black',
      fill = TRUE
    )
  })
  
  output$CurrentTeam <- renderInfoBox({
    infoBox(
      "Current Team",
      results %>%
        filter(driverId == thedriver()$driverId) %>%
        slice_max(order_by = raceId, n = 1) %>%
        left_join(constructors, by = "constructorId") %>%
        pull(name),
      icon = icon("group"),
      color = 'black',
      fill = TRUE
    )
  })
  
  output$NumberofPoles <- renderInfoBox({
    infoBox(
      "Number of Poles",
      driver_standings %>%
        filter(driverId == thedriver()$driverId) %>%
        filter(position == 1) %>%
        nrow(),
      icon = icon("running"),
      color = 'red',
      fill = TRUE
    )
  })
  
  output$NumberofRaceWins <- renderInfoBox({
    infoBox(
      "Number of Race Wins",
      driver_standings %>%
        filter(driverId == thedriver()$driverId) %>%
        pull(wins) %>%
        sum(),
      icon = icon("flag"),
      color = 'red',
      fill = TRUE
    )
  })
  
  output$NumberofWDC <- renderInfoBox({
    wdc <- driver_standings %>%
      left_join(races, by = "raceId") %>%
      group_by(year, driverId) %>%
      summarise(sumpoints = sum(points)) %>%
      group_by(year) %>%
      slice_max(order_by = sumpoints, n = 1) %>%
      ungroup() %>%
      count(driverId) %>%
      filter(driverId == thedriver()$driverId) %>%
      pull(n)
    infoBox(
      "Number of WDC",
      ifelse(length(wdc) == 0, 0, wdc),
      icon = icon("trophy"),
      color = 'red',
      fill = TRUE
    )
  })
  
  
  #page5
  
  # make icon
  F1_flag <-  makeIcon(
    iconUrl = "https://cdn-icons-png.flaticon.com/512/2418/2418809.png",
    iconWidth = 30,
    iconHeight = 30,
    iconAnchorX = 25,
    iconAnchorY = 34
  )
  
  
  output$circuits_map <- renderLeaflet({
    #c2 = circuits_Z2 %>% filter(country == input$country)
    # c1 = c2 %>% filter(year == input$year)
    #  c1 = circuits_Z2 %>% filter(year == input$year)
    circuits_Z2 = circuits_Z1 %>%
      filter(country == input$country) %>%
      filter(year == input$year_circuit) %>%
      mutate(
        popusText = paste(
          strong("Year:"),
          year,
          "<br>",
          strong("Country:"),
          country,
          "<br>",
          strong("Circuit name:"),
          name.x,
          "<br>",
          strong("Introduction:"),
          url.x
        )
      )
    
    leaflet (data = circuits_Z2) %>%
      addProviderTiles("OpenStreetMap.France") %>%
      addMarkers(
        lat =  ~ lat,
        lng = ~ lng,
        icon = F1_flag,
        popup = ~ popusText
      )
    
  })
  
  
  
  
  #page6
  output$myTable = renderDataTable({
    return(datatable(allData[[as.numeric(input$Data)]], rownames = FALSE))
  })
  
  
}
#shiny
shinyApp(ui = ui, server = server)
