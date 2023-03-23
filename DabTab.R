library('plotly')
library('shiny')
library('dplyr')
library('tidyr')
library('ggplot2')
library('shiny')
library('shinydashboard')
library('leaflet')
library('shinyWidgets')
library('bs4Dash')
library('shinyjs')
#library('shinytheme')
library(sodium)
library(DT)




button_color_css <- "
#DivCompClear, #FinderClear, #EnterTimes{
/* Change the background color of the update button
to blue. */
background: DodgerBlue;
/* Change the text size to 15 pixels. */
font-size: 15px;
}"

my_class <- "
.my-class {
background-color: #f2f2f2;
border: 1px solid #ddd;
padding: 10px;
}
"


# Define a function to read the data
read_data <- function(filepath) {
  read.csv(filepath, stringsAsFactors = FALSE)
}

# Define the plot1 function
plot1 <-function(df) {
  df[-1,] %>%
    filter(!is.na(member_casual) & !is.na(as.numeric(ride_duration_1))) %>%
    group_by(member_casual) %>%
    summarize(total_ride_duration = sum(as.numeric(ride_duration_1))) %>%
    ggplot(aes(x = member_casual, y = total_ride_duration, fill = member_casual)) +
    geom_bar(stat = "identity") +
    labs(x="Membership Type",y="Hours",title = "Ride Duration by Membership", subtitle = "My subtitle", caption = "Source: https://divvy-tripdata.s3.amazonaws.com/index.html")+
    theme(
      plot.title = element_text(hjust = 0.5, size=8, face="bold"),
      plot.subtitle = element_text(hjust=0.5),
      plot.caption = element_text(hjust = 0, face="italic"),
      plot.caption.position = "plot",
      axis.title.x = element_text(),
      axis.title.y = element_text(),
      legend.position = "none",
      plot.margin = margin(t = 20, r = 10, b = 40, l = 10)
    )+
    coord_flip()
}

# Define the plot2 function
plot2 <- function(df) {
  df[-1,] %>%
    filter(!is.na(weekday) & !is.na(ride_duration_1)) %>%
    group_by(member_casual, weekday) %>%
    summarize(total_ride_duration = sum(as.numeric(ride_duration_1))) %>%
    ggplot(aes(x = weekday, y = total_ride_duration, group = member_casual, color = member_casual)) +
    geom_line() +
    geom_point(size = 2) +
    ggtitle("Ride duration of member and casual riders by weekday") +
    xlab("Weekday") +
    ylab("Ride Duration (by hours)") +
    scale_x_discrete(labels = c("Mon.", "Tue.", "Wed.", "Thurs.", "Fri.", "Sat.", "Sun.")) +
    facet_wrap(~member_casual) +
    theme_bw()
}


#Difine the plot3 function(perform on different set of data)

plot3 <- function(df){
  
  # Find the top ten stations with the highest count values
  top_stations <- df %>% 
    top_n(10, count) 
  
  # Create the leaflet map with the base tiles
  leaflet() %>% 
    addTiles() %>% 
    # Add circle markers for the top ten stations
    addCircleMarkers(data = top_stations, 
                     ~pt_lng, ~pt_lat, 
                     color = "red", 
                     radius = 10,
                     stroke = FALSE,
                     fillOpacity = 1) %>%
    # Add popup text for all markers
    addMarkers(data = df, 
               ~pt_lng, ~pt_lat, 
               popup = paste("Station ID: ", df$station_id, "<br>",
                             "Frequency: ", df$count)) 
  
}

# Define the UI
if (interactive()) {
  # Main login screen
  library(shiny)
  library(shinyjs)
  
  library(shiny)
  library(shinyjs)
  
  library(shiny)
  library(shinyjs)
  
  loginUI<-
    div(
      id = "loginpage", 
      style = "width: 80%; max-width: 400px; margin: 0 auto; padding: 20px; 
          background-color: #7ecc49; color: #fff; font-family: 'Helvetica Neue', 
          Helvetica, Arial, sans-serif; border: 1px solid #d1d5da; border-radius: 3px;",
      tags$img(src = "github_logo.png", height = 50, width = 50, style = "margin: 0 auto; display: block;"),
      tags$h1("Sign in to DabTab", class = "h4 text-gray-dark text-center mb-4 font-weight-normal"),
      wellPanel(
        div(
          class = "form-group",
          tagList(
            "Username or email address", 
            textInput("userName", "", placeholder = "Enter your username or email address"
            )
          )
        ),
        br(),
        div(
          class = "form-group",
          tagList(
            "Password", 
            passwordInput("passwd", "", placeholder = "Enter your password")
          )
        ),
        br(),
        div(
          class = "form-group",
          actionButton("login", "Sign in", class = "btn btn-primary btn-block btn-lg")
        ),
        shinyjs::hidden(
          div(id = "nomatch",
              tags$p("Oops! Incorrect username or password!",
                     style = "color: red; font-weight: 600; 
                      padding-top: 5px; font-size: 16px;", 
                     class = "text-center"))
        )
      ),
      div(
        class = "text-center",
        tags$a(href = "#", "Forgot password?", class = "forgot-password-link text-gray-dark")
      ),
      tags$style(
        "body {
      background-image: url('https://som.edu.vn/wp-content/uploads/2022/12/khai-niem-va-thuat-ngu-data-science.jpeg');
      background-size: cover;
      background-position: center center;
    }"
      )
    )
  
  
  
  
  mainUI<-function(id) {
    ns <- NS(id)
    
    navbarPage("Dynamic Application",
               position = c("static-top"),
               tabPanel("Dashboard",                      
                        tags$style(button_color_css),
                        id = ns("dashboard-tab"),
                        useShinyjs(),
                        sidebarLayout(
                          sidebarPanel(
                            shinyjs::hidden(selectInput(ns("datasets"), "Select a dataset", 
                                                        choices = c("FEBRUARY 2022" = "modified_data.csv",
                                                                    "AUGUST 2022" = "modified_data_2.csv",
                                                                    "DECEMBER 2022" = "modified_data_3.csv"))),
                            radioButtons(ns("plots"), "Select a plot", 
                                         choices = c("BARCHART" = "plot 1", 
                                                     "LINECHART" = "plot 2",
                                                     "MAP"= "plot 3")),
                            shinyjs::hidden(downloadButton(ns("downloadPlot"), "Download plot")),
                            
                            shinyjs::hidden(
                              selectInput(ns("map_datasets"), "Select a dataset for the map", 
                                          choices = c("FEBRUARY 2022" = "station_data_1.csv",
                                                      "AUGUST 2022" = "station_data_2.csv",
                                                      "DECEMBER 2022" = "station_data_3.csv"))
                            )
                          ),
                          mainPanel(
                            tabsetPanel(type = "tab",
                                        tabPanel("Help",tags$video(
                                          width = "320",
                                          height = "240",
                                          controls = TRUE,
                                          HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/jsfB5gQO9QM" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen></iframe>'),
                                          type = "video/mp4"
                                        )
                                        )),
                            tabPanel("Visualization",plotlyOutput(ns("output_plot")),
                                     leafletOutput(ns("my_map"))
                            )
                          ))
                        ,
                        em("Postive and negative percentages indicate an increase and decrease from the baseline period (median value between January 3 and February 6, 2020) respectively.")
               ),
               navbarMenu("Messages",
                          icon = icon("chart-bar"),
                          tabPanel("Explore more",fluid=TRUE,
                                   tags$style(button_color_css),
                                   titlePanel("Explore more")
                          ),
                          tabPanel("Ask me",fluid=TRUE,
                                   tags$style(button_color_css),
                                   titlePanel("Ask me")
                          )
                          
               ),
               navbarMenu("Menu",
                          icon = icon("bars"),
                          tabPanel("Username", titlePanel(ns(uiOutput("username_tab")))),
                          tabPanel("Permission", icon = icon("user-tag"),titlePanel(uiOutput(ns("permission_tab")))),
                          tabPanel("Settings", icon = icon("cog")),
                          tabPanel("Support", icon = icon("hands-helping")),
                          tabPanel("More Info", icon = icon("info-circle")),
                          tabPanel("Logout", icon = icon("sign-out-alt"),uiOutput(ns("logoutbtn")))
                          
               )
               ,
               fluidRow(
                 
                 div(class = "my-class",
                     fluidRow(
                       column(12, h1("Section 7")),
                       column(12, h1("Section 8")),
                       column(12, h1("Section 9")),
                       column(12, h1("Section 10")),
                       column(12, h1("Section 11")),
                       column(12, h1("Section 12")),
                       column(12, h1("Section 13")),
                       column(12, h1("Section 14")),
                       column(12, h1("Section 15")),
                       column(12, h1("Section 16")),
                       column(12, h1("Section 17")),
                       column(12, h1("Section 18")),
                       column(12, h1("Section 19")),
                       column(12, h1("Section 20")),
                       column(12, h1("Section 21")),
                       column(12, h1("Section 22")),
                       column(12, h1("Section 23")),
                     )
                 )))
    
  }
  
  
  
  
  ui<-fluidPage(theme = bslib::bs_theme(bootswatch = "darkly"),
                tags$head(
                  tags$style(
                    HTML(
                      "
              .navbar-nav {
              display: flex;
              justify-content: end;
              width: 100%;
              }
              
              
              .navbar-nav:last-child {
              margin-right: 0;
              margin-left: auto;
              
              }
              
              .navbar-nav:last-of-type .nav-item:nth-child(2) {
              margin-right: 0;
              margin-left: auto;
              
              }
              
              " )
                  )),
                
                mainUI("dabtab")
                
  )
  
  
  
  
  
  myRenderServer <-  function(input, output, session){
    ns <- session$ns
    df1 <- reactiveValues(data=NULL)
    df2<-reactiveValues(data=NULL)
    
    data <- reactive({
      req(input$datasets)
      filepath = paste0("DS_01/", input$datasets)
      read.csv(filepath)
    })
    
    map_data <- reactive({
      req(input$map_datasets)
      filepath = paste0("DS_01/", input$map_datasets)
      read.csv(filepath)
    })
    
    observe({
      df1$data <- data
      df2$data <-map_data
    })
    
    
    
    output$plot1 <- renderPlotly({
      if (is.null(data())) return(NULL)
      if (input$plots == "plot 1") {
        plot1(df1) %>% ggplotly()}
      
    })
    
    
    output$plot2 <- renderPlotly({
      if (is.null(data())) return(NULL)
      if (input$plots == "plot 2") {
        plot2(df1) %>% ggplotly()}
      
    })
    
    
    output$my_map <- renderLeaflet({
      if (is.null(map_data())) return(NULL)
      if (input$plots == "plot 3") {
        plot3(df2)
      }
    })
    output$logoutbtn <- renderUI({
      req(USER$login)
      tags$li(a(icon("fa fa-sign-out"), "Logout", 
                href="javascript:window.location.reload(true)"),
              class = "dropdown", 
              style = "background-color: #eee !important; border: 0;
                  font-weight: bold; margin:5px; padding: 10px;")
    })
    
    
    
    # Render the username and permission tabs
    output$username_tab <- renderUI({
      if (USER$login==TRUE) {
        credentials[,"username"][which(credentials$username_id==input$userName)]
        # render username info
      }
    })
    
    output$permission_tab <- renderUI({
      if (USER$login==TRUE) {
        credentials[,"permission"][which(credentials$username_id==input$userName)]
        
      }
    })
    
    observeEvent(input$plots, {
      if (input$plots == "plot 3") {
        shinyjs::show("map_datasets")
        shinyjs::hide("downloadPlot")
        shinyjs::hide("datasets")
      } else {
        shinyjs::hide("map_datasets")
        shinyjs::show("downloadPlot")
        shinyjs::show("datasets")
        
      }
    })
    
    output$downloadPlot <- downloadHandler(
      filename = function() {
        paste0("viz",input$plots, ".png")
      },
      content = function(file) {
        # Pass the plot to be downloaded to the png function
        png(file)
        # Render the appropriate plot depending on the selected option
        if (input$plots == "plot 1") {
          print(plot1(data()))
        } else if(input$plots == "plot 2") {
          print(plot2(data()))
        }
        # Close the plot device to save the plot
        dev.off()
      }
      
      
    )
  }        
  
  
  
  # Define the server
  server <- function(input, output, session) {
    
    credentials = data.frame(
      username_id = c("myuser", "myuser1"),
      passod   = sapply(c("mypass", "mypass1"),password_store),
      permission  = c("basic", "advanced"), 
      stringsAsFactors = F
    )
    
    shinyjs::useShinyjs()
    login =FALSE
    
    USER <- reactiveValues(login=login)
    observe({ 
      if (USER$login == FALSE) {
        if (!is.null(input$login)) {
          if (input$login > 0) {
            Username <- isolate(input$userName)
            Password <- isolate(input$passwd)
            if(length(which(credentials$username_id==Username))==1) { 
              pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
              pasverify <- password_verify(pasmatch, Password)
              if(pasverify) {
                USER$login <- TRUE
              } else {
                shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
                shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
              }
            } else {
              shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
          } 
        }
      }    
    })
    
    
    observeEvent(input$login,{
      if (USER$login == TRUE) { 
        # Show appropriate page based on permission level
        if (credentials[,"permission"][which(credentials$username_id==input$userName)]=="basic"){
          callModule(myRenderServer, "dabtab")    
        } 
      } else {
        renderUI({loginUI})
      }
    })
    
    
  }
  shinyApp(ui, server)
}

