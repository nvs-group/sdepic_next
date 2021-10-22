# EPIC Demo 10/19/2021 ----
# version 1.0
# New build two page scenario implementation
# 01 Load Libraries ----

library(shiny)  # ver1.5 June23, 2020
library(shinydashboard)  # ver0.7.1 October 17, 2018
library(shinydashboardPlus)  # ver0.7.5 July 15, 2020
library(shinyWidgets)  # ver0.5.4 October 6, 2020

library(tidyverse)  # ver1.3.0 November 21, 2019

#library(Hmisc)  # ver4.4-1 August 11, 2020
library(rmarkdown)  # ver2.5 October 21, 2020
library(DT)  # ver0.16 October 14, 2020
library(tools)  # ver4.0.3
library(shinyjs)  # ver2.0.0 September 9, 2020
library(sodium)  # ver1.1 March 30, 2017
library(shinyBS)  # ver0.62 devtools::install_github("ebailey78/shinyBS", ref="shinyBS3")
#library(extrafont)
library(shinyLP)  # ver1.1.2 April 25, 2018
library(rdrop2)  # ver0.8.2.1 August 5, 2020
library(shinyalert)  # ver2.0.0 September 12, 2020
library(emayili)  # ver0.4.4 October 2, 2020
library(shinyhelper)  # ver0.3.2 November 9, 2020
#library(pryr) # ver0.1.4 February 18, 2018
library(shinythemes)
#library(tinytex)  # ver0.26 September 22, 2020
#library(devtools)
#library(bs4Dash)

#library(NCmisc)
# 02 Declare Variables ----
# UI Variables
Sys.setenv(TZ='America/New_York')
token <- readRDS("signin.rds")
drop_auth(rdstoken = "signin.rds")

drop_download("responses/cred.rds", overwrite = TRUE)
credentials <- readRDS("cred.rds")

credentials <- remove_rownames(credentials)
unlink("cred.rds")

# Server Variables
#info_items <- readRDS("info.rds")

#smtp <- server(host = info_items$host, 
#               port = info_items$port,
#               username = info_items$username,
#               password = info_items$password)
version_number <- "1.0"
current_date2 <- format(Sys.Date(), "%d, %B %Y, %A")

backbone <- readRDS("data2/Backbone.rds")
degree_master <- readRDS("data2/AW_Degree.rds")
cips <- readRDS("data2/CIP_List.rds")
occupation_master2 <- readRDS("data2/Occupations.rds")
occupation_master <- occupation_master2 %>% select(-c(Entry_Code, Entry_Degree))
req_degree_master <- readRDS("data2/req_degree_master.rds")
major_master <- unique(backbone %>% select( "UNITID", "CIPCODE", "AWLEVEL", "CTOTALT") %>% filter(UNITID != "No Match"))
major_master <- tibble::rowid_to_column(major_master, "ID")

state_abbr <- readRDS("data/state_abbr.rds")
#school_master <- readRDS("data/Schools.rds")
occupation_temp <- readRDS("data2/occupation_temp.rds")
alt_title_all <- readRDS("data2/alt_title_all.rds")
back_temp2 <- backbone %>% mutate(New_ED = Entry_Code)
back_temp2 <- back_temp2 %>% mutate(New_ED = ifelse(as.character(New_ED) == "01", "0.1", as.character(New_ED)))
back_temp2 <- back_temp2 %>% mutate(New_ED = ifelse(as.character(New_ED) == "05", "0.5", as.character(New_ED)))

req_degree_xwalk <- unique(back_temp2 %>% select( AWLEVEL, Entry_Code, New_ED)) %>% 
  filter(Entry_Code != "No Match" , AWLEVEL !="No Match") %>% mutate(GREATER = (as.numeric(AWLEVEL) >= as.numeric(New_ED)))

rm(back_temp2)
occupation_list <- occupation_master %>% select(OCCNAME) %>% arrange(OCCNAME)
degree_list <- degree_master %>% select(LEVELName)

major_list <- cips %>% select(CIPNAME) %>% arrange(CIPNAME)
school_master2 <- readRDS("data2/School2.rds")
school_list <- school_master2 %>% select(INSTNM) %>% arrange(INSTNM)
req_degree_list <- req_degree_master %>% select(Entry_Degree)
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}
# 04.1 Header ----
header <- dashboardHeader( uiOutput("logoutbtn"))

# 04.2 Sidebar ----
sidebar <- dashboardSidebar(
  tags$img(src = 'BlueLogo_Final.png',contentType = "image/png", style = "width: 100%; height: 180px;align:center;"),
  uiOutput("sidebarpanel")
  )

# 04.3 Body ----
body <- dashboardBody(
  tags$head(tags$meta( name="viewport", content="width=device-width, initial-scale=1.0")),
  tags$head(includeScript("returnClick.js")),
  tags$script(HTML("$('body').addClass('fixed');")),
  tags$head(tags$script('
      // Define function to set height of tab items
      setHeight = function() {
        var window_height = $(window).height();
        var header_height = $(".main-header").height();
        var boxHeight = window_height - header_height - 25;
        $(".display_window").height(boxHeight);
      };
      // Set input$box_height when the connection is established
      $(document).on("shiny:connected", function(event) {
        setHeight();
      });
      // Refresh the box height on every window resize event    
      $(window).on("resize", function(){
        setHeight();
        
      });
    ')),
  useShinyjs(),
  useShinyalert(),
#  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "grid.css")),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
  tags$head(tags$style(HTML(".my_class {font-weight: bold;color:#eaedef;}"))),
  tags$head(tags$style(HTML(".my_style_1{ background-image: url('landingpage.jpg');}"))),
  # 04.4 TABS ----
  tabItems(
    tabItem(tabName = "landingpage",
            box(width = 12, class = "display_window my_style_1",
                    fluidRow(column(width = 1),
                             column(width = 5,class = "first_item_pad",
                                    h1("Education and Career Planning at Your Fingertips"),
                                    br(),br(),
                                    h2("EPIC is a premier educational planning software designed to help students pave their 
                                      way towards the best educational experience
                                      for them. Sign up today and personalize your
                                      account to create an epic future!"),
                                    br(),br()
                                    )),
                    fluidRow(column(width = 1), align = "center",
                             column(width = 2,
                                           actionButton(inputId = "signinbtn", label = "Sign In", class = "landing_button")),
                                    column(width = 2,
                                    actionButton(inputId = "add_user", label = "Create Account", class = "landing_button"))
                             ),
            bsModal(id = "loginpage", title = strong("SIGN IN", style ="color:black ;font-size:120%;"), trigger = "signinbtn", width = '400px', 
                    close.button = FALSE,
                    wellPanel(
                   #   tags$h2("", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                      textInput("userName", placeholder="Username", label = tagList(icon("user"), "Username"), width = "100%"),
                      passwordInput("passwd", placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
                      br(),
                      div(align = "center",actionButton("login", "SIGN IN", class = "landing_button")),
                      br(),
                      div(align = "center", actionButton(inputId = "passwd_reset", label = "Forgot Password?", class = "forgot_password"))
                      )
            ),
            bsModal(id = "createpage", title = strong("SIGN UP", style ="color:black ;font-size:120%;"), trigger = "add_user", width = '400px', 
                    close.button = TRUE,
                    wellPanel(
                      textInput("Names_add", labelMandatory("Name")),
                      textInput("Email_add", labelMandatory("Email")),
                      selectInput("State_add", "State of Residence",
                                  choices = state_abbr$State, selectize = FALSE,size = 4),
                      textInput("Password_add", labelMandatory("Password")), 
                      br(),
                      div(align = "center", actionButton("go", "Create Account", class = "landing_button"))
                    )
            )
            )
            ), 

    tabItem(tabName = "analytics",
            box(width = 12, class = "display_window window_format", 
                    fluidRow(
                    actionButton(inputId = "load_analytics", label = "Load Data"),
                    actionButton(inputId = "get_csv", label = "Get csv file")
                    ),
                    dataTableOutput(outputId = "dbox_data")
                    )
    ),
    tabItem(tabName = "dashboard",
            box(width = 12, class = "display_window window_format",
                    fluidRow(column(width = 12, class = "date_favorite",
                                        h4(current_date2))
                    ),
                    fluidRow(column(width = 8, style = "margin:0;padding:0;",
                                    div(class = "header_label2",h2(uiOutput("welcome")),h3("We are here to help make education and career planning easier.")
                                        )),
                             column(width = 4,
                                    div(class = "tutorial", align= "center")
 #                                       h3("TUTORIAL"),
#                                        actionButton(inputId = "tutorial-video", label = "",
#                                                     style =  "background: url('play-video.svg');background-repeat:no-repeat;"
##                                        
#                                    ))
                                    ),
                    fluidRow(column(width = 3,
                                           div(class = "dash_header", h4("BUILD")),
                                           div(class = "dash_buttons",
                                               actionButton(inputId = "build_new2", label = "",icon = icon("plus"), class = "dash_new_build"),
                                               h4("Build New Scenario..."))),
                                    column(width = 3,
                                           div(class = "dash_header", h4("EXPLORE")),
                                           div(class = "dashboard_explore school_top", 
                                               h3("Schools"),
                                               div(class = "dashbutton",
                                               h5("Learn more about a", br(), "schools cost, size,", br(), 
                                                  "administration, and", br(),"more!")),
                                           actionButton(inputId = "dash_schools", label = div("Explore", icon("chevron-circle-right")))
                                           )),
                                    column(width = 3,
                                           div(class = "dash_header", h4(br())), 
                                          div(class = "dashboard_explore major_top", 
                                              h3("Majors"),
                                              div(class = "dashbutton",
                                                  h5("Learn more about", br(), "the best majors",br(), 
                                                    "and where they can",br(), "lead to!")),
                                              actionButton(inputId = "dash_majors", label = div("Explore", icon("chevron-circle-right"))))),
                                    column(width = 3,
                                           div(class = "dash_header", h4(br())),
                                           div(class = "dashboard_explore occupation_top", 
                                               h3("Occupations"),
                                               div(class = "dashbutton",
                                                   h5("Discover jobs with",br(), "the highest salaries",br(),
                                                      "and the most",br(), "sought after jobs!")),
                                               actionButton(inputId = "dash_occupations", label = div("Explore", icon("chevron-circle-right")))))
                                        
                    ),
                    
                    
                    fluidRow(column(width = 12,
                                    div(class = "dash_header",
                                        h4("FAVORITES")))),
                    fluidRow(column(width = 12,
                                    div(id = "favorite_box",
                                        uiOutput(outputId = "favorite_container")
                                    ))),
                    fluidRow(column(width = 12,
                                    div(class = "dash_header",
                                        h4("COMPARISONS")))),
                    bsModal(id = "detail_modal", title = strong("Details", style ="color:black ;font-size:120%;"), trigger = "detail", size = "large", 
                              uiOutput(outputId = "detail_info")
                            ),
                    fluidRow(column(width = 12,
                                    div(id = "comparison_box",
                                        plotOutput(outputId = "comparison_container")
                                    )))
            )
    )),
    tabItem(tabName = "build",
            box(width = 12, class = "display_window window_format", 
                    fluidRow(column(width = 1),
                             column(width = 10,
                                    div(class = "build-header build_header1", h2("Lets Get Started!")),
                                    div(class = "build-header text_regular_color", h1("Where would you like to begin?")),
                                    div(class = "build-header text_regular_color", 
                                        h4("Select a category to begin building your scenario. We recommend starting with the category",
                                          br(),
                                          "that you have the strongest preference in.") 
                                          )
                                    )
                             ),
                    fluidRow(
                             column(width = 7, 
                                    div(class = "build-grouping",
                                      fluidRow(column(width = 2),
                                               column(width = 8,
                                                      class = "build-grouping-title" ,h2("EDUCATION PLAN")),
                                               column(width = 2)),
                                      fluidRow(column(width = 4,
                                                      align = "center", style = "padding: 0px;",
                                                      div(class = "choices1", uiOutput("school_select")),
                                                      div(class = "infochip", uiOutput("schoolchoice"))
                                                      ),
                                               column(width = 4, align = "center",style = "padding: 0px;",
                                                      div(class = "choices1", uiOutput("major_select")),
                                                      div(class = "infochip", uiOutput("majorchoice"))
                                                      ),
                                               column(width = 4, align = "center",style = "padding: 0px;",
                                                      div(class = "choices1", uiOutput("degree_select")),
                                                      div(class = "infochip", uiOutput("degreechoice"))
                                                      ))
                                    )),
                             column(width = 5, 
                                    div(class = "build-grouping",
                                      fluidRow(column(width = 1),
                                               column(width = 10,
                                                      class = "build-grouping-title" ,h2("CAREER PLAN")),
                                               column(width = 1)
                                               ),
                                      fluidRow(column(width = 6,
                                                      align = "center",style = "padding: 0px;",
                                                      div(class = "choices1", uiOutput("occupation_select")),
                                                      div(class = "infochip", uiOutput("occupationchoice"))
                                                      ),
                                                      column(width = 6, align = "center",style = "padding: 0px;",
                                                            div(class = "choices1", uiOutput("req_degree_select")),
                                                            div(class = "infochip", uiOutput("req_degreechoice"))
                                                            )
                                               )
                                    ),
                             column(width = 1)
                            )),
                    fluidRow(
                             column(width = 11,align = "right",
                                    actionButton(inputId = "next_button", label = "Build Scenario"))
                             
                    )
                    ),
                    bsModal(id = "school_modal", title = strong(h4("Search Schools",style="color:#e2ac24;")), trigger = "school_select", size = "large", 
                            
                                   div(
                                     div(class = "modal_input",
                                         selectInput(inputId = "sm_school", label = "School", choices = c("All" = '',school_list$INSTNM), width = "100%")),
                                     div(class = "modal_input",
                                         selectInput(inputId = "sm_state", label = "State", choices = c("All" = '',state_abbr$State), width = "100%")),
#                                     div(class = "modal_input",
#                                         sliderInput(inputId = "sm_room_board", label = "Room and Board",
#                                                     value = c(0,max(school_master2$ROOM_BOARD)),
#                                                     min = 0,
#                                                     max = max(school_master2$ROOM_BOARD),
#                                                     width = "100%")),
                                     div(class = "modal_input",
                                         sliderInput(inputId = "sm_annual_hi", label = "Annual Cost High",
                                                     value = c(0,max(school_master2$TotCstOutHi)),
                                                     min = 0,
                                                     max = max(school_master2$TotCstOutHi),
                                                     width = "100%")),
                                     div(class = "modal_input",
                                         sliderInput(inputId = "sm_annual_lo", label = "Annual Cost Low",
                                                     value = c(0,max(school_master2$TotCstOutLo)),
                                                     min = 0,
                                                     max = max(school_master2$TotCstOutLo),
                                                     width = "100%"))
                                   ),
                                   div(style=" width: '100%';", DTOutput('school_table_modal')),
                                   footer = tagList(
                                     div(style = "float:left;display: inline-block;vertical-align:top;",actionButton(inputId = "sm_favorite", label = "Add to Schools"))
                                   )
                            ),
                    bsModal(id = "major_modal", title = strong("Search Majors", style = "color:#37b749; font-size:120%;"), trigger = "major_select", size = "large", 
                            
                              div(
                                div(class = "modal_input",
                                    selectInput(inputId = "mm_major", label = "Major", choices = c("All" = '', major_list$CIPNAME), width = "100%")),
                                div(class = "modal_input",
                                    selectInput(inputId = "mm_occupation", label = "Occupation", choices = c("All" = '', occupation_list$OCCNAME), width = "100%")),
                                div(class = "modal_input",
                                    selectInput(inputId = "mm_entry_degree", label = "Entry Degree", choices = c("All" = '', unique(req_degree_master$Entry_Degree)), width = "100%"))
                                
                              ),
                              div(style=" width: '100%';margin-left:0px;padding:0px;", DTOutput('major_table_modal')),
                            footer = tagList(
                              div(style = "float:left;display: inline-block;vertical-align:top;",actionButton(inputId = "mm_favorite", label = "Add to Majors"))
                            )
                            ),
                    bsModal(id = "occupation_modal", title = strong("Search Occupations", style ="color:#477ddd;font-size:120%;"), trigger = "occupation_select", size = "large", 
                            
                              div(
                                div(class = "modal_input",
                                    selectInput(inputId = "om_occupation", label = "Occupation", choices = c("All" = '', occupation_master$OCCNAME),
                                                width = "100%")),
                                div(class = "modal_input",
                                    selectInput(inputId = "om_entry_degree", label = "Entry Degree", choices = c("All" = '', req_degree_master$Entry_Degree),
                                                width = "100%")),
                                div(class = "modal_input",
                                    selectInput(inputId = "om_required_exp", label = "Required Experience", choices = c("All" = '', occupation_master$Experience), 
                                                width = "100%")),
                                div(class = "modal_input",
                                    sliderInput(inputId = "om_starting_salary", label = "Starting Salary",
                                                value = c(0,round(max(occupation_master$X17p),0)),
                                                min = 0, 
                                                max = round(max(occupation_master$X17p),0), 
                                                width = "100%")),
                                div(class = "modal_input",
                                    sliderInput(inputId = "om_growth_rate", label = "Growth Rate",
                                                value = c(round(min(occupation_master$EmplyPC),0),round(max(occupation_master$EmplyPC),0)),
                                                min = round(min(occupation_master$EmplyPC),0),
                                                max = round(max(occupation_master$EmplyPC),0),
                                                width = "100%"))
                              ),
                              div(style=" width: '100%';", DTOutput('occupation_table_modal')),
                            footer = tagList(
                              div(style = "float:left;display: inline-block;vertical-align:top;",actionButton(inputId = "om_favorite", label = "Add to Occupations"))
                            )
                    ),
                    bsModal(id = "degree_modal", title = strong("Degrees", style ="color:#e22b2b ;font-size:120%;"), trigger = "degree_select", size = "large",
                            
                              div(class = "modal_input",
                                  selectInput(inputId = "dm_degree", label = "Degree", choices = c("All" = '', degree_list$LEVELName),
                                              width = "100%")),
                              div(align = "center", class = "smaller_modal_table", DTOutput('degree_table_modal')),
                            footer = tagList(
                                div(style = "float:left;display: inline-block;vertical-align:top;",actionButton(inputId = "dm_favorite", label = "Add to Degrees"))
                            )
                    ),
            bsModal(id = "req_degree_modal", title = strong("Entry Degree", style ="color:#cc33ff ;font-size:120%;"), trigger = "req_degree_select", size = "large",
                    
                      div(class = "modal_input",
                          selectInput(inputId = "rm_req_degree", label = "Entry Degree", choices = c("All" = '', req_degree_master$Entry_Degree),
                                      width = "100%")),
                      div(align = "center", class = "smaller_modal_table", DTOutput('req_degree_table_modal')),
                    footer = tagList(
                      div(style = "float:left;display: inline-block;vertical-align:top;",actionButton(inputId = "rm_favorite", label = "Add to Entry Degrees"))
                    )
            )
            
            ),
    tabItem(tabName = "scenario",
            box(width = 12, class = "display_window window_format",  
                    fluidRow(column(width = 12,class = "date_favorite",
                                    uiOutput("favorites"))),
                    fluidRow(column(width = 2, style = "padding: 0 5px",
                                    div(class = "scenario_text1",
                                        h1("Save Options")),
                                    div(class = "scenario_text3",
                                        h2("Select and add your favorite options. Your 
                                        favorite options will be saved to your dashboard
                                        where you can view comparison graphs and
                                        return on investment reports.")
                                    ),
                                    uiOutput("scenario_available")),
                             column(width = 10,
                                    div(id = "dt_scenario", 
                                        div(class = "header_label", style = "margin: 0;", p("Scenario Table")),
                                        dataTableOutput(outputId = "scenario_table")))
                    ),
                    fluidRow(
                             column(width = 4, align = "center",
                                    actionButton(inputId = "add_favorite_button", label = "Add to Favorites", class = "scenario-button")),
                             column(width = 4, align = "center",
                                        actionButton(inputId = "build_new_button", label = "Build New Scenario...",
                                                     icon = icon("fas fa-plus-circle"), class = "scenario-button")
                                    ),
                             column(width = 4,align = "center",
                                    actionButton(inputId = "return_dashboard_button", label = "Return to Dashboard", class = "scenario-button")
                                    )
                             )
                    )
            ),
    tabItem(tabName = "explore",
            box(width = 12, class = "display_window window_format",
                    h1("Explore"))),
    tabItem(tabName = "school",
            box(width = 12, class = "display_window window_format", 
                   column(width = 2,style = "padding:0px;margin:0px;margin-top:2em;",
                      div(class = "explore-header", style = "background-color:#e2ac24;",
                          p("Explore Schools")),
                      div(align = 'left',
                      style = "background-color:white;padding-left: 5px;padding-right: 5px;margin-left:0px;",
                        selectInput(inputId = "es_school", label = "School", choices = c("All" = '',school_master2$INSTNM), width = "95%"),
                        selectInput(inputId = "es_state", label = "State", choices = c("All" = '',state_abbr$State), width = "95%"),
#                        sliderInput(inputId = "es_room_board", label = "Room and Board",
#                                    value = c(0,max(school_master2$ROOM_BOARD)),
#                                    min = 0,
#                                    max = max(school_master2$ROOM_BOARD),
#                                    width = "100%"),
                        sliderInput(inputId = "es_annual_hi", label = "Annual Cost High",
                                    value = c(0,max(school_master2$TotCstOutHi)),
                                    min = 0,
                                    max = max(school_master2$TotCstOutHi),
                                    width = "95%"),
                        sliderInput(inputId = "es_annual_lo", label = "Annual Cost Low",
                                    value = c(0,max(school_master2$TotCstOutLo)),
                                    min = 0,
                                    max = max(school_master2$TotCstOutLo),
                                    width = "95%"),
                      actionButton(inputId = "school_favorite", label = "Add to Favorites")
                      )
                      ),
                      column(width = 10,
                          div(class = "explore-table", 
                              tabBox(width = 12, id = "es_tab", side = "left",
                                     tabPanel("Table", div(class = "school_header_label", p("School Table")),
                                              DT::dataTableOutput(
                                       outputId = "explore_school_table", width = "100%", height = "auto")),
                                     tabPanel("Favorites", div(id = "school_favorite_box",
                                                  uiOutput(outputId = "school_favorite_container")
                                              ))
                                     )
                              ))
            )
    ),
    tabItem(tabName = "major",
            box(width = 12, class = "display_window window_format", 
                    column(width = 2, style ="padding:0px;margin:0px;",
                           div(class= "explore-header", style = "background-color:#37b749;",
                               p("Explore Majors")),
                           div(align = 'left',
                               style = "background-color:white;padding-left: 5px;margin-left:0px;",
                               selectInput(inputId = "em_major", label = "Major", choices = c("All" = '', major_list$CIPNAME),
                                           width = "100%"),
                               selectInput(inputId = "em_degree", label = "Degree", choices = c("All" = '', degree_list$LEVELName),
                                           width = "100%"),
                               selectInput(inputId = "em_school", label = "School Name", choices = c("All" = '', school_list$INSTNM), 
                                           width = "100%"),
                               actionButton(inputId = "major_favorite", label = "Add to Favorites")
                           )
                      ),
                        column(width = 10,
                          div(class = "explore-table", 
                              tabBox(width = 12, id = "em_tab", side = "left",
                                     tabPanel("Table", div(class = "major_header_label", p("Major Table")),
                                              DT::dataTableOutput(outputId = "em_table",
                                                                  width = "100%",
                                                                  height = "auto")
                                              ),
                                     tabPanel("Favorites",
                                              div(id = "major_favorite_box",
                                                  uiOutput(outputId = "major_favorite_container")
                                              ))
                                     )
                                    ))
                    )
            ),
    tabItem(tabName = "occupation",
            box(width = 12, class = "display_window window_format",
                    column(width = 2, style ="padding:0px;margin:0px;",
                           div(class ="explore-header", style = "background-color:#477ddd;",
                               p("Explore Occupations")),
                           div(align = 'left',
                               style = "background-color:white;padding-left: 5px;margin-left:0px;",
                               textInput(inputId = "occupation_text", label = "Occupation", width = '100%'),
                               selectInput(inputId = "eo_entry_degree", label = "Entry Degree", choices = c("All" = '', req_degree_master$Entry_Degree),
                                           width = "100%"),
                               selectInput(inputId = "eo_required_exp", label = "Required Experience", choices = c("All" = '', occupation_master$Experience), 
                                           width = "100%"),
                               sliderInput(inputId = "eo_starting_salary", label = "Starting Salary",
                                           value = c(0,round(max(occupation_master$X17p),0)),
                                           min = 0, 
                                           max = round(max(occupation_master$X17p),0), 
                                           width = "100%"),
                               sliderInput(inputId = "eo_growth_rate", label = "Growth Rate",
                                           value = c(round(min(occupation_master$EmplyPC),0),round(max(occupation_master$EmplyPC),0)),
                                           min = round(min(occupation_master$EmplyPC),0),
                                           max = round(max(occupation_master$EmplyPC),0),
                                           width = "100%"),
                               actionButton(inputId = "occupation_favorite", label = "Add to Favorites")
                               )
                      ),
                    column(width = 10,
                           div(class = "explore-table", 
                               tabBox(width = 12, id = "eo_tab", side = "left",
                                      tabPanel("Table", div(class = "occupation_header_label", p("Occupation Table")),
                                               DT::dataTableOutput(
                                                 outputId = "explore_occupation_table",
                                                 width = "100%",
                                                 height = "auto"
                                               )),
                                      tabPanel("Favorites", 
                                               div(id = "occupation_favorite_box",
                                                   uiOutput(outputId = "occupation_favorite_container")
                                               ))
                                      )
                           ))
                    )
            ),
    tabItem(tabName = "profile",
            box(width = 12, class = "display_window window_format", 
                    fluidPage(
                      fluidRow(column(width = 2),
                               column(width = 8,
                      div(align = 'left', style = "font-size: 16px; padding-top: 0px; margin-top:2em",
                          h2("Profile")),
                      textInput(inputId = "profile_name", label = "Name", value = ''),
                      textInput(inputId = "profile_email", label = "Email", value = ''),
                      selectInput(inputId = "profile_state", label = "State of Residence", 
                                  choices = state_abbr$State, selected = ''),
                      textInput(inputId = "profile_activity", label = "Activities/Interests"),
                      textAreaInput(inputId = "profile_biography", label = "Biography", rows = 6)
                               )
                    ))
                    )),
    tabItem(tabName = "settings",
            box(width = 12, class = "display_window window_format", 
                    h1("Settings"))),
    tabItem(tabName = "tools",
            box(width = 12, class = "display_window window_format",
                    fluidRow(column(width = 2, align = "center",
                                    div(class = "tools-url-list",style = "margin-top:25px;",p("Explore Career Occupations:"),
                                      actionButton(inputId = "onetonline", "O*Net Online", class = "tools-url-link")),
                                    div(class = "tools-url-list",p("Compare Schools:"),
                                      actionButton(inputId = "collegescorecard", "College Score Card", class = "tools-url-link",
                                                   onclick ="window.open('https://collegescorecard.ed.gov/', '_blank')")),
                                    div(class = "tools-url-list",p("School Details:"),
                                      actionButton(inputId = "collegenavigator", "College Navigator", class = "tools-url-link",
                                                   onclick ="window.open('https://nces.ed.gov/collegenavigator/', '_blank')")),
                                    div(class = "tools-url-list",p("Skills Assessment:"),
                                      actionButton(inputId = "careeronestop", "Career One Stop", class = "tools-url-link")),
                                    div(class = "tools-url-list",p("Advanced Skills Assessment:"),
                                      actionButton(inputId = "wowi", "Wowi Career Assessment", class = "tools-url-link")),
                                    div(class = "tools-url-list",p("What do you want to do for a living?"), 
                                      actionButton(inputId = "mynextmove", "My Next Move", class = "tools-url-link")),
                                    div(class = "tools-url-list",p("BLS site for exploring, finding, and pursuing a career path:"), 
                                    actionButton(inputId = "careeronestop1", "Career One Stop", class = "tools-url-link"))
                                    ),
                              column(width = 10, 
                                     div(class = "video_display_window",
                              htmlOutput("web_page")
                              ))
                              )
                    )
            ),
    tabItem(tabName = "tutorials",
            box(width = 12, class = "display_window window_format", 
                    h1("Tutorials"))),
    tabItem(tabName = "help",
            box(width = 12, class = "display_window window_format", 
                    h1("Help")))
  )
)

# 04.5 UI ----
ui <- dashboardPage(header, sidebar, body)

# 05 SERVER ----
server <- function(input, output, session) {
 
# GLOBAL SERVER VARIABLES ---------- 
  observe({shinyjs::runjs("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';")}) 
  login <- FALSE
  USER <- reactiveValues(login = login)  
  output$logoutbtn <- renderUI({
    req(USER$login)
    tags$li(a(icon("fa fa-sign-out"), "Logout", 
              href="javascript:window.location.reload(true)"),
            class = "dropdown", 
            style = "background-color: #eee !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;")
  })
  number_favorites <- 0
  major_filter <- cips
  school_filter <- school_master2 %>% select("UNITID", "INSTNM")
  occupation_filter <- occupation_master %>% select("OCCNAME", "OCCCODE")
  degree_filter <- degree_master
  req_degree_filter <- req_degree_master
  
  school_scenario <- school_master2 
  occupation_scenario <- occupation_master %>% select("OCCNAME", "OCCCODE", "X17p", "Experience","MedOccF")
  major_scenario <- cips
  degree_scenario <- degree_master
  req_degree_scenario <- req_degree_master
  fav_in_app <- tibble("ID" = character(), "UNITID" = character(), "CIPCODE" = character(), "AWLEVEL" = character(), "OCCCODE" = character())
  current_user <- tibble()
  
# Landing Page ---------------------

  output$sidebarpanel <- renderUI({
    if (USER$login == TRUE ){ 
      if (credentials[,"permission"][which(credentials$username_id==input$userName)]=="advanced") {
        sidebarMenu(id = "tabs", 
                    menuItem("Profile", tabName = "profile", icon = icon("user")),
                    menuItem("Dashboard", tabName = "dashboard", icon = icon("tachometer-alt"),selected = TRUE),
                    menuItem("Scenarios", tabName = "scenario_main", icon = icon("plus"),
                             menuSubItem("Build Scenario", tabName = "build", icon = icon("plus")),
                             menuSubItem("View/Edit Scenario", tabName = "scenario", icon = icon("plus"))
                    ),
                    
                    menuItem("Explore", tabName = "explore", icon = icon("fas fa-compass"),
                             menuSubItem("Explore Schools", tabName = "school"),
                             menuSubItem("Explore Occupations", tabName = "occupation"),
                             menuSubItem("Explore Majors", tabName = "major")),
                                        menuItem("Settings", tabName = "settings", icon = icon("cog")),
                    menuItem("Tutorials", tabName = "tutorials", icon = icon("video")),
                    menuItem("Tools", tabName = "tools", icon = icon("wrench",lib = "glyphicon"))
                                      #  menuItem("Help", tabName = "help", icon = icon("question-circle")),
                                      #  searchInput(inputId = "search", label = "", placeholder = "Search EPIC")
        )
      } else if (credentials[,"permission"][which(credentials$username_id==input$userName)]=="admin") {
        sidebarMenu(id = "tabs",
                    menuItem("Analytics", tabName = "analytics", icon = icon("fas fa-chart-bar")),
                    menuItem("Profile", tabName = "profile", icon = icon("user")),
                    menuItem("Dashboard", tabName = "dashboard", icon = icon("tachometer-alt"),selected = TRUE),
                    menuItem("Scenarios", tabName = "scenario_main", icon = icon("plus"),
                             menuSubItem("Build Scenario", tabName = "build", icon = icon("plus")),
                             menuSubItem("View/Edit Scenario", tabName = "scenario", icon = icon("plus"))
                    ),
                    menuItem("Explore", tabName = "explore", icon = icon("fas fa-compass"),
                             menuSubItem("Explore Schools", tabName = "school"),
                             menuSubItem("Explore Occupations", tabName = "occupation"),
                             menuSubItem("Explore Majors", tabName = "major")),
                    #                    menuItem("Settings", tabName = "settings", icon = icon("cog")),
                    menuItem("Tutorials", tabName = "tutorials", icon = icon("video")),
                    menuItem("Tools", tabName = "tools", icon = icon("wrench",lib = "glyphicon"))
                    #                    menuItem("Help", tabName = "help", icon = icon("question-circle")),
                    #                    searchInput(inputId = "search", label = "", placeholder = "Search EPIC")
        )
      }
    } else {
      sidebarMenu(id = "tabs",
                  menuItem("Landing Page", tabName = "landingpage")
      )
    }
  })
  
# Login ----------------------------
  observe({
    if (USER$login == TRUE ){
      updateTabItems(session, "tabs", "dashboard") } else {
        updateTabItems(session, "tabs", "landingpage")
      }
  }) 
  visitor_data <- tibble("visitor" = character(),
                         "user" = character(),
                         "time_stamp" = character(),
                         "page" = character())
  analytic_data <- tibble("visitor" = character(),
                          "user" = character(),
                          "time_stamp" = character(),
                          "page" = character())
  
  user_scenarios <- tibble("user" = character(),
                           "scenario" = character(),
                           "source" = character(),
                           "category" = character(),
                           "id" = character())
  
  user_favorites <- tibble("user" = character(),
                           "school" = character(),
                           "major" = character(),
                           "occupation" = character(),
                           "degree" = character())
  visitor_num <- numeric()
  pro_name <- character()
  pro_email <- character()
  pro_state <- character()
  pro_user <- character()
  scenario_file <- paste0(pro_user,"scenario.rds")
  favorite_file <- paste0(pro_user,"favorite.rds")
  # FUNCTIONS 
  isValidEmail <- function(x) {
    grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), ignore.case=TRUE)
  }
#  observe({
#    set_user_var2()
#    update_profile()
#    
#  })
  observeEvent(input$login,{
    if (USER$login == FALSE) {
      Username <- isolate(input$userName)
      Password <- isolate(input$passwd)
      if(length(which(credentials$username_id==Username))==1) { 
        pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
        pasverify <- password_verify(pasmatch, Password)
        if(pasverify) {
          toggleModal(session, "loginpage", toggle = "toggle")
          USER$login <- TRUE
          current_user <<- credentials %>% filter(passod %in% pasmatch)
          #          get_visitor()
          set_user_var()
          update_profile()
          #          log_event()
          get_files()
          output$welcome <- renderUI({paste0("Welcome ",pro_name,"!")})
        } else {
          shinyalert(title = "Username or Password incorrect", type = "error")
        }
      } else {
        shinyalert(title = "Username or Password incorrect", type = "error")
      }
    } 
  })  
 
  get_visitor <- function() {
    drop_download("responses/counter.rds", overwrite = TRUE)
    visitor <- readRDS("counter.rds")
    visitor_num <<- visitor + 1
    saveRDS(visitor_num, "counter.rds")
    drop_upload("counter.rds", path = "responses")
    unlink("counter.rds")
  }
  update_profile <- function() {
    updateTextInput(session, inputId = "profile_name", label = "Name", value = pro_name)
    updateTextInput(session, inputId = "profile_email", label = "Email", value = pro_email)
    updateSelectInput(session, inputId = "profile_state", label = "State of Residence", choices = state_abbr$State, selected = pro_state)
  }
  get_files <- function() {
    filename2 <- paste0("responses/",scenario_file)
    if(drop_exists(filename2) == TRUE) {
      drop_download(filename2, overwrite = TRUE)
      user_scenarios <<- readRDS(scenario_file)
      clean_scenario()
      school_favorite_cards()
      major_favorite_cards()
      occupation_favorite_cards()
      update_scenario()
    } 
    filename3 <- paste0("responses/",favorite_file)
    if(drop_exists(filename3) == TRUE) {
      drop_download(filename3, overwrite = TRUE)
      user_favorites <<- readRDS(favorite_file)
      clean_favorite()
      favor_temp <- user_favorites 
      if(NROW(favor_temp) > 0) {
        for(i in 1:NROW(favor_temp)){
          scen_temp <- backbone %>% filter(UNITID %in% favor_temp$school[i], CIPCODE %in% favor_temp$major[i],
                                           OCCCODE %in% favor_temp$occupation[i], AWLEVEL %in% favor_temp$degree[i]) %>%
            select(ID, UNITID, CIPCODE, AWLEVEL, OCCCODE)
          fav_in_app <<- rbind(fav_in_app, scen_temp)
        }
        favorite_cards()
      } 
    }
  }
  #  log_event <- function() {
  #    current_time <- Sys.time()
  #    log_temp <- tibble("visitor" = visitor_num,"user" = pro_user, "time_stamp" = current_time, "page" = input$tabs)
  #    visitor_data <<- rbind(visitor_data, log_temp)
  #    print(visitor_data)
  #  }
  #  observeEvent(input$tabs, {
  #    log_event()
  #  })
  
  set_user_var <- function() {
    pro_name <<- current_user$username_id[1]
    pro_email <<- current_user$email[1]
    pro_state <<- current_user$state[1]
    pro_user <<- current_user$id[1]
    scenario_file <<- paste0(pro_user,"scenario.rds")
    favorite_file <<- paste0(pro_user,"favorite.rds")
  }
  set_user_var2 <- function() {
    pro_name <<- "Free User"
    pro_email <<- "email@email.com"
    pro_state <<- "Virginia"
    pro_user <<- 0
    user_scenarios <<- readRDS("Demoscenario.rds")
    user_favorites <<- readRDS("Demofavorite.rds")
    update_scenario()
    favor_temp <- user_favorites 
    if(NROW(favor_temp) > 0) {
      for(i in 1:NROW(favor_temp)){
        scen_temp <- backbone %>% filter(UNITID %in% favor_temp$school[i], CIPCODE %in% favor_temp$major[i],
                                         OCCCODE %in% favor_temp$occupation[i], AWLEVEL %in% favor_temp$degree[i]) %>%
          select(ID, UNITID, CIPCODE, AWLEVEL, OCCCODE)
        fav_in_app <<- rbind(fav_in_app, scen_temp)
      }
      favorite_cards()
    } 
  }
  clean_cred <- function() {
    unlink("cred.rds")
  }
  clean_scenario <- function() {
    unlink(scenario_file)
  }
  clean_favorite <- function() {
    unlink(favorite_file)
  }
# Create New Account ---------------

  observeEvent(input$add_user, {
    ### This is the pop up board for input a new row
    showModal(modalDialog(title = "Add a new account",
                          textInput(paste0("Names_add", input$Add_row_head), labelMandatory("Name")),
                          textInput(paste0("Email_add", input$Add_row_head), labelMandatory("Email")),
                          selectInput(paste0("State_add", input$Add_row_head), "State of Residence",
                                      choices = state_abbr$State, selectize = FALSE,size = 4),
                          textInput(paste0("Password_add", input$Add_row_head), labelMandatory("Password")), 
                          actionButton("go", "Create account"),
                          easyClose = TRUE, footer = NULL ))
  })
  
  observeEvent(input$go, {
    new_row=data.frame(
      id = paste0("USER", NROW(credentials) + 1),
      username_id = input$Names_add,
      email = input$Email_add,
      state = input$State_add,
      passod = sapply(input$Password_add,password_store),
      permission = "advanced",
      stringsAsFactors = FALSE
    )
    drop_download("responses/cred.rds", overwrite = TRUE)
    creds <- readRDS("cred.rds")
    creds <- remove_rownames(credentials)
    creds <- rbind(creds, new_row)
    saveRDS(creds, "cred.rds")
    drop_upload("cred.rds", path = "responses")
    credentials <<- creds
    clean_cred()
    toggleModal(session, modalId = "createpage", toggle = "toggle")
#    removeModal()
  })
# Analytics ------------------------
  #  observeEvent(input$load_analytics, {
  #    filesInfo <- drop_dir("analytics")
  #    filePaths <- filesInfo$path_lower
  #     analytic_data <<- lapply(filePaths, drop_read_csv, stringsAsFactors = FALSE)
  #    analytic_data <<- do.call(rbind, analytic_data)
  #    output$dbox_data <- renderDataTable({
  #       DT::datatable(data = analytic_data)   
  #    })
  #  })
  
  #  observeEvent(input$get_csv, {
  #    write.csv(analytic_data, "analytics_data.csv")
  #    email <- envelope() %>%
  #      from("romriellsteven@gmail.com") %>%
  #      to("lcchapman@gmail.com") %>% 
  #      subject("This is the csv file from the app") %>%
  #      text("Congrats") %>%
  #      attachment(c("analytics_data.csv"))
  #    smtp(email, verbose = TRUE)
  #    unlink("analytics_data.csv")
  #  }) 
# Profile --------------------------
  observe({
    if(input$profile_state != ''){
      residence_temp <- state_abbr %>% filter(State %in% input$profile_state) %>% select(STABBR)
      residence_temp <- as.character(residence_temp)
      for(i in 1:nrow(school_scenario)){
        school_scenario$CostLo[i] <<- ifelse(school_scenario$STABBR[i] == residence_temp,school_scenario$TotCstInLo[i],
                                             school_scenario$TotCstOutLo[i])
        school_scenario$CostHi[i] <<- ifelse(school_scenario$STABBR[i] == residence_temp, school_scenario$TotCstInHi[i],
                                             school_scenario$TotCstOutHi[i])
      } 
    }else {
      school_scenario$CostLo <<- school_scenario$TotCstOutLo
      school_scenario$CostHi <<- school_scenario$TotCstOutHi
    }
    for(i in 1:nrow(school_scenario)){
      school_scenario$ADMPCT[i] <<- ifelse(school_scenario$APPLCN[i] != 0, paste(round(100 * (school_scenario$ADMSSN[i]/school_scenario$APPLCN[i]), 0), "%", sep = ""),
                                           0)
    }
    school_scenario <<- school_scenario %>% mutate(FTETOT = FTEUG + FTEGD)
    school_scenario <<- school_scenario %>% mutate(PERCENT150 = paste(round(100 * pc150, 0), "%", sep = ""))
    school_scenario <<- school_scenario %>% mutate(InStateGnt = paste(round(IGRNT_P, 0), "%", sep = ""))
    school_modal_master <<- school_scenario
  })
  
  
# Dashboard ------------------------
 
  checkList <- list()
  graphList <-vector(mode = "list")
  obsList <- list()
  detailList <- list()
  graph_parameters <- ggplot() + 
    xlab('Years') +
    ylab('Total Earnings in Thousands') +
    labs(title = 'Cummulative Cash Flow') +
    theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 16)) +
    theme(legend.direction = "vertical", legend.position = c(0.16,0.76)) +
    theme(legend.title = element_text( size=12), legend.text=element_text(size=12)) 
  
  favorite_cards <- function() {
    scenario_temp <- fav_in_app
    scenario_temp <- left_join(scenario_temp, school_scenario, by = "UNITID")
    scenario_temp <- left_join(scenario_temp, major_scenario, by = "CIPCODE")
    scenario_temp <- left_join(scenario_temp, degree_scenario, by = "AWLEVEL")
    scenario_temp <- left_join(scenario_temp, occupation_scenario, by = "OCCCODE")
    
    if(NROW(scenario_temp) > 0){
      output$favorite_container <- renderUI({
        args <- lapply(1:NROW(scenario_temp), function(x) card2(x,
                                                                name = scenario_temp$ID[x],
                                                                school = scenario_temp$INSTNM[x],
                                                                major = scenario_temp$CIPNAME[x],
                                                                degree = scenario_temp$LEVELName[x],
                                                                occupation = scenario_temp$OCCNAME[x]))
        args$cellArgs <- list(
          style = "width: 260px;height: auto;margin: 5px;margin-bottom:50px;margin-left:25px;line-height: '110%';"
        )
        do.call(shiny::flowLayout, args)
      })
    } else {
      output$favorite_container <- renderUI({NULL})
      obsList <<- list()
    }
  }  
  card2 <- function(x,name,school,major,degree,occupation) {
    make_observer(name)
    make_check_obs(name)
    make_detail_obs(name,school,major,degree,occupation)
    div(class = "card",
        div(style = "display:inline-block;vertical-align:top;width: 260px;margin:0px;padding:0px;",
            splitLayout(cellWidths = c("17%","60%","23%"),
                        div(style = "margin-bottom:22px;margin-top:-10px;padding:0px;",
                            checkboxInput(inputId = paste0("check",name), label = "", width = "20px")),
                        h3("Option ",x,style = "margin-top: 0px;"),
                        actionButton(inputId = paste0("button",name),label = "", icon = icon("trash"),
                                     style = "margin:0px;color:#b30000;padding:2px 4px;"))),
        hr(style = "margin:0px;border-top: 1px solid #000000;"),
        br(),
        div(style = "font-family: Arial;font-size:16px; color:#999999;",
            p(school),
            p(major),
            p(degree),
            p(occupation)),
        div(class = "view_detail",actionButton(inputId = paste0("details",name), label = "View Details",
                                               style = "border:0px;color:#4fa0f7;background-color:white;margin-left:0px;padding:0px;
                                                 border-bottom: 1px solid #4fa0f7;font-size:16px;"))
    )
  }
  make_observer <- function(name){
    btName <- paste0("button",name)
    if(is.null(obsList[[btName]])){
      obsList[[btName]] <<- observeEvent(input[[btName]], {
        fav_in_app <<- fav_in_app %>% filter(!(ID == name))
        update_fav_num()
        update_user_favs()
        favorite_cards()
        graphList <<-vector(mode = "list")
        build_graph()
      }, ignoreInit = TRUE, once = TRUE)
    }
  }
  make_check_obs <- function(x) {
    checkNm <- paste0("check",x)
    if(is.null(checkList[[checkNm]])){
      checkList[[checkNm]] <<- observeEvent(input[[checkNm]], {
        if(input[[checkNm]] == TRUE){
          graphList <<- append(graphList, x)
          build_graph()
        } else if(NROW(graphList) > 0){
          graphList <<- graphList[-which(graphList %in% x)] 
          build_graph()
        }
      },ignoreInit = TRUE)
    }
  }
  make_detail_obs <- function(name,school,major,degree,occupation) {
    detailNm <- paste0("details",name)
    if(is.null(detailList[[detailNm]])){
      detailList[[detailNm]] <<- observeEvent(input[[detailNm]], {
        school_id <- filter(school_filter, INSTNM %in% school) %>% select(UNITID)
        occupation_id <- filter(occupation_filter, OCCNAME %in% occupation) %>% select(OCCCODE)
        degree_id <- filter(degree_filter, LEVELName %in% degree) %>% select(AWLEVEL)
        major_id <- filter(major_filter, CIPNAME %in% major) %>% select(CIPCODE)
        
        detail_temp <- backbone %>% filter(UNITID %in% school_id, OCCCODE %in% occupation_id, AWLEVEL %in% degree_id, CIPCODE %in% major_id)
        detail_temp <- left_join(detail_temp, school_scenario, by = "UNITID")
        detail_temp <- left_join(detail_temp, cips, by = "CIPCODE")
        detail_temp <- left_join(detail_temp, degree_master, by = "AWLEVEL")
        detail_temp <- left_join(detail_temp, occupation_master, by = "OCCCODE")
        detail_temp <- left_join(detail_temp, req_degree_master, by = "Entry_Code")
        
        toggleModal(session, "detail_modal", toggle = "toggle")
        output$detail_info <- renderUI({
          div(
            fluidRow(column(width = 4,
                            h3(style =  "text-decoration: underline;",  "School"),
                            p(style = "line-height:1.5;",  detail_temp$INSTNM,br(),
                              paste0(detail_temp$CITY,", ", detail_temp$STABBR[1]),br(),
                              paste0("New Enrollment :  ", detail_temp$ENRLT[1]),br(),
                              paste0("Total Enrollment :  ", detail_temp$FTEUG[1] + detail_temp$FTEGD[1]),br(),
                              paste0("Cost Lo:  ", "$",detail_temp$CostLo[1]),br(),
                              paste0("Cost Hi:  ", "$", detail_temp$CostHi[1]))
            ),
            column(width = 4,
                   h3(style =  "text-decoration: underline;","Occupation"),
                   p(detail_temp$OCCNAME,br(),
                     paste0("Growth Rate(%) :  ", detail_temp$EmplyPC[1]),br(),
                     paste0("Entry Degree :  ", detail_temp$Entry_Degree[1]),br(),
                     paste0("Required Experience :  ", detail_temp$Experience[1]),br(),
                     paste0("Median Salary :  ", "$",detail_temp$X50p[1]))
            ),
            column(width = 4,
                   h3(style =  "text-decoration: underline;","Major"),
                   p(detail_temp$CIPNAME[1],br(),
                     paste0("Degree :  ", detail_temp$LEVELName[1]),br(),
                     paste0("# of Degrees :  ", detail_temp$CTOTALT[1]))
            )
            )
          )
        })
      },ignoreInit = TRUE)
    }
  }
  build_graph <- function() {
    if(length(graphList) == 0){
      output$comparison_container <- renderPlot(height = 650,{
        graph_parameters
      })
    } else {
      graph_backbone <- backbone %>% filter(ID %in% graphList)
      graph_backbone <- left_join(graph_backbone, school_scenario, by = "UNITID")
      graph_backbone <- left_join(graph_backbone, major_scenario, by = "CIPCODE")
      graph_backbone <- left_join(graph_backbone, degree_scenario, by = "AWLEVEL")
      graph_backbone <- left_join(graph_backbone, occupation_scenario, by = "OCCCODE")
      graph_backbone <- graph_backbone %>% select("ID", "INSTNM", "CIPNAME", "OCCNAME", "LEVELName", "X17p", "Years","TotCstOutHi","MedOccF")
      
      #Beginning Loop to create data
      graph_scenario <- tibble("Years" = numeric(), "Scenario" = character(), "Running_Total" = numeric(), stringsAsFactors = FALSE)
      
      for(j in(1:nrow(graph_backbone))){
        years <- 0
        running_total <- 0
        TTuition <- 0
        TIncome <- 0
        if(graph_backbone$Years[j] != 0 & !is.na(graph_backbone$Years[j])){
          if(graph_backbone$Years[j] == 0.8) {graph_backbone$Years[j] <- 1}
          if(graph_backbone$Years[j] == 1.5) {graph_backbone$Years[j] <- 2}
          for(i in(1:graph_backbone$Years[j])){
            scenario <- paste0(str_trunc(graph_backbone$OCCNAME[j],30,"right"),"\n",
                               str_trunc(graph_backbone$INSTNM[j],30,"right"),"\n",
                               str_trunc(graph_backbone$LEVELName[j],30,"right"),"\n")
            if(years == 0){
              graph_temp <- tibble("Years" = years, "Scenario" = scenario, "Running_Total" = running_total)
              graph_scenario <- rbind(graph_scenario, graph_temp)
            }
            
            years <- years + 1
            running_total <- running_total - graph_backbone$TotCstOutHi[j]
            running_total <- round(running_total, digits = 0)
            graph_temp <- tibble("Years" = years, "Scenario" = scenario, "Running_Total" = running_total)
            graph_scenario <- rbind(graph_scenario, graph_temp)
          }
          TTuition <- (-running_total)
        } else {
          scenario <- paste0(str_trunc(graph_backbone$OCCNAME[j],30,"right"))
          TTuition <- 1
        }
        if(years == 0){
          graph_temp <- tibble("Years" = years, "Scenario" = scenario, "Running_Total" = running_total)
          graph_scenario <- rbind(graph_scenario, graph_temp)
        }
        years <- years + 1
        year_change <- graph_backbone$X17p[j]
        running_total <- running_total + year_change
        TIncome <- TIncome + year_change
        graph_temp <- tibble("Years" = years, "Scenario" = scenario, "Running_Total" = running_total)
        graph_scenario <- rbind(graph_scenario, graph_temp)
        for(k in(1:(50 - years))){
          years <- years + 1
          tf <- (1 + ((0.00002 * ((k) ^ 2)) - 0.0023 * (k) + 0.0603))
          year_change <- year_change * tf * graph_backbone$MedOccF[j]
          TIncome <- TIncome + year_change
          TIncome <- round(TIncome, digits = 0)
          running_total <- running_total + year_change
          running_total <- round(running_total, digits = 0)
          graph_temp <- tibble("Years" = years, "Scenario" = scenario, "Running_Total" = running_total)
          graph_scenario <- rbind(graph_scenario, graph_temp)
        }
        # add graph data        
        graph_scene <- graph_scenario %>% filter(scenario == scenario)
        graph_parameters <- graph_parameters + geom_line(data = graph_scene, aes(x = Years, y = Running_Total/1000, colour = Scenario),size = 1.5, show.legend = TRUE)
      }
      output$comparison_container <- renderPlot(height = 650,{
        graph_parameters
      })
    }
  }
  observe({
    output$comparison_container <- renderPlot(height = 650, {
      graph_parameters
    })
  })
  observeEvent(input$build_new2, {
    clear_build()
    updateTabItems(session, "tabs", selected = "build")
  })
  observeEvent(input$dash_schools, {
    updateTabItems(session, "tabs", selected = "school")
  })
  observeEvent(input$dash_majors, {
    updateTabItems(session, "tabs", selected = "major")
  })
  observeEvent(input$dash_occupations, {
    updateTabItems(session, "tabs", selected = "occupation")
  })
# Build Scenario -------------------
  temp_choice <- reactiveValues(school_status = 1, major_status = 1, occupation_status = 1, degree_status = 1, reqdegree_status = 1)
  school_build_selected <- vector(mode = "list")
  degree_build_selected <- vector(mode = "list")
  occupation_build_selected <- vector(mode = "list")
  major_build_selected <- vector(mode = "list")
  req_degree_build_selected <- vector(mode = "list")
  main_modal_reactive <- reactiveVal(0)
  edit_scenario <- 0
  
  clear_schoolchoices <- function() {
    school_build_selected <<- list()
    school_cards()
  }
  clear_majorchoices <- function() {
    major_build_selected <<- list()
    major_cards()
  }
  clear_occupationchoices <- function() {
    occupation_build_selected <<- list()
    occupation_cards()
  }
  clear_degreechoices <- function() {
    degree_build_selected <<- list()
    degree_cards()
  }
  clear_req_degreechoices <- function() {
    req_degree_build_selected <<- list()
    req_degree_cards()
  }
  add_scenario <- function(){
    if(edit_scenario < 1){
      scen_temp <- new_scenario_name
    } else  {
      scen_temp <- temp_scenario
    }
    if(!is_empty(school_build_selected)){
      for(i in 1:NROW(school_build_selected)){
        school_temp <- filter(school_filter, INSTNM %in% school_build_selected) %>% select(UNITID)
        sch_temp <- tibble("user" = pro_user, "scenario" = scen_temp, "source" = "build",  "category" = "school", "id" = school_temp$UNITID[i] )
        user_scenarios <<- rbind(user_scenarios, sch_temp)
      }
    }
    if(!is_empty(major_build_selected)){
      for(i in 1:NROW(major_build_selected)){
        major_temp <- filter(major_filter, CIPNAME %in% major_build_selected) %>% select(CIPCODE)
        maj_temp <- tibble("user" = pro_user, "scenario" = scen_temp, "source" = "build", "category" = "major", "id" = major_temp$CIPCODE[i] )
        user_scenarios <<- rbind(user_scenarios, maj_temp)
      }
    }
    if(!is_empty(occupation_build_selected)){
      for(i in 1:NROW(occupation_build_selected)){
        occupation_temp <- filter(occupation_filter, OCCNAME %in% occupation_build_selected) %>% select(OCCCODE)
        occ_temp <- tibble("user" = pro_user, "scenario" = scen_temp, "source" = "build", "category" = "occupation", "id" = occupation_temp$OCCCODE[i] )
        user_scenarios <<- rbind(user_scenarios, occ_temp)
      }
    }
    if(!is_empty(degree_build_selected)){
      for(i in 1:NROW(degree_build_selected)){
        degree_temp <- filter(degree_filter, LEVELName %in% degree_build_selected) %>% select(AWLEVEL)
        deg_temp <- tibble("user" = pro_user, "scenario" = scen_temp, "source" = "build", "category" = "degree", "id" = degree_temp$AWLEVEL[i] )
        user_scenarios <<- rbind(user_scenarios, deg_temp)
      }
    }
    if(!is_empty(req_degree_build_selected)){
      for(i in 1:NROW(req_degree_build_selected)){
        req_degree_temp <- filter(req_degree_filter, Entry_Degree %in% req_degree_build_selected) %>% select(Entry_Code)
        req_deg_temp <- tibble("user" = pro_user, "scenario" = scen_temp, "source" = "build", "category" = "required", "id" = req_degree_temp$Entry_Code[i] )
        user_scenarios <<- rbind(user_scenarios, req_deg_temp)
      }
    }
  }
    observe({
      if(temp_choice$school_status == 1){
        output$school_select <- renderUI({
          actionButton(inputId = "school_button", label = "", 
                       style ="background: url('epicButtons-18.svg');background-repeat:no-repeat;border-bottom:7px solid; border-bottom-color: #999999;") 
        })
      } else if(temp_choice$school_status == 3){
        output$school_select <- renderUI({
          actionButton(inputId = "school_button", label = "", 
                       style ="background: url('epicButtons-22.svg');background-repeat:no-repeat;border:1px solid #e2ac24; border-bottom:7px solid #e2ac24;")
        })
      }
    })  
    observe({  
      if(temp_choice$major_status == 1){
        output$major_select <- renderUI({
          actionButton(inputId = "major_button", label = "", 
                       style ="background: url('epicButtons-19.svg');background-repeat:no-repeat;border-bottom:7px solid #999999;")
        })
      } else if(temp_choice$major_status == 3){
        output$major_select <- renderUI({
          actionButton(inputId = "major_button", label = "",
                       style ="background: url('epicButtons-23.svg');background-repeat:no-repeat;border:1px solid #37b749; border-bottom:7px solid #37b749;")
        }) 
      }
    })
    observe({
      if(temp_choice$occupation_status == 1){
        output$occupation_select <- renderUI({
          actionButton(inputId = "occupation_button", label = "", 
                       style ="background: url('epicButtons-20.svg');background-repeat:no-repeat;border-bottom:7px solid #999999;")
        }) 
      } else if(temp_choice$occupation_status == 3){
        output$occupation_select <- renderUI({
          actionButton(inputId = "occupation_button", label = "", 
                       style ="background: url('epicButtons-24.svg');background-repeat:no-repeat;border:1px solid #477ddd;border-bottom:7px solid #477ddd;")
        }) 
      }
    })
    observe({
      if(temp_choice$degree_status == 1){
        output$degree_select <- renderUI({
          actionButton(inputId = "degree_button", label = "", 
                       style ="background: url('epicButtons-21.svg');background-repeat:no-repeat;border-bottom:7px solid #999999;")
        })
      } else if(temp_choice$degree_status == 3){
        output$degree_select <- renderUI({
          actionButton(inputId = "degree_button", label = "", 
                       style ="background: url('epicButtons-25.svg');background-repeat:no-repeat;border:1px solid #e22b2b;border-bottom:7px solid #e22b2b;")
        })
      }
    })  
    observe({
      if(temp_choice$reqdegree_status == 1){
        output$req_degree_select <- renderUI({
          actionButton(inputId = "req_degree_button", label = "", 
                       style ="background: url('epicButtons-26.svg');background-repeat:no-repeat;border-bottom:7px solid #999999;")
        })
      } else if(temp_choice$reqdegree_status == 3){
        output$req_degree_select <- renderUI({
          actionButton(inputId = "req_degree_button", label = "", 
                       style ="background: url('epicButtons-27.svg');background-repeat:no-repeat;border:1px solid #cc33ff;border-bottom:7px solid #cc33ff;")
        })
      }
    })
    update_filter <- function(x){
      if(x == "major"){
        school_filtered_choices()
        occupation_filtered_choices()
        degree_filtered_choices()
        req_degree_filtered_choices()
      } else if(x == "school") {
        major_filtered_choices()
        occupation_filtered_choices()
        degree_filtered_choices()
        req_degree_filtered_choices()
      } else if(x == "occupation") {
        school_filtered_choices()
        major_filtered_choices()
        degree_filtered_choices()
        req_degree_filtered_choices()
      } else if(x == "degree") {
        school_filtered_choices()
        major_filtered_choices()
        occupation_filtered_choices()
        req_degree_filtered_choices()
      } else if(x == "required") {
        school_filtered_choices()
        major_filtered_choices()
        occupation_filtered_choices()
        degree_filtered_choices()
      } else if(x == "all") {
        school_filtered_choices()
        major_filtered_choices()
        occupation_filtered_choices()
        degree_filtered_choices()
        req_degree_filtered_choices()
      }
    }
    observeEvent(input$school_button,{
      temp_choice$school_status <<- 3
      update_filter("school")
      #    log_event2("school_build")
    })
    observeEvent(input$major_button,{
      temp_choice$major_status <<- 3
      update_filter("major")
      #    log_event2("major_build")
    })
    observeEvent(input$occupation_button,{
      temp_choice$occupation_status <<- 3
      update_filter("occupation")
      #    log_event2("occupation_build")
    })
    observeEvent(input$degree_button,{
      temp_choice$degree_status <<- 3
      update_filter("degree")
      #    log_event2("degree_build")
    })
    observeEvent(input$req_degree_button,{
      temp_choice$reqdegree_status <<- 3
      update_filter("required")
      #    log_event2("req_degree_build")
    })
 
    observeEvent(input$next_button,{
      #    log_event2("next_button")
      if(is_empty(major_build_selected) & is_empty(degree_build_selected) & is_empty(occupation_build_selected) & is_empty(school_build_selected) & 
         is_empty(req_degree_build_selected)){
        showModal(modalDialog(title = "Please select at least one item",
                              "Unable to build scenario!",
                              easyClose = TRUE))
      } else {
        add_scenario()
        #      log_event2("scenario_table")
        updateTabItems(session, "tabs", selected = "scenario")
      }
    })
    clear_filters <- function() {
      updateSelectInput(session,inputId = "sm_school", label = "School", choices = c("All" = '',school_list$INSTNM))
      updateSelectInput(session,inputId = "sm_state", label = "State", choices = c("All" = '',state_abbr$State))
      updateSliderInput(session,inputId = "sm_annual_hi", label = "Annual Cost High",
                        value = c(0,max(school_scenario$CostHi)),
                        min = 0,
                        max = max(school_scenario$CostHi))
      updateSliderInput(session,inputId = "sm_annual_lo", label = "Annual Cost Low",
                        value = c(0,max(school_scenario$CostLo)),
                        min = 0,
                        max = max(school_scenario$CostLo))
      updateSelectInput(session,inputId = "mm_major", label = "Major", choices = c("All" = '', major_list$CIPNAME))
      updateSelectInput(session,inputId = "mm_occupaton", label = "Occupation", choices = c("All" = '', occupation_list$OCCNAME))
      updateSelectInput(session,inputId = "mm_entry_degree", label = "Entry Degree", choices = c("All" = '', req_degree_master$Entry_Degree))
      updateSelectInput(session,inputId = "om_occupation", label = "Occupation", choices = c("All" = '', occupation_master$OCCNAME))
      updateSelectInput(session,inputId = "om_entry_degree", label = "Entry Degree",
                        choices = c("All" = '', req_degree_master$Entry_Degree))
      updateSelectInput(session,inputId = "om_required_exp", label = "Required Experience",
                        choices = c("All" = '', occupation_master$Experience))
      updateSliderInput(session,inputId = "om_starting_salary", label = "Starting Salary",
                        value = c(0,round(max(occupation_master$X17p),0)),
                        min = 0, 
                        max = round(max(occupation_master$X17p),0))
      updateSliderInput(session,inputId = "om_growth_rate", label = "Growth Rate",
                        value = c(round(min(occupation_master$EmplyPC),0),round(max(occupation_master$EmplyPC),0)),
                        min = round(min(occupation_master$EmplyPC),0),
                        max = round(max(occupation_master$EmplyPC),0))
      updateSelectInput(session, inputId = "dm_degree", label = "Degree", choices = c("All" = '', degree_list$LEVELName))
      updateSelectInput(session, inputId = "rm_req_degree", label = "Entry Degree", choices = c("All" = '', req_degree_master$Entry_Degree))
    }
    clear_build <- function() {
      temp_choice$school_status <<- 1
      temp_choice$major_status <<- 1
      temp_choice$occupation_status <<- 1
      temp_choice$degree_status <<- 1
      temp_choice$reqdegree_status <<- 1
      clear_occupationchoices()
      clear_majorchoices()
      clear_schoolchoices()
      clear_degreechoices()
      clear_req_degreechoices()
      edit_scenario <<- 0
      update_scenario()
      update_filter("all")
      clear_filters()
    }
    #    save_scenario()
   
# School -------------------------
  school_modal_favorite <- c(character())
  school_modal_master <- school_scenario
  sm_master2 <- reactiveVal(0)
  school_modal_var <- reactive ({ 
    sm_temp <- sm_master2()
    sm_school_temp <- school_modal_master
    if (!is.null(input$sm_school) & input$sm_school != '') {
      sm_school_temp <- filter(sm_school_temp, INSTNM %in% input$sm_school)
    }
    if (input$sm_state != '') {
      sm_school_temp <- filter(sm_school_temp, State %in% input$sm_state)
    }
    #    sm_school_temp <- filter(sm_school_temp, (ROOM_BOARD >= input$sm_room_board[1] & ROOM_BOARD <= input$sm_room_board[2]))
    sm_school_temp <- filter(sm_school_temp, (CostHi >= input$sm_annual_hi[1] & CostHi <= input$sm_annual_hi[2]))
    sm_school_temp <- filter(sm_school_temp, (CostLo >= input$sm_annual_lo[1] & CostLo <= input$sm_annual_lo[2]))
    
    if(is.null(input$sm_school) |input$sm_school == ''){
      updateSelectInput(session, inputId = "sm_school", label = "School",
                        choices = isolate(c(All = '', sort(unique(sm_school_temp$INSTNM)))), selected = '')
    }
    if(is.null(input$sm_state) |input$sm_state == ''){
      updateSelectInput(session, inputId = "sm_state", label = "State",
                        choices = isolate(c(All = '', sort(unique(sm_school_temp$State)))), selected = '')
    }
    
    #    sm_school_temp <- sm_school_temp %>% mutate(GRADR150 = BAGR150 + L4GR150)   
    school_modal_favorite <<- sm_school_temp
    
    sm_school_temp <- sm_school_temp %>% select("INSTNM", "STABBR", "CITY", "ADMPCT", 
                                                "PERCENT150", "CostLo", "CostHi")
    
    sm_temp1 <- sm_school_temp$CostLo == 0
    sm_school_temp <- cbind(sm_school_temp, sm_temp1)
    
    #Rename table column headers     
    sm_school_temp <- sm_school_temp %>% rename("School<br>Name" = "INSTNM", "State" = "STABBR", "City" = "CITY",
                                                "Admit" = "ADMPCT", "Grad<br>Rate<br>150%" = "PERCENT150",
                                                "Cost Lo" = "CostLo", "Cost Hi" = "CostHi")
  })
  school_filtered_choices <- function(){
    if(is_empty(major_build_selected) & is_empty(degree_build_selected) & is_empty(occupation_build_selected) & is_empty(req_degree_build_selected)){
      school_modal_master <<- school_scenario
      value <- sm_master2() + 1
      sm_master2(value) 
      return()
    }
    school_temp <- backbone
    if(!is_empty(major_build_selected)) {
      curriculum_temp <- filter(major_filter, CIPNAME %in% major_build_selected) %>% select(CIPCODE)
      school_temp <- filter(school_temp, CIPCODE %in% curriculum_temp$CIPCODE)
    }
    if(!is_empty(occupation_build_selected)) {
      occupation_temp <- filter(occupation_filter, OCCNAME %in% occupation_build_selected) %>% select(OCCCODE)
      school_temp <- filter(school_temp, OCCCODE %in% occupation_temp$OCCCODE)
    }
    if(!is_empty(degree_build_selected)) {
      degree_temp <- filter(degree_filter, LEVELName %in% degree_build_selected) %>% select(AWLEVEL)
      school_temp <- filter(school_temp, AWLEVEL %in% degree_temp$AWLEVEL)
    }
    if(!is_empty(req_degree_build_selected)){
      r_temp <- filter(req_degree_filter, Entry_Degree %in% req_degree_build_selected) %>% select(Entry_Code)
      req_degree_temp <- req_degree_xwalk %>% filter(Entry_Code %in% r_temp$Entry_Code, GREATER == "TRUE")
      school_temp <- school_temp %>% filter(Entry_Code %in% r_temp$Entry_Code, AWLEVEL %in% req_degree_temp$AWLEVEL)
    }
    school_modal_master <<- school_scenario %>% filter(UNITID %in% school_temp$UNITID)
    value <- sm_master2() + 1
    sm_master2(value) 
  }
  school_cards <- function() {
    school_temp <- school_filter %>% filter(INSTNM %in% school_build_selected)
    if(NROW(school_temp) > 0){
      output$schoolchoice <- renderUI({
        args <- lapply(1:NROW(school_temp), function(x) schoolcard2(name = school_temp$UNITID[x], school = school_temp$INSTNM[x]))
        do.call(shiny::verticalLayout, args)        
      })
    } else {
      output$schoolchoice <- renderUI({NULL})
      schoolobsList <<- list()
    }
  }	  
  schoolcard2 <- function(name,school) {
    school_observer(name,school)
    div(class = "chip", style = "border-color: #e2ac24; background-color:#e2ac24;", 
        splitLayout(cellWidths = c("85%","15%"),
                    p(school),                        
                    actionButton(inputId = paste0("schooldel",name),label = "x",class = "delete_button",
                                 style = "border-color: #e2ac24; background-color: #e2ac24;"))        
    )
  }
  school_observer <- function(name,school){
    btName <- paste0("schooldel",name)
    if(length(schoolobsList[[btName]]) == 0){
      schoolobsList[[btName]] <<- observeEvent(input[[btName]], {
        school_build_selected <<- school_build_selected[school_build_selected != school]
        schoolobsList <<- list()
        school_cards()
        update_filter("school")
      }, ignoreInit = TRUE, once = TRUE)
    }
  }
  observe({
    output$school_table_modal <- renderDT({
      DT::datatable(
        data = school_modal_var(),
        escape = FALSE,
        rownames = FALSE,
        extensions = 'Responsive',
        class="cell-border stripe",
        options = list(
          headerCallback = DT::JS(
            "function(thead) {",
            "  $(thead).css('font-size', '14px');",
            "}"),
          dom = 'tip',
          order = list(list(7, 'asc'), list(5, 'asc')),
          saveState = TRUE,
          filter = FALSE,
          #         autoWidth = TRUE,
          columnDefs = (list(list(visible=FALSE, targets=c(7)),
                             list(width = '180px', targets = 0),
                             list(width = '25px', targets = 1),
                             list(width = '140px', targets = 2),
                             list(width = '63px', className = 'dt-body-right', targets = c(3,4,5,6)),
                             list(targets = 0,
                                  render = JS(
                                    "function(data, type, row, meta) {",
                                    "return type === 'display' && data.length > 20 ?",
                                    "'<span title=\"' + data + '\">' + data.substr(0, 20) + '...</span>' : data;",
                                    "}"))
          )),
          lengthMenu = 10
        ),
        callback = JS('table.page(3).draw(false);'),
        selection = list(mode = 'single')
      ) %>%
        formatStyle(
          0,
          target = 'row',
          color = 'black',
          fontWeight = 'normal',
          fontSize = '12px',
          lineHeight = '100%',
          margin = '0px',
          padding = '2px'
        )
    })
  })
  observeEvent(input$sm_favorite, {
    req(input$school_table_modal_rows_selected)
    school_list_temp <- school_modal_favorite[input$school_table_modal_rows_selected,]
    if(NROW(school_build_selected) < 5 ){
      if(school_list_temp$INSTNM %in% school_build_selected){
        return()
      } else {
        school_build_selected <<- rbind(school_build_selected, school_list_temp$INSTNM)
        school_cards()
        update_filter("school")
      }
    }
  })
  
# Major --------------------------
  major_modal_favorite <- c(character())
  major_modal_master <- major_scenario
  mm_master2 <- reactiveVal(0)
  major_modal_var <- reactive ({  
    mm_temp <- mm_master2()
    major_temp <- unique(backbone %>% select(CIPCODE, OCCCODE, Entry_Code))
    major_temp <- major_temp %>% filter(CIPCODE %in% major_modal_master$CIPCODE)
    major_temp <- left_join(major_temp, occupation_filter, by = "OCCCODE")
    major_temp <- left_join(major_temp, cips, by = "CIPCODE")
    major_temp <- left_join(major_temp, req_degree_filter, by = "Entry_Code")
    if(!is_empty(occupation_build_selected)){
      major_temp <- major_temp %>% filter(OCCNAME %in% occupation_build_selected)
    }
    if(!is_empty(req_degree_build_selected)) {
      major_temp <- major_temp %>% filter(Entry_Degree %in% req_degree_build_selected)
    }
    
    if(!is.null(input$mm_occupation) & input$mm_occupation !='') {
      occupation_filter <- filter(occupation_filter, OCCNAME %in% input$mm_occupation) %>% select(OCCCODE)
      major_temp <- filter(major_temp, OCCCODE %in% occupation_filter)
    }
    
    if(!is.null(input$mm_major) & input$mm_major !='') {
      major_filter <- filter(cips, CIPNAME %in% input$mm_major) %>% select(CIPCODE)
      major_temp <- filter(major_temp, CIPCODE %in% major_filter)                      
    }
    if(!is.null(input$mm_entry_degree) & input$mm_entry_degree !=''){
      entry_degree_filter <- filter(req_degree_master, Entry_Degree %in% input$mm_entry_degree) %>% select(Entry_Code)
      major_temp <- filter(major_temp, Entry_Code %in% entry_degree_filter$Entry_Code)
    }
    
    major_modal_favorite <<- major_temp
    
    if(is.null(input$mm_major)|input$mm_major == ''){
      updateSelectInput(session, inputId = "mm_major", label = "Major",
                        choices = isolate(c(All = '', sort(unique(major_temp$CIPNAME)))), selected = '')
    }
    if(is.null(input$mm_occupation)|input$mm_occupation == ''){
      updateSelectInput(session, inputId = "mm_occupation", label = "Occupation",
                        choices = isolate(c(All = '', sort(unique(major_temp$OCCNAME)))), selected = '')
    }
    if(is.null(input$mm_entry_degree)|input$mm_entry_degree == ''){
      updateSelectInput(session, inputId = "mm_entry_degree", label = "Entry Degree",
                        choices = isolate(c(All = '', sort(unique(major_temp$Entry_Degree)))), selected = '')
    }
    major_temp <- major_temp %>% select(CIPNAME, OCCNAME, Entry_Degree)
    #Rename table column headers     
    major_temp <- major_temp %>% rename("Major" = "CIPNAME", "Entry Degree" = "Entry_Degree",
                                        "Occupation" = "OCCNAME")
  })
  major_filtered_choices <- function(){
    if(is_empty(school_build_selected) & is_empty(degree_build_selected) & is_empty(occupation_build_selected) & is_empty(req_degree_build_selected)){
      major_modal_master <<- cips
      value <- mm_master2() +1
      mm_master2(value) 
      return()
    }
    major_temp <- backbone
    if(!is_empty(school_build_selected)) {
      school_temp <- filter(school_filter, INSTNM %in% school_build_selected) %>% select(UNITID)
      major_temp <- filter(major_temp, UNITID %in% school_temp$UNITID)
    }
    if(!is_empty(occupation_build_selected)) {
      occupation_temp <- filter(occupation_filter, OCCNAME %in% occupation_build_selected) %>% select(OCCCODE)
      major_temp <- filter(major_temp, OCCCODE %in% occupation_temp$OCCCODE)
    }
    if(!is_empty(degree_build_selected)) {
      degree_temp <- filter(degree_filter, LEVELName %in% degree_build_selected) %>% select(AWLEVEL)
      major_temp <- filter(major_temp, AWLEVEL %in% degree_temp$AWLEVEL)
    }
    if(!is_empty(req_degree_build_selected)){
      r_temp <- filter(req_degree_filter, Entry_Degree %in% req_degree_build_selected) %>% select(Entry_Code)
      req_degree_temp <- req_degree_xwalk %>% filter(Entry_Code %in% r_temp$Entry_Code, GREATER == "TRUE")
      major_temp <- major_temp %>% filter(Entry_Code %in% r_temp$Entry_Code, AWLEVEL %in% req_degree_temp$AWLEVEL)
    }
    major_modal_master <<- cips %>% filter(CIPCODE %in% major_temp$CIPCODE)
    value <- mm_master2() + 1
    mm_master2(value)
  }
  major_cards <- function() {
    major_temp <- major_filter %>% filter(CIPNAME %in% major_build_selected)
    if(NROW(major_temp) > 0){
      output$majorchoice <- renderUI({
        args <- lapply(1:NROW(major_temp), function(x) majorcard2(name = major_temp$CIPCODE[x], major = major_temp$CIPNAME[x]))
        do.call(shiny::verticalLayout, args)        
      })
    } else {
      output$majorchoice <- renderUI({NULL})
      majorobsList <<- list()
    }
  }	  
  majorcard2 <- function(name,major) {
    major_observer(name,major)
    div(class = "chip", style = "border-color: #37b749;background-color:#37b749;",
        splitLayout(cellWidths = c("85%","15%"),
                    p(major),                        
                    actionButton(inputId = paste0("majordel",name),label = "x", class = "delete_button",
                                 style = "border-color: #37b749; background-color: #37b749;"))        
    )
  }
  major_observer <- function(name,major){
    btName <- paste0("majordel",name)
    if(length(majorobsList[[btName]]) == 0){
      majorobsList[[btName]] <<- observeEvent(input[[btName]], {
        major_build_selected <<- major_build_selected[major_build_selected != major]
        majorobsList <<- list()
        major_cards()
        update_filter("major")
      }, ignoreInit = TRUE, once = TRUE)
    }
  }
  observe({  
    output$major_table_modal <- renderDT({
      DT::datatable(
        data = major_modal_var(),
        escape = TRUE,
        rownames = FALSE,
        extensions = 'Responsive',
        class="cell-border stripe",
        options = list(
          headerCallback = DT::JS(
            "function(thead) {",
            "  $(thead).css('font-size', '14px');",
            "}"),
          dom = 'tip',
          order = list(list(0, 'asc')),
          saveState = TRUE,
          filter = FALSE,
          #        autoWidth = FALSE,
          columnDefs = (list(list(width = '270px', targets =c(0,1,2)),
                             list(targets = c(0,1,2),
                                  render = JS(
                                    "function(data, type, row, meta) {",
                                    "return type === 'display' && data.length > 35?",
                                    "'<span title=\"' + data + '\">' + data.substr(0, 35) + '...</span>' : data;",
                                    "}"))
          )),
          lengthMenu = c(15)
        ),
        callback = JS('table.page(3).draw(false);'),
        selection = list(mode = 'single')
      ) %>%
        formatStyle(
          0,
          target = 'row',
          color = 'black',
          fontWeight = 'normal',
          fontSize = '12px',
          lineHeight = '100%'
        )
    })
  }) 
  observeEvent(input$mm_favorite, {
    req(input$major_table_modal_rows_selected)
    major_list_temp <- major_modal_favorite[input$major_table_modal_rows_selected,]
    if(NROW(major_build_selected) < 5) {
      if(major_list_temp$CIPNAME %in% major_build_selected){
        return()
      } else {
        major_build_selected <<- rbind(major_build_selected, major_list_temp$CIPNAME)
        major_cards()
        update_filter("major")
      }
    }
  })
  
# Degree -------------------------
  degreeobsList <- list()
  degree_modal_favorite <- c(character())
  degree_modal_master <- degree_master
  dm_master2 <- reactiveVal(0)
  degree_modal_var <- reactive({
    dm_temp <- dm_master2()
    dm_degree_temp <- degree_modal_master
    if(!is.null(input$dm_degree) & input$dm_degree != '') {
      dm_degree_temp <- filter(dm_degree_temp, LEVELName %in% input$dm_degree)
    }
    if(is.null(input$dm_degree) | input$dm_degree == '') {
      updateSelectInput(session, inputId = "dm_degree", label = "Degree", choices = isolate(c("All" = '', dm_degree_temp$LEVELName)))
    }
    degree_modal_favorite <<- dm_degree_temp %>% select(LEVELName) 
    dm_degree_temp <- dm_degree_temp %>% select(LEVELName) %>% rename("Degree" = "LEVELName")
  })
  degree_filtered_choices <- function(){
    if(is_empty(major_build_selected) & is_empty(school_build_selected) & is_empty(occupation_build_selected) & is_empty(req_degree_build_selected)){
      degree_modal_master <<- degree_master
      value <- dm_master2() +1
      dm_master2(value) 
      return()
    }
    degree_temp <- backbone
    if(!is_empty(school_build_selected)) {
      school_temp <- filter(school_filter, INSTNM %in% school_build_selected) %>% select(UNITID)
      degree_temp <- filter(degree_temp, UNITID %in% school_temp$UNITID)
    }
    if(!is_empty(occupation_build_selected)) {
      occupation_temp <- filter(occupation_filter, OCCNAME %in% occupation_build_selected) %>% select(OCCCODE)
      degree_temp <- filter(degree_temp, OCCCODE %in% occupation_temp$OCCCODE)
    }
    if(!is_empty(major_build_selected)) {
      major_temp <- filter(major_filter, CIPNAME %in% major_build_selected) %>% select(CIPCODE)
      degree_temp <- filter(degree_temp, CIPCODE %in% major_temp$CIPCODE)
    }
    if(!is_empty(req_degree_build_selected)){
      r_temp <- filter(req_degree_filter, Entry_Degree %in% req_degree_build_selected) %>% select(Entry_Code)
      req_degree_temp <- req_degree_xwalk %>% filter(Entry_Code %in% r_temp$Entry_Code, GREATER == "TRUE")
      degree_temp <- degree_temp %>% filter(Entry_Code %in% r_temp$Entry_Code, AWLEVEL %in% req_degree_temp$AWLEVEL)
    }
    
    degree_temp <- left_join(degree_temp, degree_filter, by = "AWLEVEL")
    degree_temp <- degree_temp %>% distinct(LEVELName, .keep_all = TRUE) 
    degree_temp <- degree_temp[order(degree_temp$AWLEVEL),,drop = FALSE] %>% select(LEVELName)
    degree_modal_master <<- degree_temp
    value <- dm_master2() +1
    dm_master2(value)
  }
  degree_cards <- function() {
    degree_temp <- degree_filter %>% filter(LEVELName %in% degree_build_selected)
    if(NROW(degree_temp) > 0){
      output$degreechoice <- renderUI({
        args <- lapply(1:NROW(degree_temp), function(x) degreecard2(name = degree_temp$AWLEVEL[x], degree = degree_temp$LEVELName[x]))
        do.call(shiny::verticalLayout, args)        
      })
    } else {
      output$degreechoice <- renderUI({NULL})
      degreeobsList <<- list()
    }
  }	  
  degreecard2 <- function(name,degree) {
    degree_observer(name,degree)
    div(class = "chip", style = "border-color:#e22b2b;background-color:#e22b2b;",
        splitLayout(cellWidths = c("85%","15%"),
                    p(degree),                        
                    actionButton(inputId = paste0("degreedel",name),label = "x", class = "delete_button",
                                 style = "border-color: #e22b2b; background-color: #e22b2b;"))        
    )
  }
  degree_observer <- function(name,degree){
    btName <- paste0("degreedel",name)
    if(length(degreeobsList[[btName]]) == 0){
      degreeobsList[[btName]] <<- observeEvent(input[[btName]], {
        degree_build_selected <<- degree_build_selected[degree_build_selected != degree]
        degreeobsList <<- list()
        degree_cards()
        update_filter("degree")
      }, ignoreInit = TRUE, once = TRUE)
    }
  }
  observe({
    output$degree_table_modal <- renderDT({
      DT::datatable(
        data = degree_modal_var(),
        escape = FALSE,
        rownames = FALSE,
        extensions = 'Responsive',
        class="cell-border stripe",
        options = list(
          headerCallback = DT::JS(
            "function(thead) {",
            "  $(thead).css('font-size', '14px');",
            "}"),
          dom = 'tip',
          saveState = TRUE,
          filter = FALSE,
          #         autoWidth = TRUE,
          lengthMenu = c(15,20)
        ),
        callback = JS('table.page(3).draw(false);'),
        selection = list(mode = 'single')
      ) %>%
        formatStyle(
          0,
          target = 'row',
          color = 'black',
          fontWeight = 'normal',
          fontSize = '12px',
          lineHeight = '100%',
          margin = '0px',
          padding = '2px'
        )
    })
  })
  observeEvent(input$dm_favorite, {
    req(input$degree_table_modal_rows_selected)
    degree_list_temp <- degree_modal_favorite[input$degree_table_modal_rows_selected,]
    if(NROW(degree_build_selected) < 5) {
      if(degree_list_temp %in% degree_build_selected){
        return()
      } else {
        degree_build_selected <<- rbind(degree_build_selected, degree_list_temp)
        degree_cards()
        update_filter("degree")
      }
    }
  })
  
# Occupation ---------------------
  occupation_modal_favorite <- c(character())
  occupation_modal_master <- occupation_master2
  om_master2 <- reactiveVal(0)
  occupation_modal_var <- reactive({
    om_temp <- om_master2()
    occupation_modal_temp <- occupation_modal_master
    if(!is.null(input$om_occupation) & input$om_occupation !='') {
      occupation_modal_temp <- filter(occupation_modal_temp, OCCNAME %in% input$om_occupation) 
    }
    
    if(!is.null(input$om_entry_degree) & input$om_entry_degree !='') {
      occupation_modal_temp <- filter(occupation_modal_temp, Entry_Degree %in% input$om_entry_degree) 
    }
    if(!is.null(input$om_required_exp) & input$om_required_exp !='') {
      occupation_modal_temp <- filter(occupation_modal_temp, Experience %in% input$om_required_exp) 
    }
    occupation_modal_temp <- filter(occupation_modal_temp, (EmplyPC >= input$om_growth_rate[1] & EmplyPC <= input$om_growth_rate[2]))
    occupation_modal_temp <- filter(occupation_modal_temp, (X17p >= input$om_starting_salary[1] & X17p <= input$om_starting_salary[2]))
    
    if(is.null(input$om_occupation) |input$om_occupation == ''){
      updateSelectInput(session, inputId = "om_occupation", label = "Occupation",
                        choices = isolate(c(All = '', sort(unique(occupation_modal_temp$OCCNAME)))), selected = '')
    }
    if(is.null(input$om_entry_degree) |input$om_entry_degree == ''){
      updateSelectInput(session, inputId = "om_entry_degree", label = "Entry Degree",
                        choices = isolate(c(All = '', sort(unique(occupation_modal_temp$Entry_Degree)))), selected = '')
    }
    if(is.null(input$om_required_exp) |input$om_required_exp == ''){
      updateSelectInput(session, inputId = "om_required_exp", label = "Required Experience",
                        choices = isolate(c(All = '', sort(unique(occupation_modal_temp$Experience)))), selected = '')
    }
    occupation_modal_favorite <<- occupation_modal_temp
    occupation_modal_temp <- occupation_modal_temp %>% select("OCCNAME", "EmplyChg", "EmplyPC", "SelfEmpl", "Entry_Degree", "Experience", "X10p", "X17p",
                                                              "X25p", "X50p", "X75p", "X82p", "X90p")
    
    occupation_modal_temp <- occupation_modal_temp %>% rename("Occupation" = "OCCNAME", "Job<br>Growth<br>Num" = "EmplyChg",
                                                              "Job<br>Growth<br>(%)" = "EmplyPC", "Self<br>Employ<br>%" = "SelfEmpl",
                                                              "Typical<br>Entry<br>Degree" = "Entry_Degree",
                                                              "Level of<br>Experience<br>Required" = "Experience",
                                                              "10th<br>%ile<br>Salary" = "X10p", "17th<br>%ile<br>Salary" = "X17p",
                                                              "25th<br>%ile<br>Salary" = "X25p", "Median<br>Salary" = "X50p",
                                                              "75th<br>%ile<br>Salary" = "X75p", "82th<br>%ile<br>Salary" = "X82p",
                                                              "90th<br>%ile<br>Salary" = "X90p")
  })
  occupation_filtered_choices <- function(){
    if(is_empty(major_build_selected) & is_empty(degree_build_selected) & is_empty(school_build_selected) & is_empty(req_degree_build_selected)){
      occupation_modal_master <<- occupation_master2
      value <- om_master2() + 1
      om_master2(value) 
      return()
    }
    occupation_temp <- backbone
    if(!is_empty(school_build_selected)) {
      school_temp <- filter(school_filter, INSTNM %in% school_build_selected) %>% select(UNITID)
      occupation_temp <- filter(occupation_temp, UNITID %in% school_temp$UNITID)
    }
    if(!is_empty(degree_build_selected)) {
      degree_temp <- filter(degree_filter, LEVELName %in% degree_build_selected) %>% select(AWLEVEL)
      occupation_temp <- filter(occupation_temp, AWLEVEL %in% degree_temp$AWLEVEL)
    }
    if(!is_empty(major_build_selected)) {
      major_temp <- filter(major_filter, CIPNAME %in% major_build_selected) %>% select(CIPCODE)
      occupation_temp <- filter(occupation_temp, CIPCODE %in% major_temp$CIPCODE)
    }
    if(!is_empty(req_degree_build_selected)){
      r_temp <- filter(req_degree_filter, Entry_Degree %in% req_degree_build_selected) %>% select(Entry_Code)
      req_degree_temp <- req_degree_xwalk %>% filter(Entry_Code %in% r_temp$Entry_Code, GREATER == "TRUE")
      occupation_temp <- occupation_temp %>% filter(Entry_Code %in% r_temp$Entry_Code, AWLEVEL %in% req_degree_temp$AWLEVEL)
    }
    
    occupation_modal_master <<- occupation_master2 %>% filter(OCCCODE %in% occupation_temp$OCCCODE)
    value <- om_master2() + 1
    om_master2(value)
  }
  occupation_cards <- function() {
    occupation_temp <- occupation_filter %>% filter(OCCNAME %in% occupation_build_selected)
    if(NROW(occupation_temp) > 0){
      output$occupationchoice <- renderUI({
        args <- lapply(1:NROW(occupation_temp), function(x) occupationcard2(name = occupation_temp$OCCCODE[x],
                                                                            occupation = occupation_temp$OCCNAME[x]))
        do.call(shiny::verticalLayout, args)        
      })
    } else {
      output$occupationchoice <- renderUI({NULL})
      occupationobsList <<- list()
    }
  }	  
  occupationcard2 <- function(name,occupation) {
    occupation_observer(name,occupation)
    div(class = "chip", style = "border-color: #477ddd;background-color:#477ddd;",
        splitLayout(cellWidths = c("85%","15%"),
                    p(occupation),                        
                    actionButton(inputId = paste0("occupationdel",name),label = "x", class = "delete_button",
                                 style = "border-color: #477ddd; background-color: #477ddd;"))        
    )
  }
  occupation_observer <- function(name,occupation){
    btName <- paste0("occupationdel",name)
    if(length(occupationobsList[[btName]]) == 0){
      occupationobsList[[btName]] <<- observeEvent(input[[btName]], {
        occupation_build_selected <<- occupation_build_selected[occupation_build_selected != occupation]
        occupationobsList <<- list()
        occupation_cards()
        update_filter("occupation")
      }, ignoreInit = TRUE, once = TRUE)
    }
  }
  observe({  
    output$occupation_table_modal <- renderDT({
      DT::datatable(
        data = occupation_modal_var(),
        escape = FALSE,
        rownames = FALSE,
        extensions = 'Responsive',
        class="cell-border stripe",
        options = list(
          
          headerCallback = DT::JS(
            "function(thead) {",
            "  $(thead).css('font-size', '12px');",
            "}"),
          dom = 'tip',
          order = list(list(7, 'desc')),
          saveState = TRUE,
          filter = FALSE,
          autoWidth = FALSE,
          columnDefs = (list(list(visible=FALSE, targets =c(6,8,10,11)),
                             list(width = '58px', targets = c(1,2,3,7,9,12)),
                             list(width = '105px', targets = c(5)),
                             list(width = '190px', targets = c(0)),
                             list(width = '170px', targets = c(4)),
                             list(targets = c(0,4),
                                  render = JS(
                                    "function(data, type, row, meta) {",
                                    "return type === 'display' && data.length > 20?",
                                    "'<span title=\"' + data + '\">' + data.substr(0, 20) + '...</span>' : data;",
                                    "}"))
          )),
          lengthMenu = c(10)
        ),
        callback = JS('table.page(3).draw(false);'),
        selection = list(mode = 'single')
      ) %>%
        formatStyle(
          0,
          target = 'row',
          color = 'black',
          fontWeight = 'normal',
          fontSize = '12px',
          lineHeight = '100%'
        )
    })
  })
  observeEvent(input$om_favorite, {
    req(input$occupation_table_modal_rows_selected)
    occupation_list_temp <- occupation_modal_favorite[input$occupation_table_modal_rows_selected,]
    if(NROW(occupation_build_selected) < 5) {
      if(occupation_list_temp$OCCNAME %in% occupation_build_selected){
        return()
      } else {
        occupation_build_selected <<- rbind(occupation_build_selected, occupation_list_temp$OCCNAME)
        occupation_cards()
        update_filter("occupation")
      }
    }
  })
# Typical Degree -----------------
  req_degreeobsList <- list()
  req_degree_modal_favorite <- c(character())
  req_degree_modal_master <- req_degree_master
  rdm_master2 <- reactiveVal(0)
  req_degree_modal_var <- reactive({
    rdm_temp <- rdm_master2()
    rdm_degree_temp <- req_degree_modal_master
    if(!is.null(input$rm_req_degree) & input$rm_req_degree != '') {
      rdm_degree_temp <- filter(rdm_degree_temp, Entry_Degree %in% input$rm_req_degree)
    }
    if(is.null(input$rm_req_degree) | input$rm_req_degree == '') {
      updateSelectInput(session, inputId = "rm_req_degree", label = "Entry Degree", choices = isolate(c("All" = '', rdm_degree_temp$Entry_Degree)))
    }
    req_degree_modal_favorite <<- rdm_degree_temp %>% select(Entry_Degree) 
    rdm_degree_temp <- rdm_degree_temp %>% select(Entry_Degree) %>% rename("Entry Degree" = "Entry_Degree")
  }) 
  req_degree_filtered_choices <- function(){
    if(is_empty(major_build_selected) & is_empty(school_build_selected) & is_empty(occupation_build_selected) & is_empty(degree_build_selected)){
      req_degree_modal_master <<- req_degree_master
      value <- rdm_master2() +1
      rdm_master2(value) 
      return()
    }
    req_degree_temp <- backbone
    if(!is_empty(school_build_selected)) {
      school_temp <- filter(school_filter, INSTNM %in% school_build_selected) %>% select(UNITID)
      req_degree_temp <- filter(req_degree_temp, UNITID %in% school_temp$UNITID)
    }
    if(!is_empty(occupation_build_selected)) {
      occupation_temp <- filter(occupation_filter, OCCNAME %in% occupation_build_selected) %>% select(OCCCODE)
      req_degree_temp <- filter(req_degree_temp, OCCCODE %in% occupation_temp$OCCCODE)
    }
    if(!is_empty(major_build_selected)) {
      major_temp <- filter(major_filter, CIPNAME %in% major_build_selected) %>% select(CIPCODE)
      req_degree_temp <- filter(req_degree_temp, CIPCODE %in% major_temp$CIPCODE)
    }
    if(!is_empty(degree_build_selected)) {
      degree_temp <- filter(degree_filter, LEVELName %in% degree_build_selected) %>% select(AWLEVEL)
      req_degree_temp <- filter(req_degree_temp, AWLEVEL %in% degree_temp$AWLEVEL)
    }
    req_degree_temp <- left_join(req_degree_temp, req_degree_filter, by = "Entry_Code")
    req_degree_temp <- req_degree_temp %>% distinct(Entry_Code, .keep_all = TRUE) %>% select(Entry_Degree) 
    req_degree_modal_master <<- req_degree_temp
    value <- rdm_master2() +1
    rdm_master2(value)
  }
  req_degree_cards <- function() {
    req_degree_temp <- req_degree_filter %>% filter(Entry_Degree %in% req_degree_build_selected)
    if(NROW(req_degree_temp) > 0){
      output$req_degreechoice <- renderUI({
        args <- lapply(1:NROW(req_degree_temp), function(x) req_degreecard2(name = req_degree_temp$Entry_Code[x], req_degree = req_degree_temp$Entry_Degree[x]))
        do.call(shiny::verticalLayout, args)        
      })
    } else {
      output$req_degreechoice <- renderUI({NULL})
      req_degreeobsList <<- list()
    }
  }	  
  req_degreecard2 <- function(name,req_degree) {
    req_degree_observer(name,req_degree)
    div(class = "chip", style = "border-color:#cc33ff;background-color:#cc33ff;",
        splitLayout(cellWidths = c("85%","15%"),
                    p(req_degree),                        
                    actionButton(inputId = paste0("reqdegreedel",name),label = "x", class = "delete_button",
                                 style = "border-color: #cc33ff; background-color: #cc33ff;"))        
    )
  }
  req_degree_observer <- function(name,req_degree){
    btName <- paste0("reqdegreedel",name)
    if(length(req_degreeobsList[[btName]]) == 0){
      req_degreeobsList[[btName]] <<- observeEvent(input[[btName]], {
        req_degree_build_selected <<- req_degree_build_selected[req_degree_build_selected != req_degree]
        req_degreeobsList <<- list()
        req_degree_cards()
        update_filter("required")
      }, ignoreInit = TRUE, once = TRUE)
    }
  }
  observe({
    output$req_degree_table_modal <- renderDT({
      DT::datatable(
        data = req_degree_modal_var(),
        escape = FALSE,
        rownames = FALSE,
        extensions = 'Responsive',
        class="cell-border stripe",
        options = list(
          headerCallback = DT::JS(
            "function(thead) {",
            "  $(thead).css('font-size', '14px');",
            "}"),
          dom = 'tip',
          saveState = TRUE,
          filter = FALSE,
          #         autoWidth = TRUE,
          lengthMenu = c(15,20)
        ),
        callback = JS('table.page(3).draw(false);'),
        selection = list(mode = 'single')
      ) %>%
        formatStyle(
          0,
          target = 'row',
          color = 'black',
          fontWeight = 'normal',
          fontSize = '12px',
          lineHeight = '100%',
          margin = '0px',
          padding = '2px'
        )
    })
  })
  observeEvent(input$rm_favorite, {
    req(input$req_degree_table_modal_rows_selected)
    req_degree_list_temp <- req_degree_modal_favorite[input$req_degree_table_modal_rows_selected,]
    if(NROW(req_degree_build_selected) < 5) {
      if(req_degree_list_temp == "No Match") {
        return()
      } else if(req_degree_list_temp %in% req_degree_build_selected){
        return()
      } else {
        req_degree_build_selected <<- rbind(req_degree_build_selected, as.list(req_degree_list_temp))
        req_degree_cards()
        update_filter("required")
      }
    }
  })
  
# View/Edit Scenario ---------------
  choosen_scenario <- character()
  new_scenario_name <- character()
  temp_scenario <- character()
  favorite_temp <- character()
  update_fav_num <- function() {
    favor_num <- fav_in_app
    output$favorites <- renderUI({h4(paste0("Favorite Options (", NROW(favor_num),")"))})
  }
  build_radio <- function(){  
    scen_temp2 <- user_scenarios %>%  distinct(user_scenarios$scenario, .keep_all = TRUE)
    scen_temp2 <- scen_temp2[grep("Scenario", scen_temp2$scenario),]
    if(!is_empty(scen_temp2$scenario)){
      output$scenario_available <- renderUI({
        div(
          div(id = "scenario_radio",
              awesomeRadio(inputId = "scen_radio", label = "", choices = sort(scen_temp2$scenario),
                           selected = scen_temp2$scenario[NROW(scen_temp2$scenario)])), 
          div(id = "scen_buttons",
              splitLayout(cellWidths = c("50%","50%"), align = "center",
                          actionButton(inputId = "scen_edit", label = "Edit"),
                          actionButton(inputId = "scen_del", label = "Delete")) ))
      })
    } else {
      output$scenario_available <- renderUI({
        div(id = "scenario_radio",
            awesomeRadio(inputId = "scen_radio", label = "", choices = sort(scen_temp2$scenario),
                         selected = scen_temp2$scenario[NROW(scen_temp2$scenario)]))
      })
    }
  } 
  observeEvent(input$tabs,{
    req(selected = "scenario")
    build_radio()
    update_fav_num()
  })
  create_scenario_table <- function(){
    scen_temp <- filter(user_scenarios, scenario %in% choosen_scenario)
    sch_temp <- scen_temp %>% filter(category == "school") %>% select(id)
    maj_temp <- scen_temp %>% filter(category == "major") %>% select(id)
    occ_temp <- scen_temp %>% filter(category == "occupation") %>% select(id)
    deg_temp <- scen_temp %>% filter(category == "degree") %>% select(id)
    req_deg_temp <- scen_temp %>% filter(category == "required") %>% select(id)
    scenario_temp <- backbone
    if(!is_empty(sch_temp$id)) {
      scenario_temp <- filter(scenario_temp, UNITID %in% sch_temp$id)
    }
    if(!is_empty(maj_temp$id)) {
      scenario_temp <- filter(scenario_temp, CIPCODE %in% maj_temp$id)
    }
    if(!is_empty(occ_temp$id)) {
      scenario_temp <- filter(scenario_temp, OCCCODE %in% occ_temp$id)
    }
    if(!is_empty(deg_temp$id)) {
      scenario_temp <- filter(scenario_temp, AWLEVEL %in% deg_temp$id)
    }
    if(!is_empty(req_deg_temp$id)) {
#      scenario_temp <- filter(scenario_temp, Entry_Code %in% req_deg_temp$id)
      
#      r_temp <- filter(req_degree_filter, Entry_Degree %in% req_degree_build_selected) %>% select(Entry_Code)
      req_degree_temp <- req_degree_xwalk %>% filter(Entry_Code %in% req_deg_temp$id, GREATER == "TRUE")
      scenario_temp <- scenario_temp %>% filter(Entry_Code %in% req_deg_temp$id, AWLEVEL %in% req_degree_temp$AWLEVEL)
    }
    favorite_temp <<- scenario_temp
    scenario_temp <- left_join(scenario_temp, school_scenario, by = "UNITID")
    scenario_temp <- left_join(scenario_temp, major_scenario, by = "CIPCODE")
    scenario_temp <- left_join(scenario_temp, degree_scenario, by = "AWLEVEL")
    scenario_temp <- left_join(scenario_temp, occupation_scenario, by = "OCCCODE")
    scenario_temp <- left_join(scenario_temp, req_degree_scenario, by = "Entry_Code")
    scenario_temp <- scenario_temp %>% select(ID,UNITID, CIPCODE, AWLEVEL, CTOTALT, OCCCODE, INSTNM, STABBR, CostLo, CostHi, CIPNAME, 
                                              LEVELName, OCCNAME, X17p)
    scenario_temp <- scenario_temp %>% rename("School" = "INSTNM", "State" = "STABBR", "Cost Hi" = "CostHi", "Cost Lo" = "CostLo",
                                              "Major" = "CIPNAME", "Degree" = "LEVELName", 
                                              "Occupation" = "OCCNAME", "Salary" = "X17p")
    output$scenario_table <- renderDataTable({
      DT::datatable(
        data = scenario_temp,
        escape = FALSE,
        rownames = FALSE,
        extensions = 'Responsive',
        class="cell-border stripe",
        options = list(
          headerCallback = DT::JS(
            "function(thead) {",
            "  $(thead).css('font-size', '22px');",
            "}"),
          dom = 'tip',
          saveState = TRUE,
          filter = FALSE,
          #           autoWidth = TRUE,
          columnDefs = (list(list(visible=FALSE, targets=c(0,1,2,3,4,5)),
                             list(width = '230px', targets =c(6,10,11,12)),
                             list(width = '75px', targets =7),
                             list(width = '85x', targets = c(8,9,13)),
                             list(className = 'dt-center', targets = 7),
                             list(targets = c(6,10,11,12),
                                  render = JS(
                                    "function(data, type, row, meta) {",
                                    "return type === 'display' && data.length > 22?",
                                    "'<span title=\"' + data + '\">' + data.substr(0, 22) + '...</span>' : data;",
                                    "}"))
          )),
          pageLength = 10
        ),
        selection = list(mode = 'multiple')
      ) %>%
        formatStyle(
          0,
          target = 'row',
          color = 'black',
          fontWeight = 'normal',
          fontSize = '16px',
          lineHeight = '150%',
          margin = '0px'
        )
    })
  }
  observeEvent(input$add_favorite_button, {
    req(input$scenario_table_rows_selected)
    favorite_to_add <- favorite_temp[input$scenario_table_rows_selected,]
    favor_temp <- fav_in_app %>% filter(ID %in% favorite_to_add$ID)
    if(is_empty(favor_temp$ID)){
      fav_temp <- favorite_to_add %>% select(ID, UNITID, CIPCODE, AWLEVEL, OCCCODE)
      fav_in_app <<- rbind(fav_in_app, fav_temp)
      update_fav_num()
      update_user_favs()
      favorite_cards()
    }
  })
  observeEvent(input$scen_radio, {
    choosen_scenario <<- input$scen_radio
    create_scenario_table()
  })
  observeEvent(input$return_dashboard_button, {
    updateTabItems(session, "tabs", selected = "dashboard")
  })
  
  observeEvent(input$build_new_button, {
    clear_build()
    updateTabItems(session, "tabs", selected = "build")
  })
  
  observeEvent(input$scen_edit, {
    edit_scenario <<- 1
    temp_scenario <<- choosen_scenario
    school_temp <- user_scenarios %>% filter(scenario == temp_scenario, source == "build", category == "school") %>% select(id)
    school_build_selected2 <- school_filter %>% filter(UNITID %in% school_temp$id) %>% select(INSTNM)
    if(is_empty(school_build_selected2$INSTNM)) {
      school_build_selected <<- vector(mode = "list")
    } else {
      school_build_selected <<- vector(mode = "list")
      school_build_selected <<- cbind(school_build_selected, school_build_selected2$INSTNM)
    }
    
    major_temp <- user_scenarios %>% filter(scenario == temp_scenario, source == "build", category == "major") %>% select(id)
    major_build_selected2 <- major_filter %>% filter(CIPCODE %in% major_temp$id) %>% select(CIPNAME)
    if(is_empty(major_build_selected2$CIPNAME)) {
      major_build_selected <<- vector(mode = "list")
    } else {
      major_build_selected <<- vector(mode = "list")
      major_build_selected <<- cbind(major_build_selected, major_build_selected2$CIPNAME)
    }
    
    occupation_temp <- user_scenarios %>% filter(scenario == temp_scenario, source == "build", category == "occupation") %>% select(id)
    occupation_build_selected2 <- occupation_filter %>% filter(OCCCODE %in% occupation_temp$id) %>% select(OCCNAME)
    if(is_empty(occupation_build_selected2$OCCNAME)) {
      occupation_build_selected <<- vector(mode = "list")
    } else {
      occupation_build_selected <<- vector(mode = "list")
      occupation_build_selected <<- cbind(occupation_build_selected, occupation_build_selected2$OCCNAME)
    }
    
    degree_temp <- user_scenarios %>% filter(scenario == temp_scenario, source == "build", category == "degree") %>% select(id)
    degree_build_selected2 <- degree_filter %>% filter(AWLEVEL %in% degree_temp$id) %>% select(LEVELName)
    if(is_empty(degree_build_selected2$LEVELName)) {
      degree_build_selected <<- vector(mode = "list")
    } else {
      degree_build_selected <<- vector(mode = "list")
      degree_build_selected <<- cbind(degree_build_selected, degree_build_selected2$LEVELName)
    }
    req_degree_temp <- user_scenarios %>% filter(scenario == temp_scenario, source == "build", category == "required") %>% select(id)
    req_degree_build_selected2 <- req_degree_filter %>% filter(Entry_Code %in% req_degree_temp$id) %>% select(Entry_Degree)
    if(is_empty(req_degree_build_selected2$Entry_Degree)) {
      req_degree_build_selected <<- vector(mode = "list")
    } else {
      req_degree_build_selected <<- vector(mode = "list")
      req_degree_build_selected <<- cbind(req_degree_build_selected, req_degree_build_selected2$Entry_Degree)
    }
    user_scenarios <<- user_scenarios %>% filter(!(scenario == temp_scenario))
    school_cards()
    major_cards()
    occupation_cards()
    degree_cards()
    req_degree_cards()
    update_filter("all")
    build_radio()
    temp_choice$school_status <<- 3
    temp_choice$major_status <<- 3
    temp_choice$occupation_status <<- 3
    temp_choice$degree_status <<- 3
    temp_choice$reqdegree_status <<- 3
    updateTabItems(session, "tabs", selected = "build")
  })
  
  observeEvent(input$scen_del, {
    showModal(modalDialog(
      title= paste0("Are you sure you want to Delete ", input$scen_radio),
      footer = tagList(actionButton("confirmDelete", "Delete"),
                       modalButton("Cancel")
      )
    ))
  })
  observeEvent(input$confirmDelete, {
    req(input$scen_del)
    user_scenarios <<- user_scenarios %>% filter(!(scenario == input$scen_radio))
    #    save_scenario()
    build_radio()
    output$scenario_table <- renderDataTable({NULL})
    removeModal()
  }) 
  update_scenario <- function() {
    user_scen_temp <- user_scenarios %>% select(scenario) %>% filter(!(scenario == "favorite")) %>% distinct(scenario, .keep_all = FALSE)
    if(NROW(user_scen_temp) == 0){
      new_scenario_name <<- "Scenario 1"
    } else {
      temp_val <- user_scenarios %>% select(scenario) %>% filter(!(scenario == "favorite")) %>% distinct(scenario, .keep_all = FALSE)
      temp_val <- as.vector(temp_val$scenario)
      temp_val <- str_remove_all(temp_val, "Scenario ")
      temp_val <- max(as.numeric(temp_val))
      new_scenario_name <<- paste0("Scenario ",temp_val + 1)
    }
  }
  
  update_user_favs <- function() {
    user_favorites <<- tibble("user" = character(), "school" = character(), "major" = character(), "occupation" = character(),
                              "degree" = character())
    if(NROW(fav_in_app) > 0){
      for(i in 1:NROW(fav_in_app)){
        fav_temp <- tibble("user" = pro_user, "school" = fav_in_app$UNITID[i], "major" = fav_in_app$CIPCODE[i],
                           "occupation" = fav_in_app$OCCCODE[i], "degree" = fav_in_app$AWLEVEL[i])
        user_favorites <<- rbind(user_favorites, fav_temp)
      }
    }
  }
 
# Explore School -------------------
  
  school_favorite_temp <- character()
  schooldetailList <- list()
  schoolobsList <- list()
  schoolfavobsList <- list()
  
  explore_school_var <- reactive ({  
    explore_school_temp <- school_scenario
    if(!is.null(input$es_school) & input$es_school !='') {
      explore_school_temp <- filter(explore_school_temp, INSTNM %in% input$es_school)
    }
    if( input$es_state !='') {
      explore_school_temp <- filter(explore_school_temp, State %in% input$es_state)
    }
    #      explore_school_temp <- filter(explore_school_temp, (ROOM_BOARD >= input$es_room_board[1] & ROOM_BOARD <= input$es_room_board[2]))
    explore_school_temp <- filter(explore_school_temp, (CostHi >= input$es_annual_hi[1] & CostHi <= input$es_annual_hi[2]))
    explore_school_temp <- filter(explore_school_temp, (CostLo >= input$es_annual_lo[1] & CostLo <= input$es_annual_lo[2]))
    
    if(is.null(input$es_school) |input$es_school == ''){
      updateSelectInput(session, inputId = "es_school", label = "School",
                        choices = isolate(c(All = '', sort(unique(explore_school_temp$INSTNM)))), selected = '')
    }
    if(is.null(input$es_state) |input$es_state == ''){
      updateSelectInput(session, inputId = "es_state", label = "State",
                        choices = isolate(c(All = '', sort(unique(explore_school_temp$State)))), selected = '')
    }
    #    explore_school_temp <- explore_school_temp %>% mutate(GRADR150 = BAGR150 + L4GR150)   
    school_favorite_temp <<- explore_school_temp
    explore_school_temp <- explore_school_temp %>% select("INSTNM", "STABBR", "CITY", "WEBADDR", "ADMPCT", "ENRLT", "FTETOT",
                                                          "PERCENT150", "InStateGnt", "CostLo", "CostHi")
    
    es_temp1 <- explore_school_temp$CostLo == 0
    explore_school_temp <- cbind(explore_school_temp, es_temp1)
    
    #Rename table column headers     
    explore_school_temp <- explore_school_temp %>% rename("School<br>Name" = "INSTNM", "State" = "STABBR", "City" = "CITY",
                                                          "Web<br>Address" = "WEBADDR", "Admit" = "ADMPCT", "Enroll" = "ENRLT",
                                                          "Total<br>Enroll" = "FTETOT",
                                                          "Grad<br>Rate<br>150%" = "PERCENT150", "Inst<br>Grant" = "InStateGnt",
                                                          "Cost Lo" = "CostLo", "Cost Hi" = "CostHi")
  })
  school_favorite_cards <- function() {
    fav_card <- tibble()
    fav_temp <- user_scenarios %>% filter(source %in% "favorite", category %in% "school")
    fav_card <- school_master2 %>% filter(UNITID %in% fav_temp$id)         
    scenario_temp <- fav_card 
    if(NROW(scenario_temp) > 0){
      output$school_favorite_container <- renderUI({
        args <- lapply(1:NROW(scenario_temp), function(x) fav_school_card(name = scenario_temp$UNITID[x],
                                                                          school = scenario_temp$INSTNM[x],
                                                                          city = scenario_temp$CITY[x],
                                                                          state = scenario_temp$STABBR[x],
                                                                          new_enroll = scenario_temp$ENRLT[x],
                                                                          tuition_low = scenario_temp$TotCstInHi[x],
                                                                          tuition_hi = scenario_temp$TotCstOutHi[x]))
        args$cellArgs <- list(
          style = "width: 350px;height: auto;margin: 5px;margin-bottom:50px;margin-left:45px;line-height: '110%';"
        )
        do.call(shiny::flowLayout, args)
      })
    } else {
      output$school_favorite_container <- renderUI({NULL})
      schoolobsList <<- list()
    }
  }
  
  fav_school_card <- function(name,school,city,state,new_enroll,tuition_low,tuition_hi) {
    make_school_observer(name)
    make_school_detail_obs(name)
    make_school_fav_obs(name, school)
    div(
      class = "explore_card", 
      div(
        style = "display:inline-block;vertical-align:top;width: 338px;margin:0px;padding: 15px;border:2px solid #e9ecee;
        border-top: 12px solid #e2ac24;border-radius: 10px;",
        splitLayout(
          cellWidths = c("92%", "8%"),
          h4(school, style = "margin-top: 5px;overflow:hidden;"),
          actionButton(
            inputId = paste0("schoolbutton", name),
            label = "",
            icon = icon("trash"),
            style = "margin:0px;padding: 2px 5px;color:#cc0000;font-size:12px;"
          )
        ),
        h5(style = "font-style: italic;margin-top:0px;",city, ",", state),
        hr(style = "margin:0px;border-top: 1px solid #000000;"),
        br(),
        div(
          style = "font-family: Arial;font-size:16px; color:#999999;",
          p(strong( "Newly Enrolled :"),new_enroll),
          p( strong("Annual In State Cost :"),"$",tuition_low),
          p(strong("Annual Out of State Cost :"),"$",tuition_hi ),
          hr(style = "margin:0px;border-top: 1px solid #000000;") ,
          br(),
          div(
            div(style = "float:left;display: inline-block;vertical-align:top;",
                actionButton(inputId = paste0("school_fav_add", name), "Add to Scenario", style = "color:white; background-color:#e2ac24;")),
            div(
              class = "view_detail", style = "float:right;",
              actionButton(
                inputId = paste0("school_details", name),
                label = "More Details",
                style = "border:0px;color:#4fa0f7;background-color:white;margin-left:0px;padding:0px;
                                                 border-bottom: 1px solid #4fa0f7;font-size:16px;"
              )
            ))
        )
      )
    )
  }
  make_school_observer <- function(name){
    schoolbtName <- paste0("schoolbutton",name)
    if(length(schoolobsList[[schoolbtName]]) == 0){
      schoolobsList[[schoolbtName]] <<- observeEvent(input[[schoolbtName]], {
        user_scenarios <<- user_scenarios %>% filter(!(id == name & scenario == "favorite" & category == "school"))   
        #        save_scenario()
        schoolobsList <<- list()
        school_favorite_cards()
      }, ignoreInit = TRUE, once = TRUE)
    }
  }
  make_school_detail_obs <- function(name) {
    schooldetailNm <- paste0("school_details",name)
    if(length(schooldetailList[[schooldetailNm]]) == 0){
      schooldetailList[[schooldetailNm]] <<- observeEvent(input[[schooldetailNm]], {
        # this is where school details goes
        
      },ignoreInit = TRUE)
    }
  }
  make_school_fav_obs <- function(name, school){
    schoolfavobsNm <- paste0("school_fav_add", name)
    if(is.null(schoolfavobsList[[schoolfavobsNm]])) {
      schoolfavobsList[[schoolfavobsNm]] <<- observeEvent(input[[schoolfavobsNm]], {
        if(NROW(school_build_selected) < 5) {
          if(school %in% school_build_selected){
            return()
          } else {
            school_build_selected <<- rbind(school_build_selected, school)
            school_cards()
            update_filter("school")
          }
        }
      },ignoreInit = TRUE)
    }
  }
  observe({  
    output$explore_school_table <- renderDT({
      DT::datatable(
        data = explore_school_var(),
        escape = FALSE,
        rownames = FALSE,
        class="cell-border stripe",
        extensions = 'Responsive',
        options = list(
          headerCallback = DT::JS(
            "function(thead) {",
            "  $(thead).css('font-size', '15px');",
            "}"),
          dom = 'tip',
          pageLength = 15,
          order = list(list(11, 'asc'), list(9, 'asc')),
          saveState = TRUE,
          filter = FALSE,
          #         autoWidth = TRUE,
          columnDefs = (list(list(visible=FALSE, targets=11),
                             list(width = '250px', targets = 0),
                             list(width = '25px', targets = 1),
                             list(width = '185px', targets = 2),
                             list(width = '250px', target = 3),
                             list(width = '63px', className = 'dt-body-right', targets = c(4,5,6,7,8,9,10)),
                             list(targets = c(0,3),
                                  render = JS(
                                    "function(data, type, row, meta) {",
                                    "return type === 'display' && data.length > 20?",
                                    "'<span title=\"' + data + '\">' + data.substr(0, 20) + '...</span>' : data;",
                                    "}"))
          )),
          
          lengthMenu = c(15)
        ),
        callback = JS('table.page(3).draw(false);'),
        selection = list(mode = 'single')
      ) #%>%
        #formatStyle(0,target = 'row', color = 'black',fontWeight = 'normal',fontSize = '14px',lineHeight = '100%',
         # margin = '0px', paddingleft = '10px')
    })
  })
  observeEvent(input$school_favorite, {
    req(input$explore_school_table_rows_selected)
    favorite_to_add <- school_favorite_temp[input$explore_school_table_rows_selected,]    
    fav_school <- favorite_to_add %>% select(UNITID)        
    favor_temp <- user_scenarios %>% filter(scenario %in% "favorite", source %in% "favorite", category %in% "school", id %in% fav_school)
    if(is_empty(favor_temp$scenario)){
      fav_temp <- tibble("user" = pro_user, "scenario" = "favorite", "source" = "favorite",  "category" = "school", "id" = fav_school$UNITID )
      user_scenarios <<- rbind(user_scenarios, fav_temp)
      #      save_scenario()
      school_favorite_cards()
    }
  })  
# Explore Occupation ---------------
  occupation_favorite_temp <- character()
  occupationdetailList <- list()
  occupationobsList <- list()
  occupationfavobsList <- list()
  explore_occupation_master2 <- reactiveVal(0)
  explore_occupation_filter <- occupation_filter
  
  occupation_alt_filter <- reactive({
    occupation_t1 <- alt_title_all
    if(!is.null(input$occupation_text)){
      occupation_t1 <- filter(occupation_t1 , grepl(input$occupation_text, occupation_t1$AltName, ignore.case = TRUE))
    }
  })
  observeEvent(input$occupation_text,{
    if(!is.null(input$occupation_text)){
      explore_occupation_filter <<- occupation_filter %>% filter(occupation_filter$OCCCODE %in% occupation_alt_filter()$OCCCODE) 
      value <- explore_occupation_master2() + 1
      explore_occupation_master2(value)
    }
  }) 
  observeEvent(input$DTDesc, {
    info_title <- as.character(explore_occupation_var()$Occupation[input$DTDesc[1]])
    info_code <- as.character(explore_occupation_var()$OCCCODE[input$DTDesc[1]])
    info_description <- filter(occupation_temp, grepl(info_code, occupation_temp$OCCCODE,
                                                      ignore.case = TRUE)) %>% select(Description)
    showModal(modalDialog(
      title = paste0(info_title),
      div(style = "padding: 10px 15px;margin: 5px;border: 1px solid black;", paste0(info_description)
      ),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  observeEvent(input$DTVideo, {
    info_title <- as.character(explore_occupation_var()$Occupation[input$DTVideo[1]])
    info_code <- as.character(explore_occupation_var()$OCCCODE[input$DTVideo[1]])
    soc_code <- filter(occupation_temp, grepl(info_code, occupation_temp$OCCCODE,
                                              ignore.case = TRUE)) %>% select(SOCCODE_2010)
    soc_code <- as.character(soc_code$SOCCODE_2010[1])
    occ_video <- paste0("https://cdn.careeronestop.org/OccVids/OccupationVideos/",soc_code, ".mp4")
    showModal(modalDialog(
      title = paste0(info_title),
      div(style = "margin: 5px;",tags$video(id = "video2", src = occ_video, type = "video/mp4", controls = "controls",width = "100%")
      ),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  explore_occupation_var <- reactive({
    eo_temp <- explore_occupation_master2()
    occupation_temp <- occupation_master2
    if(!is.null(input$occupation_text)){
      occupation_temp <- filter(occupation_temp, OCCNAME %in% explore_occupation_filter$OCCNAME)
    }
    if(!is.null(input$eo_entry_degree) & input$eo_entry_degree !='') {
      occupation_temp <- filter(occupation_temp, Entry_Degree %in% input$eo_entry_degree) 
    }
    if(!is.null(input$eo_required_exp) & input$eo_required_exp !='') {
      occupation_temp <- filter(occupation_temp, Experience %in% input$eo_required_exp) 
    }
    occupation_temp <- filter(occupation_temp, (EmplyPC >= input$eo_growth_rate[1] & EmplyPC <= input$eo_growth_rate[2]))
    occupation_temp <- filter(occupation_temp, (X17p >= input$eo_starting_salary[1] & X17p <= input$eo_starting_salary[2]))
    
    if(is.null(input$eo_entry_degree) |input$eo_entry_degree == ''){
      updateSelectInput(session, inputId = "eo_entry_degree", label = "Entry Degree",
                        choices = isolate(c(All = '', sort(unique(occupation_temp$Entry_Degree)))), selected = '')
    }
    if(is.null(input$eo_required_exp) |input$eo_required_exp == ''){
      updateSelectInput(session, inputId = "eo_required_exp", label = "Required Experience",
                        choices = isolate(c(All = '', sort(unique(occupation_temp$Experience)))), selected = '')
    }
    occupation_favorite_temp <<- occupation_temp
    occupation_temp <- occupation_temp %>% select("OCCNAME", "EmplyChg", "EmplyPC", "SelfEmpl", "Entry_Degree", "Experience", "X10p", 
                                                  "X25p", "X50p", "X75p", "X90p", "OCCCODE")
    if(NROW(occupation_temp) > 0) {
      occupation_temp <- occupation_temp %>% mutate(GID = 1:nrow(occupation_temp))
      
      occupation_temp <- occupation_temp %>% mutate(Description = paste0('<span style="float:right;"><a href="javascript:void(0)" onmousedown="',
                                                                         'Shiny.onInputChange(\'DTDesc\',[', GID, ',Math.random()]);',
                                                                         ' event.preventDefault(); event.stopPropagation(); return false;"><font color="grey">Descrip</font></a></span>'))
      occupation_temp <- occupation_temp %>% mutate(Video = paste0('<span style="float:right;"><a href="javascript:void(0)" onmousedown="',
                                                                   'Shiny.onInputChange(\'DTVideo\',[', GID, ',Math.random()]);',
                                                                   ' event.preventDefault(); event.stopPropagation(); return false;"><font color="grey">Video</font></a></span>'))
    }
    occupation_temp <- occupation_temp %>% rename("Occupation" = "OCCNAME", "Growth<br>in Jobs(#)" = "EmplyChg",
                                                  "Growth<br>in Jobs(%)" = "EmplyPC", "Percent<br>Self<br>Employed" = "SelfEmpl",
                                                  "Typical<br>Entry<br>Degree" = "Entry_Degree",
                                                  "Level of<br>Experience<br>Required" = "Experience",
                                                  "10th<br>Percentile<br>Salary" = "X10p", 
                                                  "25th<br>Percentile<br>Salary" = "X25p", "Median<br>Salary" = "X50p",
                                                  "75th<br>Percentile<br>Salary" = "X75p", 
                                                  "90th<br>Percentile<br>Salary" = "X90p")
  })
  observeEvent(input$occupation_favorite, {
    req(input$explore_occupation_table_rows_selected)
    favorite_to_add <- occupation_favorite_temp[input$explore_occupation_table_rows_selected,]    
    fav_occupation <- favorite_to_add %>% select(OCCCODE)        
    favor_temp <- user_scenarios %>% filter(scenario %in% "favorite", source %in% "favorite", category %in% "occupation", id %in% fav_occupation)
    if(is_empty(favor_temp$scenario)){
      fav_temp <- tibble("user" = pro_user, "scenario" = "favorite", "source" = "favorite",  "category" = "occupation", "id" = fav_occupation$OCCCODE )
      user_scenarios <<- rbind(user_scenarios, fav_temp)
      #      save_scenario()
      occupation_favorite_cards()
    }
  })

  observe({  
    output$explore_occupation_table <- renderDT({
      DT::datatable(
        data = explore_occupation_var(),
        escape = FALSE,
        rownames = FALSE,
        class="cell-border stripe", 
        extensions = 'Responsive', 
        options = list(
          headerCallback = DT::JS(
            "function(thead) {",
            "  $(thead).css('font-size', '15px');",
            "}"),
          dom = 'tip',
          order = list(list(7, 'desc')),
          saveState = TRUE,
          pageLength = 15,
          filter = FALSE,
#          autoWidth = TRUE,
          columnDefs = (list(list(visible=FALSE, targets = c(11,12)))
                        #              list(width = '300px', targets =c(11,13,14))
          ),
          #                               list(width = '25px', targets =c(12)),
          #                               list(width = '110px', targets = c(15,16,17,18)))),
          lengthMenu = c(15)
        ),
        selection = list(mode = 'single')
      ) %>%
        formatStyle(0,target = 'row', color = 'black',fontWeight = 'normal',fontSize = '15px',lineHeight = '100%')
    })
  })
  occupation_favorite_cards <- function() {
    fav_card <- tibble()
    fav_temp <- user_scenarios %>% filter(source %in% "favorite", category %in% "occupation")
    fav_card <- occupation_master2 %>% filter(OCCCODE %in% fav_temp$id)         
    scenario_temp <- fav_card 
    if(NROW(scenario_temp) > 0){
      output$occupation_favorite_container <- renderUI({
        args <- lapply(1:NROW(scenario_temp), function(x) fav_occupation_card(name = scenario_temp$OCCCODE[x],
                                                                              occupation = scenario_temp$OCCNAME[x],
                                                                              jobs = scenario_temp$Emply2018[x],
                                                                              growth = scenario_temp$EmplyPC[x],
                                                                              medwage = scenario_temp$MedWage[x],
                                                                              entry = scenario_temp$Entry_Degree[x],
                                                                              experience = scenario_temp$Experience[x],
                                                                              percent_10 = scenario_temp$X10p[x],
                                                                              percent_90 = scenario_temp$X90p[x]))
        args$cellArgs <- list(
          style = "width: 350px;height: auto;margin: 5px;margin-bottom:50px;margin-left:45px;line-height: '110%';"
        )
        do.call(shiny::flowLayout, args)
      })
    } else {
      output$occupation_favorite_container <- renderUI({NULL})
      occupationobsList <<- list()
    }
  }
  
  fav_occupation_card <- function(name,occupation,jobs,growth,medwage,entry,experience,percent_10, percent_90) {
    make_occupation_observer(name)
    make_occupation_detail_obs(name)
    make_occupation_fav_obs(name, occupation)
    div(
      class = "explore_card", 
      div(
        style = "display:inline-block;vertical-align:top;width: 338px;margin:0px;padding: 15px;border:2px solid #e9ecee;
        border-top: 12px solid #477ddd;border-radius: 10px;",
        splitLayout(
          cellWidths = c("92%", "8%"),
          h4(occupation, style = "margin-top: 5px;overflow:hidden;"),
          actionButton(
            inputId = paste0("occupationbutton", name),
            label = "",
            icon = icon("trash"),
            style = "margin:0px;padding: 2px 5px;color:#cc0000;font-size:12px;"
          )
        ),
        hr(style = "margin:0px;border-top: 1px solid #000000;"),
        br(),
        div(
          style = "font-family: Arial;font-size:16px; color:#999999;",
          p(strong("Jobs(K) :"), jobs),
          p(strong("Growth Rate :"), growth,"%"),
          p(strong("Entry Degree :"),entry),
          p(strong("Years of Exp :"), experience),
          p(strong("10th Percentile :"), "$",percent_10),
          p(strong("Median Pay :", "$"),medwage),
          p(strong("90th Percentile :", "$"),percent_90),
          
          hr(style = "margin:0px;border-top: 1px solid #000000;") ,
          br(),
          div(
            div(style = "float:left;display: inline-block;vertical-align:top;",
                actionButton(inputId = paste0("occupation_fav_add", name), "Add to Scenario",style = "color:white; background-color:#477ddd;")),
            div(
              class = "view_detail", style = "float:right;",
              actionButton(
                inputId = paste0("occupation_details", name),
                label = "More Details",
                style = "border:0px;color:#4fa0f7;background-color:white;margin-left:0px;padding:0px;
                                                 border-bottom: 1px solid #4fa0f7;font-size:16px;"
              )
            ))
        )
      )
    )
  }
  make_occupation_observer <- function(name){
    occupationbtName <- paste0("occupationbutton",name)
    if(is.null(occupationobsList[[occupationbtName]])){
      occupationobsList[[occupationbtName]] <<- observeEvent(input[[occupationbtName]], {
        user_scenarios <<- user_scenarios %>% filter(!(id == name & scenario == "favorite" & category == "occupation"))   
        #        save_scenario()
        occupationobsList <<- list()
        occupation_favorite_cards()
      }, ignoreInit = TRUE, once = TRUE)
    }
  }
  make_occupation_detail_obs <- function(name) {
    occupationdetailNm <- paste0("occupation_details",name)
    if(is.null(occupationdetailList[[occupationdetailNm]])){
      occupationdetailList[[occupationdetailNm]] <<- observeEvent(input[[occupationdetailNm]], {
        # this is where occupation details goes
        
      },ignoreInit = TRUE)
    }
  }
  make_occupation_fav_obs <- function(name, occupation){
    occupationfavobsNm <- paste0("occupation_fav_add", name)
    if(is.null(occupationfavobsList[[occupationfavobsNm]])) {
      occupationfavobsList[[occupationfavobsNm]] <<- observeEvent(input[[occupationfavobsNm]], {
        if(NROW(occupation_build_selected) < 5) {
          if(occupation %in% occupation_build_selected){
            return()
          } else {
            occupation_build_selected <<- rbind(occupation_build_selected, occupation)
            occupation_cards()
            update_filter("occupation")
          }
        }
      },ignoreInit = TRUE)
    }
  }
  
# Explore Major --------------------
  major_favorite_temp <- character()
  majordetailList <- list()
  majorobsList <- list()
  majorfavobsList <- list()
  explore_major_var <- reactive ({ 
    curriculum_temp <- major_master
    if(!is.null(input$em_school) & input$em_school !='') {
      school_temp <- filter(school_master2, INSTNM %in% input$em_school) %>% select(UNITID)
      curriculum_temp <- filter(curriculum_temp, UNITID %in% school_temp)
    }
    if(!is.null(input$em_major) & input$em_major !='') {
      curr_temp <- filter(cips, CIPNAME %in% input$em_major) %>% select(CIPCODE)
      curriculum_temp <- filter(curriculum_temp, CIPCODE %in% curr_temp)                      
    }
    if(!is.null(input$em_degree) & input$em_degree !='') {
      degree_temp <- filter(degree_master, LEVELName %in% input$em_degree) %>% select(AWLEVEL)
      curriculum_temp <- filter(curriculum_temp, AWLEVEL %in% degree_temp)
    }
    curriculum_temp <- left_join(curriculum_temp, cips, by = "CIPCODE")
    curriculum_temp <- left_join(curriculum_temp, degree_master, by = c("AWLEVEL"))
    curriculum_temp <- left_join(curriculum_temp, school_master2, by = "UNITID")
    major_favorite_temp <<- curriculum_temp %>% select("ID", "CIPNAME", "LEVELName", "INSTNM", "CITY", "STABBR", "GTotCstOutHi", "ROOM_BOARD")
    curriculum_temp <- curriculum_temp %>% select("CIPNAME", "LEVELName", "INSTNM", "CTOTALT")
    if(is.null(input$em_school) |input$em_school == ''){
      updateSelectInput(session, inputId = "em_school", label = "School",
                        choices = isolate(c(All = '', sort(unique(curriculum_temp$INSTNM)))), selected = '')
    }
    if(is.null(input$em_major)|input$em_major == ''){
      updateSelectInput(session, inputId = "em_major", label = "Major",
                        choices = isolate(c(All = '', sort(unique(curriculum_temp$CIPNAME)))), selected = '')
    }
    if(is.null(input$em_degree) | input$em_degree == ''){
      updateSelectInput(session, inputId = "em_degree", label = "Degree",
                        choices = isolate(c(All = '', sort(unique(curriculum_temp$LEVELName)))), selected = '') 
    }
    #Rename table column headers  

    curriculum_temp <- curriculum_temp %>% rename("School<br>Name" = "INSTNM", 
                                                  "Major" = "CIPNAME", "Degree<br>Name" = "LEVELName",
                                                  "Number of<br>Degrees" = "CTOTALT")
  })
  major_favorite_cards <- function() {
    fav_card <- tibble()
    fav_temp <- user_scenarios %>% filter(source %in% "favorite", category %in% "major")
    fav_card <- major_master
    fav_card <- fav_card %>% filter(ID %in% fav_temp$id) 
    fav_card <- left_join(fav_card, cips, by = "CIPCODE")
    fav_card <- left_join(fav_card, degree_master, by = c("AWLEVEL"))
    fav_card <- left_join(fav_card, school_master2, by = "UNITID")
    fav_card <- fav_card %>% select("ID", "CIPNAME", "LEVELName", "INSTNM", "CITY", "STABBR", "CTOTALT")
    scenario_temp <- fav_card 
    if(NROW(scenario_temp) > 0){
      output$major_favorite_container <- renderUI({
        args <- lapply(1:NROW(scenario_temp), function(x) fav_major_card(name = scenario_temp$ID[x],
                                                                         school = scenario_temp$INSTNM[x],
                                                                         city = scenario_temp$CITY[x],
                                                                         state = scenario_temp$STABBR[x],
                                                                         major = scenario_temp$CIPNAME[x],
                                                                         degree = scenario_temp$LEVELName[x],
                                                                         num_degrees = scenario_temp$CTOTALT[x])
        )
        args$cellArgs <- list(
          style = "width: 350px;height: auto;margin: 5px;margin-bottom:50px;margin-left:45px;line-height: '110%';"
        )
        do.call(shiny::flowLayout, args)
      })
    } else {
      output$major_favorite_container <- renderUI({NULL})
      majorobsList <<- list()
    }
  }
  
  fav_major_card <- function(name,school,city,state,major,degree, num_degrees) {
    make_major_observer(name)
    make_major_detail_obs(name)
    make_major_fav_obs(name, major)
    div(
      class = "explore_card", 
      div(
        style = "display:inline-block;vertical-align:top;width: 338px;margin:0px;padding: 15px;border:2px solid #e9ecee;
        border-top: 12px solid #37b749;border-radius: 10px;",
        splitLayout(
          cellWidths = c("92%", "8%"),
          h4(major, style = "margin-top: 5px;overflow:hidden;"),
          actionButton(
            inputId = paste0("majorbutton", name),
            label = "",
            icon = icon("trash"),
            style = "margin:0px;padding: 2px 5px;color:#cc0000;font-size:12px;"
          )
        ),
        h5(style = "font-style: italic;margin-top:0px;",school),
        h5(style = "font-style: italic;margin-top:0px;",city, ",", state),
        hr(style = "margin:0px;border-top: 1px solid #000000;"),
        br(),
        div(
          style = "font-family: Arial;font-size:16px; color:#999999;",
          p(strong("Degree :"), degree),
          p(strong("Certificates Awarded :"), num_degrees),
          hr(style = "margin:0px;border-top: 1px solid #000000;") ,
          br(),
          div(
            div(style = "float:left;display: inline-block;vertical-align:top;",
                actionButton(inputId = paste0("major_fav_add", name), "Add to Scenario", style = "color:white; background-color:#37b749;")),
            div(
              class = "view_detail", style = "float:right;",
              actionButton(
                inputId = paste0("major_details", name),
                label = "More Details",
                style = "border:0px;color:#4fa0f7;background-color:white;margin-left:0px;padding:0px;
                                                 border-bottom: 1px solid #4fa0f7;font-size:16px;"
              )
            ))
        )
      )
    )
  }
  make_major_observer <- function(name){
    majorbtName <- paste0("majorbutton",name)
    if(length(majorobsList[[majorbtName]]) == 0){
      majorobsList[[majorbtName]] <<- observeEvent(input[[majorbtName]], {
        user_scenarios <<- user_scenarios %>% filter(!(id == name & scenario == "favorite" & category == "major"))   
        #       save_scenario()
        majorobsList <<- list()
        major_favorite_cards()
      }, ignoreInit = TRUE, once = TRUE)
    }
  }
  make_major_detail_obs <- function(name) {
    majordetailNm <- paste0("major_details",name)
    if(length(majordetailList[[majordetailNm]]) == 0){
      majordetailList[[majordetailNm]] <<- observeEvent(input[[majordetailNm]], {
        # this is where major details goes
        
      },ignoreInit = TRUE)
    }
  }
  make_major_fav_obs <- function(name, major){
    majorfavobsNm <- paste0("major_fav_add", name)
    if(is.null(majorfavobsList[[majorfavobsNm]])) {
      majorfavobsList[[majorfavobsNm]] <<- observeEvent(input[[majorfavobsNm]], {
        if(NROW(major_build_selected) < 5) {
          if(major %in% major_build_selected){
            return()
          } else {
            major_build_selected <<- rbind(major_build_selected, major)
            major_cards()
            update_filter("major")
          }
        }
      },ignoreInit = TRUE)
    }
  }
  observe({  
    output$em_table <- renderDT({
      DT::datatable(
        data = explore_major_var(),
        escape = FALSE,
        rownames = FALSE,
        class="cell-border stripe",
        extensions = 'Responsive',
        options = list(
          headerCallback = DT::JS(
            "function(thead) {",
            "  $(thead).css('font-size', '15px');",
            "}"),
          dom = 'tip',
          order = list(list(3, 'desc')),
          saveState = TRUE,
          pageLength = 15,
          filter = FALSE,
          #        autoWidth = FALSE,
          columnDefs = (list(list(width = '330px', targets =c(0,1,2)),
                             list(width = '80px', targets =c(3)))),
          lengthMenu = c(15)
        ),
        selection = list(mode = 'single')
      ) #%>%
       # formatStyle(0,target = 'row', color = 'black', fontWeight = 'normal', fontSize = '15px', lineHeight = '100%')
    })
  })
  observeEvent(input$major_favorite, {
    req(input$em_table_rows_selected)
    favorite_to_add <- major_favorite_temp[input$em_table_rows_selected,]    
    fav_major <- favorite_to_add %>% select(ID)        
    favor_temp <- user_scenarios %>% filter(scenario %in% "favorite", source %in% "favorite", category %in% "major", id %in% fav_major)
    if(is_empty(favor_temp$scenario)){
      fav_temp <- tibble("user" = pro_user, "scenario" = "favorite", "source" = "favorite",  "category" = "major", "id" = fav_major$ID )
      user_scenarios <<- rbind(user_scenarios, fav_temp)
      #      save_scenario()
      major_favorite_cards()
    }
  })    
# Tutorials ------------------------
  
# Tools ----------------------------
  observeEvent(input$onetonline, {
    output$web_page <- renderUI({
      iframe(width = "100%",
             height = "801",
             url_link = "https://www.onetonline.org/"
      )
    })
  })
  observeEvent(input$collegescorecard, {
    #    output$web_page <- renderUI({
    #      iframe(width = "100%",
    #             height = "751",
    #             url_link = "https://collegescorecard.ed.gov/"
    #             )
    #    })
  })
  observeEvent(input$collegenavigator, {
    #   output$web_page <- renderUI({
    #      iframe(width = "100%",
    #             height = "751",
    #             url_link = "https://nces.ed.gov/collegenavigator/"
    #             )
    #    })
  })
  observeEvent(input$careeronestop, {
    output$web_page <- renderUI({
      iframe(width = "100%",
             height = "801",
             url_link = "https://www.careeronestop.org/"
      )
    })
  })
  observeEvent(input$wowi, {
    output$web_page <- renderUI({
      iframe(width = "100%",
             height = "801",
             url_link = "https://www.wowi.com/"
      )
    })
  })
  observeEvent(input$mynextmove, {
    output$web_page <- renderUI({
      iframe(width = "100%",
             height = "801",
             url_link = "https://www.mynextmove.org/"
      )
    })
  })
  observeEvent(input$careeronestop1, {
    output$web_page <- renderUI({
      iframe(width = "100%",
             height = "801",
             url_link = "https://www.careeronestop.org/"
      )
    })
  })
# SERVER VARIABLES  

  save_scenario <- function() {
    if(pro_name != "Demo") {
      saveRDS(user_scenarios, scenario_file)
      drop_upload(scenario_file, path = "responses")
    }
    clean_scenario()
  }
  
  save_favorite <- function() {
    if(pro_name != "Demo") {
      saveRDS(user_favorites, favorite_file)
      drop_upload(favorite_file, path = "responses")
    }
    clean_favorite()
  }
  session$onSessionEnded(function() {
    if(favorite_file != "favorite.rds"){
    if(pro_name != "Demo") {
      saveRDS(user_favorites, favorite_file)
      drop_upload(favorite_file, path = "responses")
      saveRDS(user_scenarios, scenario_file)
      drop_upload(scenario_file, path = "responses")
      unlink(scenario_file)
      unlink(favorite_file)
      }
    }
    
  })
#      autoInvalidate <- reactiveTimer(500)
  
#      observe({
  # Invalidate and re-execute this reactive expression every time the
  # timer fires.
#        autoInvalidate()
#        print(mem_used())
  # Do something each time this is invalidated.
  # The isolate() makes this observer _not_ get invalidated and re-executed
  # when input$n changes.
  #print(paste("The value of input$n is", isolate(input$n)))
#      })
#  mem_used()

#END OF SERVER CODE  
}

shinyApp(ui, server) 