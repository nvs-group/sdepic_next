# Self Directed EPIC 8/19/2020 ----
# 01 Load Libraries ----
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)

library(tidyverse)

library(Hmisc)
library(rmarkdown)
library(DT)
library(tools)
library(shinyjs)
library(sodium)
library(shinyBS)
library(extrafont)
library(shinyLP)
library(rdrop2)
library(shinyalert)
#library(shinythemes)

# 02 Declare Variables ----
# UI Variables
token <- readRDS("signin.rds")
drop_auth(rdstoken = "signin.rds")
#credentials = data.frame(
#  id = "USER1",
#  username_id = "Demo",
#  email = "demo@email.com",
#  state = "Virginia",
#  passod   = sapply("password",password_store),
#  permission  = c("advanced"), 
#  stringsAsFactors = F
#)
#saveRDS(credentials, "cred.rds")
#drop_upload("cred.rds", path = "responses")
drop_download("responses/cred.rds", overwrite = TRUE)
credentials <- readRDS("cred.rds")
# Server Variables
backbone <- readRDS("data/Backbone.rds")

backbone <- tibble::rowid_to_column(backbone, "ID")
alt_title <- readRDS("data/AltTitle.rds")
degree_master <- readRDS("data/AW_Degree.rds")

cips <- readRDS("data/CIP_List.rds")
cips <- rbind(cips, "No Match")
cips <- cips %>% rename("CIPCODE" = "Codevalue", "CIPNAME" = "valueLabel")
#write.csv(cips, "cips.csv")
#ent_degree <- readRDS("data/Ent_Degree.rds")
state_abbr <- readRDS("data/state_abbr.rds")
major_master <- readRDS("data/CIPS.rds")

#ent_degree <- readRDS("data/Ent_Degree.rds")
occupation_master <- readRDS("data/Occupations.rds")

#occupation_master <- occupation_master %>% rename("Entry_Code" = "AWLEVEL", "Entry_Degree" = "LEVELName")
#occupation_master <- occupation_master %>% rename("OCCNAME" = "occ_title")
school_master <- readRDS("data/Schools.rds")
school_master2 <- school_master %>% filter(UNITID %in% unique(backbone$UNITID))
school_master2 <- school_master2 %>% mutate(ROOM_BOARD = (ROOMAMT + BOARDAMT + RMBRDAMT))
school_master3 <- school_master2
school_master3$INSTNM <- make.unique(as.character(school_master3$INSTNM), sep = "_")



state_abbr_master <- readRDS("data/state_abbr.rds")
school_master2 <- left_join(school_master2, state_abbr_master, by = "STABBR")
today <- Sys.Date()
current_date2 <- format(today, "%d, %B %Y, %A")

school_list <- school_master3 %>% filter(UNITID %in% unique(backbone$UNITID)) %>% select(INSTNM)
school_list <- school_list[order(school_list),,drop = FALSE]
school_list2 <- school_list

major_list <- cips %>% filter(CIPCODE %in% backbone$CIPCODE) %>% select(CIPNAME)
major_list <- major_list[order(major_list),,drop = FALSE]
major_list2 <- major_list

occupation_list <- occupation_master %>% filter(OCCCODE %in% backbone$OCCCODE) %>% select(OCCNAME)
occupation_list <- occupation_list[order(occupation_list),,drop = FALSE]
occupation_list2 <- occupation_list

degree_list <- degree_master %>% select(LEVELName)
degree_list2 <- degree_list

school_list3 <- school_master %>% filter(UNITID %in% unique(major_master$UNITID)) %>% select(INSTNM)
school_list3 <- school_list3[order(school_list3),,drop = FALSE]

major_list3 <- cips %>% filter(CIPCODE %in% unique(major_master$CIPCODE)) %>% select(CIPNAME)
major_list3 <- major_list3[order(major_list3),,drop = FALSE]

degree_list3 <- degree_master %>% filter(AWLEVEL %in% unique(major_master$AWLEVEL)) %>% select(LEVELName)
pro_name <- character()
pro_email <- character()
pro_state <- character()
pro_user <- character()
# 03 Functions ----
school_list4 <- tibble("INSTNM" = character())
school_list4 <- as_tibble(make.unique(as.character(school_list$INSTNM), sep = "_"))
#write_csv(school_master, "School.csv")

# 04.1 Header ----
header <- dashboardHeaderPlus( uiOutput("logoutbtn"))

# 04.2 Sidebar ----
sidebar <- dashboardSidebar(
  tags$img(src = 'BlueLogo_Final.png',contentType = "image/png", style = "width: 285px; height: 180px;align:center;"),
  uiOutput("sidebarpanel"))

# 04.3 Body ----
body <- dashboardBody(
  tags$script(HTML("$('body').addClass('fixed');")),
  tags$head(tags$script('
      // Define function to set height of tab items
      setHeight = function() {
        var window_height = $(window).height();
        var header_height = $(".main-header").height();
        var boxHeight = window_height - header_height - 50;
        $("#dashboard_window").height(boxHeight);
        $("#build_window").height(boxHeight);
        $("#manage_window").height(boxHeight);
        $("#compare_window").height(boxHeight);
        $("#explore_window").height(boxHeight);
        $("#school_window").height(boxHeight);
        $("#major_window").height(boxHeight);
        $("#occupation_window").height(boxHeight);
        $("#profile_window").height(boxHeight);
        $("#settings_window").height(boxHeight);
        $("#tools_window").height(boxHeight);
        $("#help_window").height(boxHeight);
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
  tags$head(
    tags$style(HTML("
                      .my_class {
                      font-weight: bold;
                      color:#eaedef;
                      }"))
  ),
  
  # 04.4 TABS ----
  tabItems(
    tabItem(tabName = "login",
            div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                wellPanel(
                  tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                  textInput("userName", placeholder="Username", label = tagList(icon("user"), "Username")),
                  passwordInput("passwd", placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
                  br(),
                  div(
                    style = "text-align: center;",
                    actionButton("login", "SIGN IN", style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
                    br(),
                    br(),
                    br()
                  ),
                  div(
                    style = "text-align: center;",
                    actionButton("add_user", "Create Account", style = "color: white; background-color:#808080;
                                 padding: 10px 15px; width: 200px; cursor: pointer;
                                 font-size: 16px; font-weight: 600;"),
                    br()
                  )
                )
            )),
    tabItem(tabName = "dashboard",
            boxPlus(id = "dashboard_window", width = 12, style ="padding: 0px;margin: 0px;overflow-y: auto;overflow-x:hidden;",
                    fluidRow(column(width = 6,
                                    div(id = "search_dash",
                                        searchInput(inputId = "search_dashboard", label = "", placeholder = "Search"))),
                             column(width = 3),
                             column(width = 3,
                                    div(id = "current_date",
                                        p(current_date2)))
                    ),
                    fluidRow(column(width = 8,
                                    div(class = "header_label2",h2(uiOutput("welcome")),h4("We are here to help make education planning easier.")),
                                    div(class = "dash_header",
                                        splitLayout(cellWidths = c("32%", "68%"),
                                                    p("BUILD"), p("EXPLORE"))
                                    ),
                                    div(id = "dash_buttons",
                                        splitLayout(class = "dbutton2", cellWidths = c("31%","23%","23%","23%"),
                                                    div(id="bn1", width = '80%', closable = FALSE, align = "center", style = "height: 200px;",
                                                        actionButton(inputId = "build_new2", label = "",icon = icon("plus")),
                                                        p("Build New Scenario...")),
                                                    div(id = "bn2", width = '80%', closable = FALSE, align = "left",style = "height: 200px; line-height:1;",
                                                        h3("Schools"),
                                                        div(style = "font-size:16px;color:#999999;padding-left:15px;",
                                                            h5("Learn more about a"),
                                                            h5("schools cost, size,"), h5("administration, and"),
                                                            h5("more!")),
                                                        actionButton(inputId = "dash_schools", label = "Explore", icon = icon("chevron-circle-right"))),
                                                    div(id = "bn3", width = '80%', closable = FALSE, align = "left",style = "height: 200px;",
                                                        h3("Majors"),
                                                        div(style = "font-size:16px;color:#999999;padding-left:15px;",h5("Learn more about"),
                                                            h5("the best majors"), h5("and where they can"),
                                                            h5("lead to!")),
                                                        actionButton(inputId = "dash_majors", label = "Explore", icon = icon("chevron-circle-right"))),
                                                    div(id = "bn4", width = '80%', closable = FALSE, align = "left",style = "height: 200px;",
                                                        h3("Occupations"),
                                                        div(style = "font-size:16px;color:#999999;padding-left:15px;",h5("Discover jobs with"),
                                                            h5("the highest salaries"), h5("and the most"),
                                                            h5("sought after jobs!")),
                                                        actionButton(inputId = "dash_occupations", label = "Explore", icon = icon("chevron-circle-right")))
                                        ))
                    ),
                    column(width = 4,
                           div(id = "tutorial", 
                               h3("TUTORIAL"),
                               tags$video(id='videoID', type = 'video/mp4', src='rec21.mp4', controls = 'controls', width = 300, height = 200),
                               div(align = "left",style = "padding:35px; font-size: 18px;line-height: 120%;",
                               HTML(paste("Watch this quick tutorial", "video to get acquainted", "with the website and all", "of its functions", sep="<br/>"))
                    )))
                    ),
                    fluidRow(column(width = 12,
                                    div(class = "dash_header",
                                        p("FAVORITES")))),
                    fluidRow(column(width = 12,
                                    div(id = "favorite_box",
                                        uiOutput(outputId = "favorite_container")
                                    ))),
                    fluidRow(column(width = 12,
                                    div(class = "dash_header",
                                        p("COMPARISONS")))),
                    fluidRow(column(width = 12,
                                    div(id = "comparison_box",
                                        plotOutput(outputId = "comparison_container")
                                    )))
                    
            )#
    ),
    tabItem(tabName = "build",
            boxPlus(id = "build_window", width = 12, style = "padding: 0px;margin: 0px;overflow-y: auto;overflow-x:hidden;",
                    
                    fluidRow(column(width = 1),
                             column(width = 10,
                                    id = "header1", uiOutput(outputId = "build_header1"))),
                    fluidRow(column(width = 1),
                             column(width = 10,
                                    id = "header2", uiOutput(outputId = "build_header2"))),
                    fluidRow(column(width = 1),
                             column(width = 10,
                                    id = "header3", uiOutput(outputId = "build_header3"))),
                    fluidRow(column(width = 1),
                             column(width = 10,
                                    id = "header4", uiOutput(outputId = "build_header4"))),
                    fluidRow(column(width = 1),
                             column(width = 10,
                                    div(id = "choices1", align = "center",
                                        splitLayout(cellWidths = c("25%","25%","25%","25%"), uiOutput("school_select"),uiOutput("major_select"),
                                                    uiOutput("occupation_select"), uiOutput("degree_select"))))),
                    
                    fluidRow(
                      column(width = 1),
                      column(width = 6,
                             div(id = "schoolinfo1",
                                 pickerInput(inputId = "school_next", choices = c("Search for a school..." = NA ,school_list$INSTNM),
                                             selected = NULL,
                                             options = pickerOptions(
                                               liveSearch = TRUE,
                                               virtualScroll = 6,
                                               size = 6,
                                               liveSearchPlaceholder = "Search for a school...",
                                               dropupAuto = FALSE)
                                 ))),
                      actionButton(inputId = "school_add", label = "", icon = icon("plus"))
                    ),
                    fluidRow(column(width = 1),
                             column(width = 6,
                                    div(id = "schoolinfo2",
                                         uiOutput("schoolchoice")
                                    ))),
                    fluidRow(column(width = 1),
                             column(width = 6,
                                    div(id = "schoolinfo3",
                                        splitLayout(cellWidths = c("8%", "52%", "8%", "32%"),
                                        actionButton(inputId = "school_more", label = "", icon = icon("plus")),
                                        tags$h4("Show more search options"),
                                        actionButton(inputId = "school_hint", label ="", icon = icon("far fa-lightbulb")),
                                        tags$h4("Hint")
                                        )))),
                    
                    fluidRow(column(width = 1),
                             column(width = 6,
                                    div(id = "majorinfo1",
                                        pickerInput(inputId = "major_next", choices = c("Search for a major..." = NA, major_list$CIPNAME),
                                                    selected = NULL,
                                                    options = pickerOptions(
                                                      liveSearch = TRUE,
                                                      virtualScroll = 6,
                                                      size = 6,
                                                      liveSearchPlaceholder = "Search for a major...",
                                                      dropupAuto = FALSE
                                                    )))),
                             actionButton(inputId = "major_add", label = "+")),
                    fluidRow(column(width = 1),
                             column(width = 6,
                                    div(id = "majorinfo2", 
                                        uiOutput("majorchoice")
                                    ))),
                    fluidRow(column(width = 1),
                             column(width = 6,
                                    div(id = "majorinfo3",
                                        splitLayout(cellWidths = c("8%", "52%", "8%", "32%"),
                                                    actionButton(inputId = "major_more", label = "", icon = icon("plus")),
                                                    tags$h4("Show more search options"),
                                                    actionButton(inputId = "major_hint", label ="", icon = icon("far fa-lightbulb")),
                                                    tags$h4("Hint")
                                                      )))),
                    fluidRow(column(width = 1),
                             column(width = 6,
                                    div(id = "occupationinfo1",
                                        pickerInput(inputId = "occupation_next", choices = c("Search for an occupation..." = NA, occupation_list$OCCNAME),
                                                    selected = NULL,
                                                    options = pickerOptions(
                                                      liveSearch = TRUE,
                                                      virtualScroll = 6,
                                                      size = 6,
                                                      liveSearchPlaceholder = "Search for an occupation...",
                                                      dropupAuto = FALSE
                                                    )))),
                             actionButton(inputId = "occupation_add", label = "+")),
                    fluidRow(column(width = 1),
                             column(width = 6,
                                    div(id = "occupationinfo2",
                                        uiOutput("occupationchoice")
                                    ))),
                    fluidRow(column(width = 1),
                             column(width = 6,
                                    div(id = "occupationinfo3",
                                        splitLayout(cellWidths = c("8%", "52%", "8%", "32%"),
                                                    actionButton(inputId = "occupation_more", label = "", icon = icon("plus")),
                                                    tags$h4("Show more search options"),
                                                    actionButton(inputId = "occupation_hint", label ="", icon = icon("far fa-lightbulb")),
                                                    tags$h4("Hint")
                                                      )))),
                    
                    fluidRow(column(width = 1),
                             column(width = 10,
                                    div(id = "degreeinfo1", class = "largerCheck",
                                        checkboxGroupInput(inputId = "degree_checkbox", label = NULL, choices = c(degree_list$LEVELName))
                                    ))),
                    fluidRow(column(width = 1),
                             column(width = 10,
                                    div(id = "choice_line",style = "border-bottom:1px solid #999999;"))),
                    fluidRow(column(width = 10),
                             column(width = 2,
                                    uiOutput("favorites"))),
                    fluidRow(column(width = 2,
                                    div(id = "scenario_text",
                                        uiOutput("scenario_text1"),
                                        uiOutput("scenario_text2")
                                    ),
                                    uiOutput("scenario_available")),
                             column(width = 10,
                                    div(id = "dt_scenario", 
                                        div(class = "header_label", p("Scenario Table")),
                                        dataTableOutput(outputId = "scenario_table")))
                    ),
                    fluidRow(column(width = 3
                                   ),
                             column(width = 3,
                                    uiOutput("add_favorite")),
                             column(width = 3,
                                    uiOutput("build_new")),
                             column(width = 3,
                                    uiOutput("return_dashboard"))),
                    fluidRow(column(width = 1),
                             column(width = 3,
                                    align = "center",
                                    uiOutput("previous_select")),
                             column(width = 4),
                             column(width = 3,
                                    align = "center",
                                    uiOutput("next_select")) 
                    )
            )),
    
    tabItem(tabName = "explore",
            boxPlus(id = "explore_window", width = 12, style = "padding: 0px;margin: 0px;",
                    h1("Explore"))),
    
    tabItem(tabName = "school",
            boxPlus(id = "school_window", width = 12, style = "padding: 0px;margin: 0px;overflow-y: auto;",
                    
                   column(width = 2,style = "padding:0px;margin:0px;",
                      div(align = 'center', style = "font-size: 14px; padding-top: 10px;padding-bottom: 10px;margin: 0em;border: 0px; background-color:#e2ac24;color:white;",
                          h2("Explore Schools")),
                      div(align = 'left',
                      style = "background-color:white;padding-left: 5px;margin-left:0px;",
                        selectInput(inputId = "es_school", label = "School", choices = c("All" = '',school_list$INSTNM), width = "100%"),
                        selectInput(inputId = "es_state", label = "State", choices = c("All" = '',state_abbr_master$State), width = "100%"),
                        sliderInput(inputId = "es_room_board", label = "Room and Board",
                                    value = max(school_master2$ROOM_BOARD),
                                    min = min(school_master2$ROOM_BOARD),
                                    max = max(school_master2$ROOM_BOARD),
                                    width = "90%"),
                        sliderInput(inputId = "es_annual_hi", label = "Annual Cost High",
                                    value = max(school_master2$TotCstOutHi),
                                    min = min(school_master2$TotCstOutHi),
                                    max = max(school_master2$TotCstOutHi),
                                    width = "90%"),
                        sliderInput(inputId = "es_annual_lo", label = "Annual Cost Low",
                                    value = max(school_master2$TotCstOutLo),
                                    min = min(school_master2$TotCstOutLo),
                                    max = max(school_master2$TotCstOutLo),
                                    width = "90%"),
                      actionButton(inputId = "school_favorite", label = "Add to Favorites"),
                      uiOutput(outputId = "school_return")
                      )
                      ),
                      column(width = 10,
                          div(align = 'center', style = "font-size: 16px; padding: 0px; margin-top:2em;
                      margin-left: 0px; margin-right: 0px;overflow-y: auto;", 
                              tabBox(width = 12, id = "es_tab", side = "left",
                                     tabPanel("Table", div(class = "school_header_label", p("School Table")),
                                              DT::dataTableOutput(
                                       outputId = "es_table", width = "100%", height = "auto")),
                                     tabPanel("Favorites", "Favorites"))
                              ))
            )
    ),
    tabItem(tabName = "major",
            boxPlus(id = "major_window", width = 12, style = "padding: 0px;margin: 0px;overflow-y: auto;",
                    column(width = 2, style ="padding:0px;margin:0px;",
                           div(align = 'center', style = "font-size: 14px; padding-top: 10px;padding-bottom: 10px;margin: 0em;border: 0px; background-color:#37b749;color:white;",
                               h2("Explore Majors")),
                           div(align = 'left',
                               style = "background-color:white;padding-left: 5px;margin-left:0px;",
                               selectInput(inputId = "em_major", label = "Major", choices = c("All" = '', major_list3$CIPNAME),
                                           width = "100%"),
                               selectInput(inputId = "em_degree", label = "Degree", choices = c("All" = '', degree_list3$LEVELName),
                                           width = "100%"),
                               selectInput(inputId = "em_school", label = "School Name", choices = c("All" = '', school_list3$INSTNM), 
                                           width = "100%"),
                               actionButton(inputId = "major_favorite", label = "Add to Favorites"),
                               uiOutput(outputId = "major_return")
                           )
                      ),
                        column(width = 10,
                          div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:2em;
                          margin-left: 0px; margin-right: 0px;overflow-y: auto;", 
                              tabBox(width = 12, id = "em_tab", side = "left",
                                     tabPanel("Table", div(class = "major_header_label", p("Major Table")),
                                              DT::dataTableOutput(
                                                outputId = "em_table",
                                                width = "100%",
                                                height = "auto"
                                              )),
                                     tabPanel("Favorites", "Favorites"))
                                    ))
                    )
            ),
    tabItem(tabName = "occupation",
            boxPlus(id = "occupation_window", width = 12, style = "padding: 0px;margin: 0px;overflow-y: auto;",
                    column(width = 2, style ="padding:0px;margin:0px;",
                           div(align = 'center', style = "font-size: 14px; padding-top: 10px;padding-bottom: 10px;margin: 0em;border: 0px; background-color:#477ddd;color:white;",
                               h2("Explore Occupations")),
                           div(align = 'left',
                               style = "background-color:white;padding-left: 5px;margin-left:0px;",
                               selectInput(inputId = "eo_occupation", label = "Occupation", choices = c("All" = '', occupation_master$OCCNAME),
                                           width = "100%"),
                               selectInput(inputId = "eo_entry_degree", label = "Entry Degree", choices = c("All" = '', occupation_master$Entry_Degree),
                                           width = "100%"),
                               selectInput(inputId = "eo_required_exp", label = "Required Experience", choices = c("All" = '', occupation_master$Experience), 
                                           width = "100%"),
                               sliderInput(inputId = "eo_starting_salary", label = "Starting Salary",
                                           value = round(min(occupation_master$X17p),0),
                                           min = round(min(occupation_master$X17p),0), 
                                           max = round(max(occupation_master$X17p),0), 
                                           width = "100%"),
                               sliderInput(inputId = "eo_growth_rate", label = "Growth Rate",
                                           value = round(min(occupation_master$EmplyPC),0),
                                           min = round(min(occupation_master$EmplyPC),0),
                                           max = round(max(occupation_master$EmplyPC),0),
                                           width = "100%"),
                               actionButton(inputId = "occupation_favorite", label = "Add to Favorites"),
                               uiOutput(outputId = "occupation_return")
                               )
                      ),
                    column(width = 10,
                           div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:2em;
                          margin-left: 0px; margin-right: 0px;overflow-y: auto;", 
                               tabBox(width = 12, id = "eo_tab", side = "left",
                                      tabPanel("Table", div(class = "occupation_header_label", p("Occupation Table")),
                                               DT::dataTableOutput(
                                                 outputId = "eo_table",
                                                 width = "100%",
                                                 height = "auto"
                                               )),
                                      tabPanel("Favorites", "Favorites"))
                           ))
                    )
            ),
    tabItem(tabName = "profile",
            boxPlus(id = "profile_window", width = 12, style = "padding: 0px;margin: 0px;",
                    fluidPage(
                      fluidRow(column(width = 2),
                               column(width = 8,
                      div(align = 'left', style = "font-size: 16px; padding-top: 0px; margin-top:2em",
                          h2("Profile")),
                      textInput(inputId = "profile_name", label = "Name", value = pro_name),
                      textInput(inputId = "profile_email", label = "Email", value = pro_email),
                      selectInput(inputId = "profile_state", label = "State of Residence", 
                                  choices = state_abbr$State, selected = pro_state),
                      textInput(inputId = "profile_activity", label = "Activities/Interests"),
                      textAreaInput(inputId = "profile_biography", label = "Biography", rows = 6)
                               )
                    ))
                    )),
    tabItem(tabName = "settings",
            boxPlus(id = "settings_window", width = 12, style = "padding: 0px;margin: 0px;",
                    h1("Settings"))),
    tabItem(tabName = "tools",
            boxPlus(id = "tools_window", width = 12, style = "padding: 0px;margin: 0px;",
                    h1("Tools"))),
    tabItem(tabName = "help",
            boxPlus(id = "help_window", width = 12, style = "padding: 0px;margin: 0px;",
                    h1("Help")))
  )
)

# 04.5 UI ----
ui <- dashboardPagePlus( header, sidebar, body, useShinyjs(), tags$head(tags$meta( name="viewport", content="width=1920")),
                         tags$head(
                           tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
                           tags$head(includeScript("returnClick.js"))
                         ))

# 05 SERVER ----
server <- function(input, output, session) {
  login <- FALSE
  USER <- reactiveValues(login = login)
  
  observeEvent(input$login,{
    if (USER$login == FALSE) {
      Username <- isolate(input$userName)
      Password <- isolate(input$passwd)
      if(length(which(credentials$username_id==Username))==1) { 
        pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
        pasverify <- password_verify(pasmatch, Password)
        if(pasverify) {
          USER$login <- TRUE
          current_user <<- credentials %>% filter(passod %in% pasmatch)
          set_user_var()
          update_profile()
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
  set_user_var <- function() {
    pro_name <<- current_user$username_id[1]
    pro_email <<- current_user$email[1]
    pro_state <<- current_user$state[1]
    pro_user <<- current_user$id[1]
    scenario_file <<- paste0(pro_user,"scenario.rds")
    favorite_file <<- paste0(pro_user,"favorite.rds")
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
      update_scenario()
    } 
    filename3 <- paste0("responses/",favorite_file)
    if(drop_exists(filename3) == TRUE) {
      drop_download(filename3, overwrite = TRUE)
      user_favorites <<- readRDS(favorite_file)
      favor_temp <- user_favorites 
      if(NROW(favor_temp) > 0) {
        favorite_cards()
    } 
    }
  }
  onevent("dblclick", "occupation_next", click("occupation_add"))
  onevent("dblclick", "major_next", click("major_add"))
  onevent("dblclick", "school_next", click("school_add"))

  
  output$logoutbtn <- renderUI({
    req(USER$login)
    tags$li(a(icon("fa fa-sign-out"), "Logout", 
              href="javascript:window.location.reload(true)"),
            class = "dropdown", 
            style = "background-color: #eee !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;")
  })
  output$sidebarpanel <- renderUI({
    if (USER$login == TRUE ){ 
      if (credentials[,"permission"][which(credentials$username_id==input$userName)]=="advanced") {
        
        sidebarMenu(id = "tabs", 
                    menuItem("Profile", tabName = "profile", icon = icon("user")),
                    menuItem("Dashboard", tabName = "dashboard", icon = icon("tachometer-alt"),selected = TRUE),
                    menuItem("Scenarios", tabName = "build", icon = icon("plus")),
                    menuItem("Explore", tabName = "explore", icon = icon("fas fa-compass"),
                             menuSubItem("Explore Schools", tabName = "school"),
                             menuSubItem("Explore Occupations", tabName = "occupation"),
                             menuSubItem("Explore Majors", tabName = "major")),
                    menuItem("Settings", tabName = "settings", icon = icon("cog")),
                    menuItem("Tools", tabName = "tools", icon = icon("wrench",lib = "glyphicon")),
                    menuItem("Help", tabName = "help", icon = icon("question-circle")),
                    searchInput(inputId = "search", label = "", placeholder = "Search EPIC"))
      }
    } else {
      sidebarMenu(id = "log_tabs",
                  menuItem("Login", tabName = "login")
      )
    }
  })
  observe({
    if (USER$login == TRUE ){
      updateTabItems(session, "tabs", "dashboard") } else {
        updateTabItems(session, "log_tabs", "login")
      }
  })
  
  observe({
    shinyjs::runjs("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';")
  }) 
  observeEvent(input$add_user, {
    ### This is the pop up board for input a new row
    showModal(modalDialog(title = "Add a new account",
                          textInput(paste0("Names_add", input$Add_row_head), "Name"),
                          textInput(paste0("Email_add", input$Add_row_head), "Email"),
                          selectInput(paste0("State_add", input$Add_row_head), "State of Residence",
                                      choices = state_abbr$State, selectize = FALSE,size = 4),
                          textInput(paste0("Password_add", input$Add_row_head), "Password"), 
                          actionButton("go", "Add item"),
                          easyClose = TRUE, footer = NULL ))
    
  })

  observeEvent(input$go, {
    new_row=data.frame(
      id = paste0("USER", NROW(credentials) + 1),
      username_id = input[[paste0("Names_add", input$Add_row_head)]],
      email = input[[paste0("Email_add", input$Add_row_head)]],
      state = input[[paste0("State_add", input$Add_row_head)]],
      passod = sapply(input[[paste0("Password_add", input$Add_row_head)]],password_store),
      permission = "advanced",
      stringsAsFactors = FALSE
    )
    credentials <<- rbind(credentials, new_row)
    saveRDS(credentials, "cred.rds")
    drop_upload("cred.rds", path = "responses")
    removeModal()
  })
  current_user <- tibble()
  build_variables <- reactiveValues(current_page = 1)
  temp_choice <- reactiveValues(school_status = 1, major_status = 1, occupation_status = 1, degree_status = 1)
#
  schoolclick <- 0
  majorclick <- 0
  occupationclick <- 0
  degreeclick <- 0
#  load data
  number_favorites <- 0
  major_filter <- cips
  school_filter <- school_master3 %>% select("UNITID", "INSTNM")
  occupation_filter <- occupation_master %>% select("OCCNAME", "OCCCODE")
  degree_filter <- degree_master
  
  school_scenario <- school_master %>% select("UNITID", "INSTNM", "STABBR", "TotCstInHi", "TotCstOutHi")
  occupation_scenario <- occupation_master %>% select("OCCNAME", "OCCCODE", "X17p", "Entry_Code", "Entry_Degree", "Experience")
  occupation_scenario3 <- occupation_master %>% select("OCCNAME", "OCCCODE", "X17p", "Entry_Code", "Entry_Degree", "Experience","MedOccF")
  major_scenario <- cips
  degree_scenario <- degree_master
  
  school_scenario2 <- school_master %>% select("UNITID", "INSTNM", "STABBR", "TotCstInHi", "TotCstOutHi")
  occupation_scenario2 <- occupation_master %>% select("OCCNAME", "OCCCODE", "X17p", "Entry_Code", "Entry_Degree", "Experience")
  major_scenario2 <- cips
  degree_scenario2 <- degree_master
  school_scenario2$INSTNM <- strtrim(school_scenario2$INSTNM, 22)
  major_scenario2$CIPNAME <- strtrim(major_scenario2$CIPNAME, 22)
  occupation_scenario2$OCCNAME <- strtrim(occupation_scenario2$OCCNAME, 22)
  degree_scenario2$LEVELName <- strtrim(degree_scenario2$LEVELName, 22)
  
  school_list_selected <- vector(mode = "list")
  degree_list_selected <- vector(mode = "list")
  occupation_list_selected <- vector(mode = "list")
  major_list_selected <- vector(mode = "list")
  #old saved data format
  user_scen01 <- tibble("ID" = numeric(), "scenario" = character())
  #new saved data format
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

  choosen_scenario <- character()
  new_scenario_name <- character()
  temp_scenario <- character()
  favorite_temp <- character()
  obsList <- list()
  detailList <- list()
  checkList <- list()
  graphList <-vector(mode = "list")
  
  eo_occuption_list <- c(character())
  eo_entry_degree_list <- c(character())
  eo_required_exp_list <- c(character())
  
  es_school_list <- c(character())
  es_state_list <- c(character())
  
  em_school_list <<- c(character())
  em_curriculum_list <<- c(character())
  em_degree_list <<- c(character())
  
  scenario_source <- 1
  edit_scenario <- 0
  scenario_file <- paste0(pro_user,"scenario.rds")
  favorite_file <- paste0(pro_user,"favorite.rds")
  schoolobsList <- list()
  majorobsList <- list()
  occupationobsList <- list()
  
  graph_parameters <- ggplot() + 
    xlab('Years') +
    ylab('Total Earnings in Thousands') +
    labs(title = 'Cummulative Cash Flow') +
    theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 16)) +
    theme(legend.direction = "vertical", legend.position = c(0.16,0.76)) +
    theme(legend.title = element_text( size=12), legend.text=element_text(size=12))
  
  showschool <- function() {
    shinyjs::show(id = "schoolinfo1")
    shinyjs::show(id = "schoolinfo2")
    shinyjs::show(id = "schoolinfo3")
    shinyjs::show(id = "school_add")
  }
  hideschool <- function() {
    shinyjs::hide(id = "schoolinfo1")
    shinyjs::hide(id = "schoolinfo2")
    shinyjs::hide(id = "schoolinfo3")
    shinyjs::hide(id = "school_add")
  }
  showmajor <- function() {
    shinyjs::show(id = "majorinfo1")
    shinyjs::show(id = "majorinfo2")
    shinyjs::show(id = "majorinfo3")
    shinyjs::show(id = "major_add")
  }
  hidemajor <- function() {
    shinyjs::hide(id = "majorinfo1")
    shinyjs::hide(id = "majorinfo2")
    shinyjs::hide(id = "majorinfo3")
    shinyjs::hide(id = "major_add")
  }
  showoccupation <- function() {
    shinyjs::show(id = "occupationinfo1")
    shinyjs::show(id = "occupationinfo2")
    shinyjs::show(id = "occupationinfo3")
    shinyjs::show(id = "occupation_add")
  }
  hideoccupation <- function() {
    shinyjs::hide(id = "occupationinfo1")
    shinyjs::hide(id = "occupationinfo2")
    shinyjs::hide(id = "occupationinfo3")
    shinyjs::hide(id = "occupation_add")
  }
  showchoices <- function() {
    shinyjs::show(id = "choices1")
  }
  hidechoices <- function() {
    shinyjs::hide(id = "choices1")
  }
  showdegree <- function() {
    shinyjs::show(id = "degreeinfo1")
  }
  hidedegree <- function() {
    shinyjs::hide(id = "degreeinfo1")
  }
  showheader <- function() {
    shinyjs::show(id = "header1")
    shinyjs::show(id = "header2")
    shinyjs::show(id = "header3")
    shinyjs::show(id = "header4")
  }
  hideheader <- function() {
    shinyjs::hide(id = "header1")
    shinyjs::hide(id = "header2")
    shinyjs::hide(id = "header3")
    shinyjs::hide(id = "header4")
  }
  showscenario <- function() {
    hideschool()
    hidemajor()
    hideoccupation()
    hidedegree()
    shinyjs::show(id = "favorites")
    shinyjs::show(id = "scenario_text")
    shinyjs::show(id = "scenario_available")
    shinyjs::show(id = "dt_scenario")
 #   shinyjs::show(id = "return_current")
    shinyjs::show(id = "add_favorite")
    shinyjs::show(id = "build_new")
    shinyjs::show(id = "return_dashboard")
  }
  hidescenario <- function() {
    shinyjs::hide(id = "favorites")
    shinyjs::hide(id = "scenario_text")
    shinyjs::hide(id = "scenario_available")
    shinyjs::hide(id = "dt_scenario")
#    shinyjs::hide(id = "return_current")
    shinyjs::hide(id = "add_favorite")
    shinyjs::hide(id = "build_new")
    shinyjs::hide(id = "return_dashboard")
  }
  showline <- function() {
    shinyjs::show(id = "choice_line")
  }
  hideline <- function() {
    shinyjs::hide(id = "choice_line")
  }
  shownext <- function() {
    shinyjs::show(id = "next_select")
  }
  hidenext <- function() {
    shinyjs::hide(id = "next_select")
  }
  showstep <- function(x) {
    if(x == 1) {
      output$build_header1 <- renderUI({ p("Step 1") })
    } else if(x == 2) {
      output$build_header1 <- renderUI({ p("Step 2") })
    } else if(x == 3) {
      output$build_header1 <- renderUI({ p("Step 3") })
    } else if(x == 4) {
      output$build_header1 <- renderUI({ p("Step 4") })
    } else {return()}
  }
  show_school_pref <- function() {
    output$build_header2 <- renderUI({ p("School Preferences")})
    output$build_header3 <- renderUI({ p("Search for schools that you are interested in attending. You may add up to") })
    output$build_header4 <- renderUI({ p("five schools.")})
  }
  show_major_pref <- function() {
    output$build_header2 <- renderUI({ p("Major Preferences")})
    output$build_header3 <- renderUI({ p("Search for majors that you are interested in studying. You may add up to") })
    output$build_header4 <- renderUI({ p("five majors.")})
  }
  show_occupation_pref <- function() {
    output$build_header2 <- renderUI({ p("Occupation Preferences")})
    output$build_header3 <- renderUI({ p("Search for careers that you are interested in persuing. You may add up to") })
    output$build_header4 <- renderUI({ p("five occupations.")})
  }
  show_degree_pref <- function() {
    output$build_header2 <- renderUI({ p("Degree Preferences")})
    output$build_header3 <- renderUI({ p("Search for a degree that you are interested in obtaining. You may select") })
    output$build_header4 <- renderUI({ p("multiple based on your preferences, or if you are not sure.")})
  }
  show_choice_page <- function() {
    showchoices()
    hideschool()
    hidemajor()
    hideoccupation()
    hidedegree()
  }
  school_page <- function() {
    show_school_pref()
    hidechoices()
    showschool()
    hidemajor()
    hideoccupation()
    hidedegree()
  }
  major_page <- function() {
    show_major_pref()
    hidechoices()
    hideschool()
    showmajor()
    hideoccupation()
    hidedegree()
  }
  occupation_page <- function() {
    show_occupation_pref()
    hidechoices()
    hideschool()
    hidemajor()
    showoccupation()
    hidedegree()
  }
  degree_page <- function() {
    show_degree_pref()
    hidechoices()
    hideschool()
    hidemajor()
    hideoccupation()
    showdegree()
  }
  
  clear_schoolchoices <- function() {
    school_list_selected <<- list()
    school_cards()
  }
  clear_majorchoices <- function() {
    major_list_selected <<- list()
    major_cards()
  }
  clear_occupationchoices <- function() {
    occupation_list_selected <<- list()
    occupation_cards()
  }
  pagetotal <- function() {
    temp <- schoolclick + majorclick + occupationclick + degreeclick
  }
  
  clear_build <- function() {
    temp_choice$school_status <<- 1
    temp_choice$major_status <<- 1
    temp_choice$occupation_status <<- 1
    temp_choice$degree_status <<- 1
    schoolclick <<- 0
    majorclick <<- 0
    occupationclick <<- 0
    degreeclick <<- 0
    clear_occupationchoices()
    clear_majorchoices()
    clear_schoolchoices()
    updatePickerInput(session, inputId = "school_next", choices = school_list$INSTNM)
    updatePickerInput(session, inputId = "major_next", choices = major_list$CIPNAME)
    updatePickerInput(session, inputId = "occupation_next", choices = occupation_list$OCCNAME)
    updateCheckboxGroupInput(session, inputId = "degree_checkbox", choices = c(degree_list$LEVELName), selected = NULL)
    school_list_selected <<- vector(mode = "list")
    degree_list_selected <<- vector(mode = "list")
    occupation_list_selected <<- vector(mode = "list")
    major_list_selected <<- vector(mode = "list")
    build_variables$current_page <<- 1
    edit_scenario <<- 0
    # scenario name   
    update_scenario()
  }
  observe({
    if(input$profile_state != ''){
      residence_temp <- state_abbr %>% filter(State %in% input$profile_state) %>% select(STABBR)
      residence_temp <- as.character(residence_temp)
      for(i in 1:nrow(school_scenario2)){
        school_scenario2$TOTALT[i] <<- ifelse(school_scenario2$STABBR[i] == residence_temp, school_scenario2$TotCstInHi[i],
                                     school_scenario2$TotCstOutHi[i])
      } 
    }else {
      school_scenario2$TOTALT <<- school_scenario2$TotCstOutHi
    }
  })
  observe({
    if(build_variables$current_page == 1){
      showheader()
      hidescenario()
      output$build_header1 <- renderUI({ p("Lets Get Started!") })
      output$build_header2 <- renderUI({ p("Where would you like to begin?")})
      output$build_header3 <- renderUI({ p("Select a category to begin building your scenario. We recommend starting with the category") })
      output$build_header4 <- renderUI({ p("that you have the strongest preference in.")})
      show_choice_page()
      showline()
      hidenext()
    }
  })
  observe({
    if(build_variables$current_page == 2){
      output$build_header1 <- renderUI({ p("Great Job!") })
      output$build_header2 <- renderUI({ p("Where would you like to go next?")})
      output$build_header3 <- renderUI({ p("You are off to a great start. Select another category to continue. We recommend selecting the category") })
      output$build_header4 <- renderUI({ p("that you have the next strongest preference in.")})
      show_choice_page()
      hidenext()
    }
  })

  observe({
    if(build_variables$current_page == 3){
      output$build_header1 <- renderUI({ p("Halfway done!") })
      output$build_header2 <- renderUI({ p("Where would you like to go next?")})
      output$build_header3 <- renderUI({ p("You are making great progress. Select another category to continue building. We recommend selecting the category") })
      output$build_header4 <- renderUI({ p("that you have the next strongest preference in.")})
      show_choice_page()
      hidenext()
    }
  })

  observe({
    if(build_variables$current_page == 4){
      output$build_header1 <- renderUI({ p("One more to go!") })
      output$build_header2 <- renderUI({ p("Select the last category")})
      output$build_header3 <- renderUI({ p("You are almost done! Select the last category to continue building. If you want to make changes") })
      output$build_header4 <- renderUI({ p("to previous categories, you may select the previous button.")})
      show_choice_page()
      hidenext()
    }
  })

  observe({
    if(build_variables$current_page == 5){
      showheader()
      hidescenario()
      output$build_header1 <- renderUI({ p("Building complete.") })
      output$build_header2 <- renderUI({ p("Lets Build Your Scenario!")})
      output$build_header3 <- renderUI({ p("Great job filing out your preferences. If you wish to make changes to any of your preferences in the") })
      output$build_header4 <- renderUI({ p("categories, select the previous button. If you are ready to continue select the build scenario button!")})
      show_choice_page()
      showline()
      shownext()
      output$next_select <- renderUI({actionButton(inputId = "next_button", label = "Build Scenario") })
    }
  })
  observe({
    if(build_variables$current_page == 6){
        showstep(pagetotal())
        school_page()
        showline()
        shownext()
 #       school_cards()
        output$next_select <- renderUI({actionButton(inputId = "next_button", label = "Next", icon = icon("arrow-right")) })
    }
  })
  observe({
    if(build_variables$current_page == 7){
        showstep(pagetotal())
        major_page()
        showline()
        shownext()
        output$next_select <- renderUI({actionButton(inputId = "next_button", label = "Next", icon = icon("arrow-right")) })
    }
  })
  observe({
    if(build_variables$current_page == 8){
        showstep(pagetotal())
        occupation_page()
        showline()
        shownext()
        output$next_select <- renderUI({actionButton(inputId = "next_button", label = "Next", icon = icon("arrow-right")) })
    }
  })
  observe({
    if(build_variables$current_page == 9){
        showstep(pagetotal())
        degree_page()
        showline()
        shownext()
        output$next_select <- renderUI({actionButton(inputId = "next_button", label = "Next", icon = icon("arrow-right")) })
    }
  })
  observe({
    if(build_variables$current_page == 10){
      hideheader()
      hidechoices()
      hideline()
      hidenext()
      build_radio()
      showscenario()
      output$add_favorite <- renderUI({actionButton(inputId = "add_favorite_button", label = "Add to Favorites") })
      output$build_new <- renderUI({
        div(id = "b_new", align = "center",style = "display: inline-block;",
            splitLayout(cellWidths = c("20%", "80%"),
        actionButton(inputId = "build_new_button", label = "", icon = icon("plus")),
        tags$h3("Build New Scenario...")
        ))
        })
      
      output$return_dashboard <- renderUI({actionButton(inputId = "return_dashboard_button", label = "Return to Dashboard") })
      update_fav_num()
      output$scenario_text1 <- renderUI({p("Save Options")})
      output$scenario_text2 <- renderUI({
        div(id = "scenario_text3",
            p("Select and add"),
            p("your favorite"),
            p("options. Your"),
            p("favorite options"),
            p("will be saved to"),
            p("your dashboard"),
            p("where you can"),
            p("view compari-"),
            p("son graphs and"),
            p("return on invest-"),
            p("ment reports.")
        )
      })
    }
  })
  update_fav_num <- function() {
    favor_num <- user_favorites %>% filter(user %in% pro_user)
    output$favorites <- renderUI({p(paste0("Favorite Options (", NROW(favor_num),")"))})
  }


  observe({
    if(temp_choice$school_status == 1){
      output$school_select <- renderUI({
        actionButton(inputId = "school_button", label = "", 
                     style ="background: url('epicButtons-18.svg');border-bottom:7px solid; border-bottom-color: #999999;") 
      })
    } else if(temp_choice$school_status == 3){
      output$school_select <- renderUI({
        actionButton(inputId = "school_button", label = "", 
                     style ="background: url('epicButtons-22.svg');border:1px solid #e2ac24; border-bottom:7px solid #e2ac24;")
      })
    }
    if(temp_choice$major_status == 1){
      output$major_select <- renderUI({
        actionButton(inputId = "major_button", label = "", 
                     style ="background: url('epicButtons-19.svg');border-bottom:7px solid; border-bottom-color: #999999;")
      })
    } else if(temp_choice$major_status == 3){
      output$major_select <- renderUI({
        actionButton(inputId = "major_button", label = "",
                     style ="background: url('epicButtons-23.svg');border:1px solid #37b749; border-bottom:7px solid #37b749;")
      }) 
    }
    if(temp_choice$occupation_status == 1){
      output$occupation_select <- renderUI({
        actionButton(inputId = "occupation_button", label = "", 
                     style ="background: url('epicButtons-20.svg');border-bottom:7px solid #999999;")
      }) 
    } else if(temp_choice$occupation_status == 3){
      output$occupation_select <- renderUI({
        actionButton(inputId = "occupation_button", label = "", 
                     style ="background: url('epicButtons-24.svg');border:1px solid #477ddd;border-bottom:7px solid #477ddd;")
      }) 
    } 
    if(temp_choice$degree_status == 1){
      output$degree_select <- renderUI({
        actionButton(inputId = "degree_button", label = "", 
                     style ="background: url('epicButtons-21.svg');border-bottom:7px solid #999999;")
      })
    } else if(temp_choice$degree_status == 3){
      output$degree_select <- renderUI({
        actionButton(inputId = "degree_button", label = "", 
                     style ="background: url('epicButtons-25.svg');border:1px solid #e22b2b;border-bottom:7px solid #e22b2b;")
      })
    }
  })
  
  observeEvent(input$school_button,{
    build_variables$current_page <<- 6
    schoolclick <<- 1
  })
  observeEvent(input$major_button,{
    build_variables$current_page <<- 7
    majorclick <<- 1
  })
  observeEvent(input$occupation_button,{
    build_variables$current_page <<- 8
    occupationclick <<- 1
  })
  observeEvent(input$degree_button,{
    build_variables$current_page <<- 9
    degreeclick <<- 1
  })
  # 06 Next button ----
  observeEvent(input$next_button,{
    if(build_variables$current_page == 5) {
      if(is_empty(major_list_selected) & is_empty(degree_list_selected) & is_empty(occupation_list_selected) & is_empty(school_list_selected)){
        showModal(modalDialog(title = "Please select at least one item",
                              "Unable to build scenario!",
                              easyClose = TRUE))
      } else {
      add_scenario()
      build_variables$current_page <<- 10
      }
    }
    if(build_variables$current_page == 6){
        temp_choice$school_status <<- 3
        schoolclick <<- 1
        major_unavailable()
        occupation_unavailable()
        degree_unavailable()
        build_variables$current_page <<- 1 + pagetotal()
    }
    if(build_variables$current_page == 7){
        temp_choice$major_status <<- 3
        majorclick <<- 1
        school_unavailable()
        occupation_unavailable()
        degree_unavailable()
        build_variables$current_page <<- 1 + pagetotal()
    }
    if(build_variables$current_page == 8){
        temp_choice$occupation_status <<- 3
        occupationclick <<- 1
        school_unavailable()
        major_unavailable()
        degree_unavailable()
        build_variables$current_page <<- 1 + pagetotal()
    }
    if(build_variables$current_page == 9){
          degree_list_selected <<- input$degree_checkbox
          temp_choice$degree_status <<- 3
          degreeclick <<- 1
          school_unavailable()
          major_unavailable()
          occupation_unavailable()
          build_variables$current_page <<- 1 + pagetotal()
    }
  })

  # 07 school info ----    
  observeEvent(input$school_add, {
    if(input$school_next != "NA"){
      if(NROW(school_list_selected) < 5) {
        if(input$school_next %in% school_list_selected){
          return()
        } else {
          school_list_selected <<- rbind(school_list_selected, input$school_next)
          school_cards()
        }
      }
    }
  })
  school_cards <- function() {
    school_temp <- school_filter %>% filter(INSTNM %in% school_list_selected)
    if(NROW(school_temp) > 0){
      output$schoolchoice <- renderUI({
        args <- lapply(1:NROW(school_temp), function(.x) schoolcard2(.x,
                                                                     name = school_temp$UNITID[.x],
                                                                     school = school_temp$INSTNM[.x]))
        args$cellArgs <- list(
          style = "
          width: 210px;
          height: 50px;
		      padding: 0px;
		      padding-left: 15px;
		      top: 50%;
		      border-color: #e2ac24;
          background-color:#e2ac24;
          margin: 10px;
          font-size: 1em;
          line-height: 50px;
		      border-radius: 25px;
		      border-bottom:none !important;
		      color: white;		  
          "
        )        
        do.call(shiny::flowLayout, args)        
      })
    } else {
      output$schoolchoice <- renderUI({NULL})
      schoolobsList <<- list()
    }
  }	  
  schoolcard2 <- function(x,name,school) {
    school_observer(name,school)  
    trim_school <- strtrim(school, 22)
    div(class = "newchip",
        div(style = "display:inline-block;vertical-align:top;width:210px;",
            splitLayout(cellWidths = c("80%","20%"),
                        p(trim_school),                        
                        actionButton(inputId = paste0("schooldel",name),label = "x",
                                     style = "margin:0px;color:white;padding:0px;font-size: 1em; border-color: #e2ac24; background-color: #e2ac24;top: 50%;")))        
    )
  }
  school_observer <- function(name,school){
    btName <- paste0("schooldel",name)
    if(length(schoolobsList[[btName]]) == 0){
      schoolobsList[[btName]] <<- observeEvent(input[[btName]], {
        school_list_selected <<- school_list_selected[school_list_selected != school]
        schoolobsList <<- list()
        school_cards()
      }, ignoreInit = TRUE, once = TRUE)
    }
  } 
  
  #08 Major info ----
  observeEvent(input$major_add, {
    if(input$major_next != "NA"){
      if(NROW(major_list_selected) < 5) {
        if(input$major_next %in% major_list_selected){
          return()
        } else {
          major_list_selected <<- rbind(major_list_selected, input$major_next)
          major_cards()
        }
      }
    }
  })
  
  major_cards <- function() {
    major_temp <- major_filter %>% filter(CIPNAME %in% major_list_selected)
    if(NROW(major_temp) > 0){
      output$majorchoice <- renderUI({
        args <- lapply(1:NROW(major_temp), function(.x) majorcard2(.x,
                                                                     name = major_temp$CIPCODE[.x],
                                                                     major = major_temp$CIPNAME[.x]))
        args$cellArgs <- list(
          style = "
          width: 210px;
          height: 50px;
		      padding: 0px;
		      padding-left: 15px;
		      top: 50%;
		      border-color: #37b749;
          background-color:#37b749;
          margin: 10px;
          font-size: 1em;
          line-height: 50px;
		      border-radius: 25px;
		      border-bottom:none !important;
		      color: white;		  
          "
        )        
        do.call(shiny::flowLayout, args)        
      })
    } else {
      output$majorchoice <- renderUI({NULL})
      majorobsList <<- list()
    }
  }	  
  majorcard2 <- function(x,name,major) {
    major_observer(name,major)  
    trim_major <- strtrim(major, 22)
    div(class = "newchip",
        div(style = "display:inline-block;vertical-align:top;width:210px;",
            splitLayout(cellWidths = c("80%","20%"),
                        p(trim_major),                        
                        actionButton(inputId = paste0("majordel",name),label = "x",
                                     style = "margin:0px;color:white;padding:0px;font-size: 1em; border-color: #37b749; background-color: #37b749;top: 50%;")))        
    )
  }
  major_observer <- function(name,major){
    btName <- paste0("majordel",name)
    if(length(majorobsList[[btName]]) == 0){
      majorobsList[[btName]] <<- observeEvent(input[[btName]], {
        major_list_selected <<- major_list_selected[major_list_selected != major]
        majorobsList <<- list()
        major_cards()
      }, ignoreInit = TRUE, once = TRUE)
    }
  }
  

  # 09 Occupation info ----
  observeEvent(input$occupation_add, {
    if(input$occupation_next != "NA"){
      if(NROW(occupation_list_selected) < 5) {
        if(input$occupation_next %in% occupation_list_selected){
          return()
        } else {
          occupation_list_selected <<- rbind(occupation_list_selected, input$occupation_next)
          occupation_cards()
        }
      }
    }
  })
  
  occupation_cards <- function() {
    occupation_temp <- occupation_filter %>% filter(OCCNAME %in% occupation_list_selected)
    if(NROW(occupation_temp) > 0){
      output$occupationchoice <- renderUI({
        args <- lapply(1:NROW(occupation_temp), function(.x) occupationcard2(.x,
                                                                   name = occupation_temp$OCCCODE[.x],
                                                                   occupation = occupation_temp$OCCNAME[.x]))
        args$cellArgs <- list(
          style = "
          width: 210px;
          height: 50px;
		      padding: 0px;
		      padding-left: 15px;
		      top: 50%;
		      border-color: #477ddd;
          background-color:#477ddd;
          margin: 10px;
          font-size: 1em;
          line-height: 50px;
		      border-radius: 25px;
		      border-bottom:none !important;
		      color: white;		  
          "
        )        
        do.call(shiny::flowLayout, args)        
      })
    } else {
      output$occupationchoice <- renderUI({NULL})
      occupationobsList <<- list()
    }
  }	  
  occupationcard2 <- function(x,name,occupation) {
    occupation_observer(name,occupation)  
    trim_occupation <- strtrim(occupation, 22)
    div(class = "newchip",
        div(style = "display:inline-block;vertical-align:top;width:210px;",
            splitLayout(cellWidths = c("80%","20%"),
                        p(trim_occupation),                        
                        actionButton(inputId = paste0("occupationdel",name),label = "x",
                                     style = "margin:0px;color:white;padding:0px;font-size: 1em; border-color: #477ddd; background-color: #477ddd;top: 50%;")))        
    )
  }
  occupation_observer <- function(name,occupation){
    btName <- paste0("occupationdel",name)
    if(length(occupationobsList[[btName]]) == 0){
      occupationobsList[[btName]] <<- observeEvent(input[[btName]], {
        occupation_list_selected <<- occupation_list_selected[occupation_list_selected != occupation]
        occupationobsList <<- list()
        occupation_cards()
      }, ignoreInit = TRUE, once = TRUE)
    }
  }
  

  school_unavailable <- function(){
    if(is_empty(major_list_selected) & is_empty(degree_list_selected) & is_empty(occupation_list_selected)){
      return()
    }
    school_temp <- backbone
    if(!is_empty(major_list_selected)) {
      curriculum_temp <- filter(major_filter, CIPNAME %in% major_list_selected) %>% select(CIPCODE)
      school_temp <- filter(school_temp, CIPCODE %in% curriculum_temp$CIPCODE)
    }
    if(!is_empty(degree_list_selected)) {
      degree_temp <- filter(degree_filter, LEVELName %in% degree_list_selected) %>% select(AWLEVEL)
      school_temp <- filter(school_temp, AWLEVEL %in% degree_temp$AWLEVEL)
    }
    if(!is_empty(occupation_list_selected)) {
      occupation_temp <- filter(occupation_filter, OCCNAME %in% occupation_list_selected) %>% select(OCCCODE)
      school_temp <- filter(school_temp, OCCCODE %in% occupation_temp$OCCCODE)
    }
    school_temp <- left_join(school_temp, school_filter, by = "UNITID")
    school_temp2 <- unique(school_temp$INSTNM)
    school_temp3 <- filter(school_list, INSTNM %nin% school_temp2)
    school_list2$INSTNM <- school_list$INSTNM
    school_state <- school_list$INSTNM %in% school_temp3$INSTNM
    school_list2 <- cbind(school_list2,school_state)
    school_list2 <- school_list2[order(school_list2$school_state),]
    disabled_choices <- school_list2$INSTNM %in% school_temp3$INSTNM
    updatePickerInput(session, inputId = "school_next", choices = school_list2$INSTNM,
                      choicesOpt = list(
                        disabled = disabled_choices,
                        style = ifelse(disabled_choices,yes = "color: rgba(119, 119, 119, 0.5);",no = "")
                      ))
  }
  major_unavailable <- function(){
    if(is_empty(school_list_selected) & is_empty(degree_list_selected) & is_empty(occupation_list_selected)){
      return()
    }
    major_temp <- backbone
    if(!is_empty(school_list_selected)) {
      school_temp <- filter(school_filter, INSTNM %in% school_list_selected) %>% select(UNITID)
      major_temp <- filter(major_temp, UNITID %in% school_temp$UNITID)
    }
    if(!is_empty(degree_list_selected)) {
      degree_temp <- filter(degree_filter, LEVELName %in% degree_list_selected) %>% select(AWLEVEL)
      major_temp <- filter(major_temp, AWLEVEL %in% degree_temp$AWLEVEL)
    }
    if(!is_empty(occupation_list_selected)) {
      occupation_temp <- filter(occupation_filter, OCCNAME %in% occupation_list_selected) %>% select(OCCCODE)
      major_temp <- filter(major_temp, OCCCODE %in% occupation_temp$OCCCODE)
    }
    major_temp <- left_join(major_temp, major_filter, by = "CIPCODE")
    major_temp2 <- unique(major_temp$CIPNAME)
    major_temp3 <- filter(major_list, CIPNAME %nin% major_temp2)
    major_list2$CIPNAME <- major_list$CIPNAME
    major_state <- major_list$CIPNAME %in% major_temp3$CIPNAME
    major_list2 <- cbind(major_list2,major_state)
    major_list2 <- major_list2[order(major_list2$major_state),]
    disabled_choices <- major_list2$CIPNAME %in% major_temp3$CIPNAME
    updatePickerInput(session, inputId = "major_next", choices = major_list2$CIPNAME,
                      choicesOpt = list(
                        disabled = disabled_choices,
                        style = ifelse(disabled_choices,yes = "color: rgba(119, 119, 119, 0.5);",no = "")
                      ))
  }
  occupation_unavailable <- function(){
    if(is_empty(major_list_selected) & is_empty(degree_list_selected) & is_empty(school_list_selected)){
      return()
    }
    occupation_temp <- backbone
    if(!is_empty(school_list_selected)) {
      school_temp <- filter(school_filter, INSTNM %in% school_list_selected) %>% select(UNITID)
      occupation_temp <- filter(occupation_temp, UNITID %in% school_temp$UNITID)
    }
    if(!is_empty(degree_list_selected)) {
      degree_temp <- filter(degree_filter, LEVELName %in% degree_list_selected) %>% select(AWLEVEL)
      occupation_temp <- filter(occupation_temp, AWLEVEL %in% degree_temp$AWLEVEL)
    }
    if(!is_empty(major_list_selected)) {
      major_temp <- filter(major_filter, CIPNAME %in% major_list_selected) %>% select(CIPCODE)
      occupation_temp <- filter(occupation_temp, CIPCODE %in% major_temp$CIPCODE)
    }
    occupation_temp <- left_join(occupation_temp, occupation_filter, by = "OCCCODE")
    occupation_temp2 <- unique(occupation_temp$OCCNAME)
    occupation_temp3 <- filter(occupation_list, OCCNAME %nin% occupation_temp2)
    occupation_list2$OCCNAME <- occupation_list$OCCNAME
    occupation_state <- occupation_list$OCCNAME %in% occupation_temp3$OCCNAME
    occupation_list2 <- cbind(occupation_list2,occupation_state)
    occupation_list2 <- occupation_list2[order(occupation_list2$occupation_state),]
    disabled_choices <- occupation_list2$OCCNAME %in% occupation_temp3$OCCNAME
    updatePickerInput(session, inputId = "occupation_next", choices = occupation_list2$OCCNAME,
                      choicesOpt = list(
                        disabled = disabled_choices,
                        style = ifelse(disabled_choices,yes = "color: rgba(119, 119, 119, 0.5);",no = "")
                      ))
  }
  degree_unavailable <- function(){
    if(is_empty(major_list_selected) & is_empty(school_list_selected) & is_empty(occupation_list_selected)){
      return()
    }
    degree_temp <- backbone
    if(!is_empty(school_list_selected)) {
      school_temp <- filter(school_filter, INSTNM %in% school_list_selected) %>% select(UNITID)
      degree_temp <- filter(degree_temp, UNITID %in% school_temp$UNITID)
    }
    if(!is_empty(occupation_list_selected)) {
      occupation_temp <- filter(occupation_filter, OCCNAME %in% occupation_list_selected) %>% select(OCCCODE)
      degree_temp <- filter(degree_temp, OCCCODE %in% occupation_temp$OCCCODE)
    }
    if(!is_empty(major_list_selected)) {
      major_temp <- filter(major_filter, CIPNAME %in% major_list_selected) %>% select(CIPCODE)
      degree_temp <- filter(degree_temp, CIPCODE %in% major_temp$CIPCODE)
    }
    degree_temp <- left_join(degree_temp, degree_filter, by = "AWLEVEL")
    degree_temp <- degree_temp[order(degree_temp$AWLEVEL),]
    degree_temp2 <- unique(degree_temp$LEVELName)
    updateCheckboxGroupInput(session, inputId = "degree_checkbox", choices = degree_temp2, selected = degree_list_selected)
    
  }
  add_scenario <- function(){

    if(edit_scenario < 1){
      scen_temp <- new_scenario_name
    } else  {
      scen_temp <- temp_scenario
      
    }
#    edit_scenario <<- 0
    if(!is_empty(school_list_selected)){
      for(i in 1:NROW(school_list_selected)){
        school_temp <- filter(school_filter, INSTNM %in% school_list_selected) %>% select(UNITID)
        sch_temp <- tibble("user" = pro_user, "scenario" = scen_temp, "source" = "build",  "category" = "school", "id" = school_temp$UNITID[i] )
        user_scenarios <<- rbind(user_scenarios, sch_temp)
      }
    }
    if(!is_empty(major_list_selected)){
      for(i in 1:NROW(major_list_selected)){
        major_temp <- filter(major_filter, CIPNAME %in% major_list_selected) %>% select(CIPCODE)
        maj_temp <- tibble("user" = pro_user, "scenario" = scen_temp, "source" = "build", "category" = "major", "id" = major_temp$CIPCODE[i] )
        user_scenarios <<- rbind(user_scenarios, maj_temp)
      }
    }
    if(!is_empty(occupation_list_selected)){
      for(i in 1:NROW(occupation_list_selected)){
        occupation_temp <- filter(occupation_filter, OCCNAME %in% occupation_list_selected) %>% select(OCCCODE)
        occ_temp <- tibble("user" = pro_user, "scenario" = scen_temp, "source" = "build", "category" = "occupation", "id" = occupation_temp$OCCCODE[i] )
        user_scenarios <<- rbind(user_scenarios, occ_temp)
      }
    }
    if(!is_empty(degree_list_selected)){
      for(i in 1:NROW(degree_list_selected)){
        degree_temp <- filter(degree_filter, LEVELName %in% degree_list_selected) %>% select(AWLEVEL)
        deg_temp <- tibble("user" = pro_user, "scenario" = scen_temp, "source" = "build", "category" = "degree", "id" = degree_temp$AWLEVEL[i] )
        user_scenarios <<- rbind(user_scenarios, deg_temp)
      }
    }
    save_scenario()
  }
  
  save_scenario <- function() {
    if(pro_name != "Demo") {
      saveRDS(user_scenarios, scenario_file)
      drop_upload(scenario_file, path = "responses")
    }
  }
build_radio <- function(){  
    scen_temp2 <- user_scenarios %>%  distinct(user_scenarios$scenario, .keep_all = TRUE)
    scen_temp2 <- scen_temp2[grep("Scenario", scen_temp2$scenario),]
#    write.csv(user_scen01, "userdata.csv")
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
#    choosen_scenario <<- "Scenario 1"
    
  }
  observeEvent(input$scen_radio, {
    choosen_scenario <<- input$scen_radio
    create_scenario_table()
  })
  
  observeEvent(input$scen_edit, {
    edit_scenario <<- 1
    temp_scenario <<- choosen_scenario
    school_temp <- user_scenarios %>% filter(scenario == temp_scenario, source == "build", category == "school") %>% select(id)
    school_list_selected2 <- school_filter %>% filter(UNITID %in% school_temp$id) %>% select(INSTNM)
    if(is_empty(school_list_selected2$INSTNM)) {
      school_list_selected <<- vector(mode = "list")
    } else {
      school_list_selected <<- vector(mode = "list")
      school_list_selected <<- cbind(school_list_selected, school_list_selected2$INSTNM)
    }
    
    major_temp <- user_scenarios %>% filter(scenario == temp_scenario, source == "build", category == "major") %>% select(id)
    major_list_selected2 <- major_filter %>% filter(CIPCODE %in% major_temp$id) %>% select(CIPNAME)
    if(is_empty(major_list_selected2$CIPNAME)) {
      major_list_selected <<- vector(mode = "list")
    } else {
      major_list_selected <<- vector(mode = "list")
      major_list_selected <<- cbind(major_list_selected, major_list_selected2$CIPNAME)
    }
    
    occupation_temp <- user_scenarios %>% filter(scenario == temp_scenario, source == "build", category == "occupation") %>% select(id)
    occupation_list_selected2 <- occupation_filter %>% filter(OCCCODE %in% occupation_temp$id) %>% select(OCCNAME)
    if(is_empty(occupation_list_selected2$OCCNAME)) {
      occupation_list_selected <<- vector(mode = "list")
    } else {
      occupation_list_selected <<- vector(mode = "list")
      occupation_list_selected <<- cbind(occupation_list_selected, occupation_list_selected2$OCCNAME)
    }
    
    degree_temp <- user_scenarios %>% filter(scenario == temp_scenario, source == "build", category == "degree") %>% select(id)
    degree_list_selected2 <- degree_filter %>% filter(AWLEVEL %in% degree_temp$id) %>% select(LEVELName)
    if(is_empty(degree_list_selected2$LEVELName)) {
      degree_list_selected <<- vector(mode = "list")
    } else {
      degree_list_selected <<- vector(mode = "list")
      degree_list_selected <<- cbind(degree_list_selected, degree_list_selected2$LEVELName)
    }
    
    user_scenarios <<- user_scenarios %>% filter(!(scenario == temp_scenario))
    
    school_cards()
    major_cards()
    occupation_cards()
    
    school_unavailable()
    major_unavailable()
    occupation_unavailable()
    degree_unavailable()
    
    build_radio()
    build_variables$current_page <<- 5
    
    temp_choice$school_status <<- 3
    schoolclick <<- 1
    temp_choice$major_status <<- 3
    majorclick <<- 1
    temp_choice$occupation_status <<- 3
    occupationclick <<- 1
    temp_choice$degree_status <<- 3
    degreeclick <<- 1
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
    save_scenario()
    build_radio()
    output$scenario_table <- renderDataTable({NULL})
    removeModal()
  })
  
  create_scenario_table <- function(){
    scen_temp <- filter(user_scenarios, scenario %in% choosen_scenario)
    sch_temp <- scen_temp %>% filter(category == "school") %>% select(id)
    maj_temp <- scen_temp %>% filter(category == "major") %>% select(id)
    occ_temp <- scen_temp %>% filter(category == "occupation") %>% select(id)
    deg_temp <- scen_temp %>% filter(category == "degree") %>% select(id)
    
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
    scenario_temp <- left_join(scenario_temp, school_scenario2, by = "UNITID")
    scenario_temp <- left_join(scenario_temp, major_scenario2, by = "CIPCODE")
    scenario_temp <- left_join(scenario_temp, degree_scenario2, by = "AWLEVEL")
    scenario_temp <- left_join(scenario_temp, occupation_scenario2, by = "OCCCODE")
    
    scenario_temp <- scenario_temp %>% rename("School" = "INSTNM", "State" = "STABBR", "Cost" = "TOTALT",
                                              "Major" = "CIPNAME", "Degree" = "LEVELName", 
                                              "Occupation" = "OCCNAME", "Salary" = "X17p"
    )
#    saveRDS(scenario_temp, "scen_temp.rds")
    favorite_temp <<- scenario_temp
    output$scenario_table <- renderDataTable({
      DT::datatable(
        data = scenario_temp,
        escape = FALSE,
        rownames = FALSE,
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
          columnDefs = (list(list(visible=FALSE, targets=c(0,1,2,3,4,5,8,9,16,17,18)),
                             list(width = '255px', targets =c(6,11,13,14)),
                             list(width = '25px', targets =c(7,12)),
                             list(width = '55px', targets = c(10,15)),
                             list(className = 'dt-center', targets = c(7,12)))),
          pageLength = 12
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
    
    fav_school <- favorite_to_add %>% select(UNITID)
    fav_major <- favorite_to_add %>% select(CIPCODE)
    fav_occupation <- favorite_to_add %>% select(OCCCODE)
    fav_degree <- favorite_to_add %>% select(AWLEVEL)
    
    favor_temp <- user_favorites %>% filter(school %in% fav_school$UNITID, major %in% fav_major$CIPCODE, occupation %in% fav_occupation$OCCCODE,
                                            degree %in% fav_degree$AWLEVEL)
    if(is_empty(favor_temp$school)){
      fav_temp <- tibble("user" = pro_user, "school" = fav_school$UNITID, "major" = fav_major$CIPCODE,
                             "occupation" = fav_occupation$OCCCODE, "degree" = fav_degree$AWLEVEL)
      user_favorites <<- rbind(user_favorites, fav_temp)
      save_favorite()
      update_fav_num()
      favorite_cards()
    }
  })
  save_favorite <- function() {
    if(pro_name != "Demo") {
    saveRDS(user_favorites, favorite_file)
    drop_upload(favorite_file, path = "responses")
    }
  }
  observeEvent(input$return_dashboard_button, {
    updateTabItems(session, "tabs", selected = "dashboard")
  })
  observeEvent(input$build_new2, {
    clear_build()
    scenario_source <<- 2
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
  observeEvent(input$build_new_button, {
    clear_build()
    
  })
  
  update_scenario <- function() {
    user_scen_temp <- user_scenarios %>% distinct(scenario, .keep_all = TRUE)
    if(NROW(user_scen_temp) == 0){
      new_scenario_name <<- "Scenario 1"
    } else {
      temp_val <- user_scenarios %>% select(scenario) %>% filter(!(scenario == "Favorite")) %>% distinct(scenario, .keep_all = FALSE)
      temp_val <- as.vector(temp_val$scenario)
      temp_val <- str_remove_all(temp_val, "Scenario ")
      temp_val <- max(as.numeric(temp_val))
      new_scenario_name <<- paste0("Scenario ",temp_val + 1)
    }
  }

  favorite_cards <- function() {
    fav_card <- tibble()
    fav_temp <- user_favorites %>% filter(user %in% pro_user)
    for(i in 1:NROW(fav_temp)){
      scen_temp <- backbone
      scen_temp <- scen_temp %>% filter(UNITID %in% fav_temp$school[i], CIPCODE %in% fav_temp$major[i],
                                                OCCCODE %in% fav_temp$occupation[i], AWLEVEL %in% fav_temp$degree[i]) %>% select(ID)
      fav_card <- rbind(fav_card, scen_temp)
    }
    fav_card <- fav_card %>% mutate("scenario" = "Favorite")
    user_scen01 <<- fav_card
    scenario_temp <- user_scen01 %>% filter(scenario %in% "Favorite") 
    scenario_temp <- left_join(scenario_temp, backbone, by = "ID")
    scenario_temp <- left_join(scenario_temp, school_scenario, by = "UNITID")
    scenario_temp <- left_join(scenario_temp, major_scenario, by = "CIPCODE")
    scenario_temp <- left_join(scenario_temp, degree_scenario, by = "AWLEVEL")
    scenario_temp <- left_join(scenario_temp, occupation_scenario, by = "OCCCODE")

    if(NROW(scenario_temp) > 0){
      output$favorite_container <- renderUI({
        args <- lapply(1:NROW(scenario_temp), function(.x) card2(.x,
                                                                 name = scenario_temp$ID[.x],
                                                                 school = scenario_temp$INSTNM[.x],
                                                                 major = scenario_temp$CIPNAME[.x],
                                                                 degree = scenario_temp$LEVELName[.x],
                                                                 occupation = scenario_temp$OCCNAME[.x]))
        args$cellArgs <- list(
          style = "
          width: 260px;
          height: auto;
          margin: 5px;
          margin-bottom:50px;
          margin-left:45px;
          line-height: '110%';
          "
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
    make_detail_obs(name)
    div(class = "card",
        div(style = "display:inline-block;vertical-align:top;width: 260px;margin:0px;padding:0px;",
            splitLayout(cellWidths = c("17%","60%","23%"),
                        div(style = "margin-bottom:22px;margin-top:-10px;padding:0px;",
                            checkboxInput(inputId = paste0("check",name), label = "", width = "22px")),
                        h3("Option ",x,style = "margin-top: 0px;"),
                        actionButton(inputId = paste0("button",name),label = "", icon = icon("trash"),
                                     style = "margin:0px;color:red;"))),
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
    if(length(obsList[[btName]]) == 0){
      obsList[[btName]] <<- observeEvent(input[[btName]], {
        user_scen01 <<- user_scen01 %>% filter(!(ID == name & scenario == "Favorite"))
        obs_temp <- left_join(user_scen01, backbone, by = "ID")
        user_favorites <<- user_favorites %>% filter(school %in% obs_temp$UNITID & major %in% obs_temp$CIPCODE & occupation %in% obs_temp$OCCCODE & degree %in% obs_temp$AWLEVEL)
        save_favorite()
        update_fav_num()
        obsList <<- list()
        favorite_cards()
        graphList <<-vector(mode = "list")
        build_graph()
      }, ignoreInit = TRUE, once = TRUE)
    }
  }
  make_check_obs <- function(x) {
    checkNm <- paste0("check",x)
    if(length(checkList[[checkNm]]) == 0){
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
  make_detail_obs <- function(name) {
    detailNm <- paste0("details",name)
    if(length(detailList[[detailNm]]) == 0){
      detailList[[detailNm]] <<- observeEvent(input[[detailNm]], {
#        print(detailList)
      },ignoreInit = TRUE)
    }
  }
  observe({
    output$comparison_container <- renderPlot(height = 650,{
      graph_parameters
    })
  })
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
      graph_backbone <- left_join(graph_backbone, occupation_scenario3, by = "OCCCODE")
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
  es_table_var <- reactive ({  
    es_school_temp <- school_master2
    if(!is.null(input$es_school) & input$es_school !='') {
      es_school_temp <- filter(es_school_temp, INSTNM %in% input$es_school)
    }
    if( input$es_state !='') {
      es_school_temp <- filter(es_school_temp, State %in% input$es_state)
    }
    if(input$es_room_board >= 0){
      es_school_temp <- filter(es_school_temp, ROOM_BOARD <= input$es_room_board)
    }
    if(input$es_annual_hi >= 0){
      es_school_temp <- filter(es_school_temp, TotCstOutHi <= input$es_annual_hi)
    }
    if(input$es_annual_lo >= 0){
      es_school_temp <- filter(es_school_temp, TotCstOutLo <= input$es_annual_lo)
    }
    
    es_school_list <<- unique(es_school_temp$INSTNM)
    es_state_list <<- unique(es_school_temp$State)
    
    if(is.null(input$es_school) |input$es_school == ''){
      updateSelectInput(session, inputId = "es_school", label = "School",
                        choices = isolate(c(All = '', sort(es_school_list))), selected = '')
    }
    if(is.null(input$es_state) |input$es_state == ''){
      updateSelectInput(session, inputId = "es_state", label = "State",
                        choices = isolate(c(All = '', sort(es_state_list))), selected = '')
    }
    es_school_temp <- es_school_temp %>% mutate(GRADR150 = BAGR150 + L4GR150)   
    es_school_temp <- es_school_temp %>% select("INSTNM", "STABBR", "CITY", "WEBADDR", "APPLCN", "ADMSSN", "ENRLT","ROOM_BOARD",
                                              "GRADR150", "TotCstInHi", "TotCstOutHi", "TotCstInLo", "TotCstOutLo")
    
    es_temp1 <- es_school_temp$TotCstInLo == 0
    es_school_temp <- cbind(es_school_temp, es_temp1)

    #Rename table column headers     
    es_school_temp <- es_school_temp %>% rename("School<br>Name" = "INSTNM", "State" = "STABBR", "City" = "CITY",
                                                "Web<br>Address" = "WEBADDR", "Apps" = "APPLCN",
                                                "Admit" = "ADMSSN", "Enroll" = "ENRLT",
                                                "Room and<br>Board" = "ROOM_BOARD", "Grad<br>Rate<br>150%" = "GRADR150",
                                                "In State<br>Cost w/IG" = "TotCstInLo",
                                                "Out of State<br>Cost w/IG" = "TotCstOutLo", "In State<br>Cost" = "TotCstInHi",
                                                "Out of State<br>Cost" = "TotCstOutHi"
                                                )
    #      saveRDS(curriculum_temp, "cur.rds")
  })
  observe( {  
    output$es_table <- renderDataTable({
      DT::datatable(
        data = es_table_var(),
        escape = FALSE,
        rownames = FALSE,
        class="cell-border stripe",
        options = list(
          headerCallback = DT::JS(
            "function(thead) {",
            "  $(thead).css('font-size', '15px');",
            "}"),
          dom = 'tip',
          order = list(list(13, 'asc'), list(9, 'asc')),
          saveState = TRUE,
          filter = FALSE,
 #         autoWidth = TRUE,
                      columnDefs = (list(list(visible=FALSE, targets=c(13)),
                                         list(width = '270px', targets =c(0,3)),
                                         list(width = '185px', targets = c(2)),
                                         list(width = '25px', targets =c(1)),
                                         list(width = '63px', targets = c(4,5,6,7,8,9,10,11,12)))),
          lengthMenu = 10
        ),
        selection = list(mode = 'single')
      ) %>%
        formatStyle(
          0,
          target = 'row',
          color = 'black',
          fontWeight = 'normal',
          fontSize = '14px',
          lineHeight = '100%',
          margin = '0px',
          padding = '2px'
        )
    })
  })
  eo_table_var <- reactive({
    occupation_temp <- occupation_master
    if(!is.null(input$eo_occupation) & input$eo_occupation !='') {
      occupation_temp <- filter(occupation_temp, OCCNAME %in% input$eo_occupation) 
    }
    if(!is.null(input$eo_entry_degree) & input$eo_entry_degree !='') {
      occupation_temp <- filter(occupation_temp, Entry_Degree %in% input$eo_entry_degree) 
    }
    if(!is.null(input$eo_required_exp) & input$eo_required_exp !='') {
      occupation_temp <- filter(occupation_temp, Experience %in% input$eo_required_exp) 
    }
    occupation_temp <- filter(occupation_temp, EmplyPC >= input$eo_growth_rate)
    occupation_temp <- filter(occupation_temp, X17p >= input$eo_starting_salary)
    
    eo_occupation_list <<- unique(occupation_temp$OCCNAME)
    eo_entry_degree_list <<- unique(occupation_temp$Entry_Degree)
    eo_required_exp_list <<- unique(occupation_temp$Experience)
    
    if(is.null(input$eo_occupation) |input$eo_occupation == ''){
      updateSelectInput(session, inputId = "eo_occupation", label = "Occupation",
                        choices = isolate(c(All = '', sort(eo_occupation_list))), selected = '')
    }
    if(is.null(input$eo_entry_degree) |input$eo_entry_degree == ''){
      updateSelectInput(session, inputId = "eo_entry_degree", label = "Entry Degree",
                        choices = isolate(c(All = '', sort(eo_entry_degree_list))), selected = '')
    }
    if(is.null(input$eo_required_exp) |input$eo_required_exp == ''){
      updateSelectInput(session, inputId = "eo_required_exp", label = "Required Experience",
                        choices = isolate(c(All = '', sort(eo_required_exp_list))), selected = '')
    }
    
    occupation_temp <- occupation_temp %>% select("OCCNAME", "EmplyChg", "EmplyPC", "SelfEmpl", "Entry_Degree", "Experience", "X10p", "X17p",
                                                    "X25p", "X50p", "X75p", "X82p", "X90p")
    occupation_temp$X10p <- round(occupation_temp$X10p, 0)
    occupation_temp$X17p <- round(occupation_temp$X17p, 0)
    occupation_temp$X25p <- round(occupation_temp$X25p, 0)
    occupation_temp$X50p <- round(occupation_temp$X50p, 0)
    occupation_temp$X75p <- round(occupation_temp$X75p, 0)
    occupation_temp$X82p <- round(occupation_temp$X82p, 0)
    occupation_temp$X90p <- round(occupation_temp$X90p, 0)

    occupation_temp <- occupation_temp %>% rename("Occupation" = "OCCNAME", "Growth<br>in Jobs(#)" = "EmplyChg",
                                                  "Growth<br>in Jobs(%)" = "EmplyPC", "Percent<br>Self<br>Employed" = "SelfEmpl",
                                                  "Typical<br>Entry<br>Degree" = "Entry_Degree",
                                                  "Level of<br>Experience<br>Required" = "Experience",
                                                  "10th<br>Percentile<br>Salary" = "X10p", "17th<br>Percentile<br>Salary" = "X17p",
                                                  "25th<br>Percentile<br>Salary" = "X25p", "Median<br>Salary" = "X50p",
                                                  "75th<br>Percentile<br>Salary" = "X75p", "82th<br>Percentile<br>Salary" = "X82p",
                                                  "90th<br>Percentile<br>Salary" = "X90p")
  })
  observe( {  
    output$eo_table <- renderDataTable({
      DT::datatable(
        data = eo_table_var(),
        escape = FALSE,
        rownames = FALSE,
        class="cell-border stripe",
        options = list(
          headerCallback = DT::JS(
            "function(thead) {",
            "  $(thead).css('font-size', '15px');",
            "}"),
          dom = 'tip',
          order = list(list(7, 'desc')),
          saveState = TRUE,
          filter = FALSE,
          autoWidth = TRUE,
          #            columnDefs = (list(list(width = '300px', targets =c(11,13,14)),
          #                               list(width = '25px', targets =c(12)),
          #                               list(width = '110px', targets = c(15,16,17,18)))),
          lengthMenu = c(10)
        ),
        selection = list(mode = 'single')
      ) %>%
        formatStyle(
          0,
          target = 'row',
          color = 'black',
          fontWeight = 'normal',
          fontSize = '15px',
          lineHeight = '100%'
        )
    })
  })
  em_table_var <- reactive ({  
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
    curriculum_temp <- left_join(curriculum_temp, degree_master, by = c("AWLEVEL", "Years"))
    curriculum_temp <- left_join(curriculum_temp, school_master2, by = "UNITID")
    
    curriculum_temp <- curriculum_temp %>% select( "CIPNAME", "LEVELName", "INSTNM",   
                                                  "CTOTALT")
 
    em_school_list <<- unique(curriculum_temp$INSTNM)
    em_curriculum_list <<- unique(curriculum_temp$CIPNAME)
    em_degree_list <<- unique(curriculum_temp$LEVELName)
    if(is.null(input$em_school) |input$em_school == ''){
      updateSelectInput(session, inputId = "em_school", label = "School",
                        choices = isolate(c(All = '', sort(em_school_list))), selected = '')
    }
    if(is.null(input$em_major)|input$em_major == ''){
      updateSelectInput(session, inputId = "em_major", label = "Major",
                        choices = isolate(c(All = '', sort(em_curriculum_list))), selected = '')
    }
    if(is.null(input$em_degree) | input$em_degree == ''){
      updateSelectInput(session, inputId = "em_degree", label = "Degree",
                        choices = isolate(c(All = '', sort(em_degree_list))), selected = '') 
    }
    #Rename table column headers     
    curriculum_temp <- curriculum_temp %>% rename("School<br>Name" = "INSTNM", 
                                                  "Major" = "CIPNAME", "Degree<br>Name" = "LEVELName",
                                                  "Number of<br>Degrees" = "CTOTALT")
    #      saveRDS(curriculum_temp, "cur.rds")
  })
  observe( {  
    output$em_table <- renderDataTable({
      DT::datatable(
        data = em_table_var(),
        escape = FALSE,
        rownames = FALSE,
        class="cell-border stripe",
        options = list(
          headerCallback = DT::JS(
            "function(thead) {",
            "  $(thead).css('font-size', '15px');",
            "}"),
          dom = 'tip',
          order = list(list(3, 'desc')),
          saveState = TRUE,
          filter = FALSE,
  #        autoWidth = FALSE,
                      columnDefs = (list(list(width = '330px', targets =c(0)),
                                         list(width = '330px', targets =c(1)),
                                         list(width = '330px', targets =c(2)),
                                         list(width = '80px', targets =c(3)))),
          lengthMenu = c(10)
        ),
        selection = list(mode = 'single')
      ) %>%
        formatStyle(
          0,
          target = 'row',
          color = 'black',
          fontWeight = 'normal',
          fontSize = '15px',
          lineHeight = '100%'
        )
    })
  })
  observeEvent(input$tabs, {
    req(input$tabs == "build")
    if(scenario_source == 2){
      build_variables$current_page <<- 1
      scenario_source <<- 1
    } else if(scenario_source == 3){
      build_variables$current_page <<- 6
      scenario_source <<- 1
    } else if(scenario_source == 4){
      build_variables$current_page <<- 7
      scenario_source <<- 1
    } else if(scenario_source == 5){
      build_variables$current_page <<- 8
      scenario_source <<- 1
    } else {
    build_variables$current_page <<- 10
    }
  })
  observeEvent(input$school_more, {
    scenerio_source <<- 3
    updateTabItems(session, "tabs", "school")
    output$school_return <- renderUI({
      actionButton(inputId = "s_return", label = "Return to School")
    })
  })
  observeEvent(input$s_return, {
    scenario_source <<- 3
    updateTabItems(session, "tabs", selected = "build")
    output$school_return <- renderUI({NULL})
  })
  observeEvent(input$major_more, {
    scenerio_source <<- 4
    updateTabItems(session, "tabs", "major")
    output$major_return <- renderUI({
      actionButton(inputId = "m_return", label = "Return to Major")
    })
  })
  observeEvent(input$m_return, {
    scenario_source <<- 4
    updateTabItems(session, "tabs", selected = "build")
    output$major_return <- renderUI({NULL})
  })
  observeEvent(input$occupation_more, {
    scenerio_source <<- 5
    updateTabItems(session, "tabs", "occupation")
    output$occupation_return <- renderUI({
      actionButton(inputId = "o_return", label = "Return to Occupation")
    })
  })
  observeEvent(input$o_return, {
    scenario_source <<- 5
    updateTabItems(session, "tabs", selected = "build")
    output$occupation_return <- renderUI({NULL})
  })
}

shinyApp(ui, server) 