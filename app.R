# Self Directed EPIC 4/22/2020 ----
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

# 02 Declare Variables ----
# UI Variables
school_list <- readRDS("School_list.rds")
school_list2 <- school_list

major_list <- readRDS("Major_list.rds")
major_list2 <- major_list

occupation_list <- readRDS("Occupation_list.rds")
occupation_list2 <- occupation_list

degree_list <- readRDS("Degree_list.rds")
degree_list2 <- degree_list
# Server Variables
backbone <- readRDS("Backbone.rds")
alt_title <- readRDS("data/AltTitle.rds")
degree_master <- readRDS("data/AW_Degree.rds") %>% na.omit(aw_degree)
cips <- readRDS("data/CIPS.rds")
major_master <- readRDS("data/Curriculum.rds")
ent_degree <- readRDS("data/Ent_Degree.rds")
occupation_master <- readRDS("data/Occupations.rds")
school_master <- readRDS("data/Schools.rds")
state_abbr_master <- readRDS("data/state_abbr.rds")
# 03 Functions ----


# 04.1 Header ----
header <- dashboardHeaderPlus()

# 04.2 Sidebar ----
sidebar <- dashboardSidebar(
  
    tags$img(src = 'BlueLogo_Final.png',contentType = "image/png", style = "width: 230px; height: 180px;align:center;"),
    sidebarMenu(id = "tabs",
                menuItem("Profile", tabName = "profile", icon = icon("user")),
                menuItem("Dashboard", tabName = "dashboard", icon = icon("tachometer-alt"),selected = TRUE),
                menuItem("Build Scenario", tabName = "build", icon = icon("plus")),
                menuItem("Explore", tabName = "explore", icon = icon("fas fa-compass")),
                
                menuItem("Settings", tabName = "settings", icon = icon("cog")),
                menuItem("Tools", tabName = "tools", icon = icon("wrench",lib = "glyphicon")),
                menuItem("Help", tabName = "help", icon = icon("question-circle")),
                searchInput(inputId = "search", label = "", placeholder = "Search EPIC"))
)

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
    tags$head(
        tags$style(HTML("
                      .my_class {
                      font-weight: bold;
                      color:#eaedef;
                      }"))
    ),
    
    # 04.4 TABS ----
    tabItems(
        tabItem(tabName = "dashboard",
                boxPlus(id = "dashboard_window", width = 12, style ="padding: 0px;margin: 0px;",
                        div(align = 'center', style = "font-size: 25px; padding-top: 0px; margin-top:12em",
                            h1(strong("Welcome to your EPIC Portal")),
                            h3("We are here to help plan your education! Lets go!"),
                            actionBttn(inputId = "get_started", label = "GET STARTED",
                                       style = "material-flat", color = "primary"))
                )#
        ),
        tabItem(tabName = "build",
                boxPlus(id = "build_window", width = 12, style = "padding: 0px;margin: 0px;",
                        
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
                                       div(id = "schoolinfo2", column(width = 4, uiOutput("schoolchoice1"),uiOutput("schoolchoice4")),
                                           column(width = 4, uiOutput("schoolchoice2"), uiOutput("schoolchoice5")),
                                           column(width = 4, uiOutput("schoolchoice3"))
                                               ))),
                        fluidRow(column(width = 1),
                                 column(width = 6,
                                        div(id = "schoolinfo3",
                                        checkboxInput(inputId = "school_checkbox", label = "I am not sure where I want to attend...",value = FALSE)))),
                        
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
                                        div(id = "majorinfo2",  column(width = 4, uiOutput("majorchoice1"),uiOutput("majorchoice4")),
                                            column(width = 4, uiOutput("majorchoice2"), uiOutput("majorchoice5")),
                                            column(width = 4, uiOutput("majorchoice3"))
                                        ))),
                        fluidRow(column(width = 1),
                                 column(width = 6,
                                        div(id = "majorinfo3",
                                            checkboxInput(inputId = "major_checkbox", label = "I am not sure what I want to study...",value = FALSE)))),
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
                                        div(id = "occupationinfo2", column(width = 4, uiOutput("occupationchoice1"),uiOutput("occupationchoice4")),
                                            column(width = 4, uiOutput("occupationchoice2"), uiOutput("occupationchoice5")),
                                            column(width = 4, uiOutput("occupationchoice3"))
                                        ))),
                        fluidRow(column(width = 1),
                                 column(width = 6,
                                        div(id = "occupationinfo3",
                                            checkboxInput(inputId = "occupation_checkbox", label = "I am not sure what I want to do for a career...",value = FALSE)))),
                        
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
                        fluidRow(column(width = 2),
                                 column(width = 3,
                                        uiOutput("add_favorite")),
                                 column(width = 4,
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
        tabItem(tabName = "profile",
                boxPlus(id = "profile_window", width = 12, style = "padding: 0px;margin: 0px;",
                        h1("Profile"))),
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
ui <- dashboardPagePlus( header, sidebar, body, useShinyjs(), tags$head(tags$meta( name="viewport", content="width=1280")),
                        tags$head(
                            tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
                        ))

# 05 SERVER ----
server <- function(input, output, session) {
    observe({
        shinyjs::runjs("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';")
    }) 
    build_variables <- reactiveValues(current_page = 1)
    temp_choice <- reactiveValues(school_status = 1, major_status = 1, occupation_status = 1, degree_status = 1)
    temp_scenario <- reactiveValues(page2 = 0, page4 = 0, page6 = 0, page8 = 0)
    # load data
    number_favorites <- 0
    major_filter <- cips
    school_filter <- school_master %>% select("UNITID", "INSTNM")
    occupation_filter <- occupation_master %>% select("OCCNAME", "OCCCODE")
    degree_filter <- degree_master
    
    school_scenario <- school_master %>% select("UNITID", "INSTNM", "STABBR", "TotCstInHi", "TotCstOutHi")
    occupation_scenario <- occupation_master %>% select("OCCNAME", "OCCCODE", "X17p", "Entry_Code", "Entry_Degree", "Experience")
    major_scenario <- cips
    degree_scenario <- degree_master
    
    school_list_selected <- vector(mode = "list")
    degree_list_selected <- vector(mode = "list")
    occupation_list_selected <- vector(mode = "list")
    major_list_selected <- vector(mode = "list")
    
    user_scen01 <- data.frame("ID" = numeric(), "scenario" = character())

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
      shinyjs::show(id = "favorites")
      shinyjs::show(id = "scenario_text")
      shinyjs::show(id = "scenario_available")
      shinyjs::show(id = "dt_scenario")
      shinyjs::show(id = "add_favorite")
      shinyjs::show(id = "build_new")
      shinyjs::show(id = "return_dashboard")
    }
    hidescenario <- function() {
      shinyjs::hide(id = "favorites")
      shinyjs::hide(id = "scenario_text")
      shinyjs::hide(id = "scenario_available")
      shinyjs::hide(id = "dt_scenario")
      shinyjs::hide(id = "add_favorite")
      shinyjs::hide(id = "build_new")
      shinyjs::hide(id = "return_dashboard")
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
        output$build_header4 <- renderUI({ p("five schools. If you are not sure where you want to attend, check the box.")})
    }
    show_major_pref <- function() {
        output$build_header2 <- renderUI({ p("Major Preferences")})
        output$build_header3 <- renderUI({ p("Search for majors that you are interested in studying. You may add up to") })
        output$build_header4 <- renderUI({ p("five majors. If you are not sure what you want to study, check the box.")})
    }
    show_occupation_pref <- function() {
        output$build_header2 <- renderUI({ p("Occupation Preferences")})
        output$build_header3 <- renderUI({ p("Search for careers that you are interested in persuing. You may add up to") })
        output$build_header4 <- renderUI({ p("five occupations. If you are not sure which career you want, check the box.")})
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
      output$schoolchoice1 <- renderUI({NULL}) 
      output$schoolchoice2 <- renderUI({NULL}) 
      output$schoolchoice3 <- renderUI({NULL}) 
      output$schoolchoice4 <- renderUI({NULL}) 
      output$schoolchoice5 <- renderUI({NULL}) 
    }
    clear_majorchoices <- function() {
      output$majorchoice1 <- renderUI({NULL}) 
      output$majorchoice2 <- renderUI({NULL}) 
      output$majorchoice3 <- renderUI({NULL}) 
      output$majorchoice4 <- renderUI({NULL}) 
      output$majorchoice5 <- renderUI({NULL}) 
    }
    clear_occupationchoices <- function() {
      output$occupationchoice1 <- renderUI({NULL}) 
      output$occupationchoice2 <- renderUI({NULL}) 
      output$occupationchoice3 <- renderUI({NULL}) 
      output$occupationchoice4 <- renderUI({NULL}) 
      output$occupationchoice5 <- renderUI({NULL}) 
    }
    observe({
        if(build_variables$current_page == 1){
            showheader()
            hidescenario()
            output$build_header1 <- renderUI({ p("Lets Get Started!") })
            output$build_header2 <- renderUI({ p("Where would you like to begin?")})
            output$build_header3 <- renderUI({ p("Select a category to begin building your scenario. We recommend starting with the category") })
            output$build_header4 <- renderUI({ p("that you have the strongest preference in.")})
            show_choice_page()
            shinyjs::show(id = "choice_line")
            shinyjs::show(id = "previous_select")
            shinyjs::show(id = "next_select")
        }
    })
    observe({
      if(build_variables$current_page == 2){
        if(temp_scenario$page2 == 1){
          showstep(1)
          school_page()
        } else if(temp_scenario$page2 == 2){
          showstep(1)
          major_page()
        } else if(temp_scenario$page2 == 3){
          showstep(1)
          occupation_page()
        } else if(temp_scenario$page2 == 4){
          showstep(1)
          degree_page()
        }
      }
    })
    observe({
        if(build_variables$current_page == 3){
            output$build_header1 <- renderUI({ p("Great Job!") })
            output$build_header2 <- renderUI({ p("Where would you like to go next?")})
            output$build_header3 <- renderUI({ p("You are off to a great start. Select another category to continue. We recommend selecting the category") })
            output$build_header4 <- renderUI({ p("that you have the next strongest preference in.")})
            show_choice_page()
        }
    })
    observe({
      if(build_variables$current_page == 4){
        if(temp_scenario$page4 == 1){
          showstep(2)
          school_page()
        } else if(temp_scenario$page4 == 2){
          showstep(2)
          major_page()
        } else if(temp_scenario$page4 == 3){
          showstep(2)
          occupation_page()
        } else if(temp_scenario$page4 == 4){
          showstep(2)
          degree_page()
        }
      }
    })
    observe({
        if(build_variables$current_page == 5){
            output$build_header1 <- renderUI({ p("Halfway done!") })
            output$build_header2 <- renderUI({ p("Where would you like to go next?")})
            output$build_header3 <- renderUI({ p("You are making great progress. Select another category to continue building. We recommend selecting the category") })
            output$build_header4 <- renderUI({ p("that you have the next strongest preference in.")})
            show_choice_page()
        }
    })
    observe({
      if(build_variables$current_page == 6){
        if(temp_scenario$page6 == 1){
          showstep(3)
          school_page()
        } else if(temp_scenario$page6 == 2){
          showstep(3)
          major_page()
        } else if(temp_scenario$page6 == 3){
          showstep(3)
          occupation_page()
        } else if(temp_scenario$page6 == 4){
          showstep(3)
          degree_page()
        }
      }
    })
    observe({
        if(build_variables$current_page == 7){
            output$build_header1 <- renderUI({ p("One more to go!") })
            output$build_header2 <- renderUI({ p("Select the last category")})
            output$build_header3 <- renderUI({ p("You are almost done! Select the last category to continue building. If you want to make changes") })
            output$build_header4 <- renderUI({ p("to previous categories, you may select the previous button.")})
            show_choice_page()
        }
    })
    observe({
      if(build_variables$current_page == 8){
        if(temp_scenario$page8 == 1){
          showstep(4)
          school_page()
        } else if(temp_scenario$page8 == 2){
          showstep(4)
          major_page()
        } else if(temp_scenario$page8 == 3){
          showstep(4)
          occupation_page()
        } else if(temp_scenario$page8 == 4){
          showstep(4)
          degree_page()
        }
      }
    })
    observe({
      if(build_variables$current_page == 9){
        output$build_header1 <- renderUI({ p("Building complete.") })
        output$build_header2 <- renderUI({ p("Lets Build Your Scenario!")})
        output$build_header3 <- renderUI({ p("Great job filing out your preferences. If you wish to make changes to any of your preferences in the") })
        output$build_header4 <- renderUI({ p("categories, select the previous button. If you are ready to continue select the build scenario button!")})
        show_choice_page()
      }
    })
    observe({
      if(build_variables$current_page == 10){
        hideheader()
        hidechoices()
        shinyjs::hide(id = "choice_line")
        shinyjs::hide(id = "previous_select")
        shinyjs::hide(id = "next_select")
        showscenario()
        output$favorites <- renderUI({p(paste0("Favorite Options (", number_favorites,")"))})
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
              p("view compri-"),
              p("son graphs and"),
              p("return on invest-"),
              p("ment reports.")
              )
          })
 #       create_scenario_table()
      }
    })

    observe({
        if(build_variables$current_page > 1){
            output$previous_select <- renderUI({
                actionButton(inputId = "previous_button", label = "Previous")
            })
        } else {
            output$previous_select <- renderUI(NULL)
        }
    })
    observe({
        if(build_variables$current_page == 9){
            output$next_select <- renderUI({
                actionButton(inputId = "next_button", label = "Build Scenario")
            })
        } else if(build_variables$current_page == 10){
                output$add_favorite <- renderUI({
                    actionButton(inputId = "add_favorite_button", label = "Add to Favorites")
                })
                output$build_new <- renderUI({
                  actionButton(inputId = "build_new_button", label = "Build New Scenario...")
                })
                output$return_dashboard <- renderUI({
                  actionButton(inputId = "return_dashboard_button", label = "Return to Dashboard")
                })
        } else if(build_variables$current_page < 9){
            output$next_select <- renderUI({
                actionButton(inputId = "next_button", label = "Next", icon = icon("arrow-right"))
            })
        } else {
            output$next_select <- renderUI(NULL)
        }
    })
    observe({
        if(temp_choice$school_status == 1){
        output$school_select <- renderUI({
            actionButton(inputId = "school_button", label = "", 
                         style ="background: url('epicButtons-18.svg');border-bottom:7px solid; border-bottom-color: #999999;") 
            })
        } else if(temp_choice$school_status == 2){
            output$school_select <- renderUI({
                actionButton(inputId = "school_button", label = "", 
                             style ="background: url('epicButtons-18.svg');border:1px solid #e2ac24; border-bottom:7px solid #e2ac24;")
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
        } else if(temp_choice$major_status == 2){
            output$major_select <- renderUI({
                actionButton(inputId = "major_button", label = "",
                             style ="background: url('epicButtons-19.svg');border:1px solid #37b749;border-bottom:7px solid #37b749;")
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
        } else if(temp_choice$occupation_status == 2){
            output$occupation_select <- renderUI({
                actionButton(inputId = "occupation_button", label = "", 
                             style ="background: url('epicButtons-20.svg');border:1px solid #477ddd;border-bottom:7px solid #477ddd;")
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
        } else if(temp_choice$degree_status == 2){
            output$degree_select <- renderUI({
                actionButton(inputId = "degree_button", label = "", 
                             style ="background: url('epicButtons-21.svg');border:1px solid #e22b2b;border-bottom:7px solid #e22b2b;")
            })
        } else if(temp_choice$degree_status == 3){
            output$degree_select <- renderUI({
                actionButton(inputId = "degree_button", label = "", 
                             style ="background: url('epicButtons-25.svg');border:1px solid #e22b2b;border-bottom:7px solid #e22b2b;")
            })
        }
    })
    
    observeEvent(input$school_button,{
        if(temp_choice$school_status == 3){
            return()
        } else if(temp_choice$school_status == 1){
            temp_choice$school_status <<- 2
            if(temp_choice$major_status == 2){temp_choice$major_status <<- 1}
            if(temp_choice$occupation_status == 2){temp_choice$occupation_status <<- 1}
            if(temp_choice$degree_status == 2){temp_choice$degree_status <<- 1}
        } else if(temp_choice$school_status == 2){
            temp_choice$school_status <<- 1
        }
    })
    observeEvent(input$major_button,{
        if(temp_choice$major_status == 3){
            return()
        } else if(temp_choice$major_status == 1){
            temp_choice$major_status <<- 2
            if(temp_choice$school_status == 2){temp_choice$school_status <<- 1}
            if(temp_choice$occupation_status == 2){temp_choice$occupation_status <<- 1}
            if(temp_choice$degree_status == 2){temp_choice$degree_status <<- 1}
        } else if(temp_choice$major_status == 2){
            temp_choice$major_status <<- 1
        }
    })
    observeEvent(input$occupation_button,{
        if(temp_choice$occupation_status == 3){
            return()
        } else if(temp_choice$occupation_status == 1){
            temp_choice$occupation_status <<- 2
            if(temp_choice$school_status == 2){temp_choice$school_status <<- 1}
            if(temp_choice$major_status == 2){temp_choice$major_status <<- 1}
            if(temp_choice$degree_status == 2){temp_choice$degree_status <<- 1}
        } else if(temp_choice$occupation_status == 2){
            temp_choice$occupation_status <<- 1
        }
    })
    observeEvent(input$degree_button,{
        if(temp_choice$degree_status == 3){
            return()
        } else if(temp_choice$degree_status == 1){
            temp_choice$degree_status <<- 2
            if(temp_choice$school_status == 2){temp_choice$school_status <<- 1}
            if(temp_choice$major_status == 2){temp_choice$major_status <<- 1}
            if(temp_choice$occupation_status == 2){temp_choice$occupation_status <<- 1}
        } else if(temp_choice$degree_status == 2){
            temp_choice$degree_status <<- 1
        }
    })
    # 06 Next button ----
    observeEvent(input$next_button,{
        if(build_variables$current_page == 1){
          if(temp_choice$school_status == 2){
              temp_scenario$page2 <<- 1
              temp_choice$school_status <<- 3
          } else if(temp_choice$major_status == 2){
              temp_scenario$page2 <<- 2
              temp_choice$major_status <<- 3
          } else if(temp_choice$occupation_status == 2){
              temp_scenario$page2 <<- 3
              temp_choice$occupation_status <<- 3
          } else if(temp_choice$degree_status == 2){
              temp_scenario$page2 <<- 4
              temp_choice$degree_status <<- 3
          } else {
              return()
          }
        }
        if(build_variables$current_page == 2){
          if(temp_scenario$page2 == 1){
            major_unavailable()
            occupation_unavailable()
            degree_unavailable()
          } else if(temp_scenario$page2 == 2){
            school_unavailable()
            occupation_unavailable()
            degree_unavailable()
          } else if(temp_scenario$page2 == 3){
            school_unavailable()
            major_unavailable()
            degree_unavailable()
          } else if(temp_scenario$page2 == 4){
            if(!is_empty(input$degree_checkbox)){
              degree_list_selected <<- input$degree_checkbox
              school_unavailable()
              major_unavailable()
              occupation_unavailable()
              }
          } else {
            return()
          }
        }
      
        if(build_variables$current_page == 3){
            if(temp_choice$school_status == 2){
                temp_scenario$page4 <<- 1
                temp_choice$school_status <<- 3
            } else if(temp_choice$major_status == 2){
                temp_scenario$page4 <<- 2
                temp_choice$major_status <<- 3
            } else if(temp_choice$occupation_status == 2){
                temp_scenario$page4 <<- 3
                temp_choice$occupation_status <<- 3
            } else if(temp_choice$degree_status == 2){
                temp_scenario$page4 <<- 4
                temp_choice$degree_status <<- 3
            } else {
                return()
            }
        }
      if(build_variables$current_page == 4){
        if(temp_scenario$page4 == 1){
          major_unavailable()
          occupation_unavailable()
          degree_unavailable()
        } else if(temp_scenario$page4 == 2){
          school_unavailable()
          occupation_unavailable()
          degree_unavailable()
        } else if(temp_scenario$page4 == 3){
          school_unavailable()
          major_unavailable()
          degree_unavailable()
        } else if(temp_scenario$page4 == 4){
          if(!is_empty(input$degree_checkbox)){
            degree_list_selected <<- input$degree_checkbox
            school_unavailable()
            major_unavailable()
            occupation_unavailable()
          }
        } else {
          return()
        }
      }
        if(build_variables$current_page == 5){
            if(temp_choice$school_status == 2){
                temp_scenario$page6 <<- 1
                temp_choice$school_status <<- 3
            } else if(temp_choice$major_status == 2){
                temp_scenario$page6 <<- 2
                temp_choice$major_status <<- 3
            } else if(temp_choice$occupation_status == 2){
                temp_scenario$page6 <<- 3
                temp_choice$occupation_status <<- 3
            } else if(temp_choice$degree_status == 2){
                temp_scenario$page6 <<- 4
                temp_choice$degree_status <<- 3
            } else {
                return()
            }
        }
      if(build_variables$current_page == 6){
        if(temp_scenario$page6 == 1){
          major_unavailable()
          occupation_unavailable()
          degree_unavailable()
        } else if(temp_scenario$page6 == 2){
          school_unavailable()
          occupation_unavailable()
          degree_unavailable()
        } else if(temp_scenario$page6 == 3){
          school_unavailable()
          major_unavailable()
          degree_unavailable()
        } else if(temp_scenario$page6 == 4){
          if(!is_empty(input$degree_checkbox)){
            degree_list_selected <<- input$degree_checkbox
            school_unavailable()
            major_unavailable()
            occupation_unavailable()
          }
        } else {
          return()
        }
      }
        if(build_variables$current_page == 7){
            if(temp_choice$school_status == 2){
                temp_scenario$page8 <<- 1
                temp_choice$school_status <<- 3
            } else if(temp_choice$major_status == 2){
                temp_scenario$page8 <<- 2
                temp_choice$major_status <<- 3
            } else if(temp_choice$occupation_status == 2){
                temp_scenario$page8 <<- 3
                temp_choice$occupation_status <<- 3
            } else if(temp_choice$degree_status == 2){
                temp_scenario$page8 <<- 4
                temp_choice$degree_status <<- 3
            } else {
                return()
            }
        }
      if(build_variables$current_page == 8){
        if(temp_scenario$page8 == 1){
          major_unavailable()
          occupation_unavailable()
          degree_unavailable()
        } else if(temp_scenario$page8 == 2){
          school_unavailable()
          occupation_unavailable()
          degree_unavailable()
        } else if(temp_scenario$page8 == 3){
          school_unavailable()
          major_unavailable()
          degree_unavailable()
        } else if(temp_scenario$page8 == 4){
          if(!is_empty(input$degree_checkbox)){
            degree_list_selected <<- input$degree_checkbox
            school_unavailable()
            major_unavailable()
            occupation_unavailable()
          }
        } else {
          return()
        }
      }
      if(build_variables$current_page == 9) {
        add_scenario()
        create_scenario_table()
      }
        build_variables$current_page <<-  build_variables$current_page + 1
    })
    observeEvent(input$previous_button,{
        build_variables$current_page <<-  build_variables$current_page - 1
        if(build_variables$current_page == 7){
            if(temp_scenario$page8 == 1){
                temp_choice$school_status <<- 1
                temp_scenario$page8 <<- 0
            } else if(temp_scenario$page8 == 2){
                temp_choice$major_status <<- 1
                temp_scenario$page8 <<- 0
            } else if(temp_scenario$page8 == 3){
                temp_choice$occupation_status <<- 1
                temp_scenario$page8 <<- 0
            } else if(temp_scenario$page8 == 4){
                temp_choice$degree_status <<- 1
                temp_scenario$page8 <<- 0
            }
        }
        if(build_variables$current_page == 5){
            if(temp_scenario$page6 == 1){
                temp_choice$school_status <<- 1
                temp_scenario$page6 <<- 0
            } else if(temp_scenario$page6 == 2){
                temp_choice$major_status <<- 1
                temp_scenario$page6 <<- 0
            } else if(temp_scenario$page6 == 3){
                temp_choice$occupation_status <<- 1
                temp_scenario$page6 <<- 0
            } else if(temp_scenario$page6 == 4){
                temp_choice$degree_status <<- 1
                temp_scenario$page6 <<- 0
            }
        }
        if(build_variables$current_page == 3){
            if(temp_scenario$page4 == 1){
                temp_choice$school_status <<- 1
                temp_scenario$page4 <<- 0
            } else if(temp_scenario$page4 == 2){
                temp_choice$major_status <<- 1
                temp_scenario$page4 <<- 0
            } else if(temp_scenario$page4 == 3){
                temp_choice$occupation_status <<- 1
                temp_scenario$page4 <<- 0
            } else if(temp_scenario$page4 == 4){
                temp_choice$degree_status <<- 1
                temp_scenario$page4 <<- 0
            }
        }
        if(build_variables$current_page == 1){
            if(temp_scenario$page2 == 1){
                temp_choice$school_status <<- 1
                temp_scenario$page2 <<- 0
            } else if(temp_scenario$page2 == 2){
                temp_choice$major_status <<- 1
                temp_scenario$page2 <<- 0
            } else if(temp_scenario$page2 == 3){
                temp_choice$occupation_status <<- 1
                temp_scenario$page2 <<- 0
            } else if(temp_scenario$page2 == 4){
                temp_choice$degree_status <<- 1
                temp_scenario$page2 <<- 0
            }
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
              school_chip()
            }
        }
      }
    })
    school_chip <- function() {
      if(NROW(school_list_selected) > 0){
        temp_school1 <- school_list_selected[1]
        temp_school1 <- strtrim(temp_school1, 20)
        output$schoolchoice1 <- renderUI({
          div(class = "chip",id = "sc1",splitLayout(cellWidths = c("80%","20%"),p(temp_school1),
              actionButton(inputId = "school_del1", label = "x")),  style = "border-bottom:none;")
        })
      } else {output$schoolchoice1 <- renderUI({NULL}) }
      if(NROW(school_list_selected) > 1){
        temp_school2 <- school_list_selected[2]
         temp_school2 <- strtrim(temp_school2, 20)
        output$schoolchoice2 <- renderUI({
          div(class = "chip",id = "sc2",splitLayout(cellWidths = c("80%","20%"),p(temp_school2),
              actionButton(inputId = "school_del2", label = "x")), style = "border-bottom:none;")
        })
      } else {output$schoolchoice2 <- renderUI({NULL}) }
      if(NROW(school_list_selected) > 2){
        temp_school3 <- school_list_selected[3]
        temp_school3 <- strtrim(temp_school3, 20)
        output$schoolchoice3 <- renderUI({
          div(class = "chip",id = "sc3",splitLayout(cellWidths = c("80%","20%"),p(temp_school3),
              actionButton(inputId = "school_del3", label = "x")), style = "border-bottom:none;")
        })
      } else {output$schoolchoice3 <- renderUI({NULL}) }
      if(NROW(school_list_selected) > 3){
        temp_school4 <- school_list_selected[4]
        temp_school4 <- strtrim(temp_school4, 20)
        output$schoolchoice4 <- renderUI({
          div(class = "chip",id = "sc4",splitLayout(cellWidths = c("80%","20%"),p(temp_school4),
              actionButton(inputId = "school_del4", label = "x")), style = "border-bottom:none;")
        })
      } else {output$schoolchoice4 <- renderUI({NULL}) }
      if(NROW(school_list_selected) > 4){
        temp_school5 <- school_list_selected[5]
        temp_school5 <- strtrim(temp_school5, 20)
        output$schoolchoice5 <- renderUI({
          div(class = "chip",id = "sc5",splitLayout(cellWidths = c("80%","20%"),p(temp_school5),
              actionButton(inputId = "school_del5", label = "x")), style = "border-bottom:none;")
        })
      } else {output$schoolchoice5 <- renderUI({NULL}) }
    }
    observeEvent(input$school_del1, {

      school_list_selected <<- school_list_selected[-1,,drop = FALSE]
      if(NROW(school_list_selected) > 0){
        output$schoolchoice1 <- renderUI({NULL})
        school_chip()
      } else {
        output$schoolchoice1 <- renderUI({NULL})
        school_list_selected <<- vector(mode = "list")
      }
    })
    observeEvent(input$school_del2, {
      school_list_selected <<- school_list_selected[-2,,drop = FALSE]
        output$schoolchoice2 <- renderUI({NULL})
        school_chip()
    })
    observeEvent(input$school_del3, {
      school_list_selected <<- school_list_selected[-3,,drop = FALSE]
        output$schoolchoice3 <- renderUI({NULL})
        school_chip()
    })
    observeEvent(input$school_del4, {
      school_list_selected <<- school_list_selected[-4,,drop = FALSE]
        output$schoolchoice4 <- renderUI({NULL})
        school_chip()
    })
    observeEvent(input$school_del5, {
      school_list_selected <<- school_list_selected[-5,,drop = FALSE]
        output$schoolchoice5 <- renderUI({NULL})
        school_chip()
    })
    
    #08 Major info ----
    observeEvent(input$major_add, {
      if(input$major_next != "NA"){
        if(NROW(major_list_selected) < 5) {
          if(input$major_next %in% major_list_selected){
            return()
          } else {
            major_list_selected <<- rbind(major_list_selected, input$major_next)
            major_chip()
          }
        }
      }
    })    
    major_chip <- function() {
      if(NROW(major_list_selected) > 0){
        temp_major1 <- major_list_selected[1]
        temp_major1 <- strtrim(temp_major1, 20)
        output$majorchoice1 <- renderUI({
          div(class = "chip",id = "mc1",splitLayout(cellWidths = c("80%","20%"),p(temp_major1),
                                                    actionButton(inputId = "major_del1", label = "x")),  style = "border-bottom:none;")
        })
      } else {output$majorchoice1 <- renderUI({NULL }) }
      if(NROW(major_list_selected) > 1){
        temp_major2 <- major_list_selected[2]
        temp_major2 <- strtrim(temp_major2, 20)
        output$majorchoice2 <- renderUI({
          div(class = "chip",id = "mc2",splitLayout(cellWidths = c("80%","20%"),p(temp_major2),
                                                    actionButton(inputId = "major_del2", label = "x")), style = "border-bottom:none;")
        })
      } else {output$majorchoice2 <- renderUI({NULL}) }
      if(NROW(major_list_selected) > 2){
        temp_major3 <- major_list_selected[3]
        temp_major3 <- strtrim(temp_major3, 20)
        output$majorchoice3 <- renderUI({
          div(class = "chip",id = "mc3",splitLayout(cellWidths = c("80%","20%"),p(temp_major3),
                                                    actionButton(inputId = "major_del3", label = "x")), style = "border-bottom:none;")
        })
      } else {output$majorchoice3 <- renderUI({NULL}) }
      if(NROW(major_list_selected) > 3){
        temp_major4 <- major_list_selected[4]
        temp_major4 <- strtrim(temp_major4, 20)
        output$majorchoice4 <- renderUI({
          div(class = "chip",id = "mc4",splitLayout(cellWidths = c("80%","20%"),p(temp_major4),
                                                    actionButton(inputId = "major_del4", label = "x")), style = "border-bottom:none;")
        })
      } else {output$majorchoice4 <- renderUI({NULL}) }
      if(NROW(major_list_selected) > 4){
        temp_major5 <- major_list_selected[5]
        temp_major5 <- strtrim(temp_major5, 20)
        output$majorchoice5 <- renderUI({
          div(class = "chip",id = "mc5",splitLayout(cellWidths = c("80%","20%"),p(temp_major5),
                                                    actionButton(inputId = "major_del5", label = "x")), style = "border-bottom:none;")
        })
      } else {output$majorchoice5 <- renderUI({NULL}) }
    }
    observeEvent(input$major_del1, {
      major_list_selected <<- major_list_selected[-1,,drop = FALSE]
      if(NROW(major_list_selected) > 0){
        output$majorchoice1 <- renderUI({NULL})
        major_chip()
      } else {
        output$majorchoice1 <- renderUI({NULL})
        major_list_selected <<- vector(mode = "list")
      }
    })
    observeEvent(input$major_del2, {
      major_list_selected <<- major_list_selected[-2,,drop = FALSE]
        output$majorchoice2 <- renderUI({NULL})
        major_chip()
    })
    observeEvent(input$major_del3, {
      major_list_selected <<- major_list_selected[-3,,drop = FALSE]
        output$majorchoice3 <- renderUI({NULL})
        major_chip()
    })
    observeEvent(input$major_del4, {
      major_list_selected <<- major_list_selected[-4,,drop = FALSE]
        output$majorchoice4 <- renderUI({NULL})
        major_chip()
    })
    observeEvent(input$major_del5, {
      major_list_selected <<- major_list_selected[-5,,drop = FALSE]
      output$majorchoice5 <- renderUI({NULL})
      major_chip()
    })
 # 09 Occupation info ----
    observeEvent(input$occupation_add, {
      if(input$occupation_next != "NA"){
        if(NROW(occupation_list_selected) < 5) {
          if(input$occupation_next %in% occupation_list_selected){
            return()
          } else {
            occupation_list_selected <<- rbind(occupation_list_selected, input$occupation_next)
            occupation_chip()
          }
        }
      }
    })
    occupation_chip <- function() {
      if(NROW(occupation_list_selected) > 0){
        temp_occupation1 <- occupation_list_selected[1]
        temp_occupation1 <- strtrim(temp_occupation1, 20)
        output$occupationchoice1 <- renderUI({
          div(class = "chip",id = "oc1",splitLayout(cellWidths = c("80%","20%"),p(temp_occupation1),
                                                    actionButton(inputId = "occupation_del1", label = "x")),  style = "border-bottom:none;")
        })
      } else {output$occupationchoice1 <- renderUI({NULL }) }
      if(NROW(occupation_list_selected) > 1){
        temp_occupation2 <- occupation_list_selected[2]
        temp_occupation2 <- strtrim(temp_occupation2, 20)
        output$occupationchoice2 <- renderUI({
          div(class = "chip",id = "oc2",splitLayout(cellWidths = c("80%","20%"),p(temp_occupation2),
                                                    actionButton(inputId = "occupation_del2", label = "x")), style = "border-bottom:none;")
        })
      } else {output$occupationchoice2 <- renderUI({NULL}) }
      if(NROW(occupation_list_selected) > 2){
        temp_occupation3 <- occupation_list_selected[3]
        temp_occupation3 <- strtrim(temp_occupation3, 20)
        output$occupationchoice3 <- renderUI({
          div(class = "chip",id = "oc3",splitLayout(cellWidths = c("80%","20%"),p(temp_occupation3),
                                                    actionButton(inputId = "occupation_del3", label = "x")), style = "border-bottom:none;")
        })
      } else {output$occupationchoice3 <- renderUI({NULL}) }
      if(NROW(occupation_list_selected) > 3){
        temp_occupation4 <- occupation_list_selected[4]
        temp_occupation4 <- strtrim(temp_occupation4, 20)
        output$occupationchoice4 <- renderUI({
          div(class = "chip",id = "oc4",splitLayout(cellWidths = c("80%","20%"),p(temp_occupation4),
                                                    actionButton(inputId = "occupation_del4", label = "x")), style = "border-bottom:none;")
        })
      } else {output$occupationchoice4 <- renderUI({NULL}) }
      if(NROW(occupation_list_selected) > 4){
        temp_occupation5 <- occupation_list_selected[5]
        temp_occupation5 <- strtrim(temp_occupation5, 20)
        output$occupationchoice5 <- renderUI({
          div(class = "chip",id = "oc5",splitLayout(cellWidths = c("80%","20%"),p(temp_occupation5),
                                                    actionButton(inputId = "occupation_del5", label = "x")), style = "border-bottom:none;")
        })
      } else {output$occupationchoice5 <- renderUI({NULL}) }
    }
    observeEvent(input$occupation_del1, {
      occupation_list_selected <<- occupation_list_selected[-1,,drop = FALSE]
      if(NROW(occupation_list_selected) > 0){
        output$occupationchoice1 <- renderUI({NULL})
        occupation_chip()
      } else {
        output$occupationchoice1 <- renderUI({NULL})
        occupation_list_selected <<- c(character())
      }
    })
    observeEvent(input$occupation_del2, {
      occupation_list_selected <<- occupation_list_selected[-2,,drop = FALSE]
      output$occupationchoice2 <- renderUI({NULL})
      occupation_chip()
    })
    observeEvent(input$occupation_del3, {
      occupation_list_selected <<- occupation_list_selected[-3,,drop = FALSE]
      output$occupationchoice3 <- renderUI({NULL})
      occupation_chip()
    })
    observeEvent(input$occupation_del4, {
      occupation_list_selected <<- occupation_list_selected[-4,,drop = FALSE]
      output$occupationchoice4 <- renderUI({NULL})
      occupation_chip()
    })
    observeEvent(input$occupation_del5, {
      occupation_list_selected <<- occupation_list_selected[-5,,drop = FALSE]
      output$occupationchoice5 <- renderUI({NULL})
      occupation_chip()
    })
    school_unavailable <- function(){
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
      updateCheckboxGroupInput(session, inputId = "degree_checkbox", choices = degree_temp2)
      
    }
    add_scenario <- function(){
      if(NROW(user_scen01) == 0){
        scen_temp <- "Scenario 1"
      } else {
        scen_temp2 <- user_scen01 %>% distinct(user_scen01$scenario, .keep_all = TRUE)
        scen_temp2 <- scen_temp2[grep("Scenario", scen_temp2$scenario),]
        scen_temp <- paste0("Scenario ",NROW(scen_temp2) + 1)
      }
      scenario_temp <- backbone
      if(!is_empty(school_list_selected)) {
        school_temp <- filter(school_filter, INSTNM %in% school_list_selected) %>% select(UNITID)
        scenario_temp <- filter(scenario_temp, UNITID %in% school_temp$UNITID)
      }
      if(!is_empty(occupation_list_selected)) {
        occupation_temp <- filter(occupation_filter, OCCNAME %in% occupation_list_selected) %>% select(OCCCODE)
        scenario_temp <- filter(scenario_temp, OCCCODE %in% occupation_temp$OCCCODE)
      }
      if(!is_empty(major_list_selected)) {
        major_temp <- filter(major_filter, CIPNAME %in% major_list_selected) %>% select(CIPCODE)
        scenario_temp<- filter(scenario_temp, CIPCODE %in% major_temp$CIPCODE)
      }
      if(!is_empty(degree_list_selected)) {
        degree_temp <- filter(degree_filter, LEVELName %in% degree_list_selected) %>% select(AWLEVEL)
        scenario_temp <- filter(scenario_temp, AWLEVEL %in% degree_temp$AWLEVEL)
      }
      scenario_temp <- scenario_temp %>% select(ID) %>% mutate(scenario = scen_temp)
      user_scen01 <<- rbind(user_scen01, scenario_temp)
      
      scen_temp2 <- user_scen01 %>% distinct(user_scen01$scenario, .keep_all = TRUE)
      scen_temp2 <- scen_temp2[grep("Scenario", scen_temp2$scenario),]
      output$scenario_available <- renderUI({
        div(id = "scenario_radio",
            awesomeRadio(inputId = "scen_radio", label = "", choices = scen_temp2$scenario) )
      })
    }
    create_scenario_table <- function(){
      scenario_temp <- user_scen01 %>% select(ID)
      scenario_temp <- left_join(scenario_temp, backbone, by = "ID")
      scenario_temp <- left_join(scenario_temp, school_scenario, by = "UNITID")
      scenario_temp <- left_join(scenario_temp, major_scenario, by = "CIPCODE")
      scenario_temp <- left_join(scenario_temp, degree_scenario, by = "AWLEVEL")
      scenario_temp <- left_join(scenario_temp, occupation_scenario, by = "OCCCODE")
      scenario_temp$INSTNM <- strtrim(scenario_temp$INSTNM, 23)
      scenario_temp$CIPNAME <- strtrim(scenario_temp$CIPNAME, 23)
      scenario_temp$OCCNAME <- strtrim(scenario_temp$OCCNAME, 23)
      scenario_temp$LEVELName <- strtrim(scenario_temp$LEVELName, 23)
      
      
      scenario_temp <- scenario_temp %>% rename("School" = "INSTNM", "State" = "STABBR", "Cost" = "TotCstOutHi",
                                                "Major" = "CIPNAME", "Degree" = "LEVELName", 
                                                "Occupation" = "OCCNAME", "Salary" = "X17p"
                                                )
      
      output$scenario_table <- renderDataTable({
#        scenario_temp

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
            columnDefs = (list(list(visible=FALSE, targets=c(0,1,2,3,4,5,8,15,16,17)),
                               list(width = '255px', targets =c(6,10,12,13)),
                               list(width = '25px', targets =c(7,11)),
                               list(width = '55px', targets = c(9,14)),
                               list(className = 'dt-center', targets = c(7,11)))),
            pageLength = 12
          ),
          selection = list(mode = 'multiple')
        ) %>%
#          formatCurrency(c(9,14)) %>%
          formatStyle(
            0,
            target = 'row',
            color = 'black',
            #backgroundColor = 'grey',
            fontWeight = 'normal',
            fontSize = '16px',
            lineHeight = '150%',
            margin = '0px'
          )
        })
    }
    observeEvent(input$return_dashboard_button, {
      updateTabItems(session, "tabs", selected = "dashboard")
    })
    observeEvent(input$build_new_button, {
      build_variables$current_page <<- 1
      temp_choice$school_status <<- 1
      temp_choice$major_status <<- 1
      temp_choice$occupation_status <<- 1
      temp_choice$degree_status <<- 1
      temp_scenario$page2 <<- 0
      temp_scenario$page4 <<- 0
      temp_scenario$page6 <<- 0
      temp_scenario$page8 <<- 0
      clear_occupationchoices()
      clear_majorchoices()
      clear_schoolchoices()
      updatePickerInput(session, inputId = "school_next", choices = school_list$INSTNM)
      updatePickerInput(session, inputId = "major_next", choices = occupation_list$OCCNAME)
      updatePickerInput(session, inputId = "occupation_next", choices = occupation_list$OCCNAME)
      updateCheckboxGroupInput(session, inputId = "degree_checkbox", choices = degree_list, selected = NULL)
      school_list_selected <<- vector(mode = "list")
      degree_list_selected <<- vector(mode = "list")
      occupation_list_selected <<- vector(mode = "list")
      major_list_selected <<- vector(mode = "list")
    })
}

shinyApp(ui, server) 