#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'   DO NOT REMOVE.
#'
#' @noRd
app_ui <- function(request) {
  shiny::tagList(
    golem_add_external_resources(),
    bs4Dash::bs4DashPage(
      title = "AlloMate",
      skin = "black",
      bs4Dash::bs4DashNavbar(
        title = shiny::tagList(
          shiny::tags$img(
            src = "www/allomate.png",
            height = "45",
            width = '45',
            style = "margin-right: 8px;"
          ),
          shiny::span("AlloMate")
        )
      ),
      help=NULL,
      bs4Dash::bs4DashSidebar(
        skin = "light",
        status = "danger",
        fixed = TRUE,
        expandOnHover = TRUE,
        bs4Dash::sidebarMenu(
          id = "MainMenu",
          flat = FALSE,
          tags$li(class = "header", style = "color: grey; margin-top: 10px; margin-bottom: 10px; padding-left: 15px;", "Menu"),
            bs4Dash::menuItem("Home", tabName = "home", icon = shiny::icon("house")),
          tags$li(class = "header", style = "color: grey; margin-top: 18px; margin-bottom: 10px; padding-left: 15px;", "Analysis"),
            bs4Dash::menuItem("AlloMate", tabName = "allomate", icon = shiny::icon("diagram-project")),
          tags$li(class = "header", style = "color: grey; margin-top: 18px; margin-bottom: 10px; padding-left: 15px;", "Information"),
            bs4Dash::menuItem("Help", tabName = "help", icon = shiny::icon("circle-question"))
        )
      ),
      footer = bs4Dash::dashboardFooter(
        right = div(
          style = "display: flex; align-items: center;",  # Align text and images horizontally
          div(
            style = "display: flex; flex-direction: column; margin-right: 15px; text-align: right;",
            div("2026 Breeding Insight"),
            div("Funded by USDA through UF|IFAS")
          ),
          div(
            a(
              img(src = "www/usda-logo-color.png", height = "45px"),
              style = "margin-right: 15px;"
            ),
            a(
              img(src = "www/cornell_seal_simple_web_b31b1b.png", height = "45px")
            )
          )
        ),
        left = div(
          style = "display: flex; align-items: center; height: 100%;",  
          sprintf("v%s", as.character(utils::packageVersion("AlloMate")))
        )
      ),
      bs4Dash::dashboardBody(
        shinyjs::useShinyjs(),
        shinydisconnect::disconnectMessage(), #Adds generic error message for any error if not already accounted for
        tags$style(
          HTML(
            ".main-footer {
            background-color: white;
            color: grey;
            height: 65px;
            padding-top: 5px;
            padding-bottom: 5px;
          }
          .main-footer a {
            color: grey;
          }"
          )
        ),
        bs4Dash::tabItems(
          bs4Dash::tabItem(tabName = "home", mod_home_ui("home_1")),
          bs4Dash::tabItem(tabName = "allomate", mod_allomate_ui("allomate_1")),
          bs4Dash::tabItem(tabName = "help", mod_help_ui("help_1"))
        )
      )
    )
  )
}

