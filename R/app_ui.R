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
      skin = "light",
      bs4Dash::bs4DashNavbar(
        title = shiny::tagList(
          shiny::tags$img(
            src = "www/allomate.png",
            height = "30",
            style = "margin-right: 8px;"
          ),
          shiny::span("AlloMate")
        )
      ),
      bs4Dash::bs4DashSidebar(
        skin = "light",
        status = "info",
        fixed = TRUE,
        expandOnHover = TRUE,
        bs4Dash::sidebarMenu(
          id = "MainMenu",
          bs4Dash::menuItem("Home", tabName = "home", icon = shiny::icon("house")),
          bs4Dash::menuItem("AlloMate", tabName = "allomate", icon = shiny::icon("diagram-project")),
          bs4Dash::menuItem("Help", tabName = "help", icon = shiny::icon("circle-question"))
        )
      ),
      footer = bs4Dash::dashboardFooter(
        left = shiny::div(
          sprintf("v%s", as.character(utils::packageVersion("AlloMate")))
        ),
        right = shiny::div(
          style = "display:flex; align-items:center; gap: 12px;",
          shiny::span("Breeding Insight", style = "color: grey;"),
          shiny::tags$img(src = "www/logos.png", height = "30")
        )
      ),
      bs4Dash::dashboardBody(
        shinyjs::useShinyjs(),
        shinydisconnect::disconnectMessage(),
        bs4Dash::tabItems(
          bs4Dash::tabItem(tabName = "home", mod_home_ui("home_1")),
          bs4Dash::tabItem(tabName = "allomate", mod_allomate_ui("allomate_1")),
          bs4Dash::tabItem(tabName = "help", mod_help_ui("help_1"))
        )
      )
    )
  )
}

