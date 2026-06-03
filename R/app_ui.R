#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'   DO NOT REMOVE.
#'
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Dynamic sidebar color theme — only sets the :root CSS variables
    # ── Sidebar color theme ──────────────────────────────────────────────────────
    # Change this value to switch the active sidebar menu item color.
    # Available options: "azure", "green", "yellow", "grey", "purple", "red"
    # ─────────────────────────────────────────────────────────────────────────────
    tags$head(tags$style(HTML(sprintf(
      ":root { --sidebar-core: var(--%s-core); --sidebar-lite: var(--%s-lite); --sidebar-deep: var(--%s-deep); }",
      "red", "red", "red"
    )))),
    bs4Dash::bs4DashPage(
      title = "AlloMate",
      skin  = "black",
      bs4Dash::bs4DashNavbar(
        title = shiny::tagList(
          shiny::tags$img(
            src    = "www/allomate.png",
            height = "45",
            width  = "45",
            style  = "margin-right: 8px;"
          ),
          shiny::span("AlloMate")
        )
      ),
      help = NULL,
      bs4Dash::bs4DashSidebar(
        skin          = "light",
        status        = "danger",
        fixed         = TRUE,
        sidebarCollapsible = TRUE,
        sidebarCollapsed   = FALSE,
        expandOnHover      = FALSE,
        
        bs4Dash::sidebarMenu(
          id   = "MainMenu",
          flat = FALSE,
          tags$li(
            class = "header",
            style = "color: grey; margin-top: 10px; margin-bottom: 10px; padding-left: 15px;",
            "Menu"
          ),
          bs4Dash::menuItem("Home", tabName = "home", icon = shiny::icon("house")),
          tags$li(
            class = "header",
            style = "color: grey; margin-top: 18px; margin-bottom: 10px; padding-left: 15px;",
            "Data Wrangling"
          ),
          bs4Dash::menuItem("Pedigree Cleaning", tabName = "ped_cleaner", icon = shiny::icon("diagram-project")),
          tags$li(
            class = "header",
            style = "color: grey; margin-top: 18px; margin-bottom: 10px; padding-left: 15px;",
            "Analysis"
          ),
          bs4Dash::menuItem("Mate Allocation", tabName = "allomate", icon = shiny::icon("bullseye")),
          tags$li(
            class = "header",
            style = "color: grey; margin-top: 18px; margin-bottom: 10px; padding-left: 15px;",
            "Information"
          ),
          bs4Dash::menuItem(
            "Source Code",
            icon = shiny::icon("circle-info"),
            href = "https://github.com/Breeding-Insight/AlloMate",
            newTab = TRUE
          ),
          bs4Dash::menuItem("Help", tabName = "help", icon = shiny::icon("circle-question"))
        )
      ),
      footer = bs4Dash::dashboardFooter(
        left = div(
          style = "display: flex; align-items: center; height: 100%;",
          sprintf("v%s", as.character(utils::packageVersion("AlloMate")))
        ),
        right = div(
          style = "display: flex; align-items: center;",
          div(
            style = "display: flex; flex-direction: column; margin-right: 15px; text-align: right;",
            div("2026 Breeding Insight"),
            div("Funded by USDA through UF|IFAS")
          ),
          div(
            style = "margin-right: 15px;",
            a(
              img(src = "www/logos.png", height = "45px")
            )
          )
        )
      ),
      bs4Dash::dashboardBody(
        shinyjs::useShinyjs(),
        shinydisconnect::disconnectMessage(),
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
          bs4Dash::tabItem(tabName = "home", mod_Home_ui("Home_1")),
          bs4Dash::tabItem(tabName = "ped_cleaner", mod_ped_cleaner_ui("ped_cleaner_1")),
          bs4Dash::tabItem(tabName = "allomate",    mod_allomate_ui("allomate_1")),
          bs4Dash::tabItem(tabName = "help",        mod_help_ui("help_1"))
        )
      )
    )
  )
}