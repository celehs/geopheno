#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    bs4Dash::dashboardPage(
      title = "GeoPheno",
      header = bs4Dash::dashboardHeader(title = "GeoPheno Demo"),
      # dashboard sidebar
      sidebar = bs4Dash::dashboardSidebar(
        bs4Dash::sidebarMenu(
          id = "sidebarMenu",
          shinyWidgets::pickerInput(
            inputId = "pheno",
            label = "Select Phenotype:",
            choices = LETTERS[1:5]
          ),
          shinyWidgets::pickerInput(
            inputId = "year",
            label = "Select Year:",
            choices = 1:3
          ),
          bs4Dash::menuItem(
            text = "View State/County Data",
            tabName = "tab_state_county"
          ),
          bs4Dash::menuItem(
            text = "View VISN Data",
            tabName = "tab_visn"
          )
        ),
        skin = "light",
        minified = FALSE
      ),
      # dashboard body
      body = bs4Dash::dashboardBody(
        bs4Dash::tabItems(
          # tab for state/county
          bs4Dash::tabItem(
            tabName = "tab_state_county",
            shiny::fluidRow(
              bs4Dash::bs4Card(
                title = "Prevalence by State/County",
                height = 800,
                collapsible = FALSE,
                shiny::textOutput("text"),
                shiny::tags$p("Select geographic regions to drill down"),
                shiny::actionButton(inputId = "drill_down", label = "Drill Down"),
                shiny::actionButton(inputId = "drill_up", label = "Drill Up"),
                leaflet::leafletOutput("leafdown", height = 600)
              ),
              bs4Dash::bs4Card(
                title = "Demographics by State/County",
                height = 800,
                collapsible = FALSE,
                echarts4r::echarts4rOutput("bar1", height = "250px"),
                echarts4r::echarts4rOutput("bar2", height = "250px"),
                echarts4r::echarts4rOutput("bar3", height = "250px")
              )
            )
          ),
          # tab for visn
          bs4Dash::tabItem(
            tabName = "tab_visn",
            shiny::fluidRow(
              bs4Dash::bs4Card(
                title = "Prevalence by VISN",
                height = 800,
                collapsible = FALSE,
                shiny::textOutput("text2"),
                leaflet::leafletOutput("leafdown_visn", height = 600)
              ),
              bs4Dash::bs4Card(
                title = "Demographics by VISN",
                height = 800,
                collapsible = FALSE,
                echarts4r::echarts4rOutput("visn_bar1", height = "250px"),
                echarts4r::echarts4rOutput("visn_bar2", height = "250px"),
                echarts4r::echarts4rOutput("visn_bar3", height = "250px")
              )
            )
          )
        )
      )
    )

  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "geopheno"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
