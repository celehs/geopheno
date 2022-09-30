#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  demographic <- readRDS(system.file("data-raw/demographic.rds", package = "geopheno", mustWork = TRUE))
  geographic <- readRDS(system.file("data-raw/geographic.rds", package = "geopheno", mustWork = TRUE))

  tmp <- geographic$county_state_visn %>%
    dplyr::distinct(VISN, VISNNAME) %>%
    dplyr::arrange(VISN)

  visn_name <- tmp$VISNNAME
  names(visn_name) <- tmp$VISN

  tmp2 <- geographic$county_state_visn %>%
    dplyr::mutate(STATEFP = stringr::str_sub(FIPS, 1, 2)) %>%
    dplyr::distinct(STATEFP, ST) %>%
    dplyr::arrange(STATEFP)

  state_name <- tmp2$ST
  names(state_name) <- tmp2$STATEFP

  spdfs_state_county <- list(
    geographic$tl_2020_us_state,
    geographic$tl_2020_us_county
  )

  spdfs_visn <- list(
    geographic$FY2017_Q4_VISN,
    geographic$FY2017_Q4_VISN
  )

  output$text <- shiny::renderText({
    glue::glue("Pheno {input$pheno}, Year {input$year}")
  })

  output$text2 <- shiny::renderText({
    glue::glue("Pheno {input$pheno}, Year {input$year}")
  })

  # create leafdown object
  my_leafdown <- leafdown::Leafdown$new(
    spdfs_list = spdfs_state_county,
    map_output_id = "leafdown",
    input = input,
    join_map_levels_by = c("STATEFP" = "STATEFP")
  )

  leafdown_visn <- leafdown::Leafdown$new(
    spdfs_list = spdfs_visn,
    map_output_id = "leafdown_visn",
    input = input,
    join_map_levels_by = c("VISN" = "VISN")
  )

  # object for storing reactive values
  rv <- shiny::reactiveValues()
  rv$update_leafdown <- 0

  # observer for the drilling down
  shiny::observeEvent(
    eventExpr = input$drill_down,
    handlerExpr = {
      my_leafdown$drill_down()
      rv$update_leafdown <- rv$update_leafdown + 1
    }
  )

  # observer for the drilling up
  shiny::observeEvent(
    eventExpr = input$drill_up,
    handlerExpr = {
      my_leafdown$drill_up()
      rv$update_leafdown <- rv$update_leafdown + 1
    }
  )

  data <- shiny::reactive({
    shiny::req(rv$update_leafdown)
    # meta_data  <- my_leafdown$curr_data

    if (my_leafdown$curr_map_level == 1) {
      data_state_subset <- demographic$state %>%
        dplyr::filter(
          PHENO == input$pheno,
          YEAR == as.numeric(input$year)
        )
      data <- dplyr::left_join(
        x = my_leafdown$curr_data %>% dplyr::select(REGION:INTPTLON),
        y = data_state_subset,
        by = c("STUSPS" = "ST")
      ) %>%
        dplyr::mutate(LABEL = NAME)
    } else {
      data_county_subset <- demographic$county %>%
        dplyr::filter(
          PHENO == input$pheno,
          YEAR == as.numeric(input$year)
        )
      data <- dplyr::left_join(
        x = my_leafdown$curr_data %>% dplyr::select(STATEFP:INTPTLON),
        y = data_county_subset,
        by = c("GEOID" = "FIPS")
      ) %>%
        dplyr::mutate(LABEL = paste0(NAME, ", ", state_name[STATEFP]))
    }
    my_leafdown$add_data(data)
    data
  })

  data_visn <- shiny::reactive({
    data <- dplyr::left_join(
      x = leafdown_visn$curr_data %>%
        dplyr::select(VISN:Shape_Area),
      y = demographic$visn %>%
        dplyr::filter(PHENO == input$pheno, YEAR == as.numeric(input$year)),
      by = "VISN"
    )
    leafdown_visn$add_data(data)
    data
  })

  output$leafdown <- leaflet::renderLeaflet({
    shiny::req(spdfs_state_county)
    shiny::req(data)
    # browser()
    data <- data()
    data$y <- data$PREV
    fillcolor <- leaflet::colorNumeric(palette = "Blues", domain = data$y)

    labels <- sprintf(
      "<strong>%s</strong><br/>
      Cases: %s<br/>
      Total: %s<br/>
      Prevalance: %s<br/>
      </sup>",
      data$LABEL,
      data$CASES,
      data$TOT_POP,
      paste0(formatC(data$PREV, digits = 1, format = "f"), "%")
    ) %>% lapply(htmltools::HTML)

    my_leafdown$draw_leafdown(
      fillColor = ~ fillcolor(data$y),
      weight = 2,
      fillOpacity = 0.8,
      color = "grey",
      label = labels,
      highlight = leaflet::highlightOptions(weight = 5, color = "#666", fillOpacity = 0.7)
    ) %>%
      # set the view to be center on the USA
      leaflet::setView(-95, 39, 4) %>%
      my_leafdown$keep_zoom(input) %>%
      leaflet::addLegend(
        position = "topright",
        pal = fillcolor,
        values = ~ data$PREV,
        title = "Prevalence", # glue::glue("Pheno {input$pheno}, Year {input$year}"), # "Prevalence",
        labFormat = leaflet::labelFormat(suffix = "%"),
        opacity = 1
      ) %>%
      leaflet::addProviderTiles(provider = "CartoDB.Positron")
  })

  output$leafdown_visn <- leaflet::renderLeaflet({
    # shiny::req(spdfs_state_county)
    # shiny::req(data)
    # browser()
    data <- data_visn()
    data$y <- data$PREV
    fillcolor <- leaflet::colorNumeric(palette = "Blues", domain = data$y)
    labels <- sprintf(
      "<strong>VISN %s</strong>: %s<br/>
      Cases: %s<br/>
      Total: %s<br/>
      Prevalance: %s<br/>
      </sup>",
      data$VISN,
      visn_name[data$VISN],
      data$CASES,
      data$TOT_POP,
      paste0(formatC(data$PREV, digits = 1, format = "f"), "%")
    ) %>% lapply(htmltools::HTML)
    leafdown_visn$draw_leafdown(
      fillColor = ~ fillcolor(data$y),
      weight = 2,
      fillOpacity = 0.8,
      color = "grey",
      label = labels,
      highlight = leaflet::highlightOptions(weight = 5, color = "#666", fillOpacity = 0.7)
    ) %>%
      # set the view to be center on the USA
      leaflet::setView(-95, 39, 4) %>%
      leafdown_visn$keep_zoom(input) %>%
      leaflet::addLegend(
        position = "topright",
        pal = fillcolor,
        values = ~ data$PREV,
        title = "Prevalence", # glue::glue("Phenotype {input$pheno}, Year {input$year}"),
        labFormat = leaflet::labelFormat(suffix = "%"),
        opacity = 1
      ) %>%
      leaflet::addProviderTiles(provider = "CartoDB.Positron")
  })

  output$bar1 <- echarts4r::renderEcharts4r({
    df <- my_leafdown$curr_sel_data()
    shiny::validate(
      shiny::need(nrow(df) > 0, "Select regions on the map to add their values to this graph.")
    )
    # df$name <- ifelse(is.na(df$NAME_2), as.character(df$ST), as.character(df$NAME_2))
    df %>%
      dplyr::rename(
        `0 - 14 years` = AGEGRP_1,
        `15 - 24 years` = AGEGRP_2,
        `25 - 54 years` = AGEGRP_3,
        `55 - 64 years` = AGEGRP_4,
        `65 years and over` = AGEGRP_5
      ) %>%
      tidyr::pivot_longer(cols = `0 - 14 years`:`65 years and over`, names_to = "age_grp") %>%
      dplyr::group_by(LABEL) %>%
      dplyr::mutate(percent = value / 100) %>%
      echarts4r::e_charts(age_grp) %>%
      echarts4r::e_bar(percent) %>%
      # echarts4r::e_flip_coords() %>%
      echarts4r::e_x_axis(axisLabel = list(interval = 0, rotate = 15)) %>%
      echarts4r::e_y_axis(
        formatter = echarts4r::e_axis_formatter("percent")
      ) %>%
      echarts4r::e_axis_labels(x = "Age", y = glue::glue("Pheno {input$pheno}, Year {input$year}")) %>%
      echarts4r::e_tooltip(formatter = echarts4r::e_tooltip_item_formatter("percent"))
    # echarts4r::e_title(subtext = "Prevalence by Age Group")
  })

  output$bar2 <- echarts4r::renderEcharts4r({
    df <- my_leafdown$curr_sel_data()
    shiny::validate(
      shiny::need(nrow(df) > 0, "Select regions on the map to add their values to this graph.")
    )
    # df$name <- ifelse(is.na(df$NAME_2), as.character(df$ST), as.character(df$NAME_2))
    df %>%
      dplyr::rename(Male = SEX_1, Female = SEX_2) %>%
      tidyr::pivot_longer(cols = Male:Female, names_to = "sex") %>%
      dplyr::group_by(LABEL) %>%
      dplyr::mutate(percent = value / 100) %>%
      echarts4r::e_charts(sex) %>%
      echarts4r::e_bar(percent) %>%
      # echarts4r::e_flip_coords() %>%
      echarts4r::e_y_axis(
        formatter = echarts4r::e_axis_formatter("percent")
      ) %>%
      echarts4r::e_axis_labels(x = "Sex", y = glue::glue("Pheno {input$pheno}, Year {input$year}")) %>%
      # echarts4r::e_legend(show = FALSE) %>%
      echarts4r::e_tooltip(formatter = echarts4r::e_tooltip_item_formatter("percent"))
  })

  output$bar3 <- echarts4r::renderEcharts4r({
    df <- my_leafdown$curr_sel_data()
    shiny::validate(
      shiny::need(nrow(df) > 0, "Select regions on the map to add their values to this graph.")
    )
    # df$name <- ifelse(is.na(df$NAME_2), as.character(df$ST), as.character(df$NAME_2))
    df %>%
      dplyr::rename(
        `White` = RACE_1,
        `Black or African American` = RACE_2,
        `American Indian and Alaska Native` = RACE_3,
        `Asian` = RACE_4,
        `Native Hawaiian and Other Pacific Islander` = RACE_5,
        `Other races` = RACE_6
      ) %>%
      tidyr::pivot_longer(cols = `White`:`Other races`, names_to = "race") %>%
      dplyr::group_by(LABEL) %>%
      dplyr::mutate(percent = value / 100) %>%
      echarts4r::e_charts(race) %>%
      echarts4r::e_bar(percent) %>%
      # echarts4r::e_flip_coords() %>%
      echarts4r::e_x_axis(axisLabel = list(interval = 0, rotate = 10)) %>%
      echarts4r::e_y_axis(
        formatter = echarts4r::e_axis_formatter("percent")
      ) %>%
      echarts4r::e_axis_labels(x = "Race", y = glue::glue("Pheno {input$pheno}, Year {input$year}")) %>%
      # echarts4r::e_legend(show = FALSE) %>%
      echarts4r::e_tooltip(formatter = echarts4r::e_tooltip_item_formatter("percent"))
  })

  output$visn_bar1 <- echarts4r::renderEcharts4r({
    df <- leafdown_visn$curr_sel_data()
    shiny::validate(
      shiny::need(nrow(df) > 0, "Select regions on the map to add their values to this graph.")
    )
    # df$name <- ifelse(is.na(df$NAME_2), as.character(df$ST), as.character(df$NAME_2))
    df %>%
      dplyr::rename(
        `0 - 14 years` = AGEGRP_1,
        `15 - 24 years` = AGEGRP_2,
        `25 - 54 years` = AGEGRP_3,
        `55 - 64 years` = AGEGRP_4,
        `65 years and over` = AGEGRP_5
      ) %>%
      tidyr::pivot_longer(cols = `0 - 14 years`:`65 years and over`, names_to = "age_grp") %>%
      dplyr::group_by(VISN) %>%
      dplyr::mutate(percent = value / 100) %>%
      echarts4r::e_charts(age_grp) %>%
      echarts4r::e_bar(percent) %>%
      # echarts4r::e_flip_coords() %>%
      echarts4r::e_x_axis(axisLabel = list(interval = 0, rotate = 15)) %>%
      echarts4r::e_y_axis(
        formatter = echarts4r::e_axis_formatter("percent")
      ) %>%
      echarts4r::e_axis_labels(x = "Age", y = glue::glue("Pheno {input$pheno}, Year {input$year}")) %>%
      echarts4r::e_tooltip(formatter = echarts4r::e_tooltip_item_formatter("percent"))
    # echarts4r::e_title(subtext = "Prevalence by Age Group")
  })


  output$visn_bar2 <- echarts4r::renderEcharts4r({
    df <- leafdown_visn$curr_sel_data()
    shiny::validate(
      shiny::need(nrow(df) > 0, "Select regions on the map to add their values to this graph.")
    )
    # df$name <- ifelse(is.na(df$NAME_2), as.character(df$ST), as.character(df$NAME_2))
    df %>%
      dplyr::rename(Male = SEX_1, Female = SEX_2) %>%
      tidyr::pivot_longer(cols = Male:Female, names_to = "sex") %>%
      dplyr::group_by(VISN) %>%
      dplyr::mutate(percent = value / 100) %>%
      echarts4r::e_charts(sex) %>%
      echarts4r::e_bar(percent) %>%
      # echarts4r::e_flip_coords() %>%
      echarts4r::e_y_axis(
        formatter = echarts4r::e_axis_formatter("percent")
      ) %>%
      echarts4r::e_axis_labels(x = "Sex", y = glue::glue("Pheno {input$pheno}, Year {input$year}")) %>%
      # echarts4r::e_legend(show = FALSE) %>%
      echarts4r::e_tooltip(formatter = echarts4r::e_tooltip_item_formatter("percent"))
  })

  output$visn_bar3 <- echarts4r::renderEcharts4r({
    df <- leafdown_visn$curr_sel_data()
    shiny::validate(
      shiny::need(nrow(df) > 0, "Select regions on the map to add their values to this graph.")
    )
    # df$name <- ifelse(is.na(df$NAME_2), as.character(df$ST), as.character(df$NAME_2))
    df %>%
      dplyr::rename(
        `White` = RACE_1,
        `Black or African American` = RACE_2,
        `American Indian and Alaska Native` = RACE_3,
        `Asian` = RACE_4,
        `Native Hawaiian and Other Pacific Islander` = RACE_5,
        `Other races` = RACE_6
      ) %>%
      tidyr::pivot_longer(cols = `White`:`Other races`, names_to = "race") %>%
      dplyr::group_by(VISN) %>%
      dplyr::mutate(percent = value / 100) %>%
      echarts4r::e_charts(race) %>%
      echarts4r::e_bar(percent) %>%
      # echarts4r::e_flip_coords() %>%
      echarts4r::e_x_axis(axisLabel = list(interval = 0, rotate = 10)) %>%
      echarts4r::e_y_axis(
        formatter = echarts4r::e_axis_formatter("percent")
      ) %>%
      echarts4r::e_axis_labels(x = "Race", y = glue::glue("Pheno {input$pheno}, Year {input$year}")) %>%
      # echarts4r::e_legend(show = FALSE) %>%
      echarts4r::e_tooltip(formatter = echarts4r::e_tooltip_item_formatter("percent"))
  })
}
