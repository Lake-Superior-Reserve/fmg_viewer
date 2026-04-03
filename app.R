library(dplyr)
library(stringr)
library(lubridate)
library(shiny)
library(bslib)
library(plotly)
library(leaflet)

# Load pre-processed datasets required for the app
water_data <- readRDS("app_data/water_data.rds")
site_locations <- readRDS("app_data/locations.rds")
bloom_data <- readRDS("app_data/blooms.rds")

# Separate bloom data into lake blooms and estuary blooms based on the site ID
lake_blooms <- filter(bloom_data, site != "lksba")
est_blooms <- filter(bloom_data, site == "lksba")

# Define the initial/limited set of variable choices available to the user
var_choices_limited <- list(
  "Discharge (ft³/s) - Nemadji River" = "discharge_nemadji",
  "Discharge (ft³/s) - Bois Brule River" = "discharge_brule",
  "Air Temperature (°C) - Offshore Buoy" = "airtemp_buoy",
  "Water Temperature (°C) - Offshore Buoy" = "watertemp",
  "Wind Speed (m/s) - Port Wing" = "windspeed_lake",
  "Wind Speed (m/s) - Pokegama Bay" = "windspeed_slre",
  "Precipitation (mm) - Pokegama Bay" = "precip_slre",
  "Water Temperature (°C)" = "temp",
  "Turbidity (NTU)" = "turb"
)

# Define the full set of variable choices (includes nutrients and chlorophyll-a)
var_choices <- c(
  var_choices_limited,
  "Total Phosphorus (mg/L)" = "tp",
  "Chlorophyll-a (μg/L)" = "chl"
)

# Helper function to convert a year input string into a start and end date range
get_year_range <- function(year_input) {
  if (year_input == "all") {
    return(c(as.Date("2015-01-01"), as.Date("2024-12-31")))
  }

  start_date <- ymd(str_c(year_input, "-01-01"))
  end_date <- ymd(str_c(year_input, "-12-31"))
  return(c(start_date, end_date))
}

# MODIFIED: Added a parameter to adjust line color
# Helper function to generate a vertical dashed line for a given date on a Plotly graph
make_line <- function(d, line_color = "forestgreen") {
  list(
    type = "line",
    yref = "paper",
    y0 = 0,
    y1 = 1,
    x0 = d,
    x1 = d,
    line = list(color = line_color, dash = "dash")
  )
}

# Define the user interface (UI)
ui <- page_fluid(
  title = "Floody, Muddy, and Green Data Viewer",
  tags$head(
    # Inject custom CSS to style the Leaflet map's close button
    tags$style(HTML(
      "
    .leaflet-close-btn {
      position: absolute;
      top: 10px;
      right: 10px;
      z-index: 1000;
      background-color: white;
      width: 30px;
      height: 30px;
      line-height: 30px;
      text-align: center;
      font-weight: bold;
      font-size: 20px;
      color: black;
      border: 2px solid rgba(0,0,0,0.2);
      border-radius: 4px;
      background-clip: padding-box;
      cursor: pointer;
      padding: 0;
      box-shadow: none;
    }
    .leaflet-close-btn:hover {
      background-color: #f4f4f4;
      color: black;
    }
  "
    ))
  ),
  # Main control header card
  card(
    card_header(
      style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
      div("Floody, Muddy, and Green Data Viewer"),
      div(
        style = "display: flex; align-items: center; gap: 15px;",
        # Input for unlocking different "phases" (extra variables/features)
        textInput(
          "phase",
          NULL,
          placeholder = "Enter Phase Code",
          width = "200px"
        ),
        uiOutput("bloomcheck"), # Placeholder for bloom toggles (shows in phase3)
        checkboxInput(
          inputId = "show_second_plot",
          label = "Show Second Plot",
          value = TRUE,
          width = "160px"
        ),
        checkboxInput(
          inputId = "lock_y_axis",
          label = "Lock Y-Axis",
          value = FALSE,
          width = "110px"
        ),
        actionButton(
          inputId = "show_map_button",
          label = "Show Site Map"
        )
      )
    )
  ),

  # Output container for the dynamically generated plots
  uiOutput("plots")
)

# Define the server logic
server <- function(input, output, session) {
  # Dynamically render UI layout based on whether one or two plots are requested
  output$plots <- renderUI({
    if (input$show_second_plot) {
      # Render side-by-side plots if show_second_plot is checked
      layout_column_wrap(
        width = 1 / 2,
        fillable = TRUE,
        # First Plot Card
        card(
          full_screen = TRUE,
          height = "80vh",
          card_header(
            div(
              div(
                style = "display: flex; gap: 10px; margin-bottom: 5px;",
                uiOutput("var_x_input"),
                uiOutput("var_input"),
              ),
              div(
                style = "display: flex; gap: 10px;",
                uiOutput("year_out"),
                uiOutput("site_out")
              )
            )
          ),
          plotlyOutput("timeseries_plot", height = "100%")
        ),
        # Second Plot Card
        card(
          full_screen = TRUE,
          height = "80vh",
          card_header(
            div(
              div(
                style = "display: flex; gap: 10px; margin-bottom: 5px;",
                uiOutput("var_x_input2"),
                uiOutput("var_input2")
              ),
              div(
                style = "display: flex; gap: 10px;",
                uiOutput("year_out_2"),
                uiOutput("site_out_2")
              )
            )
          ),
          plotlyOutput("timeseries_plot_2", height = "100%")
        )
      )
    } else {
      # Render a single full-width plot if show_second_plot is unchecked
      layout_column_wrap(
        width = 1,
        fillable = TRUE,
        card(
          full_screen = TRUE,
          min_height = "80vh",
          card_header(
            div(
              div(
                style = "display: flex; gap: 10px; margin-bottom: 5px;",
                uiOutput("var_x_input"),
                uiOutput("var_input")
              ),
              div(
                style = "display: flex; gap: 10px;",
                uiOutput("year_out"),
                uiOutput("site_out")
              )
            )
          ),
          plotlyOutput("timeseries_plot", height = "100%")
        )
      )
    }
  })

  # Reactive values to hold the state of the bloom toggles
  lake_bloom_state <- reactiveVal(FALSE)
  est_bloom_state <- reactiveVal(FALSE)

  # Render checkboxes for showing blooms only if the "phase" is unlocked to phase3
  output$bloomcheck <- renderUI({
    if (isTruthy(phase_state()) && phase_state() == "phase3") {
      div(
        style = "display: flex; gap: 15px;  white-space: nowrap; margin-right: 15px",
        checkboxInput(
          inputId = "show_lake_blooms",
          label = "Show Lake Blooms",
          value = FALSE,
          width = "160px"
        ),
        checkboxInput(
          inputId = "show_est_blooms",
          label = "Show Estuary Blooms",
          value = FALSE,
          width = "160px"
        )
      )
    }
  })

  # Update the reactive values whenever the bloom checkboxes are toggled
  observeEvent(
    input$show_lake_blooms,
    ({
      lake_bloom_state(input$show_lake_blooms)
    })
  )
  observeEvent(
    input$show_est_blooms,
    ({
      est_bloom_state(input$show_est_blooms)
    })
  )

  # Reactive value to track the current feature phase
  phase_state <- reactiveVal("phase1")

  # Update phase_state based on specific secret codes entered by the user
  observeEvent(
    input$phase,
    ({
      if (input$phase == "nutrients") {
        phase_state("phase2")
      }
      if (input$phase == "blooms") {
        phase_state("phase3")
      }
    })
  )

  # Reactive list to hold the variables available for plotting
  var_list <- reactiveVal(var_choices_limited)

  # Unlock full variable list if phase 2 or 3 is activated
  observeEvent(
    phase_state(),
    ({
      if (phase_state() %in% c("phase2", "phase3")) var_list(var_choices)
    })
  )

  # Render Y-axis variable selector for the first plot
  output$var_input <- renderUI({
    sel <- if (isTruthy(input$variable)) {
      sel <- input$variable
    } else {
      sel <- "discharge_nemadji"
    }

    # Prevent user from selecting the exact same variable on both X and Y
    selectInput(
      "variable",
      label = NULL,
      choices = var_list()[var_list() != req(input$variable_x)],
      selected = sel,
      width = "350px"
    )
  })

  # Render X-axis variable selector for the first plot
  output$var_x_input <- renderUI({
    sel <- if (isTruthy(input$variable_x)) {
      sel <- input$variable_x
    } else {
      sel <- "date"
    }

    selectInput(
      "variable_x",
      label = NULL,
      choices = c(list("Date" = "date"), var_list()),
      selected = sel,
      width = "350px"
    )
  })

  # Render Y-axis variable selector for the second plot
  output$var_input2 <- renderUI({
    sel <- if (isTruthy(var2_state())) {
      sel <- var2_state()
    } else {
      sel <- "watertemp"
    }
    selectInput(
      "variable_2",
      label = NULL,
      choices = var_list()[var_list() != req(input$variable_x2)],
      selected = sel,
      width = "350px"
    )
  })

  # Maintain the state of the second plot's Y-variable across UI re-renders
  var2_state <- reactiveVal(NULL)
  observeEvent(input$variable_2, {
    if (isTruthy(input$variable_2)) {
      var2_state(input$variable_2)
    }
  })

  # Render X-axis variable selector for the second plot
  output$var_x_input2 <- renderUI({
    sel <- if (isTruthy(varx2_state())) {
      sel <- varx2_state()
    } else {
      sel <- "date"
    }

    selectInput(
      "variable_x2",
      label = NULL,
      choices = c(list("Date" = "date"), var_list()),
      selected = sel,
      width = "350px"
    )
  })

  # Maintain the state of the second plot's X-variable across UI re-renders
  varx2_state <- reactiveVal(NULL)
  observeEvent(input$variable_x2, {
    if (isTruthy(input$variable_x2)) {
      varx2_state(input$variable_x2)
    }
  })

  # Render Site selector for the first plot (only if a site-specific variable is chosen)
  output$site_out <- renderUI({
    if (
      req(input$variable) %in%
        c("temp", "turb", "tp", "chl") |
        req(input$variable_x) %in% c("temp", "turb", "tp", "chl")
    ) {
      selectInput(
        "site",
        label = NULL,
        choices = c(
          "Barker's Island - SLRE" = "lksba",
          "Lake Superior - West" = "Site 1",
          "Lake Superior - Mid" = "Site 8",
          "Lake Superior - East" = "Mawikwe Bay"
        ),
        selected = site_state()
      )
    }
  })

  # Maintain site state for plot 1
  site_state <- reactiveVal(NULL)
  observeEvent(input$site, {
    if (isTruthy(input$site)) {
      site_state(input$site)
    }
  })

  # Render Site selector for the second plot
  output$site_out_2 <- renderUI({
    if (
      req(input$variable_2) %in%
        c("temp", "turb", "tp", "chl") |
        req(input$variable_x2) %in% c("temp", "turb", "tp", "chl")
    ) {
      selectInput(
        "site_2",
        label = NULL,
        choices = c(
          "Barker's Island - SLRE" = "lksba",
          "Lake Superior - West" = "Site 1",
          "Lake Superior - Mid" = "Site 8",
          "Lake Superior - East" = "Mawikwe Bay"
        ),
        selected = site2_state()
      )
    }
  })

  # Maintain site state for plot 2
  site2_state <- reactiveVal(NULL)
  observeEvent(input$site_2, {
    if (isTruthy(input$site_2)) {
      site2_state(input$site_2)
    }
  })

  # Render Year selector for the first plot, filtering choices to available data years
  output$year_out <- renderUI({
    if (req(input$variable_x) == "date") {
      filt_data <- filter(water_data, !is.na(water_data[[req(input$variable)]]))
    } else {
      filt_data <- filter(
        water_data,
        !is.na(water_data[[input$variable]]) |
          !is.na(water_data[[input$variable_x]])
      )
    }

    if (
      (input$variable %in%
        c("temp", "turb", "tp", "chl") |
        input$variable_x %in% c("temp", "turb", "tp", "chl")) &
        isTruthy(input$site)
    ) {
      filt_data <- filter(filt_data, site == input$site)
    }

    selectInput(
      "year",
      label = NULL,
      choices = c("All Years" = "all", sort(unique(filt_data$year))),
      selected = year_state(),
      width = "150px"
    )
  })

  # Maintain year state for plot 1
  year_state <- reactiveVal(NULL)
  observeEvent(input$year, {
    if (isTruthy(input$year)) {
      year_state(input$year)
    }
  })

  # Render Year selector for the second plot, filtering choices similarly
  output$year_out_2 <- renderUI({
    if (req(input$variable_x2) == "date") {
      filt_data <- filter(
        water_data,
        !is.na(water_data[[req(input$variable_2)]])
      )
    } else {
      filt_data <- filter(
        water_data,
        !is.na(water_data[[input$variable_2]]) |
          !is.na(water_data[[input$variable_x2]])
      )
    }

    if (
      (input$variable_2 %in%
        c("temp", "turb", "tp", "chl") |
        input$variable_x2 %in% c("temp", "turb", "tp", "chl")) &
        isTruthy(input$site_2)
    ) {
      filt_data <- filter(filt_data, site == input$site_2)
    }

    selectInput(
      "year_2",
      label = NULL,
      choices = c("All Years" = "all", sort(unique(filt_data$year))),
      selected = year2_state(),
      width = "150px"
    )
  })

  # Maintain year state for plot 2
  year2_state <- reactiveVal(NULL)
  observeEvent(input$year_2, {
    if (isTruthy(input$year_2)) {
      year2_state(input$year_2)
    }
  })

  # Factory function to build the plotly graphs based on specified UI inputs
  render_plot <- function(variable_input_id, x_variable_input_id) {
    renderPlotly({
      # force rerender when switching between 1 and 2 plots
      plot_width <- session$clientData$output_timeseries_plot_width

      # Determine which year input to use based on the current plot
      if (variable_input_id == "variable") {
        year_input = req(input$year)
      } else {
        year_input = req(input$year_2)
      }

      # Mapping for user-friendly site titles
      site_names <- c(
        "lksba" = "Barker's Island - SLRE",
        "Site 1" = "Lake Superior - West",
        "Site 8" = "Lake Superior - Mid",
        "Mawikwe Bay" = "Lake Superior - East"
      )

      # Get variable label for y-axis mapping
      var_labels <- list(
        "discharge_nemadji" = "Discharge (ft³/s) - Nemadji River",
        "discharge_brule" = "Discharge (ft³/s) - Bois Brule River",
        "airtemp_buoy" = "Air Temperature (°C) - Offshore Buoy",
        "watertemp" = "Water Temperature (°C) - Offshore Buoy",
        "windspeed_lake" = "Wind Speed (m/s) - Port Wing",
        "windspeed_slre" = "Wind Speed (m/s) - Pokegama Bay",
        "precip_slre" = "Precipitation (mm) - Pokegama Bay",
        "temp" = "Water Temperature (°C)",
        "turb" = "Turbidity (NTU)",
        "tp" = "Total Phosphorus (mg/L)",
        "chl" = "Chlorophyll-a (μg/L)"
      )

      # Extract current Y and X variable selections
      current_variable <- input[[variable_input_id]]
      y_label <- var_labels[[current_variable]]

      current_variable_x <- input[[x_variable_input_id]]
      x_label <- var_labels[[current_variable_x]]

      # Default axis layouts
      y_axis_layout <- list(
        title = y_label,
        showgrid = TRUE,
        gridcolor = "lightgray"
      )

      x_axis_layout <- list(
        title = x_label,
        showgrid = TRUE,
        gridcolor = "lightgray"
      )

      # If lock_y_axis is true, calculate full dataset range with a 5% buffer to keep axis static
      if (input$lock_y_axis) {
        var_range <- range(water_data[[current_variable]], na.rm = TRUE)
        buffer <- (var_range[2] - var_range[1]) * 0.05
        y_axis_layout$range <- c(var_range[1] - buffer, var_range[2] + buffer)

        var_range_x <- range(water_data[[current_variable_x]], na.rm = TRUE)
        buffer_x <- (var_range_x[2] - var_range_x[1]) * 0.05
        x_axis_layout$range <- c(
          var_range_x[1] - buffer_x,
          var_range_x[2] + buffer_x
        )
      }

      # Handle Time Series Plot (X-axis is date)
      if (req(input[[x_variable_input_id]]) == "date") {
        # Check if the variable is general (not site-specific)
        if (
          req(input[[variable_input_id]]) %in%
            c(
              "discharge_nemadji",
              "discharge_brule",
              "watertemp",
              "airtemp_buoy",
              "windspeed_lake",
              "windspeed_slre",
              "precip_slre"
            )
        ) {
          # Collapse data to one row per date using first()
          data <- water_data %>%
            summarise(
              across(
                c(
                  discharge_nemadji,
                  discharge_brule,
                  watertemp,
                  airtemp_buoy,
                  windspeed_lake,
                  windspeed_slre,
                  precip_slre
                ),
                first
              ),
              .by = c(date, year)
            )
        } else {
          # Handle site-specific data filtering and label updates
          if (variable_input_id == "variable") {
            data <- water_data %>%
              filter(site == req(input$site))
            y_axis_layout$title <- str_c(
              y_label,
              site_names[[input$site]],
              sep = " - "
            )
          } else {
            data <- water_data %>%
              filter(site == req(input$site_2))
            y_axis_layout$title <- str_c(
              y_label,
              site_names[[input$site_2]],
              sep = " - "
            )
          }
        }

        # Filter by year if "All Years" is not selected
        if (year_input != "all") {
          data <- data %>%
            filter(year == as.numeric(year_input))
        }

        # Combine lake and estuary bloom lines to overlay on the timeseries
        bloom_lines <- list()

        if (lake_bloom_state()) {
          lake_lines <- lapply(
            lake_blooms$date,
            make_line,
            line_color = "forestgreen"
          )
          bloom_lines <- c(bloom_lines, lake_lines)
        }

        if (est_bloom_state()) {
          est_lines <- lapply(
            est_blooms$date,
            make_line,
            line_color = "lightgreen"
          )
          bloom_lines <- c(bloom_lines, est_lines)
        }

        if (length(bloom_lines) == 0) {
          bloom_lines <- NULL
        }

        # Generate Time Series Plotly Graph
        plot_ly(
          data = data,
          x = ~date,
          y = ~ get(current_variable),
          type = "scatter",
          mode = "markers+lines",
          line = list(color = "#2E86AB", width = 2),
          hovertemplate = str_c(
            "<b>Date:</b> %{x}<br>",
            "<b>",
            y_label,
            ":</b> %{y:.2f}<br>",
            "<extra></extra>"
          )
        ) %>%
          layout(
            shapes = bloom_lines, # Add vertical bloom indicator lines
            xaxis = list(
              title = "",
              showgrid = TRUE,
              gridcolor = "lightgray",
              range = get_year_range(year_input)
            ),
            yaxis = y_axis_layout,
            hovermode = "x unified",
            plot_bgcolor = "white",
            paper_bgcolor = "white"
          ) %>%
          config(displaylogo = FALSE)
      } else {
        # Handle Scatter Plot (X-axis is another variable, not date)

        # Filter raw data based on the chosen year
        if (year_input != "all") {
          water_data_year <- water_data %>%
            filter(year == as.numeric(year_input))
        } else {
          water_data_year <- water_data
        }

        # Prepare Y-axis data
        if (input[[variable_input_id]] %in% c("temp", "turb", "tp", "chl")) {
          if (variable_input_id == "variable") {
            y_data <- water_data_year %>%
              filter(site == req(input$site)) %>%
              select(
                all_of(c("date", input[[variable_input_id]]))
              )
            y_axis_layout$title <- str_c(
              y_label,
              site_names[[input$site]],
              sep = " - "
            )
          } else {
            y_data <- water_data_year %>%
              filter(site == req(input$site_2)) %>%
              select(
                all_of(c("date", input[[variable_input_id]]))
              )
            y_axis_layout$title <- str_c(
              y_label,
              site_names[[input$site_2]],
              sep = " - "
            )
          }
        } else {
          # Summarise non-site specific data
          y_data <- water_data_year %>%
            summarise(
              across(
                all_of(
                  c(
                    input[[variable_input_id]]
                  )
                ),
                first
              ),
              .by = c(date)
            )
        }

        # Prepare X-axis data
        if (input[[x_variable_input_id]] %in% c("temp", "turb", "tp", "chl")) {
          if (x_variable_input_id == "variable_x") {
            x_data <- water_data_year %>%
              filter(site == req(input$site)) %>%
              select(
                all_of(c("date", input[[x_variable_input_id]]))
              )
            x_axis_layout$title <- str_c(
              x_label,
              site_names[[input$site]],
              sep = " - "
            )
          } else {
            x_data <- water_data_year %>%
              filter(site == req(input$site_2)) %>%
              select(
                all_of(c("date", input[[x_variable_input_id]]))
              )
            x_axis_layout$title <- str_c(
              x_label,
              site_names[[input$site_2]],
              sep = " - "
            )
          }
        } else {
          # Summarise non-site specific data
          x_data <- water_data_year %>%
            summarise(
              across(
                all_of(
                  c(
                    input[[x_variable_input_id]]
                  )
                ),
                first
              ),
              .by = c(date)
            )
        }

        # Combine X and Y data by date for plotting
        plot_data <- inner_join(x_data, y_data, by = join_by(date))

        # Generate Scatter Plotly Graph
        plot_ly(
          data = plot_data,
          x = ~ get(current_variable_x),
          y = ~ get(current_variable),
          type = "scatter",
          mode = "markers",
          marker = list(color = "#2E86AB"),
          hovertemplate = str_c(
            "<b>",
            x_label,
            ":</b> %{x:.2f}<br>",
            "<b>",
            y_label,
            ":</b> %{y:.2f}<br>",
            "<extra></extra>"
          )
        ) %>%
          layout(
            xaxis = x_axis_layout,
            yaxis = y_axis_layout,
            hovermode = "closest",
            plot_bgcolor = "white",
            paper_bgcolor = "white"
          ) %>%
          config(displaylogo = FALSE)
      }
    })
  }

  # Render the first plot
  output$timeseries_plot <- render_plot("variable", "variable_x")

  # Render the second plot using the new variable input
  output$timeseries_plot_2 <- render_plot("variable_2", "variable_x2")

  # Pop-up modal containing the site map when the button is clicked
  observeEvent(input$show_map_button, {
    showModal(modalDialog(
      tags$div(
        style = "position: relative;",
        actionButton("close_map_modal", "×", class = "leaflet-close-btn"),
        leafletOutput("site_map", height = "60vh")
      ),
      easyClose = TRUE,
      footer = NULL,
      size = "xl"
    ))
  })

  # Close the map modal manually
  observeEvent(input$close_map_modal, {
    removeModal()
  })

  # Render the Leaflet Map
  output$site_map <- renderLeaflet({
    # Identify which site correlates with variable 1
    site_name_1 <- case_when(
      input$variable == "discharge_nemadji" ~ "nemadji",
      input$variable == "discharge_brule" ~ "brule",
      input$variable %in% c("airtemp_buoy", "watertemp") ~ "llo",
      input$variable == "windspeed_lake" ~ "pngw3",
      input$variable %in% c("windspeed_slre", "precip_slre") ~ "lkspo",
      .default = input$site
    )

    # Identify which site correlates with variable 2
    site_name_2 <- case_when(
      input$variable_2 == "discharge_nemadji" ~ "nemadji",
      input$variable_2 == "discharge_brule" ~ "brule",
      input$variable_2 %in% c("airtemp_buoy", "watertemp") ~ "llo",
      input$variable_2 == "windspeed_lake" ~ "pngw3",
      input$variable_2 %in% c("windspeed_slre", "precip_slre") ~ "lkspo",
      .default = input$site_2
    )

    # Identify which site correlates with variable x1
    site_name_3 <- case_when(
      input$variable_x == "discharge_nemadji" ~ "nemadji",
      input$variable_x == "discharge_brule" ~ "brule",
      input$variable_x %in% c("airtemp_buoy", "watertemp") ~ "llo",
      input$variable_x == "windspeed_lake" ~ "pngw3",
      input$variable_x %in% c("windspeed_slre", "precip_slre") ~ "lkspo",
      .default = input$site
    )

    # Identify which site correlates with variable x2
    site_name_4 <- case_when(
      input$variable_x2 == "discharge_nemadji" ~ "nemadji",
      input$variable_x2 == "discharge_brule" ~ "brule",
      input$variable_x2 %in% c("airtemp_buoy", "watertemp") ~ "llo",
      input$variable_x2 == "windspeed_lake" ~ "pngw3",
      input$variable_x2 %in% c("windspeed_slre", "precip_slre") ~ "lkspo",
      .default = input$site_2
    )

    # Combine to find all active sites that should be highlighted
    sites_to_highlight <- unique(c(
      site_name_1,
      site_name_2,
      site_name_3,
      site_name_4
    ))

    # Split spatial data based on selection status
    mapdata_sel <- filter(site_locations, site %in% sites_to_highlight)
    mapdata_unsel <- filter(site_locations, !(site %in% sites_to_highlight))

    # Build Map
    leaflet() %>%
      addTiles() %>%
      # Add unselected markers
      addLabelOnlyMarkers(
        data = mapdata_unsel,
        lng = ~longitude,
        lat = ~latitude,
        label = ~name,
        labelOptions = ~ labelOptions(
          noHide = TRUE,
          direction = 'auto',
          style = list(
            'font-size' = '16px',
            'font-weight' = 'bold'
          )
        )
      ) %>%
      # Add selected markers (highlighted yellow)
      addLabelOnlyMarkers(
        data = mapdata_sel,
        lng = ~longitude,
        lat = ~latitude,
        label = ~name,
        labelOptions = ~ labelOptions(
          noHide = TRUE,
          direction = 'auto',
          style = list(
            'font-size' = '16px',
            'font-weight' = 'bold',
            'background-color' = "yellow"
          )
        )
      )
  })
}

# Run the app
shinyApp(ui = ui, server = server)
