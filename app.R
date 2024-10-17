# Function to check if package is installed

options(repos = c(CRAN = "https://cloud.r-project.org"))

required_packages <- c("shiny", "ggplot2", "tidyverse", "dplyr", "stringr", "ggthemes", 
                       "plotly", "highcharter", "leaflet", "sf", "shinydashboard", "shinycssloaders")

check_install_load <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package, dependencies = TRUE)
      library(package, character.only = TRUE)
    }
  }
}

check_install_load(required_packages)

# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      #home {
        display: flex;
        flex-direction: column;
        justify-content: center;
        align-items: center;
      }
      
      #home img {
        max-width: 100%;
        max-height: 70vh;
      }
  
      #line_box {
        min-heigth: 500px;
      }
  
      #pie_box{
        min-heigth: 509px;
      }
      
      .box-title {
        text-align: center;
      }
      
      body {
        background-color: #F5F5DC; 
      }
      .navbar {background-color:#DEB887;} 
      .navbar-default .navbar-brand {color:#000000;}
      .navbar-default .navbar-brand:hover {color:#000000;}
      .navbar-default .navbar-nav > li > a {color:#000000;}
      .navbar-default .navbar-nav > li > a:hover {color:#F5F5DC;}
      .navbar-default .navbar-nav > .active > a {background-color:#D2B48C; color:#000000;}
      .navbar-default .navbar-nav > .active > a:hover {background-color:#D2B48C; color:#ffffff;} 
      .navbar-default .navbar-nav > .active > a:focus {background-color:#D2B48C; color:#000000;} 
      .navbar-default .navbar-toggle .icon-bar {background-color:#000000;}
      h2 {color: #8B4513;} 
      .dropdown-menu {
        background-color: #DEB887; 
      }
      .dropdown-menu > li > a {
        color: #000000; 
      }
      .dropdown-menu > li > a:hover, .dropdown-menu > li > a:focus {
        background-color: #D2B48C; 
        color: #ffffff; 
      }
      .dropdown-menu > .active > a {
        background-color: #D2B48C; 
        color: #000000; 
      }
      .dropdown-menu > .active > a:hover, .dropdown-menu > .active > a:focus {
        background-color: #D2B48C; 
        color: #ffffff; 
      }
      .box {
        border-style: solid;
        border-width: 2px;
        border-color: #8B4513;
      }
      ")
    )
  ),
  
  navbarPage("Analysis of Petroleum Sector in India",
             tabPanel("Home",
                      div(id = "home",
                          div(id = "homeText", h2("Petroleum Sector Analysis in India", align = "center")),
                          div(id = "homeImage", img(src = "image.png", width = "auto", height = "auto"))
                      )
             ),
             tabPanel("Info",
                      fluidRow(
                               column(width = 12,
                                   h2("Petroleum Sector in India"),
                                   strong(p("The petroleum sector in India has witnessed significant changes over the past few decades, transforming the energy landscape of the nation. As one of the world's top energy consumers, India's relationship with petroleum products is multi-faceted, influencing both the domestic economy and international relations."))
                               ),
                               column(width = 12,
                                   h2("Trends and Changes"),
                                   strong(p("The petroleum sector in India has been on a consistent growth trajectory. From the late 1990s to the present day, production and consumption of petroleum products have seen a substantial increase. This surge in demand has been largely met through both domestic production and imports.")),
                                   strong(p("The import-export dynamics of petroleum products have undergone notable changes. While India continues to be a net importer of crude oil, it has become a net exporter of petroleum products. This shift has been made possible due to the expansion and modernization of India's refining capabilities.")),
                                   strong(p("On the domestic front, consumption patterns have evolved. High Speed Diesel (HSD) and Motor Spirit (MS), for instance, have witnessed increased sales, reflecting the growth of the transport sector. State-wise sales data reveals regional disparities, with consumption concentrated in more industrialized states."))
                               ),
                               column(width = 12,
                                   h2("Benefits to the Nation"),
                                   strong(p("The growth of the petroleum sector has had far-reaching impacts on India's economy. It has contributed to the nation's GDP, provided employment opportunities, and facilitated infrastructural development. The revenues from the petroleum sector have been a significant source of funds for the government, supporting public expenditure.")),
                                   strong(p("The sector's evolution has also influenced India's energy security strategy. By expanding its refining capacity, India has managed to extract more value from crude oil imports and has even entered the global petroleum product export market.")),
                                   strong(p("Despite the growth and benefits, the sector faces challenges such as dependency on crude oil imports, regional disparities in product consumption, and environmental concerns. The industry, therefore, is on a path towards diversification and sustainability, exploring alternatives like natural gas and investing in renewable energy technologies.")),
                                   strong(p("In conclusion, the petroleum sector has played a pivotal role in India's economic development, and it will continue to be a crucial part of India's energy future. However, the path forward will require balancing economic growth with sustainability concerns."))
                               ),
                               column(width = 12,
                                      h2("Overview"),
                                      strong(p("In this dashboard, we delve deeper into these trends and provide a analysis of the import, export, production, and consumption of various petroleum products in India."))
                                      )
                      )
             ),
             navbarMenu("Import/Export Analysis",
                        tabPanel("Yearly Import/Export Analysis",
                                 fluidRow(
                                   box(title = "Total Import/Export of Petroleum Products Over the Years", status = "primary", solidHeader = TRUE, 
                                       plotlyOutput("lineplot", height = "auto", width = "auto"),
                                       id = "line_box",
                                       width = 8),
                                   box(title = textOutput("selected_year_text"), status = "primary", solidHeader = TRUE,
                                       uiOutput("dynamic_output"),
                                       textOutput("dynamic_note"), id = "pie_box", height = "509px",width = 4),
                                   column(width = 12),
                                   box(uiOutput("line_description"),
                                       uiOutput("pie_description"), width = 12)
                                 )),
                        tabPanel("Monthly Import/Export Analysis",
                                 fluidRow(
                                   column(title = "Total Import/Export of Petroleum Products Over the Months", status = "primary", solidHeader = TRUE, 
                                       selectInput("sunburst_year", "Select Year:", choices = 2011:2022), width = 12),
                                   box(title = "Import", status = "primary", solidHeader = TRUE,
                                       highchartOutput("sunburst_plot_import", height = "600px", width = "auto"),
                                       textOutput("sunburst_import_note"), 
                                       width = 6),
                                   box(title = "Export", status = "primary", solidHeader = TRUE,
                                       highchartOutput("sunburst_plot_export", height = "600px", width = "auto"), 
                                       textOutput("sunburst_export_note"),
                                       width = 6),
                                   column(width = 12),
                                   box(uiOutput("sunburst_description"),
                                       width = 12)
                                 ))
             ),
             tabPanel("Yearly Production/Consumption Analysis",
                      fluidRow(
                        box(title = "Total Production/Consumption of Petroleum Products Over the Years", status = "primary", solidHeader = TRUE,
                            plotlyOutput("bubblechart", height = "auto", width = "auto"),
                            textOutput("bubble_note"),
                            width = 10),
                        column(status = "primary", solidHeader = TRUE, 
                            selectInput("selectedYear", "Select Year:", choices = 1998:2021), width = 2),
                        column(width = 12),
                        box(uiOutput("bubble_description"),
                            width = 12)
                        )),
             
             tabPanel("State-wise Sales Analysis",
                      fluidRow(
                        box(title = "Total Sales of MS and HSD Over the Years", status = "primary", solidHeader = TRUE, 
                            plotlyOutput("lineplot_state", height = "auto", width = "auto"),
                            id = "line_box",
                            width = 8),
                        box(uiOutput("map_description"),
                            id = "pie_box",
                            height = "509px",
                            width = 4),
                        column(width = 12),
                        box(title = "Sales of Motor Spirit and High Speed Diesel Over the States of India", status = "primary", solidHeader = TRUE, width = 12),
                        column(width = 12),
                        box(title = textOutput("selected_year_text_ms"), status = "primary", solidHeader = TRUE,
                            uiOutput("dynamic_output_ms"), width = 6),
                        box(title = textOutput("selected_year_text_hsd"), status = "primary", solidHeader = TRUE,
                            uiOutput("dynamic_output_hsd"), width = 6)
                      ))
  )
)

# Define server logic
server <- function(input, output, session) {
  
  data <- read.csv("./Data/import_export_years.csv")
  sun <- read.csv("./Data/import_export_months.csv")
  prod_cons <- read.csv("./Data/Production_Consumption.csv")
  
  # Split the data into import and export
  import_data <- subset(data, import_export == 'import')
  export_data <- subset(data, import_export == 'export')
  
  # Calculate total import excluding crude oil
  import_data_no_crude <- import_data
  import_data_no_crude$total <- import_data_no_crude$total - import_data_no_crude$crude_oil
  import_data_no_crude <- subset(import_data_no_crude, select = -crude_oil)
  
  # Split the sunburst data file into import and export
  sun_import <- subset(sun, import_export == "import")
  
  sun_export <- subset(sun, import_export == "export")
  
  sun_import <- sun_import %>%
    select(-total)
  
  sun_import <- pivot_longer(sun_import, cols = c(crude_oil:others), names_to = "Product", values_to = "Quantity")
  
  sun_export <- sun_export %>%
    select(-total)
  
  sun_export <- pivot_longer(sun_export, cols = c(crude_oil:others), names_to = "Product", values_to = "Quantity")
  
  sun_import <- sun_import %>%
    mutate(Product = str_to_title(Product))
  
  sun_import$Product <- gsub("_", " ", sun_import$Product) 
  
  sun_import$Product <- ifelse(sun_import$Product %in% c("Lpg", "Hsd", "Ms","Atf","Sko", "Ldo"), 
                                toupper(sun_import$Product), 
                                sun_import$Product)
  
  sun_import$Product[sun_import$Product == "Lobs lube oil"] <- "LOBS/Lube Oil"
  
  sun_export <- sun_export %>%
    mutate(Product = str_to_title(Product))
  
  sun_export$Product <- gsub("_", " ", sun_export$Product) 
  
  sun_export$Product <- ifelse(sun_export$Product %in% c("Lpg", "Hsd", "Ms","Atf","Sko", "Ldo"), 
                                toupper(sun_export$Product), 
                                sun_export$Product)
  
  sun_export$Product[sun_export$Product == "Lobs lube oil"] <- "LOBS/Lube Oil"
  
  states_ms <- st_read("./Data/India Shape/Admin2.shp") %>%
    mutate(Name = str_to_lower(ST_NM))
  
  states_hsd <- st_read("./Data/India Shape/Admin2.shp") %>%
    mutate(Name = str_to_lower(ST_NM))
  
  ms <- read.csv("./Data/MS_Sales.csv")
  
  hsd <- read.csv("./Data/HSD_Sales.csv")
  
  ms_sales <- ms %>% select(c("X2008":"X2022"))
  
  ms_sales <- ms_sales %>%
    rename_at(vars(starts_with("X")), ~ substr(., 2, nchar(.)))
  
  ms_sales <- pivot_longer(ms_sales, cols = c("2008":"2022"), names_to = "Year", values_to = "Quantity")
  
  ms_sales <- ms_sales %>%
    group_by(Year) %>%
    summarise(Total_Quantity = sum(Quantity),
              .groups = "drop")
  
  ms_sales <- ms_sales %>% as.data.frame()
  
  hsd_sales <- hsd %>% select(c("X2008":"X2022"))
  
  hsd_sales <- hsd_sales %>%
    rename_at(vars(starts_with("X")), ~ substr(., 2, nchar(.)))
  
  hsd_sales <- pivot_longer(hsd_sales, cols = c("2008":"2022"), names_to = "Year", values_to = "Quantity")
  
  hsd_sales <- hsd_sales %>%
    group_by(Year) %>%
    summarise(Total_Quantity = sum(Quantity),
              .groups = "drop")
  
  hsd_sales <- hsd_sales %>% as.data.frame()
  
  ms_sales$Year <- as.numeric(ms_sales$Year)
  
  hsd_sales$Year <- as.numeric(hsd_sales$Year)
  
  ms$States[ms$States == "dadra & nagar haveli and daman & diu"] <- "dadra and nagar haveli and daman and diu"
  
  hsd$States[hsd$States == "dadra & nagar haveli and daman & diu"] <- "dadra and nagar haveli and daman and diu"
  
  ms <- ms %>%
    mutate(Name = str_to_lower(States))
  
  hsd <- hsd %>%
    mutate(Name = str_to_lower(States))
  
  states_ms <- left_join(states_ms, ms, by = "Name")
  
  states_hsd <- left_join(states_hsd, hsd, by = "Name")
  
  states_ms <- states_ms %>%
    rename_at(vars(starts_with("X")), ~ substr(., 2, nchar(.))) %>%
    mutate(Name = str_to_title(Name))
  
  states_hsd <- states_hsd %>%
    rename_at(vars(starts_with("X")), ~ substr(., 2, nchar(.))) %>%
    mutate(Name = str_to_title(Name))
  
  states_ms <-  pivot_longer(states_ms, cols = c("2008":"2022"), names_to = "Years", values_to = "Quantities")
  
  states_hsd <-  pivot_longer(states_hsd, cols = c("2008":"2022"), names_to = "Years", values_to = "Quantities")
  
  plot_clicked <- reactiveVal(FALSE)
  plot_type <- reactiveVal(NULL)
  
  observeEvent(event_data("plotly_click", source = "lineplot"), {
    plot_clicked(TRUE)
    d <- event_data("plotly_click", source = "lineplot")
    type <- d$curveNumber
    plot_type(if(type == 1) {"import"} else if(type == 3) {"import_no_crude"} else {"export"})
  })
  
  observeEvent(event_data("plotly_click", source = "lineplot_state"), {
    plot_clicked(TRUE)
  })
  
  observeEvent(event_data("plotly_doubleclick", source = "lineplot"), {
    plot_clicked(FALSE)
    plot_type(NULL)
  })
  
  observeEvent(event_data("plotly_doubleclick", source = "lineplot_state"), {
    plot_clicked(FALSE)
  })
  
  output$selected_year_text <- renderText({
    d <- event_data("plotly_click", source = "lineplot")
    
    if (!is.null(d)) {
      clicked <- d$x
      paste("Product Compostion for Year: ",clicked,"in '000 MT") # Generate the title with the selected year
    } else {
      "Product Compostion"
    }
  })
  
  output$selected_year_text_ms <- renderText({
    d <- event_data("plotly_click", source = "lineplot_state")
    
    if (!is.null(d)) {
      clicked <- d$x
      paste("Sales of Motor Spirit for the Year: ",clicked,"in '000 MT") # Generate the title with the selected year
    } else {
      "Sales of Motor Spirit"
    }
  })
  
  output$selected_year_text_hsd <- renderText({
    d <- event_data("plotly_click", source = "lineplot_state")
    
    if (!is.null(d)) {
      clicked <- d$x
      paste("Sales of High Speed Diesel for the Year: ",clicked,"in '000 MT") # Generate the title with the selected year
    } else {
      "Sales of High Speed Diesel"
    }
  })
  
  output$dynamic_output <- renderUI({
    if (plot_clicked()) {
      plotlyOutput("piechart", width = "auto")
    } else {
      p("Click a point on the line chart to generate a pie chart")
    }
  })
  
  output$dynamic_output_ms <- renderUI({
    if (plot_clicked()) {
      withSpinner(leafletOutput("mapMS"))
    } else {
      p("Click a point on the line chart to generate a Choropleth map")
    }
  })
  
  output$dynamic_output_hsd <- renderUI({
    if (plot_clicked()) {
      withSpinner(leafletOutput("mapHSD"))
    } else {
      p("Click a point on the line chart to generate a Choropleth map")
    }
  })
  
  output$dynamic_note <- renderText({
    if (plot_clicked()) {
      if (plot_type() == "import") {
        return("Note: Others in import include Petcoke, Paraffin wax, Petroleum Jelly, Aviation Gas, MTBE, Reformate etc.")
      } else if (plot_type() == "import_no_crude") {
        return("Note: Others in import include Petcoke, Paraffin wax, Petroleum Jelly, Aviation Gas, MTBE, Reformate etc.")
      } else if (plot_type() == "export") {
        return("Note: Others in export include Petcoke/CBFS, Benzene, Hexane, MTO, Sulphur etc.")
      }
    } else {
      return("")
    }
  })
  
  mapMS <- reactive({
    
      req(plot_clicked())
      
      Sys.sleep(0)
      
      d <- event_data("plotly_click", source = "lineplot_state")
      
      if(!is.null(d)){
        clicked <- d$x 
        
        data_ms <- filter(states_ms, Years == as.character(clicked))
        
        leaflet(data_ms) %>%
          addProviderTiles(providers$CartoDB.Positron) %>%
          addPolygons(
            fillColor = ~colorNumeric("YlOrRd", Quantities)(Quantities),
            weight = 1,
            opacity = 1,
            color = "black",
            dashArray = "1",
            fillOpacity = 0.9,
            highlight = highlightOptions(
              weight = 2,
              color = "#666",
              dashArray = "",
              fillOpacity = 0.9,
              bringToFront = TRUE),
            label = ~paste(Name, ": ", Quantities)
          ) %>%
          addLegend(pal = colorNumeric("YlOrRd", data_ms$Quantities), values = ~Quantities, opacity = 0.9, title = "'000 Metric Tons")
      }
  })
  
  mapHSD <- reactive({
    
      req(plot_clicked())
      Sys.sleep(0)
      
      d <- event_data("plotly_click", source = "lineplot_state")
      
      if(!is.null(d)){
        clicked <- d$x
        data_hsd <- filter(states_hsd, Years == as.character(clicked))
        
        leaflet(data_hsd) %>%
          addProviderTiles(providers$CartoDB.Positron) %>%
          addPolygons(
            fillColor = ~colorNumeric("YlOrRd", Quantities)(Quantities),
            weight = 1,
            opacity = 1,
            color = "black",
            dashArray = "1",
            fillOpacity = 0.9,
            highlight = highlightOptions(
              weight = 2,
              color = "#666",
              dashArray = "",
              fillOpacity = 0.9,
              bringToFront = TRUE),
            label = ~paste(Name, ": ", Quantities)
          ) %>%
          addLegend(pal = colorNumeric("YlOrRd", data_hsd$Quantities), values = ~Quantities, opacity = 0.9, title = "'000 Metric Tons")
      }
  })
  
  output$lineplot <- renderPlotly({
      plt <- ggplot() +
        geom_line(data = import_data, aes(x = year, y = total, color = 'Import (Including Crude Oil)', linetype = 'Import (Including Crude Oil)'), size = 0.25) +
        geom_point(data = import_data, aes(x = year, y = total, text = paste("<b>Year:", year, "<br>Total Import:", total, "MT</b>"))) +
        geom_line(data = import_data_no_crude, aes(x = year, y = total, color = 'Import (Excluding Crude Oil)', linetype = 'Import (Excluding Crude Oil)'), size = 0.25) +
        geom_point(data = import_data_no_crude, aes(x = year, y = total, text = paste("<b>Year:", year, "<br>Total Import:", total,"MT</b>"))) +
        geom_line(data = export_data, aes(x = year, y = total, color = 'Export', linetype = 'Export'), size = 0.25) +
        geom_point(data = export_data, aes(x = year, y = total, text = paste("<b>Year:", year, "<br>Total Export:", total,"MT</b>"))) +
        scale_color_manual(values = c('Import (Including Crude Oil)' = 'blue', 'Import (Excluding Crude Oil)' = 'blue', 'Export' = 'red'),
                           name = NULL,
                           breaks = c('Import (Including Crude Oil)', 'Import (Excluding Crude Oil)', 'Export'),
                           labels = c('Import (Including Crude Oil)', 'Import (Excluding Crude Oil)', 'Export')) +
        scale_linetype_manual(values = c('Import (Including Crude Oil)' = "solid", 'Import (Excluding Crude Oil)' = "dashed", 'Export' = "solid"),
                              name = NULL,
                              breaks = c('Import (Including Crude Oil)', 'Import (Excluding Crude Oil)', 'Export'),
                              labels = c('Import (Including Crude Oil)', 'Import (Excluding Crude Oil)', 'Export')) +
        scale_x_continuous(breaks = seq(min(import_data$year), max(import_data$year), by = 1)) +
        scale_y_continuous(n.breaks = 20) +
        theme_classic() +
        theme(axis.text.x = element_text(angle = -45, hjust = 1),
              axis.text.y = element_text(angle = 0, vjust = 1),
              axis.title.x = element_text(face = "bold"),
              axis.title.y = element_text(face = "bold"),
              panel.grid.major.x = element_line(color = "lightgrey", size = 0.3),
              panel.grid.minor.x = element_line(color = "lightgrey", size = 0.3),
              panel.grid.major.y = element_line(color = "lightgrey", size = 0.3),
              panel.grid.minor.y = element_line(color = "lightgrey", size = 0.3))
      
      plt <- ggplotly(plt,tooltip = "text",source = "lineplot") %>% 
        event_register("plotly_click") %>% 
        event_register("plotly_doubleclick")
      
      plt %>% layout(plot_bgcolor = "#F5F5DC", # Beige background color
               paper_bgcolor = "#F5F5DC", # Beige background color
               title = list(x = 0.5),
               xaxis = list(title = "Year"),
               yaxis = list(title = "Total Quantity in '000 Metric Tons"),
               legend = list(bgcolor = "#E5E5D2")
               )
  })
  
  output$line_description <- renderUI({
    tagList(
      tags$style(HTML("
        #line_description {
          padding: 15px;
          font-weight: bold;
        }
      ")),
      p("The line chart provides a year-on-year analysis of the import and export trends of petroleum products in India from 1998 to 2022."),
      tags$ul(
        tags$li("It offers a comparative view of the quantities of total imports, total exports, and imports excluding crude oil."),
        tags$li("This visualization helps in understanding the changes and patterns in India's import and export of petroleum products over the years."),
        tags$li("Over the years, there has been a significant increase in both the import and export of petroleum products."),
        tags$li("The global outbreak of COVID-19 in 2019-2020 had a profound impact on the petroleum sector. As countries around the world imposed lockdowns and travel restrictions to curb the spread of the virus, the demand for petroleum products plummeted. This resulted in a noticeable dip in both imports and exports during these years.")
      )
    )
  })
  
  
  output$piechart <- renderPlotly({
    if (plot_clicked()) {
      d <- event_data("plotly_click", source = "lineplot")
      
      if (!is.null(d)){
        clicked <- d$x
        type <- d$curveNumber
        
        data_subset <- if(type == 1) {import_data} else if(type == 3) {import_data_no_crude} else {export_data}
        
        data_subset <- data_subset %>%
          filter(year == clicked) %>%
          select(-year, -import_export, -total) %>%
          gather(key = "Product", value = "Quantity")
        
        data_subset <- data_subset %>%
          mutate(Product = str_to_title(Product))
        
        data_subset$Product <- gsub("_", " ", data_subset$Product) 
        
        data_subset$Product <- ifelse(data_subset$Product %in% c("Lpg", "Hsd", "Ms","Atf","Sko", "Ldo"), 
                                      toupper(data_subset$Product), 
                                      data_subset$Product)
        
        data_subset$Product[data_subset$Product == "Lobs lube oil"] <- "LOBS/Lube Oil"
        
        plot_ly(data_subset, labels = ~Product, values = ~Quantity, type = 'pie',
                textposition = 'inside',
                textinfo='label+percent',
                marker = list(colors = colors,
                              line = list(color = '#FFFFFF', width = 1))) %>%
          layout(margin = list(l = 20, r = 20),
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                 legend = list(bgcolor = "#E5E5D2")) %>%
          layout(paper_bgcolor = '#F5F5DC')
      }
    }
  })
  
  output$pie_description <- renderUI({
    tagList(
      tags$style(HTML("
        #pie_description {
          padding: 15px;
          font-weight: bold;
        }
      ")),
      p("In the corresponding pie chart, we can observe the distribution of different petroleum products in India's total imports for a specific year. This helps us understand which petroleum products contribute the most to India's import bill. For example, in 1998, crude oil constituted the major chunk of petroleum imports, while other products like LPG, MS, and HSD also had significant shares.")
    )
  })
  
  output$sunburst_plot_import <- renderHighchart({
      sun_chart_import <- subset(sun_import, year == input$sunburst_year)
      
      dout <- data_to_hierarchical(sun_chart_import, c(months,Product), Quantity)
      
      hchart(dout, type="sunburst") %>%
        hc_tooltip(pointFormat = '<b>{point.name}</b>: {point.value}')
  })
  
  output$sunburst_import_note <- renderText({
    return ("Note: Others in import include Petcoke, Paraffin wax, Petroleum Jelly, Aviation Gas, MTBE, Reformate etc.")
  })
  
  output$sunburst_plot_export <- renderHighchart({
      sun_chart_export <- subset(sun_export, year == input$sunburst_year)
      
      dout <- data_to_hierarchical(sun_chart_export, c(months,Product), Quantity)
      
      hchart(dout, type="sunburst") %>%
        hc_tooltip(pointFormat = '<b>{point.name}</b>: {point.value}')
  })
  
  output$sunburst_export_note <- renderText({
    return ("Note: Others in export include Petcoke/CBFS, Benzene, Hexane, MTO, Sulphur etc.")
  })
  
  output$sunburst_description <- renderUI({
    tagList(
      tags$style(HTML("
        #sunburst_description {
          padding: 15px;
          font-weight: bold;
        }
      ")),
      p("The total quantity of petroleum products imported each month significantly outnumbers the quantity exported. The average monthly import volume is around 15,000, while the average monthly export volume is around 5,000."),
      tags$br(),
      p("Crude oil is the most imported product, with quantities in the range of 12,000 to 17,000 each month. On the export side, MS (Motor Spirit), Naphtha, ATF (Aviation Turbine Fuel), and HSD (High Speed Diesel) are the most exported products."),
      tags$br(),
      p("The import volume generally remains consistent throughout the year, with a slight increase observed in the months of January and October. The export volume also shows a similar trend, with peaks in the months of February, July, and December. The consistent import and export volumes throughout the year suggest a steady demand and supply of petroleum products."),
      tags$br(),
      p("The import volume of products like LPG, Naphtha, and Fuel Oil remains quite consistent throughout the year. On the export side, products like MS, Naphtha, ATF, and HSD exhibit some monthly variations. For example, exports of ATF peaked in July, while exports of HSD peaked in February.")
    )
  })
  
  output$bubblechart <- renderPlotly({
    
    year_data <- subset(prod_cons, Year == input$selectedYear)
    
    # Create the plot
    p <- ggplot(year_data, aes(x = Products, y = Production_Quantity, 
                               text = paste("Product: ", Products,
                                            "<br>Production: ", Production_Quantity,
                                            "<br>Consumption: ", Consumption_Quantity, sep = ""))) +
      geom_point(aes(size = Consumption_Quantity, color = Consumption_Quantity), alpha = 1) +
      scale_color_gradientn(colors = c("blue", "darkgreen", "orange", "red", "darkred"), guide = guide_colorbar(title = "Total Consumption in '000 Metric Tons")) +
      scale_size(range = c(1,20)) +
      scale_y_continuous(limits = c(0,120000)) +
      theme_classic() +
      labs(x = "Products",
           y = "Total Production in '000 Metric Tons",
           size = "Consumption Quantity") +
      theme(legend.position = "bottom",
            plot.background = element_rect(fill = "#F5F5DC"),
            panel.background = element_rect(fill = "#F5F5DC"),
            legend.background = element_rect(fill="#E5E5D2"),
            panel.grid.major = element_line(color = "lightgrey", linetype = "solid"),
            panel.grid.minor = element_line(color = "lightgrey", linetype = "dotted"),
            plot.title = element_text(hjust = 0.5))
    
    bubble_plt <- ggplotly(p, tooltip = "text")
    
    bubble_plt
    
  })
  
  output$bubble_note <- renderText({
    return ("Note: Others include products like Propylene, solvents (Hexane, Benzene, Toulene, Xylene, and Speciality solvents), Reformate, Mineral Turpentine Oil, Carbon Black Feed Stock, Waxes, Sulpher, Lubricants & Greses, and Petroleum Coke.")
  })
  
  output$bubble_description <- renderUI({
    tagList(
      tags$style(HTML("
        #bubble_description {
          padding: 15px;
          font-weight: bold;
        }
      ")),
      p("The bubble plots for the years 1998-2021 provide several insights into the production and consumption of petroleum products in India:"),
      tags$br(),
      p("The plots show that both production and consumption have increased over time for various petroleum products. This trend is in line with the growing economy and energy needs of the country."),
      tags$br(),
      p("The size of the bubbles, which represents consumption quantity, is larger than the production quantity (Y-axis value) for many petroleum products. This indicates that India's domestic production is not sufficient to meet its consumption needs for these products. As a result, the country must rely on imports."),
      tags$br(),
      p("Product-wise insigths:"),
      tags$ul(
        tags$li("Motor Spirit: Production and consumption have both increased over the years. The consumption has grown from 1998 to 2021, indicating increased usage of motor-spirit. This is likely due to the high demand for this fuel in the transportation sector."),
        tags$li("High Speed Diesel: Production and consumption have both increased over the years. The consumption has grown from 1998 to 2021, indicating increased usage of diesel. This is likely due to the high demand for this fuel in the transportation sector."),
        tags$li("LPG: While the production has grown slightly from 1998 to 2021, the consumption has seen a noticeable increase outspacing the production, likely driven by the growth in the usage of this fuel in households and hospitality industry.")
      )
    )
  })
  
  output$lineplot_state <- renderPlotly({
    plt <- ggplot() +
      geom_line(data = ms_sales, aes(x = Year, y = Total_Quantity, color = 'Motor Spirit', linetype = 'Motor Spirit'), size = 0.25) +
      geom_point(data = ms_sales, aes(x = Year, y = Total_Quantity, text = paste("<b>Year:", Year, "<br>Total Sales:", Total_Quantity, "MT</b>"))) +
      geom_line(data = hsd_sales, aes(x = Year, y = Total_Quantity, color = 'High Speed Diesel', linetype = 'High Speed Diesel'), size = 0.25) +
      geom_point(data = hsd_sales, aes(x = Year, y = Total_Quantity, text = paste("<b>Year:", Year, "<br>Total Sales:", Total_Quantity,"MT</b>"))) +
      scale_color_manual(values = c('Motor Spirit' = 'blue', 'High Speed Diesel' = 'red'),
                         name = NULL,
                         breaks = c('Motor Spirit', 'High Speed Diesel'),
                         labels = c('Motor Spirit', 'High Speed Diesel')) +
      scale_linetype_manual(values = c('Motor Spirit' = "solid", 'High Speed Diesel' = "solid"),
                            name = NULL,
                            breaks = c('Motor Spirit', 'High Speed Diesel'),
                            labels = c('Motor Spirit', 'High Speed Diesel')) +
      scale_x_continuous(breaks = seq(min(ms_sales$Year), max(ms_sales$Year), by = 1)) +
      scale_y_continuous(n.breaks = 10) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = -45, hjust = 1),
            axis.text.y = element_text(angle = 0, vjust = 1),
            axis.title.x = element_text(face = "bold"),
            axis.title.y = element_text(face = "bold"),
            panel.grid.major.x = element_line(color = "lightgrey", size = 0.3),
            panel.grid.minor.x = element_line(color = "lightgrey", size = 0.3),
            panel.grid.major.y = element_line(color = "lightgrey", size = 0.3),
            panel.grid.minor.y = element_line(color = "lightgrey", size = 0.3))
    
    plt <- ggplotly(plt,tooltip = "text",source = "lineplot_state") %>% 
      event_register("plotly_click") %>% 
      event_register("plotly_doubleclick")
    
    plt %>% layout(plot_bgcolor = "#F5F5DC", # Beige background color
             paper_bgcolor = "#F5F5DC", # Beige background color
             title = list(x = 0.5),
             xaxis = list(title = "Year"),
             yaxis = list(title = "Total Sales in '000 Metric Tons"),
             legend = list(bgcolor = "#E5E5D2")
      )
  })
  
  output$mapMS <- renderLeaflet({
    mapMS()
  })
  
  output$mapHSD <- renderLeaflet({
    mapHSD()
  })
  
  output$map_description <- renderUI({
    tagList(
      tags$style(HTML("
        #map_description {
          padding: 15px;
          font-weight: bold;
        }
      ")),
      p("Both Motor Spirit (MS) and High Speed Diesel (HSD) sales volumes have generally increased over the years. This suggests a rising demand for these petroleum products in India."),
      tags$br(),
      p("Comparing MS and HSD sales, HSD generally has higher sales volume. This could be due to its widespread use in various sectors such as transport, industry, and agriculture, whereas MS (Petrol) is primarily used in passenger vehicles."),
      tags$br(),
      p("A significant drop in sales can be observed in 2020, likely due to the COVID-19 pandemic and associated lockdowns reducing the demand for fuel. However, the sales volume seems to recover in 2021 and 2022, indicating a rebound in demand as restrictions ease."),
      tags$br(),
      p("The sales volume varies greatly across different states(Can be infered from the Choropleth map). For instance, Uttar Pradesh shows high sales volumes for both MS and HSD, while states like Uttarakhand have significantly lower sales. This could be due to factors such as the population, industrialization level, and the number of vehicles in these states.")
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
