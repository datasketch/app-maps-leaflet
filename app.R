#webshot::install_phantomjs()
library(lfltmagic)
library(shinypanels)
library(parmesan)
library(shinyinvoer)
library(shi18ny)
library(dsmodules)
library(colourpicker)
library(hotr)
library(dsthemer)
library(knitr)


frtypes_doc <- suppressWarnings(yaml::read_yaml("conf/frtypes.yaml"))
availableMaps <- suppressWarnings(yaml::read_yaml("conf/availableMaps.yaml")) 

ui <- panelsPage(
  useShi18ny(),
  langSelectorInput("lang", position = "fixed"),
  panel(title = ui_("upload_data"),
        collapse = TRUE,
        width = 300,
        body = uiOutput("table_input")),
  panel(title = ui_("view_data"), 
        collapse = FALSE,
        width = 450,
        body = div(
          div(
            uiOutput("select_var"),
            uiOutput("dataset") 
          )
        )),
  panel(title = ui_("edit_viz"),
        color = "chardonnay",
        width = 350,
        body = uiOutput("controls")
  ),
  panel(title =  ui_("view_viz"),
        color = "chardonnay",
        can_collapse = FALSE,
        body = leafletOutput("map_lflt", height = 550),
        footer = uiOutput("viz_icons"))
)

server <- function(input, output, session) {
  
  # Idiomas -----------------------------------------------------------------
  
  i18n <- list(
    defaultLang = "es",
    availableLangs = c("es","en")
  )
  lang <- callModule(langSelector,"lang", i18n = i18n, showSelector=FALSE)
  
  
  observeEvent(lang(),{
    uiLangUpdate(input$shi18ny_ui_classes, lang())
  })
  
  map_name <- reactiveValues(id = NULL)
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (is.null(query)) query <- "world"
    map_name$id <- query
  })
  
  
  # Modulo de carga de datos ------------------------------------------------
  
  output$table_input <- renderUI({
    choices <- c("sampleData", "pasted", "fileUpload", "googleSheets")
    names(choices) <- i_(c("sample", "paste", "upload_doc", "google"), lang = lang())
    tableInputUI("initial_data",
                 choices = choices,
                 selected =  "sampleData")
  })
  
  # lista labels módulos en idioma seleccionado
  labels <- reactive({
    
    lang <- lang()
    list_files <- list.files("data/samples/")
    list_files <- list_files[grep(paste0('_', lang), list_files)]
    files <- paste0("data/samples/", list_files)
    names(files) <- i_(c("guatemala"), lang = lang)
    
    
    list(sampleLabel = i_("select_sample", lang()), 
         sampleFiles = files,
         
         pasteLabel = i_("paste", lang()), 
         pasteValue = "", 
         pastePlaceholder = i_("paste_pl", lang()), 
         pasteRows = 5,
         
         uploadLabel = i_("upload_lb", lang()),
         uploadButtonLabel = i_("upload_bt_lb", lang()), 
         uploadPlaceholder = i_("upload_pl", lang()),
         
         googleSheetLabel = i_("google_sh_lb", lang()),
         googleSheetValue = "",
         googleSheetPlaceholder = i_("google_sh_pl", lang()),
         googleSheetPageLabel = i_("google_sh_pg_lb", lang())
    )
  })
  
  inputData <- eventReactive(labels(), {
    do.call(callModule, c(tableInput,
                          "initial_data",
                          labels()))
  })
  
  
  
  # Vista de datos ----------------------------------------------------------
  
  output$dataset <- renderUI({
    if(is.null(inputData()))return()
    # order_var <- input$var_order
    # print(order_var)
    suppressWarnings(
      hotr("data_input", data = inputData(), options = list(height = 530))
    )
  })
  
  data_fringe <- reactive({
    suppressWarnings( hotr::hotr_fringe(input$data_input))
  })
  
  data_load <- reactive({
    data_fringe()$data
  })
  
  dic_load <- reactive({
    data_fringe()$dic
  })
  
  
  output$select_var <- renderUI({
    #print(dic_load())
    available_fTypes <- names(frtypes_doc)
    data_ftype <- data_fringe()$frtype
    if(is.null(dic_load)) return()
    
    if (data_ftype %in% available_fTypes) { 
      data_order <- dic_load()$id
    } else if (grepl("Cat|Yea|Dat",data_ftype)&grepl("Num",data_ftype)){
      data_order <- c(dic_load()$id[grep("Cat|Yea|Dat", dic_load()$hdType)[1]],
                      dic_load()$id[grep("Num", dic_load()$hdType)[1]])
    } else {
      data_order <- dic_load()$id[grep("Cat|Num", dic_load()$hdType)[1]]
    }
    
    list_var <- dic_load()$id
    if (is.null(list_var)) return()
    names(list_var) <- dic_load()$label[match(list_var, dic_load()$id)]
    
    
    selectizeInput("var_order",
                   i_("var_selector", lang()),
                   choices = list_var,
                   multiple = TRUE, 
                   selected = data_order,
                   options = list(plugins= list('remove_button', 'drag_drop'))
    )
  })
  

  # Renderizar inputs con parmesan ------------------------------------------
  
  parmesan <- parmesan_load()
  parmesan_input <- parmesan_watch(input, parmesan)
  parmesan_alert(parmesan, env = environment())
  parmesan_lang <- reactive({i_(parmesan, lang(), keys = c("label", "inputs", "text"))})
  output_parmesan("controls", 
                  parmesan = parmesan_lang,
                  input = input, 
                  output = output,
                  env = environment())  
  
  

  

# Condicionales de inputs -------------------------------------------------

  hasdataNum <- reactive({
    TRUE
  })
  hasdataNA <- reactive({
    TRUE
  })
  
  input_drop_na <- reactive({
    input$drop_na
  })
  
  hasTiles <- reactive({
    is.null(input$map_tiles)
  })
  
  input_color_scale <- reactive({
    input$map_color_scale
  })
  
  input_legend_show <- reactive({
    input$legend_show
  })
  
  agg_legend_position <- reactive({
    c("topright", "bottomright", "bottomleft",
      "topleft")
  })
  
# Reactivos de inputs -----------------------------------------------------

  background <- reactive({
   color <- "#FFFFFF"
   color
  })  
  
  
  agg_palette <- reactive({
    c( '#AEFAEF', '#4f083d')
  })
  
  agg_opts <- reactive({
    "a"
  })
  
  format_cat_opts <- reactive({
    choices <- c("As Title", "UPPER", "lower", "Firstupper")
    names(choices) <- i_(c("Title", "Upper", "Lower", "Firstupper"), lang = lang())
    choices 
  })
    
  agg_scale <- reactive({
    c("Numeric", "Bins", "Quantile")
  })
  
  
  map_tiles <- reactive({
    c("OpenStreetMap",
      "OpenStreetMap.Mapnik", 
      "OpenTopoMap", 
      "OpenStreetMap.HOT",
      "OpenMapSurfer.AdminBounds",
      "OpenMapSurfer.Roads",
      "OpenMapSurfer.Hillshade",
      "Stamen.TonerHybrid",
      "Stamen.Toner",
      "Stamen.TonerBackground",
      "Stamen.TonerLite",
      "Stamen.Watercolor",
      "Stamen.Terrain",
      "Stamen.TerrainBackground",
      "Esri",
      "Esri.WorldStreetMap",
      "Esri.DeLorme",
      "Esri.WorldTopoMap",
      "Esri.WorldImagery",
      "Esri.WorldShadedRelief",
      "Esri.WorldTerrain", 
      "Esri.WorldPhysical",
      "Esri.OceanBasemap",
      "Esri.NatGeoWorldMap",
      "Esri.WorldGrayCanvas",
      "HERE.reducedDay",
      "HERE.normalDayGrey",
      "HERE.hybridDay",
      "CartoDB",
      "CartoDB.PositronNoLabels",
      "CartoDB.Voyager",            
      "CartoDB.VoyagerNoLabels",
      "CartoDB.DarkMatter",          
      "CartoDB.DarkMatterNoLabels",
      "MtbMap",
      "HikeBike",
      "HikeBike.HikeBike",
      "HikeBike.HillShading",
      "NASAGIBS.ModisTerraBands367CR", 
      "NASAGIBS.ViirsEarthAtNight2012",
      "Wikimedia",
      "GeoportailFrance",
      "GeoportailFrance.ignMaps",
      "GeoportailFrance.maps",
      "GeoportailFrance.orthos"
      )
  })
    
  # Mapa leaflet ------------------------------------------------------------
  opts_viz <- reactive({
    opts_viz <- parmesan_input()
    if (is.null(opts_viz)) return()
    opts_viz$palette_colors <- paste0("#", opts_viz$palette_colors)
    opts_viz <- opts_viz[setdiff(names(opts_viz), c('theme'))]
    opts_viz
  })
  
  output$map_lflt <- renderLeaflet({
    #data = data_load(),map_name = "gtm_departments"
   print(opts_viz())
    lflt_choropleth_GnmNum(data=sample_data("Gnm-Num", 100), map_name = "world_countries", opts_viz())
  })  
  

  # output$search_map <- renderUI({
  #   shinyinvoer::searchInput("id_country", availableMaps, "buscar")
  # })
  # 
  # 
  # observe({
  #   print(input$id_country)
  # })
  
}



shinyApp(ui, server)