#webshot::install_phantomjs()
library(lfltmagic)
library(leafem)
library(rgdal)
library(makeup)
library(shinypanels)
library(parmesan)
library(shinyinvoer)
library(shi18ny)
library(dsmodules)
library(hotr)
library(dsthemer)
library(knitr)


frtypes_doc <- suppressWarnings(yaml::read_yaml("conf/frtypes.yaml"))
data_samples <- suppressWarnings( yaml::read_yaml("data/data-samples.yaml"))
ui <- panelsPage(
  useShi18ny(),
  langSelectorInput("lang", position = "fixed"),
  panel(title = ui_("upload_data"),
        collapse = TRUE,
        width = 300,
        body = uiOutput("table_input")
  ),
  panel(title = ui_("view_data"), 
        collapse = FALSE,
        width = 450,
        body = div(
          div(
            uiOutput("select_var"), #verbatimTextOutput("select_var"),#
            uiOutput("dataset") 
          )
        )),
  panel(title = ui_("edit_viz"),
        color = "chardonnay",
        width = 350,
        body = uiOutput("controls")
  ),
  panel(title =  ui_("view_viz"),
        title_plugin = downloadImageUI("down_ggmagic", "Download", c("svg", "png", "jpeg", "pdf"), display = "dropdown"),
        color = "chardonnay",
        can_collapse = FALSE,
        body = leafletOutput("map_lflt"),
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
  
  
  # Valores de URL ----------------------------------------------------------
  
  info_url <- reactiveValues(id = NULL)
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (identical(query, list())) return()
    info_url$id <- query
  })
  
  info_map <- reactiveValues(name = "world_countries")
  info_org <- reactiveValues(name = "datasketch")
  observe({
    info_url <- info_url$id
    if (is.null(info_url)) return()
    
    available_params <- names(info_url)
    
    
    if ("org" %in% available_params) {
      info_org$name <- info_url$org }
    if ("maps" %in% available_params) {
      info_map$name <- strsplit(info_url$maps, split = ",") %>% unlist()}
    
  })
  
  
  
  map_name_opts <- reactive({
    choices <- info_map$name
    names(choices) <- i_(info_map$name, lang = lang())
    choices
  })
  
  
  # Modulo de carga de datos ------------------------------------------------
  output$table_input <- renderUI({
    choices <- c("sampleData", "pasted", "fileUpload", "googleSheets")
    names(choices) <- i_(c("sample", "paste", "upload_doc", "google"), lang = lang())
    tableInputUI("initial_data",
                 choices = choices,
                 selected =  "sampleData")
  })
  
  
  
  available_samples <- reactive({
    list_files <- list.files("data/samples/")
    map_name_url <- info_map$name
    list_files <- list_files[grep(paste0(map_name_url, collapse = "|"), list_files)]
    if (identical(list_files, character(0)))  return()
    list_files
  })
  
  labels <- reactive({
    
    list_files <- available_samples()
    if (is.null(list_files)) {
      list_files <- c("sin_data_es.csv", "sin_data_en.csv")
    } else {
      list_files <- c(list_files, "sin_data_es.csv", "sin_data_en.csv")  
    }
    
    files <- paste0("data/samples/",list_files[grep(paste0('_', lang()), list_files)])
    names_files <- data_samples[names(data_samples) %in% files]
    names(files) <- names_files 
    
    list(
      sampleLabel = i_("select_sample", lang()),
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
    suppressWarnings(
      hotr("data_input", data = inputData(), options = list(height = 530), order = NULL)
    )
  })
  
  data_fringe <- reactive({
    sampleData <- input$`initial_data-tableInput` == "sampleData"
    if(grepl("sin_data", input$`initial_data-inputDataSample`) & sampleData) return()
    if (is.null(available_samples())) return()
    if (is.null(input$data_input)) return()
    suppressWarnings( hotr::hotr_fringe(input$data_input))
  })
  
  data_load <- reactive({
    if (is.null(data_fringe())) return()
    data_fringe()$data
  })
  
  dic_load <- reactive({
    if (is.null(data_fringe())) return()
    data_fringe()$dic
  })
  
  
  output$select_var <- renderUI({
    if (is.null(data_fringe())) {
      return("No tenemos muestra disponible para este país, puedes agregar la tuya dando click en las
      otras opciones de carga de data, o quedate a jugar con las opciones de visualización")
    } else {
      available_fTypes <- names(frtypes_doc)
      data_ftype <- data_fringe()$frtype
      if(is.null(dic_load())) return()
      
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
    }
  })
  
  
  # Preparación data para graficar ------------------------------------------
  
  data_draw <- reactive({
    if (is.null(data_load())) return()
    var_select <- input$var_order
    if (is.null(var_select)) return()
    d <- data_load()[var_select] 
    names(d) <- dic_draw()$label
    d
  })
  
  dic_draw <- reactive({
    if (is.null(data_load())) return()
    var_select <- input$var_order
    if (is.null(var_select)) return()
    dic_load() %>% filter(id %in% var_select)
  })
  
  # Posibles graficos -------------------------------------------------------
  
  ftype_draw <- reactive({
    if (is.null(dic_draw())) {
      x <- "Gnm-Num"
    } else {
      gt <- geoType(data = data_draw(), map_name = info_map$name)
      x <- paste0(dic_draw()$hdType, collapse = "-")
      x <- gsub("Cat", gt, x)
    }
    x
  })
  
  possible_viz <- reactive({
    if (is.null(ftype_draw())) return()
    frtypes_doc[[ftype_draw()]]
  })
  
  output$viz_icons <- renderUI({
    
    print(ftype_draw())
    print(possible_viz())
    buttonImageInput('viz_selection',
                     i_('viz_type', lang()),
                     images = possible_viz(),
                     path = 'img/svg/',
                     format = 'svg',
                     active = actual_but$active)
  })
  
  actual_but <- reactiveValues(active = 'choropleth')
  
  
  observe({
    if (is.null(input$viz_selection)) return()
    viz_rec <- possible_viz()
    if (!( input$viz_selection %in% viz_rec)) {
      actual_but$active <- viz_rec[1]
    } else {
      actual_but$active <- input$viz_selection
    }
  })
  
  
  # Condicionales -----------------------------------------------------------
  
  hasdataNum <- reactive({
    TRUE %in% grepl('Num', dic_draw()$hdType)
  })
  
  agg_opts <- reactive({
    choices <- c("sum", "mean", "median")
    names(choices) <- i_(c("sum", "mean", "median"), lang = lang())
    choices
  })
  
  conditional_border_weight <- reactive({
    bw <- input$border_weight
    bc <- TRUE
    if (bw == 0) bc <- FALSE
    bc
  })
  
  conditional_map_tiles <- reactive({
    mt <- input$map_tiles
    mb <- TRUE
    if (!is.null(mt)) mb <- FALSE
    mb
  })
  
  conditional_graticule <- reactive({
    input$map_graticule
  })
  
  tooltip_info <- reactive({
    "hola"
  })
  
  map_color_scale <- reactive({
    choices <- c("Numeric", "Bins", "Quantile")
    names(choices) <- i_(c("Numeric", "Bins", "Quantile"), lang = lang())
    choices
  })
  
  color_scale_select <- reactive({
    input$map_color_scale
  })
  
  map_tiles <- reactive({
    c("OpenStreetMap",
      "OpenStreetMap.Mapnik", 
      "OpenTopoMap", 
      "OpenStreetMap.HOT",
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
  
  parmesan <- parmesan_load()
  parmesan_input <- parmesan_watch(input, parmesan)
  parmesan_alert(parmesan, env = environment())
  parmesan_lang <- reactive({i_(parmesan, lang(), keys = c("label", "inputs", "text"))})
  output_parmesan("controls", 
                  parmesan = parmesan_lang,
                  input = input, 
                  output = output,
                  env = environment())
  
  
  opts_viz <- reactive({
    opts_viz <- parmesan_input()
    if (is.null(opts_viz)) return()
    opts_viz <- opts_viz[setdiff(names(opts_viz), c('theme'))]
    opts_viz
  })
  
  theme_load <- reactive({
    theme_select <- input$theme
    if (is.null(theme_select)) return()
    theme_get(info_org$name, theme = theme_select)
  })
  
  grid_color <- reactive({
    theme_load()$grid_color
  })
  
  background <- reactive({
    theme_load()$background_color
  })
  
  na_color <- reactive({
    theme_load()$na_color
  })
  
  agg_palette <- reactive({
    #if (is.null(color_scale_select())) return()
    p <- theme_load()$palette_colors
    if (color_scale_select() == "Numeric") p <- p[1:2]
    p
  })
  
  theme_draw <- reactive({
    l <- theme_load()
    l <- l[setdiff(names(l), c('background_color', 'palette_colors', 'branding_include', 
                               'na_color', 'grid_color', 'grid_size'))]
    l
  })
  
  lftl_viz <- reactive({
    print(data_draw())
    # do.call(paste0("lflt_", "choropleth", "_GnmNum"),
    #         c(list(opts_viz()[2])))
    geotype <- gsub("-", "", ftype_draw())
    print(geotype)
    viz <- paste0("lflt_", actual_but$active, "_", geotype)
    do.call(viz, c(list(data = data_draw(), opts = c(opts_viz(), theme_draw())
    ))
    )
    #lflt_choropleth_GnmNum(opts = c(opts_viz(), theme_draw()))
  })
  
  output$map_lflt <- renderLeaflet({
    if (is.null(opts_viz())) return()
    lftl_viz()  
  })  
  
  callModule(downloadImage, "down_lfltmagic", graph = lftl_viz(), 
             lib = "highcharter", formats = c("html", "png", "jpeg", "pdf"))
  
  
}



shinyApp(ui, server)