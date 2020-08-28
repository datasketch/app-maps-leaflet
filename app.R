#webshot::install_phantomjs()
library(lfltmagic)
library(leafem)
library(paletero)
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
library(shinycustomloader)
library(pins)
library(dspins)

frtypes_doc <- suppressWarnings(yaml::read_yaml("conf/frtypes.yaml"))
data_samples <- suppressWarnings( yaml::read_yaml("data/data-samples.yaml"))

style = "
.logo{
z-index: 1000;
bottom: 0px !important;
right: 5px !important;
}
"

ui <- panelsPage(
  showDebug(),
  useShi18ny(),
  langSelectorInput("lang", position = "fixed"),
  styles = style,
  panel(title = ui_("upload_data"),
        collapse = TRUE,
        width = 300,
        body = uiOutput("table_input")
  ),
  panel(title = ui_("view_data"), 
        collapse = FALSE,
        width = 450,
        body = div(
          #uiOutput("select_var"),
          #withImage(
          #verbatimTextOutput("select_var"),#
          verbatimTextOutput("test_url"),
          withLoader(uiOutput("dataset"), type = "image",loader = "img/loading_gris.gif"), 
          # file_location = "img/loading_fucsia.gif"
          #)
        )),
  panel(title = ui_("edit_viz"),
        color = "chardonnay",
        width = 350,
        body = div(
          verbatimTextOutput("blabla"),
          uiOutput("map_input_selector"),
          uiOutput("controls"))
  ),
  panel(title =  ui_("view_viz"),
        title_plugin = uiOutput("download"),
        #downloadImageUI("down_lfltmagic", "Download", c("html", "png", "jpeg", "pdf"), display = "dropdown"),
        color = "chardonnay",
        can_collapse = FALSE,
        body = withLoader(leafletOutput("view_lftl_viz"), type = "image",loader = "img/loading_gris.gif"),
        footer = uiOutput("viz_icons"))
)

server <- function(input, output, session) {
  
  # Idiomas -----------------------------------------------------------------
  
  i18n <- list(
    defaultLang = "en",
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
  info_org <- reactiveValues(name = NULL, id = NULL, org = "public")
 
  observe({
    info_url <- info_url$id
    if (is.null(info_url)) return()
    available_params <- names(info_url)
    
    if ("org" %in% available_params) {
      info_org$org <- info_url$org }
    
    if ("maps" %in% available_params) {
      if (is.null(info_map$name)) return()
      info_map$name <- strsplit(info_url$maps, split = ",") %>% unlist()}
    
    if ("org_name" %in% available_params) {
      info_org$name <- info_url$org_name }
    
    if ("org_id" %in% available_params) {
      info_org$id <- info_url$org_id }
    
  })
  
  # input de mapas
  
  output$map_input_selector <- renderUI({
    
    if (is.null(info_map$name)) return()
    if (length(info_map$name) == 1) {
      return()
    } else {
      choices <- info_map$name
      names(choices) <- i_(info_map$name, lang = lang())
      selectizeInput("map_name", i_('map_name'), choices)
    }
  })
  
  
  # mapa seleccionado
  
  mapa_print <- reactive({
    if (is.null(info_map$name)) return()
    map_name_select <- input$map_name 
    if (is.null(map_name_select)) map_name_select <- info_map$name
    map_name_select
  })
  
  # Data cuando no hay datos de muestra 
  
  data_fake <- reactive({
    
    map_name_select <- mapa_print()
    if (is.null(map_name_select)) return()
    
    list_samples <- list.files("data/samples/")
    this_example <- sum(grepl(map_name_select, list_samples))
    
    if (this_example > 0) {
      l <- list_samples[grepl(map_name_select, list_samples)]
      l <- l[grepl(paste0("_",lang()), l)]
      l <- paste0("data/samples/", l)
    } else {
    l <- map(map_name_select, function(x) {
      lfltmagic::fakeData(x)
    })
    names(l) <- i_(map_name_select, lang())
    }
    l
  })
  

  output$table_input <- renderUI({
    if (is.null(info_map$name)) return()
    choices <- c("sampleData", "pasted", "fileUpload", "googleSheets")
    names(choices) <- i_(c("sample", "paste", "upload_doc", "google"), lang = lang())
    tableInputUI("initial_data", i_("table_label", lang()),
                 choices = choices,
                 selected =  "sampleData")
  })
  
  
  labels <- reactive({
    
    map_name_select <- mapa_print()
    if (is.null(map_name_select)) return()
    req(data_fake())
    data_f <- data_fake()
    
    list(
      sampleLabel = i_("select_sample", lang()),
      sampleFiles = data_f,
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
    if (is.null(info_map$name)) return()
    do.call(tableInputServer, c("initial_data", labels()))
  })
  

  
  # Vista de datos ----------------------------------------------------------
  
  output$dataset <- renderUI({
    map_name_select <- mapa_print()
    if (is.null(map_name_select)) return()
    suppressWarnings(
      hotr("data_input", data = inputData(), options = list(height = 530), order = NULL)
    )
  })
  
  
  # Preparación data --------------------------------------------------------
  
  
  data_fringe <- reactive({
    map_name_select <- mapa_print()
    if (is.null(map_name_select)) return()
    if (is.null(input$data_input)) return()
    suppressWarnings( hotr::hotr_fringe(input$data_input))
  })
  
  
  data_load <- reactive({
    map_name_select <- mapa_print()
    if (is.null(map_name_select)) return()
    req(data_fringe())
    data_fringe()$data
  })
  
  dic_load <- reactive({
    map_select <- mapa_print()
    if (is.null( map_select)) return()
    req(data_fringe())
    data_fringe()$dic
  })
  
  
  # Selector de variables ---------------------------------------------------
  
  
  # data a graficar ---------------------------------------------------------
  
  data_draw <- reactive({
    req(data_load())
    d <- data_load()#[var_select] 
    names(d) <- dic_draw()$label
    d
  })
  
  dic_draw <- reactive({
    req(dic_load())
    dic_load() #%>% filter(id %in% var_select)
  })   
  
  # Tipo de datos -----------------------------------------------------------
  
  
  ftype_draw <- reactive({
    
    req(data_draw())
    req(dic_draw())
    map_select <- mapa_print()
    if (is.null( map_select)) return()
   
    x <- "Gnm-Num"
    if (is.null(dic_draw)) return(x) 
    x <- paste0(dic_draw()$hdType, collapse = "-")
    
    
    if (x == "Num-Num") {
      x <- "Gln-Glt"
    } else if (x == "Num-Num-Num"){
      x <- "Gln-Glt-Num"
    } else {
      gt <- geoType(data = data_draw(), map_name = map_select)
      if (is.null(gt)) return()
      x <- gsub("Cat", gt, x)
    }
    
    x
  })

# Iconos viz
  
  possible_viz <- reactive({
    if (is.null(ftype_draw())) return()
    frtypes_doc[[ftype_draw()]]
  })
  
  output$viz_icons <- renderUI({
    
    # print(ftype_draw())
    # print(possible_viz())
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
  
  # Condicionales de inputs -------------------------------------------------
  

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


  hasdataNum <- reactive({
     "Num" %in% dic_draw()$hdType
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
    # mt <- input$map_tiles
    # mb <- FALSE
    # if (is.null(mt)) mb <- TRUE
    # mb
    TRUE
  })
  
  conditional_graticule <- reactive({
    input$map_graticule
  })
  
  tooltip_info <- reactive({
    i_("tool_info", lang())
  })
  
  map_tiles_info <- reactive({
    i_("tiles_info", lang())
  })
  
  map_color_scale <- reactive({
    choices <- c("Numeric", "Bins", "Quantile")
    names(choices) <- i_(c("Numeric", "Bins", "Quantile"), lang = lang())
    choices
  })
  
  color_scale_select <- reactive({
    input$map_color_scale
  })
  

  na_color <- reactive({
    theme_load()$na_color
  })

  na_info <- reactive({
    i_("na_info", lang())
  })
  
  viz_last_active <- reactive({
    actual_but$active
  })

  min_map_bubble <- reactive({
    if (is.null(input$map_min_size)) return()
    input$map_min_size + 1
  })
  
# Mostrar inputs ----------------------------------------------------------

  parmesan <- parmesan_load()
  parmesan_input <- parmesan_watch(input, parmesan)
  parmesan_alert(parmesan, env = environment())
  parmesan_lang <- reactive({i_(parmesan, lang(), keys = c("label", "inputs", "text"))})


  output_parmesan("controls",
                  parmesan = parmesan_lang,
                  input = input,
                  output = output,
                  env = environment())
  
  # Adicionar theme ---------------------------------------------------------
  
  
  theme_load <- reactive({
    theme_select <- input$theme
    if (is.null(theme_select)) return()
    print(info_org$org)
    dsthemer_get(info_org$org, theme = theme_select)
  })
  
  theme_draw <- reactive({
    l <- theme_load()
    l <- l[setdiff(names(l), c('logo', 'background_color', 'palette_colors',
                               'na_color', 'grid_color', 'grid_size'))]
    l
  })
  
  grid_color <- reactive({
    theme_load()$grid_color
  })
  
  na_color <- reactive({
    theme_load()$na_color
  })
  
  background <- reactive({
    theme_load()$background_color
  })
  
  color_by_opts <- reactive({
    names(data_draw())
  })
  
  agg_palette <- reactive({
    colors <- theme_load()$palette_colors[1:2]
    colors
  })
  
  # logo_include <- reactive({
  #   theme_load()$branding_include
  # })
  # Renderizar highchart plot -----------------------------------------------
  
  opts_viz <- reactive({
    opts_viz <- parmesan_input()
    if (is.null(opts_viz)) return()
    opts_viz <- opts_viz[setdiff(names(opts_viz), c('theme'))]
    opts_viz$logo <- info_org$org
    opts_viz
  })
  

# visualización -----------------------------------------------------------

  
  
  viz_name <- reactive({
    if (is.null(ftype_draw())) return()
    if (ftype_draw() == "") return()
    geotype <- gsub("-", "", ftype_draw())
    gtype <- actual_but$active
    if (is.null(gtype)) return()
    typeV <- paste0('lflt_', gtype, '_', geotype)
    typeV
  })
  
  lftl_viz <- reactive({
    print( hasdataNum())
    req(opts_viz())
    map_select <- mapa_print()
    if (is.null( map_select)) return()
    viz <- do.call(viz_name(), c(list(data = data_draw(),
                                      map_name = map_select,
                                      opts = c(opts_viz(), theme_draw())
    )))
    viz
    
  })
  
  output$view_lftl_viz <- renderLeaflet({
    viz <- lftl_viz()
    if (is.null(viz)) return()
    suppressWarnings(
      viz
    )
  })  
  

  
  
  output$download <- renderUI({
    lb <- i_("download_viz", lang())
    dw <- i_("download", lang())
    gl <- i_("get_link", lang())
    mb <- list(textInput("name", i_("gl_name", lang())),
               textInput("description", i_("gl_description", lang())),
               selectInput("license", i_("gl_license", lang()), choices = c("CC0", "CC-BY")),
               selectizeInput("tags", i_("gl_tags", lang()), choices = list("No tag" = "no-tag"), multiple = TRUE, options = list(plugins= list('remove_button', 'drag_drop'))),
               selectizeInput("category", i_("gl_category", lang()), choices = list("No category" = "no-category")))
    downloadDsUI("download_data_button", dropdownLabel = lb, text = dw, formats = c("html","jpeg", "pdf", "png"),
                 display = "dropdown", dropdownWidth = 170, getLinkLabel = gl, modalTitle = gl, modalBody = mb,
                 modalButtonLabel = i_("gl_save", lang()), modalLinkLabel = i_("gl_url", lang()), modalIframeLabel = i_("gl_iframe", lang()),
                 modalFormatChoices = c("HTML" = "html", "PNG" = "png"))
  })


  par <- list(user_name = "brandon")
  url_par <- reactive({
    url_params(par, session)
  })

  pin_ <- function(x, bkt, ...) {
    x <- dsmodules:::eval_reactives(x)
    bkt <- dsmodules:::eval_reactives(bkt)
    nm <- input$`download_data_button-modal_form-name`
    if (!nzchar(input$`download_data_button-modal_form-name`)) {
      nm <- paste0("saved", "_", gsub("[ _:]", "-", substr(as.POSIXct(Sys.time()), 1, 19)))
      updateTextInput(session, "download_data_button-modal_form-name", value = nm)
    }
    dv <- dsviz(x,
                name = nm,
                description = input$`download_data_button-modal_form-description`,
                license = input$`download_data_button-modal_form-license`,
                tags = input$`download_data_button-modal_form-tags`,
                category = input$`download_data_button-modal_form-category`)
    dspins_user_board_connect(bkt)
    Sys.setlocale(locale = "en_US.UTF-8")
    pin(dv, bucket_id = bkt)
  }


  observe({
    req(lftl_viz())
    if (is.null(url_par()$inputs$user_name)) return()

    downloadDsServer("download_data_button", element = reactive(lftl_viz()),
                     formats = c("html", "jpeg", "pdf", "png"),
                     errorMessage = NULL,#i_("error_down", lang()),
                     modalFunction = pin_, reactive(lftl_viz()),
                     bkt = url_par()$inputs$user_name)
  })

  
}

shinyApp(ui, server)