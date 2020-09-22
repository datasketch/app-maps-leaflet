webshot::install_phantomjs()
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
library(shinydisconnect)


frtypes_doc <- suppressWarnings(yaml::read_yaml("conf/frtypes.yaml"))
available_ftypes <- names(frtypes_doc)
data_samples <- suppressWarnings( yaml::read_yaml("data/data-samples.yaml"))

style = "
.logo{
z-index: 1000;
bottom: 0px !important;
right: 5px !important;
}
.leaflet-top, .leaflet-bottom {
    z-index: 1 !important;
}
"

ui <- panelsPage(
  showDebug(),
  useShi18ny(),
  disconnectMessage(
    text = "Oh no!, la sesión a finalizado, si estabas trabajando en la app, por favor contacta a soporte y cuentanos que ha sucedido//Oh no, the session has ended, if you were working on the app, please contact support and tell us what has happened",
    refresh = "O, intenta de nuevo//Or try again",
    background = "#385573",
    colour = "white",
    overlayColour = "grey",
    overlayOpacity = 0.3,
    refreshColour = "#FBC140"
  ),
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
          uiOutput("info_dis"),
          uiOutput("select_var"),
          #verbatimTextOutput("test_url"),
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
        body = withLoader(
          uiOutput("end_sol"),
          type = "image",loader = "img/loading_gris.gif"),
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
  
  
  # Modulo de carga de datos ------------------------------------------------
  
  output$table_input <- renderUI({
    
    choices <- c("sampleData", "pasted", "fileUpload", "googleSheets")
    if (lang() == "") return()
    names(choices) <- i_(c("sample", "paste", "upload_doc", "google"), lang = lang())
    tableInputUI("initial_data",
                 i_("table_label", lang()),
                 choices = choices,
                 selected =  "sampleData")
  })
  
  # Valores de URL ----------------------------------------------------------
  
  info_url <- reactiveValues(id = NULL)
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (identical(query, list())) return()
    info_url$id <- query
  })
  
  info_org <- reactiveValues(org = "public")
  info_map <- reactiveValues(name = "world_countries")
  
  observe({
    info_url <- info_url$id
    if (is.null(info_url)) return()
    available_params <- names(info_url)
    
    if ("org" %in% available_params) {
      info_org$org <- info_url$org }
    if ("map_name" %in% available_params) {
      info_map$name <- strsplit(info_url$map_name, split = ",") %>% unlist()
    }
  })
  # Data cuando no hay datos de muestra 
  output$map_input_selector <- renderUI({
    
    if (is.null(info_map$name)) return()
    if (length(info_map$name) == 1) return()
    choices <- info_map$name
    names(choices) <- i_(info_map$name, lang = lang())
    selectizeInput("map_name", i_('map_name'), choices)
    
  })
  
  
  mapa_print <- reactive({
    if (is.null(info_map$name)) return()
    map_name_select <- input$map_name 
    #print("devbf")
    #print(map_name_select)
    if (is.null(map_name_select)) {
      map_name_select <- info_map$name
    } 
    map_name_select
  })
  
  data_fake <- reactive({
    
    map_name_select <- mapa_print()
    print(map_name_select)
    if (is.null(map_name_select)) return()
    
    list_samples <- list.files("data/samples/")
    this_example <- sum(grepl(map_name_select, list_samples))
    print("ansdjkssak")
    print(this_example)
    if (this_example > 0) {
      l <- try(list_samples[grepl(map_name_select, list_samples)])
      l <- l[grepl(paste0("_",lang()), l)]
      l <- paste0("data/samples/", l)
    } else {
      l <- map(map_name_select, function(x) {
        lfltmagic::fakeData(x)
      })
    }
    names(l) <- i_(map_name_select, lang())
    l
  })
  # lista labels módulos en idioma seleccionado
  labels <- reactive({
    
    lang <- lang()
    if (lang == "") return()
    req(data_fake())
    files <- data_fake()
    print("ola")
    
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
    do.call(tableInputServer, c("initial_data", labels()))
  })
  
  # output$debug <- renderPrint({
  #   inputData()
  # })
  
  # Vista de datos ----------------------------------------------------------
  
  output$dataset <- renderUI({
    suppressWarnings(
      hotr("data_input", data = inputData(), options = list(height = 530))
    )
  })
  
  data_fringe <- reactive({
    req(input$data_input)
    suppressWarnings(hotr::hotr_fringe(input$data_input))
  })
  
  data_load <- reactive({
    req(data_fringe())
    data_fringe()$data
  })
  

  # Tipo de datos -----------------------------------------------------------
  
  
  guess_ft <- reactive({
    req(data_load())
    guess_ft <- guess_ftypes(data = data_load(), mapa_print())
    guess_ft
  })
  
  dic_load <- reactive({
    req(data_fringe())
    dic <- data_fringe()$dic
    if (is.null(guess_ft())) {
      dic <- dic
    } else {
      dic$hdType <- guess_ft()  
    }
    
    dic
  })
  output$info_dis <- renderUI({
    req(dic_load())
    if (!is.null(guess_ft())) return()
    shinypanels::infomessage(type = "warning" , i_("data_undefine", lang()))
  })
  
  output$select_var <- renderUI({
  
    req(dic_load())
    dic_load <- dic_load()

    data_ftype <- paste0(dic_load$hdType, collapse = "-")
    print(dic_load$hdType)

    if (sum(c("Gnm", "Gcd") %in% dic_load$hdType) > 0) {
      if (data_ftype %in% available_ftypes) {
        select_var <- dic_load$id
      } else {
        sample_cats <- NULL
        if (sum(c("Gcd", "Gnm") %in% dic_load$hdType) > 0) {
          if(sum(grepl("Gnm|Gcd", dic_load$hdType)) == 0) sample_cats <- NULL
          sample_cats <- sample(dic_load$id[grepl("Gnm|Gcd", dic_load$hdType)], 1)}
        sample_nums <- NULL
        if (sum(c("Num") %in% dic_load$hdType) > 0) {
          if(sum(grepl("Num", dic_load$hdType)) == 0) sample_nums <- NULL
          sample_nums <-  sample(dic_load$id[grepl("Num", dic_load$hdType)], 1)}
        select_var <- c(sample_cats, sample_nums)
      }
    } else {
    select_var <- NULL
    }
    
    list_var <- dic_load$id
    names(list_var) <- dic_load$label[match(list_var, dic_load$id)]

    if (!is.null(select_var)) names(select_var) <- dic_load$label[match(select_var, dic_load$id)]

    selectizeInput("var_order",
                   div(class="title-data-select",i_("var_selector", lang())),
                   choices = list_var,
                   multiple = TRUE,
                   selected = select_var,
                   options = list(plugins= list('remove_button', 'drag_drop'))
    )

  })
  
  
  variables <- reactiveValues(id = NULL)
  observe({
    if (is.null(input$var_order)) return()
    variables$id <- input$var_order
  })
  # Preparación data para graficar ------------------------------------------
  
  
  dic_draw <- reactive({
    req(dic_load())
    if (is.null(variables$id)) return()
    var_select <- variables$id
    dic_load()[dic_load()$id %in% var_select,]
  })
  
  data_draw <- reactive({
    if (is.null(variables$id)) return()
    if (is.null(dic_draw())) return()
    var_select <- dic_draw()$id 
    d <- data_load()[var_select]
    names(d) <- dic_draw()$label
    d
  })
  
 
  
  
  ftype_draw <- reactive({
    if (is.null(dic_draw())) return()
    paste0(dic_draw()$hdType, collapse = "-")
  })
  
  possible_viz <- reactive({
    if (is.null(ftype_draw())) return()
    frtypes_doc[[ftype_draw()]]
  })
  
  
  actual_but <- reactiveValues(active = 'choropleth')
  
  observe({
    viz_rec <- possible_viz()
    if (is.null(viz_rec)) return()
    if (is.null(input$viz_selection)) return()
    if (!( input$viz_selection %in% viz_rec)) {
      actual_but$active <- viz_rec[1]
    } else {
      actual_but$active <- input$viz_selection
    }
  })
  
  
  output$viz_icons <- renderUI({
    buttonImageInput('viz_selection',
                     div(class="title-data-select",i_('viz_type', lang())),
                     images = possible_viz(),
                     path = 'img/svg/',
                     format = 'svg',
                     active = actual_but$active)
  })
  
  
  
  # # Renderizar inputs con parmesan ------------------------------------------

  parmesan <- parmesan_load()
  parmesan_input <- parmesan_watch(input, parmesan)
  parmesan_alert(parmesan, env = environment())
  parmesan_lang <- reactive({i_(parmesan, lang(), keys = c("label", "inputs", "text"))})
  output_parmesan("controls",
                  parmesan = parmesan_lang,
                  input = input,
                  output = output,
                  env = environment())
 
  tooltip_info <- reactive({
    i_("tool_info", lang())
  })
  
  
  map_tiles_opts <- reactive({
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
  
  conditional_map_tiles <- reactive({
    # mt <- input$map_tiles
    # mb <- FALSE
    # if (is.null(mt)) mb <- TRUE
    # mb
    TRUE
  })
  
  map_tiles_info <- reactive({
    i_("tiles_info", lang())
  })
  
  
  viz_last_active <- reactive({
    actual_but$active
  })
  
  min_map_bubble <- reactive({
    if (is.null(input$map_min_size)) return()
    input$map_min_size + 1
  })
  
  agg_opts <- reactive({
    choices <- c("sum", "mean", "median")
    names(choices) <- i_(c("sum", "mean", "median"), lang = lang())
    choices
  })
  
  map_color_scale <- reactive({
    choices <- c("Numeric", "Bins", "Quantile")
    names(choices) <- i_(c("Numeric", "Bins", "Quantile"), lang = lang())
    choices
  })
  
  color_scale_select <- reactive({
    input$map_color_scale
  })
  
# opts y theme ------------------------------------------------------------
  opts_viz <- reactive({
    
    if (is.null(info_org$org)) return()
    opts_viz <- parmesan_input()
    if (is.null(opts_viz)) return()
    opts_viz <- opts_viz[setdiff(names(opts_viz), c('theme'))]
    opts_viz$logo <- info_org$org
    opts_viz %>% discard(is.null)
  })
  
  theme_load <- reactive({
    theme_select <- input$theme
    print(info_org$org)
    if (is.null(theme_select)) return()
    th <- dsthemer_get(info_org$org, theme = theme_select)
    if (is.null(th)) return()
    th
  })
  
  background <- reactive({
    req(theme_load())
    theme_load()$background_color
  })
  
  agg_palette <- reactive({
    req(theme_load())
    colors <- theme_load()$palette_colors
    colors
  })
  
  na_color <- reactive({
    req(theme_load())
    theme_load()$na_color
  })
  
  na_info <- reactive({
    i_("na_info", lang())
  })
  
  grid_color <- reactive({
    req(theme_load())
    theme_load()$grid_color
  })
  
  conditional_border_weight <- reactive({
    bw <- input$border_weight
    bc <- TRUE
    if (bw == 0) bc <- FALSE
    bc
  })
  
  conditional_graticule <- reactive({
    input$map_graticule
  })
  
  theme_draw <- reactive({
    req(theme_load())
    l <- theme_load()
    l <- l[setdiff(names(l), c('logo', 'background_color', 'palette_colors',
                               'na_color', 'grid_color', 'grid_size'))]
    l
  })
  
# Render mapa -------------------------------------------------------------

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
    req(mapa_print())
    req(data_draw())
    req(opts_viz())
    #print(opts_viz())
    req(theme_draw())
    map_select <- mapa_print()
    viz <- do.call(viz_name(), c(list(data = data_draw(),
                                      map_name = map_select,
                                      opts = c(opts_viz(), theme_draw())
    )))
    viz
    
  })
  
  output$view_lftl_viz <- renderLeaflet({
    req(lftl_viz())
    suppressWarnings(
      lftl_viz()
    )
  })
  
  output$end_sol <- renderUI({
    if (ftype_draw() %in% available_ftypes) {
      s <- leafletOutput("view_lftl_viz", height = 500)
    } else {
      s <- infomessage(type = "warning", p(style="max-width:300px;",i_("ftypes_warning", lang())))
    }
    s
  })
  

# descarga ----------------------------------------------------------------

  
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