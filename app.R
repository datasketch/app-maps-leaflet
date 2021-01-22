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
  tags$script("
    Shiny.addCustomMessageHandler('resetValue', function(variableName) {
      Shiny.onInputChange(variableName, null);
    });
  "),
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
          uiOutput("info_dis"),
          uiOutput("var_selector"),
          withLoader(uiOutput("dataset"), type = "image",loader = "img/loading_gris.gif")
        )),
  panel(title = ui_("edit_viz"),
        color = "chardonnay",
        width = 350,
        body = div(
          #verbatimTextOutput("aver"),
          uiOutput("map_input_selector"),
          uiOutput("controls"))
  ),
  panel(title =  ui_("view_viz"),
        title_plugin = uiOutput("download"),
        #downloadImageUI("down_lfltmagic", "Download", c("html", "png", "jpeg", "pdf"), display = "dropdown"),
        color = "chardonnay",
        can_collapse = FALSE,
        body = #withLoader(
          leafletOutput("view_lftl_viz"),# type = "image",loader = "img/loading_gris.gif"),
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
  
  

  
  data_fake <- reactive({
    
    map_name_select <- mapa_print()
    #print(map_name_select)
    if (is.null(map_name_select)) return()
    
    list_samples <- list.files("data/samples/")
    this_example <- sum(grepl(map_name_select, list_samples))
    
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
  
  
  # Vista de datos ----------------------------------------------------------
  
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
  

  dic_lflt <- reactive({
    req(inputData()())
    if (is.null(mapa_print())) return()
    lfltmagic::guess_ftypes(inputData()(), info_map$name)
  })


  output$dataset <- renderUI({
    req(dic_lflt())
    suppressWarnings(
      hotr("data_input", data = inputData()(), dic = dic_lflt(), options = list(height = 530))
    )
  })


  data_load <- reactive({
    req(inputData()())
    d <- input$data_input$data
    d
  })


  dic_load <- reactive({
    input$data_input$dic
  })





  # Tipo de datos -----------------------------------------------------------


  var_select <- reactiveValues(id_default = NULL, all_vars_data = NULL)

  observe({
    if(is.null(inputData()())) return()

    req(dic_load())
    d <- dic_load()
    geo_var <- grep("Gnm|Gcd", d$hdType)
    num_var <- grep("Num", d$hdType)


    if (!identical(geo_var, integer())) {
      v <-  sample(d$id[grep("Gnm|Gcd", d$hdType)], 1)
      if (!identical(num_var, integer())) {
        v <- c(v, sample(d$id[grep("Num", d$hdType)], 1))
      }
      var_select$id_default <- v
    } else {
      var_select$id_default <- NULL
    }

    var_select$all_vars_data <- set_names(d$id, d$label)

    if (sum(names(var_select$all_vars_data) %in% names(inputData()())) < 1) var_select$id_default <- NULL

  })


  output$var_selector <- renderUI({
    if (is.null(inputData()())) return(infomessage(HTML(i_("data_wait", lang = lang()))))
    if (is.null(var_select$all_vars_data)) return()

    # if (sum(names(var_select$all_vars_data) %in% names(inputData()())) < 1) return()
    var_s <- var_select$id_default


    selectizeInput("var_order",   div(class="title-data-select", i_("var_selector", lang())),
                   choices =  var_select$all_vars_data,
                   multiple = TRUE,
                   selected =  var_s,
                   options = list(plugins= list('remove_button', 'drag_drop')))

  })
  
  

  

  var_default <- reactiveValues(id = 0)
  observe({
    if (is.null(inputData()())) {
      var_default$id <- 0
    } else {
      var_default$id <- 1
    }

  })

  observe({
    if(var_default$id == 0) {
      updateSelectizeInput(session, "var_order", selected = character(0))
      session$sendCustomMessage(type = "resetValue", message = "var_order")
    }
  })
  # 
  var_plot <- reactive({
    var_sel <- input$var_order
    var_sel
  })

  

  dic_draw <- reactive({


    if (is.null(var_plot())) return()
    d <- dic_load()

    var_sel <- var_plot()

    d <- d[d$id %in% var_sel,]

    l_f <- strsplit(available_ftypes, "-")
    l_f <- map(l_f, function(i){
      paste0(sort(i), collapse = "-")
    })

    ind_ftype <- which(sapply(l_f, function(y){
      paste0(sort(d$hdType), collapse = "-") %in% y
    })
    )[1]
    ind <- strsplit(available_ftypes[ind_ftype], "-") %>% unlist()

    order <- union(ind, unique(d[["hdType"]]))
    d <- d[order(match(d[["hdType"]], order)), ]

    d
  })
  

  data_draw <- reactive({

    if (is.null(inputData()())) return()
    req(data_load())
    if (is.null(var_plot())) return()
    req(dic_draw())
    d <- data_load()[dic_draw()[["id"]]]
    names(d) <- dic_draw()[["label"]][match(dic_draw()[["id"]], names(d))]
    d
  })


  ftype_draw <- reactive({
    req(dic_draw())
    f_t <- paste0(dic_draw()$hdType, collapse = "-")
    if (!(f_t %in% available_ftypes)) return()
    f_t
  })
  
  output$aver <- renderPrint({
    ftype_draw()
  })
  

  output$info_dis <- renderUI({
    if (is.null(inputData()())) return()
    if (is.null(var_plot())) return(infomessage(HTML(i_("ftype_null", lang = lang()))))
    if (!is.null(ftype_draw())) return()
    infomessage(HTML(i_("ftype_ms", lang = lang())))
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
    req(possible_viz())
    suppressWarnings(
      buttonImageInput('viz_selection',
                       div(class="title-data-select",i_('viz_type', lang())),
                       images = possible_viz(),
                       path = 'img/svg/',
                       #format = 'svg',
                       active = actual_but$active)
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

    tooltip_info <- reactive({
      i_("tool_info", lang())
    })

    color_scale_original <- reactive({
      req (inputData()())
      "Num"
    })

    color_scale_original <- reactive({
      if (is.null(dic_draw())) return()
      vartype <- dic_draw()$hdType[2]
      vartype
    })

    color_scale_select <- reactive({
      if (is.null(input$map_color_scale)) return()
      input$map_color_scale
    })

    display_palette <- reactive({
      if (is.null(dic_draw())) return()
      display <- "sequential"
      if (color_scale_original() == "Cat" | color_scale_select() %in% c("Bins", "Quantile")){
        display <- "categorical"
      }
      display
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




    # opts y theme ------------------------------------------------------------
    opts_viz <- reactive({

      if (is.null(info_org$org)) return()
      opts_viz <- parmesan_input()
      if (is.null(opts_viz)) return()
      opts_viz <- opts_viz[setdiff(names(opts_viz), c('theme'))]
      opts_viz$logo <- info_org$org
      # if (input$numeric_palette_div) {
      #   opts_viz$palette_colors_sequential <- dsthemer_get(info_org$org, theme = input$theme, palette = "divergent")$palette_colors
      # }
      opts_viz %>% discard(is.null)
    })
    #
    theme_load <- reactive({
      theme_select <- input$theme
      #print(info_org$org)
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

    # colourPaletteChoices_sequential <- reactive({
    #   c("Accent", "Dark2", "Paired", "Pastel1",
    #     "Pastel2", "Set1", "Set2", "Set3", "Greys")
    # })
    #
    # colourPaletteChoices_divergent <- reactive({
    #   c("Accent", "Dark2", "Paired", "Pastel1",
    #     "Pastel2", "Set1", "Set2", "Set3", "Greys")
    # })

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
      if (is.null(input$border_weigth)) return(FALSE)
      bw <- input$border_weight
      bc <- TRUE
      if (bw == 0) bc <- FALSE
      bc
    })
    #
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
      f_t <- ftype_draw()

      if (is.null(f_t) || f_t == "") f_t <- "Gnm-Num"
      geotype <- gsub("-", "", f_t)
      if (is.null(actual_but$active)) return()
      gtype <- actual_but$active
      typeV <- paste0('lflt_', gtype, '_', geotype)
      typeV
    })

      lftl_viz <- reactive({

        req(data_draw())
        if (is.null(mapa_print())) return()
        map_select <- mapa_print()

        # browser()
        viz <- do.call(viz_name(), c(list(data = data_draw(),
                                          map_name = map_select,
                                          opts = c(opts_viz(), theme_draw())
        )))
        viz
      })
      #
      output$view_lftl_viz <- renderLeaflet({
        req(lftl_viz())
        suppressWarnings(
          lftl_viz()
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
                         errorMessage = i_("error_down", lang()),
                         modalFunction = pin_, reactive(lftl_viz()),
                         bkt = url_par()$inputs$user_name)
      })
      

}

shinyApp(ui, server)