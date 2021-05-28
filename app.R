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
library(shinydisconnect)
library(shinybusy)

data_samples <- suppressWarnings( yaml::read_yaml("data/data-samples.yaml"))
maps_with_labels <- yaml::read_yaml(file = "conf/map_with_levels.yaml")

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
  shinypanels::modal(id = 'modal_plan_controls', title = ui_("modal_upgrade_title"), uiOutput("message_modal_controls")),
  shinypanels::modal(id = 'modal_plan_rows', title = ui_("modal_upgrade_title"), uiOutput("message_modal_rows")),
  useShi18ny(),
  tags$script("
    Shiny.addCustomMessageHandler('resetValue', function(variableName) {
      Shiny.onInputChange(variableName, null);
    });
  "),
  disconnectMessage(
    text = "Tu sesión ha finalizado, si tienes algún problema trabajando con la app por favor contáctanos y cuéntanos qué ha sucedido // Your session has ended, if you have any problem working with the app please contact us and tell us what happened.",
    refresh = "REFRESH",
    background = "#ffffff",
    colour = "#435b69",
    size = 14,
    overlayColour = "#2a2e30",
    overlayOpacity = 0.85,
    refreshColour = "#ffffff",
    css = "padding: 4.8em 3.5em !important; box-shadow: 0 1px 10px 0 rgba(0, 0, 0, 0.1) !important;"
  ),
  busy_start_up(
    loader = tags$img(
      src = "img/loading_gris.gif",
      width = 100
    ),
    #text = "Loading...",
    mode = "auto",
    #timeout = 3500,
    color = "#435b69",
    background = "#FFF"
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
          uiOutput("map_select"),
          uiOutput("user_ftype"),
          # uiOutput("info_dis"),
          uiOutput("var_selector"),
          withLoader(uiOutput("dataset"), type = "image",loader = "img/loading_gris.gif")
        )),
  panel(title = ui_("edit_viz"),
        color = "chardonnay",
        width = 350,
        body = div(
          #verbatimTextOutput("printest"),
          uiOutput("controls"))
  ),
  panel(title =  ui_("view_viz"),
        title_plugin = uiOutput("download"),
        #downloadImageUI("down_lfltmagic", "Download", c("html", "png", "jpeg", "pdf"), display = "dropdown"),
        color = "chardonnay",
        can_collapse = FALSE,
        body = div(
          uiOutput("error_map"),
          leafletOutput("view_lftl_viz")
        ),
        footer = uiOutput("viz_icons"))
)

server <- function(input, output, session) {
  
  # Idiomas -----------------------------------------------------------------
  
  i18n <- list(defaultLang = "en",
               availableLangs = c("en", "es")#,
               #localeDir = "locale/",  
               #customTranslationSource = "csv"
  )
  
  lang <- callModule(langSelector, "lang", i18n = i18n, showSelector = FALSE)
  
  observeEvent(lang(),{
    uiLangUpdate(input$shi18ny_ui_classes, lang())
  })
  
  output$message_modal_rows <- renderUI({
    div(
      p( style = "margin-bottom: 30px;margin-top: 15px;",
         i_("modal_upgrade_text_rows", lang = lang())),
      
      shiny::actionButton(inputId='url_upgrade', label= i_("modal_upgrade_button", lang()),
                          onclick ="window.open('https://datasketch.co/dashboard/upgrade', '_blank')")
    )
  })
  
  output$message_modal_controls <- renderUI({
    div(
      p( style = "margin-bottom: 30px;margin-top: 15px;",
         i_("modal_upgrade_text_controls", lang = lang())),
      
      shiny::actionButton(inputId='url_upgrade', label= i_("modal_upgrade_button", lang()), 
                          onclick ="window.open('https://datasketch.co/dashboard/upgrade', '_blank')")
    )
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
  
  par <- list(map_name = "world_countries", user_name = "test", org_name = NULL, plan = "basic")
  url_par <- reactive({
    url_params(par, session)
  })
  
  
  map_id <- reactive({
    mp <- url_par()$inputs$map_name
    if (is.null(mp)) mp <- "world_countries"
    if (grepl("-", mp)) {
      mp <- stringr::str_split(mp, pattern = "-") %>% unlist()
    }
    mp
  })
  
  
  # selector map if there is more than one map in the url -------------------
  
  observe({
    if (length(map_name()) == 1) return()
    output$map_select <- renderUI({
      map_opts <- map_id()
      map_opts <- setNames(map_opts, i_(map_opts, lang()))
      selectizeInput("id_map", i_("map_available", lang()), choices = map_opts)
    })
  })
  
  
  map_name <- reactive({
    if (length(map_id()) <= 1) {
      r <- map_id()
    } else {
      r <- input$id_map
    }
    r
  })
  
  
  # Data available ----------------------------------------------------------
  
  data_available <- reactive({
    mp_url <- map_name()
    lang <- lang()
    real_data <- mp_url %in% names(data_samples)
    if (real_data) {
      files <- data_samples[[mp_url]][[lang]]
      file <- purrr::map(seq_along(files), function(f) {
        r <- files[[f]]
        out <- r$path
        names(out) <- r$label
        out
      }) %>% unlist()
    } else {
      file <- purrr::map(mp_url, function(mp) {
        out <- lfltmagic::fakeData(map_name = mp)
        out
      })
      names(file) <- i_(mp_url, lang)
    }
    file
  })
  
  
  
  # call data module --------------------------------------------------------
  
  
  labels <- reactive({
    
    lang <- lang()
    if (lang == "") return()
    #req(data_fake())
    files <- data_available()
    
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
  
  dic_lflt <- reactive({
    req(inputData()())
    req(map_name())
    lfltmagic::guess_ftypes(inputData()(), map_name())
  })
  
  
  output$dataset <- renderUI({
    req(dic_lflt())
    suppressWarnings(
      hotr("data_input", data = inputData()(), dic = dic_lflt(), options = list(height = 530))
    )
  }) %>%
    bindCache(inputData()(), dic_lflt)
  
  data_load <- reactive({
    req(inputData()())
    d <- input$data_input$data
    d
  })
  
  
  dic_load <- reactive({
    input$data_input$dic
  })
  
  req_levels <- reactive({
    map_name() %in% maps_with_labels
  })
  
  
  
  # ploting var -------------------------------------------------------------
  
  find_plotvars <- reactive({
    if (is.null(dic_load())) return() # en este caso el mapa es vacio, entonces data es null y el ftype por default es gnmnum
    dic_p <- dic_load()
    #dic_p <- data.frame(id = c("a", "b"), label = c("uno", "dos"), hdType = c("Gcd", "Cat"))
    have_geo <- grepl("Gcd|Gnm", paste0(dic_p$hdType, collapse = "-"))
    if (!have_geo) return() # se debe mostrar mensaje de advertencia en el q se señala q no se encuentra información geografica que la ponga en los cuadrados
    geovar <- NULL
    idgeo <- which(dic_p$hdType %in% c("Gcd", "Gnm"))
    if (req_levels()) {
      if (length(idgeo)< 2) return() # en este caso cuando se requieren subniveles deben existir dos columnas con la información geografica
      geovar <- idgeo[1:2]
    } else {
      geovar <- idgeo[1]
    }
    catvar <- NULL
    idcat <- which(dic_p$hdType %in% "Cat")
    if (!identical(idcat, integer())) catvar <- idcat[1]
    numvar <- NULL
    idnum <- which(dic_p$hdType %in% "Num")
    if (!identical(idnum, integer())) numvar <- idnum[1]
    select_var <- c(geovar, catvar, numvar)
    select_var
  })
  
  
  var_select <- reactiveValues(id_default = NULL, all_vars_data = NULL)
  
  observe({
    if (is.null(data_load())) return()
    if (is.null(find_plotvars())) {
      var_select$id_default <- NULL
    } else {
      index <- find_plotvars()
      dic_f <- dic_load()[index,]
      var_select$id_default <- setNames(dic_f$id, dic_f$label)
    }
    var_select$all_vars_data <- setNames( dic_load()$id, dic_load()$label)
  })
  
  # var selector ------------------------------------------------------------
  
  output$var_selector <- renderUI({
    if (is.null(data_load())) return()
    var_s <-  var_select$id_default
    if (is.null(var_s)) var_s <- input$geovar_user
    selectizeInput("var_order",   div(class="title-data-select", i_("var_selector", lang())),
                   choices =  var_select$all_vars_data,
                   multiple = TRUE,
                   selected =  var_s,
                   options = list(plugins= list('remove_button', 'drag_drop')))
    
  })
  
  # si ftype no contiene gnm o gcd sale un selector donde el usuario debe elegir el tipo de dato
  observe({
    if (is.null(data_load())) return()
    if (is.null(var_select$id_default)) {
      output$user_ftype <- renderUI({
        if (!is.null(find_plotvars())) return()
        div(
          selectizeInput("geovar_user",
                         "No podemos encontrar la variable con el detalle geografico, por favor indica cual es",
                         choices = setNames( dic_load()$id, dic_load()$label),
                         multiple = TRUE,
                         selected = NULL,
                         options = list(plugins= list('remove_button', 'drag_drop'), maxItems = 1)),
          selectizeInput("geoinfo_user", "Indica que tipo de variable es", setNames(c("Gnm", "Gcd"), c("Name", "Code")))
        )
      })
    }
  })
  
  
  # dic to find ftype -------------------------------------------------------
  dic_plot <- reactive({
    if (is.null(input$var_order)) return()
    if (is.null(dic_load())) return()
    dic_plot <- dic_load() %>% filter(id %in% input$var_order)
    dic_plot
  })
  
  
  # data plot ---------------------------------------------------------------
  
  data_plot <- reactive({
    #if (is.null(input$var_order)) return()
    #index <- input$var_order
    if (is.null(dic_load())) return()
    if (is.null(dic_plot())) return()
    index <- dic_plot()$id
    index_add <- setdiff(names(data_load()), index)
    if (identical(index_add, integer())) index_add <- NULL
    index <- c(index, index_add)
    dp <- data_load()[,index]
    dic_l <- data.frame(id = names(dp)) %>% left_join(dic_load())
    names(dp) <- dic_l$label
    dp
  })
  
  
  # geo ftype ---------------------------------------------------------------
  
  ftype <- reactive({
    ftype <- NULL
    dic_p <- dic_plot()
    if (is.null(dic_p)) {
      ftype <- "GnmNum"
    } else {
      if (!is.null(input$geovar_user)) {
        dic_p$hdType[dic_p$id == input$geovar_user] <- input$geoinfo_user
      }
      dic_p <- dic_p$hdType
      if (length(dic_p) > 3) dic_p[1:3]
      ftype <- paste0(dic_p, collapse = "")
    }
    ftype
  })
  
  
  
  
  
  output$viz_icons <- renderUI({
    suppressWarnings(
      buttonImageInput('viz_selection',
                       div(class="title-data-select",i_('viz_type', lang())),
                       images = c("choropleth", "bubbles"),
                       path = 'img/svg/')
    )
  })
  
  
  
  # output$printest <- renderPrint({
  #   list(
  #     # dic_load(),
  #     tile_by_theme(),
  #     dic_plot(),
  #     #    data_plot(),
  #     ftype(),
  #     input$geovar_user,
  #     find_plotvars(),
  #     input$var_order
  #   )
  # })
  
  
  # Parmesan
  parmesan <- parmesan_load()
  parmesan_input <- parmesan_watch(input, parmesan)
  parmesan_lang <- reactive({
    i_(parmesan, lang(), keys = c("label", "choices", "text"))
  })
  output_parmesan("controls", parmesan = parmesan_lang,
                  input = input, output = output, session = session,
                  env = environment())
  
  #
  theme_load <- reactive({
    theme_select <- input$theme
    #print(info_org$org)
    if (is.null(theme_select)) return()
    orgName <- url_par()$inputs$org_name %||% "public"
    if (! orgName %in% dsthemer::dsthemer_list()) orgName <- "public"
    th <- dsthemer_get(orgName, theme = theme_select)
    if (is.null(th)) return()
    th
  })
  
  
  background <- reactive({
    req(theme_load())
    theme_load()$background_color
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
  tooltip_info <- reactive({
    i_("tool_info", lang = lang())
  })
  
  
  map_tiles_info <- reactive({
    i_("tiles_info", lang())
  })
  
  
  
  # render Options changed in parmesan --------------------------------------
  
  
  opts_viz <- reactive({
    
    orgName <- url_par()$inputs$org_name %||% "public"
    if (! orgName %in% dsthemer::dsthemer_list()) orgName <- "public"
    opts_viz <- parmesan_input()
    if (is.null(opts_viz)) return()
    opts_viz <- opts_viz[setdiff(names(opts_viz), c('theme'))]
    opts_viz$logo <- orgName 
    opts_viz
  })
  
  # Render mapa -------------------------------------------------------------
  
  viz_name <- reactive({
    gtype <- ftype()
    if (is.null(gtype)) return()
    plot_it <- input$viz_selection
    typeV <- paste0('lflt_', plot_it, '_', gtype)
    typeV
  })
  
  lftl_viz <- reactive({
    
    if (is.null(map_name())) return()
    
    tryCatch({ 
      do.call(viz_name(), c(list(data = data_plot(),
                                 map_name = map_name(),
                                 opts = c(opts_viz(), theme_draw()
                                 )
      )))
    }, 
    error = function(e){
      return()
    })
  })
  #
  output$view_lftl_viz <- renderLeaflet({
    lftl_viz()
  })
  
  output$error_map <- renderUI({
    if (is_null(data_plot())) return()
    if (!is.null(lftl_viz())) return()
    tx <- 'Asegurese que el geocode o geoname ingresado coincida con los nombres o el codigo de map_name'
  })
  
  
  
  
  
}

shinyApp(ui, server)