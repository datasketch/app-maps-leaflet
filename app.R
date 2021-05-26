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

frtypes_doc <- suppressWarnings(yaml::read_yaml("conf/frtypes.yaml"))
available_ftypes <- names(frtypes_doc)
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
          
          # uiOutput("info_dis"),
          # uiOutput("var_selector"),
          withLoader(uiOutput("dataset"), type = "image",loader = "img/loading_gris.gif")
        )),
  panel(title = ui_("edit_viz"),
        color = "chardonnay",
        width = 350,
        body = div(
          #verbatimTextOutput("aver"),
          verbatimTextOutput("printest"),
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
  })
  
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
  
  # geo ftype ---------------------------------------------------------------
  
   ftype <- reactive({
     req(dic_load())
     ftype <- NULL
     if (is.null(find_plotvars())) ftype <- "Gnm-Num"
     dic_p <- dic_load()
     dic_p <- dic_p[,find_plotvars()]
     ftype <- paste0(dic_p$hdType, collapse = "-")
     ftype
   })
  
  
  
  output$printest <- renderPrint({
    list(dic_load(),
         ftype()
    )
  })
  
  
}

shinyApp(ui, server)