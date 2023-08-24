#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bslib
#' @import dplyr
#' @noRd

# Load Lumadent loupe data
lumadent_data <- readxl::read_excel("data/Lumadent_loupe_data.xlsx")

# Load dental data

dental_data <- readxl::read_excel("data/Dental_data.xlsx") %>%
  filter(`Laser Mfg` != "") %>%
  mutate(VLT = scales::percent(as.numeric(VLT)))


app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    page_fluid(
      shinyjs::useShinyjs(),
      card(class="shadow p-3 mb-5 bg-body rounded",
           card_header(inverse = T,
                       fluidRow(
                         column(6,
                                align = 'left',
                                a(href = "https://www.lumadent.com/",
                                  img(src = "https://uploads-ssl.webflow.com/642bc00aa11863508034d79d/64c3a1d8eb00d38a837fe346_Lumadent-logo.svg",
                                      width = "270px"))
                         ),
                         column(6, align= 'right',

                                h5(a("Contact Lumadent", href= "https://www.lumadent.com/sales-representatives")),
                                h5("+1 775-829-4488")

                         ))),
           fluidRow(
             column(
               3,
               align = 'center',
               selectInput(
                 inputId = "loupestyle",
                 label = h5(style = {
                   "color: #004793;"
                 },
                 strong("Lumadent Frame")),
                 choices = sort(lumadent_data$`Lumadent Frame`),
                 selected = 1
               )
             ),
             column(
               3,
               align = 'center',
               selectInput(
                 inputId = "style",
                 label = h5(style = {
                   "color: #004793;"
                 },
                 strong("Loupe Style")),
                 choices = sort(lumadent_data$`Style`),
                 selected = 1
               )
             ),
             column(
               3,
               align = 'center',
               selectInput(
                 inputId = "mfg",
                 label = h5(style = {
                   "color: #004793;"
                 },
                 strong("Laser Manufacturer")),
                 choices = sort(dental_data$`Laser Mfg`),
                 selected = 1
               )
             ),
             column(
               3,
               align = 'center',
               selectInput(
                 inputId = "mod",
                 label = h5(style = {
                   "color: #004793;"
                 },
                 strong("Laser Model")),
                 choices = dental_data$`Laser Model`,
                 multiple = F
               )
             )),
           fluidRow(column(12,align='center',
                           h4("Search laser eye protection by
                                              selecting a loupe style and laser device"))),
           fluidRow(

             column(
               12,
               align = "center",

               actionButton("run",
                            icon = icon("magnifying-glass"),
                            style='
                       padding-left:50px;padding-right:50px;
                       padding-top:6px;
                       padding-bottom:1px;
                       font-size:80%',
                            h5(strong("Search")),
                            class = "btn-primary"))
           ),
           conditionalPanel(
             condition = "input.run",
             fluidRow(column(12, align = "center",
                             h3(style = {
                               "color: #004793;"
                             },
                             em("Selected Information")),
                             tableOutput("userInfo")
             )),

             fluidRow(column(12,
                             align = "center",
                             h3(style = {
                               "color: #004793;"
                             },
                             em("Compatible Innovative Optics Product")),
                             tableOutput("tableInfo"))),

             fluidRow(column(6, align = 'center',
                             imageOutput("productImageF",
                                         height = "75%")),
                      column(6, align = 'center',
                             imageOutput("productImageB",
                                         height = "75%"))),

             fluidRow(column(12,
                             align = 'center',
                             h3(style = {
                               "color: #004793;"
                             },
                             em("Frequently Purchased Together")))),
             fluidRow(
               column(4,
                      align = 'center',
                      card(class="shadow p-3 mb-5 bg-body rounded",
                           h5("Fit-over Glasses"),
                           imageOutput("rec1",
                                       height = "100%"),
                           tableOutput("tableRec1"))),
               column(4,
                      align = 'center',
                      card(class="shadow p-3 mb-5 bg-body rounded",
                           h5("Form-fit Glasses"),
                           imageOutput("rec2",
                                       height = "100%"),
                           tableOutput("tableRec2"))),
               column(4,
                      align = 'center',
                      card(class="shadow p-3 mb-5 bg-body rounded",
                           h5("Patient Goggles"),
                           imageOutput("rec3",
                                       height = "100%"),
                           tableOutput("tableRec3")))
             ),
             fluidRow(column(12,
                             align = 'left',
                             h1(
                               htmlOutput("mailToLink"))
             ))
           ),

           fluidRow(
             column(12,
                    p(icon("info-circle"),
                      "Your information not available in the dropdowns?
                               Contact one of our certified medical laser safety officers at (763)425-7789"))
           ),
           card_footer(h5(
             "Powered by Innovative Optics"))))
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Lumadent Laser Safety"
    ),
    tags$link(href="https://uploads-ssl.webflow.com/642bc00aa11863508034d79d/css/refractives.webflow.3cbf59264.min.css", rel="stylesheet", type="text/css")
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
