#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import dplyr
#' @import shinyjs
#' @noRd

# list loupe image paths to filter from
loupe_image_paths <- tibble("LoupeImages" = list.files(path = "www/LoupeImages/"))
# Load Orascoptic loupe data
lumadent_data <- readxl::read_excel("data/Lumadent_loupe_data.xlsx")
# Load dental data

dental_data <- readxl::read_excel("data/Dental_data.xlsx") %>%
  filter(`Laser Mfg` != "") %>%
  mutate(VLT = scales::percent(as.numeric(VLT)))

app_server <- function(input, output, session) {
  # Your application server logic
  observeEvent(input$mfg,{
    # filter dental data to select mfg
    mfg_filtered_dental_data <- dental_data %>%
      filter(`Laser Mfg` == input$mfg)
    # update select input - laser model
    updateSelectInput(inputId = "mod",
                      choices = sort(mfg_filtered_dental_data$`Laser Model`))
  })
  loupe_insert <- eventReactive(c(input$loupestyle, input$style),{
    lumadent_data %>%
      filter(`Lumadent Frame` == input$loupestyle) %>%
      filter(`Style` == input$style)
  })

  selected_data <- eventReactive(input$mod,{
    req(input$mfg)
    dental_data %>%
      filter(`Laser Mfg` == input$mfg,
             `Laser Model` == input$mod)
  })
  observeEvent(input$run,{
    shinyjs::hide(id = "run", anim = T, animType = "fade")
  })
  user_info <- eventReactive(c(input$mod, input$loupestyle, input$style),{
    tibble(
      "Loupe Style" = paste0(loupe_insert()$`Lumadent Frame`, " ", loupe_insert()$`Style`),
      "Device" = glue::glue_safe(selected_data()$`Laser Mfg`, " ", selected_data()$`Laser Model`),
      "Device Specs" = selected_data()$Wavelengths)
  })

  output$userInfo <- renderTable(bordered = T,
                                 align = "l",
                                 striped=T,
                                 hover = T,
                                 {
                                   user_info()
                                 })
  output$mailToLink <- renderUI({
    req(selected_data())
    req(input$run)
    HTML(
      c('<div class="shadow p-3 mb-5 bg-body rounded">
      <h4>
      <p>If you would like to order laser eye protection, then click the link below to email us.
      </p>
      </h4>
      <h1>
      <a href="mailto:john@innovativeoptics.com
        ?subject=New Order - Lumadent - ',
        Sys.time(),
        '&body=This email is prepopulated with data currently entered in the app. Hello, I have the Lumadent',
        user_info()$`Loupe Style`,
        'loupes. I use the',
        user_info()$`Device`,
        'device',
        user_info()$`Device Specs`,
        'and I would like to place an order for the',
        table_info()$`INVO Part Number`,
        'loupe insert as was recommended by the app. Please, confirm this order with a reply to this email as soon as possible."
        >Place an Order</a>
        </h1>
        <h4>
      <p>If a pre-populated email does not appear for you to send to the laser safety manufacturer,
      then call at (763)425-7789 and we will assist you immediately, during normal business hours.
      </p>
      </h4>
      </div>'
      )
    )

  })

  table_info <- eventReactive(c(input$mod, input$loupestyle, input$style),{
    tibble("INVO Part Number" = if_else(selected_data()$`Eyewear Lens Compatible` == "Gi1",
                                              glue::glue_safe(loupe_insert()$`Innovative Optics Insert`,"." , selected_data()$`Eyewear Lens Compatible`),
                                              glue::glue_safe(loupe_insert()$`Innovative Optics Insert`,"." , selected_data()$`Eyewear Lens Compatible`, ".2B")),
                 "Optical Density Specs" = selected_data()$`Optical Density`,
                 "% VLT" = selected_data()$VLT)
  })

  output$tableInfo <- renderTable(bordered = T,
                                  align = "l",
                                  striped=T,
                                  hover = T,
                                  {
                                    table_info()
                                  })

  rec1_table <- eventReactive(input$mod,{
    tibble("INVO Part Number" = selected_data()$`Rec1`)
  })

  output$tableRec1 <- renderTable(bordered = T,
                                  align = "l",
                                  striped=T,
                                  hover = T,
                                  {
                                    rec1_table()
                                  })

  rec2_table <- eventReactive(input$mod,{
    tibble("INVO Part Number" = selected_data()$`Rec2`)
  })

  output$tableRec2 <- renderTable(bordered = T,
                                  align = "l",
                                  striped=T,
                                  hover = T,
                                  {
                                    rec2_table()
                                  })

  rec3_table <- eventReactive(input$mod,{
    tibble("INVO Part Number" = selected_data()$`Rec3`)
  })

  output$tableRec3 <- renderTable(bordered = T,
                                  align = "l",
                                  striped=T,
                                  hover = T,
                                  {
                                    rec3_table()
                                  })

  image_location <- eventReactive(c(input$mod, input$loupestyle),{
    req(input$run)
      loupe_rec <- loupe_image_paths %>%
        filter(stringr::str_detect(loupe_image_paths$LoupeImages, sub(" ", "",  input$loupestyle)) &
                 stringr::str_detect(loupe_image_paths$LoupeImages, stringr::coll(paste0(selected_data()$`Eyewear Lens Compatible`, "."))
                 )
        )

      c(glue::glue_safe("www/LoupeImages/", loupe_rec$LoupeImages[[2]]),
        if_else(selected_data()$`Eyewear Lens Compatible` == "Pi19",
                glue::glue_safe("www/recs/", selected_data()$`Rec1`, ".jpeg"),
                glue::glue_safe("www/recs/", selected_data()$`Rec1`, ".jpg")
        ),
        if_else(selected_data()$`Eyewear Lens Compatible` == "Pi19",
                glue::glue_safe("www/recs/", selected_data()$`Rec2`, ".jpeg"),
                glue::glue_safe("www/recs/", selected_data()$`Rec2`, ".jpg")
        ),
        glue::glue_safe("www/recs/", selected_data()$`Rec3`, ".jpg"),
        glue::glue_safe("www/LoupeImages/", loupe_rec$LoupeImages[[1]]))

  })
  output$productImageF <- renderImage({
    list(src = image_location()[[1]],
         width = "100%",
         contentType = "image/png")
  }
  ,deleteFile = FALSE)

  output$productImageB <- renderImage({
    list(src = image_location()[[5]],
         width = "100%",
         contentType = "image/png")
  }
  ,deleteFile = FALSE)

  output$rec1 <- renderImage({
    list(src = image_location()[[2]],
         height = "90px",
         contentType = "image/png")
  }
  ,deleteFile = FALSE)

  output$rec2 <- renderImage({
    list(src = image_location()[[3]],
         height = "90px",
         contentType = "image/png")
  }
  ,deleteFile = FALSE)

  output$rec3 <- renderImage({
    list(src = image_location()[[4]],
         height = "90px",
         contentType = "image/png")
  }
  ,deleteFile = FALSE)

}
