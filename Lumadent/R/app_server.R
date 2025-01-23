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

options(
  gargle_oauth_email = TRUE,
  gargle_oauth_cache = ".secrets"
)

googledrive::drive_auth(cache = ".secrets", email = "innovativeopticsdatabase@gmail.com")
googlesheets4::gs4_auth(cache = ".secrets", email = "innovativeopticsdatabase@gmail.com")


sheet_id <- googledrive::drive_get("Dental_data")$id


# Load Lumadent loupe data
# lumadent_data <- readxl::read_excel("data/Lumadent_loupe_data.xlsx")

#lumadent_data <- readxl::read_excel("data/Dental_data.xlsx",
#                                    sheet = "Loupe_types") %>%
#  filter(`Mfg` == "Lumadent") %>%
#  rename(`Lumadent Frame` = Mod, Style = Size, `Innovative Optics Insert` = `Insert Part Number`) %>%
#  filter(!`Lumadent Frame` %in% c("Argon", "Standard")) %>%
#  select(-Mfg)

lumadent_data <- googlesheets4::read_sheet(sheet_id, sheet = "Loupe_types", col_types = "c")  %>%
  filter(`Mfg` == "Lumadent" & `Mod` != "Standard") %>%
  rename(`Lumadent Frame` = Mod, Style = Size, `Innovative Optics Insert` = `Insert Part Number`)


# Load lens sheet from dental_data file
#lens_data <- readxl::read_excel("data/Dental_data.xlsx", sheet = "Lens_details") %>%
#  select(-VLT)
lens_data <- googlesheets4::read_sheet(sheet_id, sheet = "Lens_details") %>%
  select(-VLT)


#dental_data <- readxl::read_excel("data/Dental_data.xlsx") %>%
#  filter(`Laser Mfg` != "") %>%
#  mutate(VLT = scales::percent(as.numeric(VLT))) %>%
#  left_join(lens_data, by = join_by(`Eyewear Lens Compatible` == Lens))

#dental_data <- readxl::read_excel("data/Dental_data.xlsx") %>%
#  filter(`Laser Mfg` != "") %>%
#  select(-Website) %>%
#  mutate(VLT = scales::percent(as.numeric(VLT))) %>%
#  left_join(lens_data, by = join_by(`Eyewear Lens Compatible` == Lens))

dental_data <- googlesheets4::read_sheet(sheet_id, sheet = "laser_info", col_types = "c") %>%
  filter(`Laser Mfg` != "") %>%
  select(-Website) %>%
  #mutate(VLT = scales::percent(as.numeric(VLT))) %>%
  left_join(lens_data, by = join_by(`Eyewear Lens Compatible` == Lens))

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


  observeEvent(input$loupestyle,{
    # filter dental data to select loupestyle
    filtered_lumadent_data <- lumadent_data  %>%
      filter(`Lumadent Frame` == input$loupestyle)
    # update select input - style
    updateSelectInput(inputId = "style",
                      choices = sort(filtered_lumadent_data$`Style`))
  })


  loupe_insert <- eventReactive(c(input$loupestyle, input$style, input$mfg, input$mod),{

    lumadent_data %>%
      filter(`Lumadent Frame` == input$loupestyle) %>%
      filter(`Style` == input$style)
  })


  selected_data <- reactive({
    req(input$mfg)
    req(input$mod)
    req(loupe_insert())

    result <- dental_data %>%
      filter(`Laser Mfg` == input$mfg,
              `Laser Model` == input$mod) %>%
      DentalLibrary::generate_lens_link(loupe_insert = loupe_insert())

#    result <- dental_data %>%
#      filter(`Laser Mfg` == input$mfg,
#             `Laser Model` == input$mod) %>%
#      mutate(`INVO Part Number Raw` = if_else(`Eyewear Lens Compatible` == "Gi1",
#                                              glue::glue_safe(loupe_insert()$`Innovative Optics Insert`,"." , `Eyewear Lens Compatible`),
#                                              glue::glue_safe(loupe_insert()$`Innovative Optics Insert`,"." , `Eyewear Lens Compatible`, ".2B")),
#             `Website`= case_when(loupe_insert()$`Innovative Optics Insert` %in% c("IVL") & `Eyewear Lens Compatible` == "Pi1" ~ "https://innovativeoptics.com/product/pi1-inview-large-laser-clip-in/",
#                                  loupe_insert()$`Innovative Optics Insert` %in% c("IVL.R") & `Eyewear Lens Compatible` == "Pi1" ~ "https://innovativeoptics.com/product/ivl-r-pi1-laser-insert-for-loupes/",
#                                  loupe_insert()$`Innovative Optics Insert` %in% c("IVR") & `Eyewear Lens Compatible` == "Pi1" ~ "https://innovativeoptics.com/product/pi1-inview-large-laser-clip-in/",
#                                  loupe_insert()$`Innovative Optics Insert` %in% c("IVR.R") & `Eyewear Lens Compatible` == "Pi1" ~ "https://innovativeoptics.com/product/ivr-r-pi1-laser-insert-for-loupes/",
#                                  loupe_insert()$`Innovative Optics Insert` %in% c("IVL") & `Eyewear Lens Compatible` == "Pi17" ~ "https://innovativeoptics.com/product/pi17-inview-large-laser-clip-in/",
#                                  loupe_insert()$`Innovative Optics Insert` %in% c("IVL.R") & `Eyewear Lens Compatible` == "Pi17" ~ "https://innovativeoptics.com/product/ivl-r-pi17-laser-insert-for-loupes/",
#                                  loupe_insert()$`Innovative Optics Insert` %in% c("IVL") & `Eyewear Lens Compatible` == "Pi19" ~ "https://innovativeoptics.com/product/pi19-inview-large-laser-clip-in/",
#                                  loupe_insert()$`Innovative Optics Insert` %in% c("IVL.R") & `Eyewear Lens Compatible` == "Pi19" ~ "https://innovativeoptics.com/product/ivl-r-pi19-laser-insert-for-loupes/",
#                                  loupe_insert()$`Innovative Optics Insert` %in% c("IVR") & `Eyewear Lens Compatible` == "Pi19" ~ "https://innovativeoptics.com/product/pi19-inview-large-laser-clip-in/",
#                                  loupe_insert()$`Innovative Optics Insert` %in% c("IVR.R") & `Eyewear Lens Compatible` == "Pi19" ~ "https://innovativeoptics.com/product/ivr-r-pi19-laser-insert-for-loupes/",
#                                  loupe_insert()$`Innovative Optics Insert` %in% c("IVL") & `Eyewear Lens Compatible` == "Pi23" ~ "https://innovativeoptics.com/product/pi23-inview-large-laser-clip-in/",
#                                  loupe_insert()$`Innovative Optics Insert` %in% c("IVL.R") & `Eyewear Lens Compatible` == "Pi23" ~ "https://innovativeoptics.com/product/ivl-r-pi23-laser-insert-for-loupes/",
#                                  loupe_insert()$`Innovative Optics Insert` %in% c("IVR") & `Eyewear Lens Compatible` == "Pi23" ~ "https://innovativeoptics.com/product/pi23-inview-large-laser-clip-in/",
#                                  loupe_insert()$`Innovative Optics Insert` %in% c("IVR.R") & `Eyewear Lens Compatible` == "Pi23" ~ "https://innovativeoptics.com/product/ivr-r-pi23-laser-insert-for-loupes/",
#                                  loupe_insert()$`Innovative Optics Insert` %in% c("IVR.R") & `Eyewear Lens Compatible` == "Pi23" ~ "https://innovativeoptics.com/product/ivr-r-pi23-laser-insert-for-loupes/",
#                                  loupe_insert()$`Innovative Optics Insert` %in% c("IVL") & `Eyewear Lens Compatible` == "Gi1" ~ "https://innovativeoptics.com/product/gi1-inview-large-laser-clip-in/",
#                                  loupe_insert()$`Innovative Optics Insert` %in% c("IVL.R") & `Eyewear Lens Compatible` == "Gi1" ~ "https://innovativeoptics.com/product/ivl-r-gi1-laser-insert-for-loupes/",
#                                  loupe_insert()$`Innovative Optics Insert` %in% c("IVR") & `Eyewear Lens Compatible` == "Gi1" ~ "https://innovativeoptics.com/product/gi1-inview-regular-laser-clip-in/",
#                                  loupe_insert()$`Innovative Optics Insert` %in% c("IVR.R") & `Eyewear Lens Compatible` == "Gi1" ~ "https://innovativeoptics.com/product/gi1-inview-regular-laser-clip-in/",
#                                  .default = Website)
#      ) %>%
#      mutate(`INVO Part Number` = glue::glue_safe("<a href='{Website}' target ='_blank'> {INVO Part Number Raw} </a> "))

    print(result$`INVO Part Number`)
    result
  })

  observeEvent(input$run,{
    shinyjs::hide(id = "run", anim = T, animType = "fade")
  })
  user_info <- eventReactive(c(input$loupestyle, input$style, input$mfg, input$mod),{

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
      <a href="mailto:sales@innovativeoptics.com
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


  table_info <- eventReactive(c(input$loupestyle, input$style, input$mfg, input$mod),{
    tibble("INVO Part Number" = selected_data()$`INVO Part Number`,
           "Optical Density Specs" = selected_data()$`Optical Density`,
           "% VLT" = selected_data()$VLT)

  })

  output$tableInfo <- renderTable(bordered = T,
                                  align = "l",
                                  striped=T,
                                  hover = T,
                                  {
                                    table_info()

                                  }, sanitize.text.function = function(x) x)

  rec1_table <- eventReactive(c(input$loupestyle, input$style, input$mfg, input$mod),{

    tibble("INVO Part Number" = selected_data()$`Rec1`)
  })

  output$tableRec1 <- renderTable(bordered = T,
                                  align = "l",
                                  striped=T,
                                  hover = T,
                                  {
                                    rec1_table()
                                  })


  rec2_table <- eventReactive(c(input$loupestyle, input$style, input$mfg, input$mod),{

    tibble("INVO Part Number" = selected_data()$`Rec2`)
  })

  output$tableRec2 <- renderTable(bordered = T,
                                  align = "l",
                                  striped=T,
                                  hover = T,
                                  {
                                    rec2_table()
                                  })

  rec3_table <- eventReactive(c(input$loupestyle, input$style, input$mfg, input$mod),{

    tibble("INVO Part Number" = selected_data()$`Rec3`)
  })

  output$tableRec3 <- renderTable(bordered = T,
                                  align = "l",
                                  striped=T,
                                  hover = T,
                                  {
                                    rec3_table()
                                  })


  image_location <- eventReactive(c(input$loupestyle, input$style, input$mfg, input$mod),{
    req(input$run)
    loupe_rec <- loupe_image_paths %>%
      filter(stringr::str_detect(loupe_image_paths$LoupeImages, sub(" ", "",  input$loupestyle)) &
               stringr::str_detect(loupe_image_paths$LoupeImages, stringr::coll(paste0(selected_data()$`Eyewear Lens Compatible`, "."))
               )
      )

    result <- c(glue::glue_safe("www/LoupeImages/", loupe_rec$LoupeImages[[2]]),
                if_else(selected_data()$`Eyewear Lens Compatible` %in% c("Pi19", "Pi23"),
                        glue::glue_safe("www/recs/", selected_data()$`Rec1`, ".jpeg"),
                        glue::glue_safe("www/recs/", selected_data()$`Rec1`, ".jpg")
                ),
                if_else(selected_data()$`Eyewear Lens Compatible`  %in% c("Pi19", "Pi23"),
                        glue::glue_safe("www/recs/", selected_data()$`Rec2`, ".jpeg"),
                        glue::glue_safe("www/recs/", selected_data()$`Rec2`, ".jpg")
                ),
                glue::glue_safe("www/recs/", selected_data()$`Rec3`, ".jpg"),
                glue::glue_safe("www/LoupeImages/", loupe_rec$LoupeImages[[1]]))

    ## print(result)
    result
  })

  output$productImageF <- renderImage({
    ## print(image_location())


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
