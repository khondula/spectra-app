#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggiraph)
library(glue)


source("R/app-functions.R")
in1 <- numericInput("in1",
                   "CDOM absorb at ref wl:",
                   min = 9.27e-5,
                   max = 1,
                   value = 10)
in2 <- numericInput("in2",
                   "CDOM spectral slope:",
                   min = 1.5e-2,
                   max = 0.05,
                   value = 0.008)
in3 <- numericInput("in3",
                   "CDOM ref wavelength:",
                   min = 100,
                   max = 800,
                   value = 440)
in4 <- numericInput("in4",
                   "NAP absorb at ref wavelength:",
                   min = 2.05e-3,
                   max = 1.73,
                   value = 5)
in5 <- numericInput("in5",
                   "NAP spectral slope:",
                   min = 1.1e-2,
                   max = 1.5e-2,
                   value = 1.3e-2)
in6 <- numericInput("in6",
                   "NAP abs ref wavelength:",
                   min = 100,
                   max = 800,
                   value = 440)
in7 <- numericInput("in7",
                   "chl a concentration (ug/L):",
                   min = 0,
                   max = 20,
                   step = 0.25,
                   value = 1)
in8 <- numericInput("in8",
                    "particulate scatter at ref wl:",
                    min =  1.81e-5,
                    max =  2.08e1,
                    value =  0.85)
in9 <- numericInput("in9",
                   "NAP backscatter ref wavelength:",
                   min = 100,
                   max = 800,
                   value = 555)
in10 <- numericInput("in10",
                   "scatter gamma:",
                   min =  0.3,
                   max =  1.7,
                   value =  0.5)
in11 <- numericInput("in11",
                   "glint error:",
                   min =  0,
                   max =  10,
                   step = 0.1,
                   value = 0)


in13 <- selectInput("spectraCol",
                    "spectra variable",
                    choices = names(spectra_join)[6:11],
                    selected = "DOC")
# outputs
out1 <- ggiraphOutput("distPlot")
# out1 <- tableOutput("distPlot")
# out2 <- ggiraphOutput("backsPlot")
# out3 <- ggiraphOutput("absPlot")
# out2 <- tableOutput("mydf")
out4 <- ggiraphOutput("RrsPlot")


# Define UI for application that draws a histogram
ui <- navbarPage("NEON Aquatic Spectra Stuff",
    tabPanel("Empirical",
        out4),
    tabPanel("Simulate",
             sidebarLayout(
                 sidebarPanel(
                     tabsetPanel(
                         tabPanel("set1", in1, in2, in4, in5, in7, in8, in9, in10,),
                         tabPanel("ref wls", in3, in6, in9, in11))),
                 # Show a plot of the generated distribution
                 mainPanel(
                     tabsetPanel(
                         tabPanel("Reflectance", out1))
                 )))
)

# Define server logic 
server <- function(input, output) {
    
    u_spectra <- reactive({
        abs_cdom_spectra <- my_wls %>% purrr::map_dbl(~input$in1 * exp(-1*input$in2 * (.x - input$in3)))
        # ## NAP Absorption
        abs_nap_spectra <- my_wls %>% purrr::map_dbl(~input$in4 * exp(-1*input$in5 * (.x - input$in6)))
        # ## Phyto Absorption
        abs_phy_spectra <- absorb_phyto * input$in7
        # ## Total Absorption
        abs_tot <- absorb_water + abs_cdom_spectra + abs_nap_spectra + abs_phy_spectra
        # ## Particulate backscattering
        backs_nap_spectra <- my_wls %>% purrr::map_dbl(~input$in8 * (input$in9/.x)^(-1*input$in10))
        # ## Total backscattering
        backs_tot <- backs_water + backs_nap_spectra
        # ## u
        u_spectra <- (backs_tot/(abs_tot + backs_tot))
        return(u_spectra)
    })
    
    rrs_df <- reactive({
        rrs_model_spectra <- (0.949 * u_spectra()) + (0.794 * u_spectra()^(2))
        Rrs_model_spectra <- (0.52 * rrs_model_spectra)/(1-1.6*rrs_model_spectra) + input$in11
        rrs_df <- data.frame(wl = my_wls, Rrs = Rrs_model_spectra)
        return(rrs_df)
    })


    # output$distPlot <- renderTable({
    #     rrs_df()
    # })
    output$distPlot <- renderGirafe({

        rs1 <- rrs_df() %>%
            ggplot(aes(x = wl, y = Rrs)) +
            geom_line() +
            theme_bw() +
            ylim(0, NA) +
            ggtitle("Reflectance")

        gg1 <- rs1 +
            geom_point_interactive(aes(tooltip = wl, data_id = wl), size = 1)
        x <- girafe(code = print(gg1))
        x    })

    output$RrsPlot <- renderGirafe({
        
        s1 <- spectra_join %>%
            ggplot(aes(x = wavelength, y = reflectance, group = aop_siteyear,
                       hover_css = "fill:none;")) +
            geom_vline(xintercept = c(665, 442), col = "red", lwd = 2, alpha = 0.5) +
            # geom_line(aes(col = DOC)) +
            coord_cartesian(ylim = c(0, 0.1)) +
            scale_color_viridis_c() +
            theme_bw()
        gg3 <- s1 +
            geom_line_interactive(aes(col = DOC, 
                                      tooltip = glue("{aop_siteyear}
                                           {flightdate}
                                           DOC: {DOC}
                                           chla: {`chlorophyll a`}
                                           tss: {TSS}
                                           suva254: {`UV Absorbance (250 nm)`}"), 
                                      data_id = aop_siteyear))
        
        x3 <- girafe(code = print(gg3), width_svg = 8,
                     options = list(
                         opts_hover_inv(css = "opacity:0.3;"),
                         opts_hover(css = "stroke-width:3;")))
        x3
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
