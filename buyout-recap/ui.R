## buyout-recap ui
## acthomas, 8-20-14

source("global.R")

#getGET <- function(inputId, selected = "") {
#  tagList(
#    singleton(tags$head(tags$script(src = "js/getBins.js"))),
#    tags$input(id = inputId,
#                class = "n_breaks",
#                value = selected),
#	tags$style(type='text/css', "#n_breaks { display:none; }")
#  )
#}

shinyUI(
    fluidPage(
        tags$head(tags$style(".container-fluid { font-size: 12px;}")),
        tags$style(type="text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"
        ),
        tags$head(tags$link(rel = "stylesheet", type = "text/css", id ='twentytwelve-fonts-css', href = "http://fonts.googleapis.com/css?family=Open+Sans:400italic,700italic,400,700&#038;subset=latin,latin-ext", media="all")),
        tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "http://war-on-ice.com/css/capcheck.css")),

        
    #      tags$head(
    #          tags$style(type="text/css", "select { font-size: 80%;}"),
    #          ),
    #    getGET(inputId="n_breaks"),
   
        h2('Contract Summary and Tools (Single Player)', align="center"),
        hr(),

        fluidRow (
            column(4,
                   verticalLayout(
                       htmlOutput ("onload.name1"),
                       htmlOutput ("nameSelect1"),
                       htmlOutput ("contractSelect"),
                       htmlOutput ("DateSelect"),
                      
                       htmlOutput ("player.info"),
                       htmlOutput ("thisDisplay"),
                       htmlOutput ("description")
                   
                   )),
            
            column(8,
                   tabsetPanel(id="tabset",
##                               tabPanel("Contract History", dataTableOutput('salaryOne')),
                               tabPanel("Buyout Calculator",
                                        dataTableOutput('salaryTwo'),
                                        downloadButton("downloadBuyout", "Download Table")
                                        ),
                               tabPanel("Cap Recapture Penalties",
                                        dataTableOutput('salaryRecapture'),
                                        downloadButton("downloadRecap", "Download Table"))
                               )
                   )
                   
            ),
        
        div(style="display:inline-block", actionButton("sharePage", "Share This Page")),
        div(style="display:inline-block", textOutput ("shareDestination"))
        
        )
    )
