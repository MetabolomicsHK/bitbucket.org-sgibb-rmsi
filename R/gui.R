#' @export
msirGUI <- function(maxRequestSize=250*1024^2) {
  options(shiny.maxRequestSize=maxRequestSize)

  app <- list(
    server = function(input, output, session) {

      col <- rainbow(100L, start=1L/5L)

      rawSpectra <- reactive({
        if (is.null(input$file)) {
          return(NULL)
        }
        filename <- file.path(dirname(input$file$datapath), input$file$name)
        ## importImzMl needs teh ".zip" file extension
        file.rename(input$file$datapath, filename)
        importImzMl(filename, centroided=TRUE)
      })

      mzRange <- reactive({
        if (is.null(rawSpectra())) {
          return(NULL)
        }

        range(unlist(lapply(rawSpectra(), mass)))
      })

      roundedMzRange <- reactive({
        if (is.null(mzRange())) {
          return(NULL)
        }
        c(floor(mzRange()[1L]), ceiling(mzRange()[2L]))
      })

      imsSlides <- eventReactive(input$submitButtion, {
        if (is.null(rawSpectra())) {
          return(NULL)
        }
        slides(rawSpectra(), range=input$range, step=input$step,
               tolerance=input$tolerance)
      })

      centers <- reactive({
        if (is.null(imsSlides())) {
          return(NULL)
        }
        attr(imsSlides(), "center")
      })

      output$imzMlSummary <- renderTable({
        if (is.null(rawSpectra())) {
          return(NULL)
        }
        categories <- c("file", "number of spectra", "mz range")
        values <- c(basename(metaData(rawSpectra()[[1L]])$file),
                    length(rawSpectra()),
                    paste0(roundedMzRange(), collapse=" - "))
        data.frame(categories, values, stringsAsFactors=FALSE)
      })

      output$levelplot <- renderPlot({
        i <- findInterval(input$center, centers()) + 1L
        main <- paste0(input$center, " +/- ", input$tolerance)

        print(levelplot(imsSlides()[,,i],
                        main=main, xlab="x/ pixel", ylab="y/ pixel",
                        scales=list(draw=FALSE), contour=TRUE, pretty=TRUE,
                        col.regions=col))
      })

      updateRangeSlider <- observe({
        input$file
        updateSliderInput(session, "range", value=roundedMzRange(),
                          min=roundedMzRange()[1L], max=roundedMzRange()[2L])
        updateSliderInput(session, "center", value=roundedMzRange()[1L],
                          min=roundedMzRange()[1L], max=roundedMzRange()[2L])
      })

      updateCenterSlider <- observe({
        if (is.null(imsSlides())) {
          return(NULL)
        }
        input$range

        if (!is.null(input$center) &&
            (input$center >= centers()[1L] &&
             input$center <= centers()[length(centers())])) {
          value <- input$center
        } else {
          value <- centers()[1L]
        }
        updateSliderInput(session, "center", value=value,
                          min=centers()[1L], max=centers()[length(centers())], step=input$step)
      })
    },
    ui = fluidPage(
      column(
        width=5,
        h1("MZ Script", align="center", style="padding-top:15px;font-size:28px")
      ),
      column(
        width=3, offset=0,
        a(img(src="http://www.ira.cinvestav.mx/Portals/0/logo.gif",
              align="right", style="padding-top:15px"),
          href="http://www.ira.cinvestav.mx/Investigaci%C3%B3n/Biotecnolog%C3%ADayBioqu%C3%ADmica/ProfesoresInvestigadores/DrRobertWinkler/GrupodeInvestigaci%C3%B3n/tabid/771/language/es-MX/Default.aspx", target="_blank")
      ),
      fluidRow(
        column(
          width=10,
          h3(strong("Script para mz"), align="center",
             style="font-size:20px;line-height:10px")
        )
      ),
      fluidRow(
        navlistPanel(
          well=FALSE,
          widths=c(2,8),
          id="tsp",
          tabPanel("Introduction", value="intro",
                   wellPanel(align="center", p("Script para generar imagenes"))),
	        tabPanel("Input Data", value="data",
                   wellPanel(align="center", p("Please upload the mzML archive and set the parameters"),
		               fileInput("file", label=h3("zip file input"), accept="application/zip"),
                   uiOutput("imzMlSummary"),
                    sliderInput("range", label=h3("mz range"), min=0, max=1, value=c(0, 1),step=1),
                   splitLayout(
                    numericInput("step", label=h3("Scan Step"), min=0.1, max=5.0, value=0.2, step=0.1),
                    numericInput("tolerance", label=h3("Scan Tolerance"), min=0.1, max=5.0, value=0.4, step=0.1)),
                   actionButton("submitButtion", "Submit"))),
          tabPanel("Results", value="results",
                   wellPanel(align="center", p("Results"),
                     sliderInput("center", label=h3("mz range"), min=0, max=1, value=0,step=1),
                    plotOutput("levelplot")
            )
          ),
    	    tabPanel("More information", value="info",
                   wellPanel(align="center", p("Extras"))
          )
	      )
      )
    )
  )

  runApp(app)
}
