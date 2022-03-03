library(shiny)
library(shinyWidgets)
library(OsteoBioR)
library(colourpicker)
library(shinyMatrix)
library(dplyr)
library(ggplot2)
library(xlsx)
library(rstan)

options(shiny.maxRequestSize = 200*1024^2)

shinyServer(function(input, output, session) {
  ### DATA -------------------------------------------------
  # isotopeVals <- matrix(c(c(1,1,1,1,2,2,2,2),
  #                         c(-10, -7, -12, -9, -14, -15, -8, NA), 
  #                         c(2, 1.5, 2.5, 2.5, 1.5, 3.5, 3, NA)), 
  #                       ncol = 3, 
  #                       dimnames = list(NULL, c("individual", "y_mean", "y_sigma")))
  isotopeVals <- data.frame(individual = rep(c(1,11,2), each = 4),
                            y_mean = c(rep(c(-10, -7, -12, -9), 2), c(-14, -15, -8, NA)),
                            y_sigma = c(rep(c(2, 1.5, 2.5, 2.5), 2), c(1.5, 3.5, 3, NA))
                            ) %>%
    as.matrix()
  
  dat <- reactiveValues()
  # dat$dataExample <- eventReactive(input$exampleData, ignoreNULL = TRUE, {
  #   as.matrix(data.frame(
  #     individual = rep(1:2, each = 6),
  #     intStart = rep(0:5, 2),
  #     intEnd = rep(1:6, 2),
  #     bone1 = c(100, 50, 20, 10, 5, 2, 100, 80, 30, 8, 15, 4),
  #     bone2 = c(100, 10, 5, 1, 1, 1, 90, 40, 5, 1, 12, 1),
  #     tooth1 = c(0, 100, 0, 0, 0, 0, 0, 80, 20, 0, 0, 0),
  #     tooth2 = c(0, 0, 100, 0, 0, 0, NA, NA, NA, NA, NA, NA)
  #   ))
  dat$dataExample <- eventReactive(input$exampleData, ignoreNULL = TRUE, {
    exmpl <- data.frame(
      individual = rep(1:2, each = 6),
      intStart = rep(0:5, 2),
      intEnd = rep(1:6, 2),
      bone1 = c(100, 50, 20, 10, 5, 2, 100, 80, 30, 8, 15, 4),
      bone2 = c(100, 10, 5, 1, 1, 1, 90, 40, 5, 1, 12, 1),
      tooth1 = c(0, 100, 0, 0, 0, 0, 0, 80, 20, 0, 0, 0),
      tooth2 = c(0, 0, 100, 0, 0, 0, NA, NA, NA, NA, NA, NA)
    ) %>% slice(-4)
    exmpl$individual[4:5] <- 11
    as.matrix(exmpl)
  })
  
  dat$dataFile <- eventReactive(input$fileData, ignoreNULL = TRUE, {
    inFile <- input$fileData
    if (is.null(inFile)) return(NULL)
    file <- inFile$datapath
    if(grepl(".csv$", file)){
      name <- read.csv(file, sep = input$colseparatorData, dec = input$decseparatorData)
    } else if(grepl(".xlsx$", file)){
      name <- read.xlsx(file, sheetIndex = 1)
    }
    
    if(any(sapply(as.matrix(name), is.character))) { shinyjs::alert("Please provide a dataset with all numeric variables.") }
    name <- name[, !(apply(name, 2, function(x) all(is.na(x))))]
    as.matrix(name)
  })
  
  
  dat$fileIso <- eventReactive(input$fileIso, ignoreNULL = TRUE, {
    inFile <- input$fileIso
    if (is.null(inFile)) return(NULL)
    file <- inFile$datapath
    if(grepl(".csv$", file)){
      name <- read.csv(file, sep = input$colseparatorIso, dec = input$decseparatorIso)
    } else if(grepl(".xlsx$", file)){
      name <- read.xlsx(file, sheetIndex = 1)
    } 
    if(any(sapply(as.matrix(name), is.character))) { shinyjs::alert("Please provide a dataset with all numeric variables.") }
    name <- name[, !(apply(name, 2, function(x) all(is.na(x))))]
    as.matrix(name)
    
  })
  
  
  
  observeEvent(input$exampleData, {
    updateMatrixInput(session, "dataMatrix", value = dat$dataExample() )
    updateMatrixInput(session, "isotope", value = isotopeVals)
  })
  
  observeEvent(input$fileData, {
    updateMatrixInput(session, "dataMatrix", value = dat$dataFile() )
  })
  
  observeEvent(input$fileIso, {
    updateMatrixInput(session, "isotope", value = dat$fileIso() )
  })
  
  ### MODEL -------------------------------------------------
  
  modDat <- eventReactive(input$dataMatrix, {
    ret <- input$dataMatrix %>%
      data.frame() %>%
      .[colSums(!is.na(.)) > 0] #%>%
      #filter(complete.cases(.))
    lapply(split(ret, ret[,input$indVar]), function(x) x[, !apply(x, 2, function(y) all(is.na(y)))])
  })
  
  observe({
    updatePickerInput(session, "timeVars", choices = input$dataMatrix %>% colnames() )
    updatePickerInput(session, "boneVars", choices = input$dataMatrix %>% colnames() )
    updateSelectizeInput(session, "indVar", choices = input$dataMatrix %>% colnames() )
  })
  
  isoMean <- eventReactive(input$isotope, {
    ret <- (input$isotope %>%
      data.frame() %>%
      .[colSums(!is.na(.)) > 0] #%>%
      #filter(complete.cases(.))
     )
    lapply(split(ret[,2], ret[,1]), function(x) as.numeric(na.omit(x)))
  })
  
  isoSigma <- eventReactive(input$isotope, {
    ret <- (input$isotope %>%
      data.frame() %>%
      .[colSums(!is.na(.)) > 0] #%>%
      #filter(complete.cases(.))
     )
    lapply(split(ret[,3], ret[,1]), function(x) as.numeric(na.omit(x)))
  })
  
  fit <- reactiveVal(NULL)
  
  nIndividuals1 <- reactive({(unique(na.omit(input$isotope[,1])))})
  nIndividuals2 <- reactive({(unique(na.omit(input$dataMatrix[,1])))})
  
  observeEvent(input$fitModel, {
    if(!(all(length(nIndividuals1())==length(nIndividuals2())) && all(nIndividuals1()==nIndividuals2()))){
      shinyjs::alert("Number of individuals must be the same in both renewal and measurements table")
    }
    fitted <- try({
      lapply(1:length(modDat()), function(x){
        withProgress({
      boneVars <- input$boneVars[which(input$boneVars %in% colnames(modDat()[[x]]))]
      estimateIntervals(renewalRates = data.frame(modDat()[[x]]),
                      timeVars = input$timeVars,
                      boneVars = boneVars,
                      isoMean = unlist(isoMean()[[x]]),
                      isoSigma = unlist(isoSigma()[[x]]),
                      iter = input$iter,
                      burnin = input$burnin,
                      chains = input$chains)
        }, value = x / length(modDat()),
      message = paste0("Calculate model for individual nr.", x))
      })
    }, silent = TRUE)
    if (inherits(fitted, "try-error")) {
      shinyjs::alert(fitted[[1]])
    } else {
      allModels <- savedModels()
      allModels <- allModels[!grepl("Current", names(allModels))]
      for(i in 1:length(fitted)){
        fit(fitted[[i]])
        newModel <- setNames(list(fit()), paste0("Current_", names(modDat())[i]))
        allModels <- c(allModels, newModel)
      }
      fit(fitted[[1]])
      savedModels(allModels)
    }
  })
  output$summary <- renderPrint({ print(fit(), pars = c("interval", "mu", "rho", "alpha")) })
  output$shiftTimePoints <- renderPrint({ 

    fits <- savedModels()[input$savedModelsShift]
    if(length(fits) == 0){
      return("Please select a model / individual")
    }
    shiftTime <- lapply(fits, function(x) getShiftTime(x, 
                              type = ifelse(input$shiftTimeAbsOrRel == "absolute", TRUE, FALSE), 
                              slope = ifelse(input$slope == "slope", TRUE, FALSE),
                              threshold = input$shiftTimeThreshold, 
                              probability = input$shiftTimeProb))
    shiftTime <- do.call("rbind", lapply(1:length(shiftTime),
                                         function(x){
                                           if(NROW(shiftTime[[x]]) > 0){
                                             data.frame(Individual = names(shiftTime)[x], shiftTime[[x]])
                                           } else {
                                             data.frame(Individual = character(0), c())
                                           }
                                         } ))
    
    ifelse(nrow(shiftTime) == 0, stop("No shifts were found"), return(shiftTime))
  })
  
  output$userDefined <- renderPrint({ 
    
    fits <- savedModels()[input$savedModelsUserDefined]
    if(length(fits) == 0){
      return("Please select a model / individual")
    }
    shiftTime <- lapply(fits, function(x) getUserDefinedEst(x, 
                                                       minim = input$from2,
                                                       maxim = input$to2,
                                                       type = input$typeEstUser))
    
    shiftTime <- do.call("rbind", lapply(1:length(shiftTime),
                                         function(x){
                                           if(NROW(shiftTime[[x]]) > 0){
                                             data.frame(Individual = names(shiftTime)[x], shiftTime[[x]])
                                           } else {
                                             data.frame(Individual = character(0), c())
                                           }
                                         } ))
    
    ifelse(nrow(shiftTime) == 0, stop("Error, Please check your interval settings"), return(shiftTime))
  })
  
  savedModels <- reactiveVal(list())
  savedPlot <- reactiveVal(list())
  savedXAxisData <- reactiveVal(data.frame())
  
  observeEvent(input$saveModel, {
    if (trimws(input$modelName) == "") {
      shinyjs::alert("Please provide a model name")
      return()
    }
    if (input$modelName %in% names(savedModels())) {
      shinyjs::alert("Model name already present. Please choose another name")
      return()
    }

    newModel <- setNames(list(fit()), input$modelName)
    allModels <- c(savedModels(), newModel)
    savedModels(allModels)
  })
  
  observe({
    updateSelectInput(session, "savedModels", choices = names(savedModels()))
    updateSelectizeInput(session, "savedModelsPlot", choices = names(savedModels()), selected = names(savedModels())[1])
    updatePickerInput(session, "savedModelsShift", choices = names(savedModels()), selected = names(savedModels())[1])
    updatePickerInput(session, "savedModelsUserDefined", choices = names(savedModels()), selected = names(savedModels())[1])
    
    updatePickerInput(session, "savedModelsTime", choices = names(savedModels()))
  })

  observeEvent(input$loadModel, {
    fit(savedModels()[[input$savedModels]])
  })
  
  uploadedNotes <- reactiveVal(NULL)
  callModule(downloadModel, "modelDownload", 
             savedModels = savedModels, uploadedNotes = uploadedNotes)
  callModule(uploadModel, "modelUpload", 
             savedModels = savedModels, uploadedNotes = uploadedNotes)
  callModule(loadRemoteModel, "predefinedModelLoad",
             savedModels = savedModels, uploadedNotes = uploadedNotes)
  
  observeEvent(input$exportSummary, {
    showModal(modalDialog(
      "Export Data",
      easyClose = TRUE,
      footer = modalButton("OK"),
      selectInput(
        "exportType",
        "File type",
        choices = c("csv", "xlsx", "json"),
        selected = "xlsx"
      ),
      conditionalPanel(
        condition = "input['exportType'] == 'csv'",
        div(style = "display: inline-block;horizontal-align:top; width: 80px;",
            textInput("colseparator", "column separator:", value = ",")),
        div(style = "display: inline-block;horizontal-align:top; width: 80px;",
            textInput("decseparator", "decimal separator:", value = "."))
      ),
      downloadButton("exportExecuteSummary", "Export")
    ))
  })
  
  output$exportExecuteSummary <- downloadHandler(
    filename = function(){
      exportFilename(input$exportType)
    },
    content = function(file){
      exportData <- extract(fit())$interval
      switch(
        input$exportType,
        csv = exportCSV(file, exportData, colseparator(), decseparator()),
        xlsx = exportXLSX(file, exportData),
        json = exportJSON(file, exportData)
      )
    }
  )
  
  observeEvent(input$deriv,{
    req(savedModels()[[input$savedModels]])
    updateNumericInput(session, "xmin", value = getDefaultPlotRange(savedModels(), deriv = input$deriv)$minX)
    updateNumericInput(session, "xmax", value = getDefaultPlotRange(savedModels(), deriv = input$deriv)$maxX)
    updateNumericInput(session, "ymin", value = getDefaultPlotRange(savedModels(), deriv = input$deriv)$minY)
    updateNumericInput(session, "ymax", value = getDefaultPlotRange(savedModels(), deriv = input$deriv)$maxY)
  })
  
  observeEvent(savedModels(),
               {
                 req(length(savedModels()) > 0)
                 updateNumericInput(session, "xmin", value = getDefaultPlotRange(savedModels(), deriv = "1")$minX)
                 updateNumericInput(session, "xmax", value = getDefaultPlotRange(savedModels(), deriv = "1")$maxX)
                 updateNumericInput(session, "ymin", value = getDefaultPlotRange(savedModels(), deriv = "1")$minY)
                 updateNumericInput(session, "ymax", value = getDefaultPlotRange(savedModels(), deriv = "1")$maxY)
               })
  
  # observeEvent(input$ymin,
  #              {
  #   ymin <- input$ymin
  #   if(!exists("ymax") || ymax == c()){
  #     ymax <- input$ymax
  #   } 
  #   yLim <- c(ymin, ymax)
  # })
  # observeEvent(input$ymax,
  #              {
  #                if(!exists("ymin") || ymin == c()){
  #                  ymin <- input$ymin
  #                } 
  #                ymax <- input$ymax
  #                yLim <- c(ymin, ymax)
  #              })
  
  output$plot <- renderPlot({ OsteoBioR::plot(fit(), prop = input$modCredInt) })
    
  
  plotAdd <- reactiveVal("new")
  observeEvent(input$newPlot, 
               plotAdd("new"))
  observeEvent(input$addPlot, 
               plotAdd("add"))
  
  plotTimeFunc <- eventReactive({input$newPlot}, {
    fits <- savedModels()[input$savedModelsPlot]
    if(length(fits) > 0){
        p <- plotTime(fits[[1]], prop = input$modCredInt, yLim = c(input$ymin, input$ymax),
                      xLim = c(input$xmin, input$xmax), deriv = input$deriv,
                      colorL = input$colorL, colorU = input$colorU, alphaL = input$alphaL, alphaU =  input$alphaU,
                      xAxisLabel = input$xAxisLabel, yAxisLabel = input$yAxisLabel,
                      sizeTextY =  input$sizeTextY , sizeTextX = input$sizeTextX,
                      sizeAxisX = input$sizeAxisX, sizeAxisY = input$sizeAxisY)
      savedPlot(p)
      savedXAxisData(getXAxisData(fits[[1]]))
      return(p)
    } else {
      return(NULL)
    }
  })
  
  plotTimeAdd <- eventReactive({input$addPlot}, {
    fits <- savedModels()[input$savedModelsPlot]
    if(length(fits) > 0 && length(savedPlot()) > 0){
        oldPlot <- savedPlot()
        oldXAxisData <- savedXAxisData()
        p <- plotTime(fits[[1]], prop = input$modCredInt, yLim = c(input$ymin, input$ymax),
                      xLim = c(input$xmin, input$xmax), oldPlot = oldPlot, oldXAxisData = oldXAxisData,
                      deriv = input$deriv, colorL = input$colorL, colorU = input$colorU,
                      alphaL = input$alphaL, alphaU =  input$alphaU,
                      sizeTextY =  input$sizeTextY , sizeTextX = input$sizeTextX,
                      xAxisLabel = input$xAxisLabel, yAxisLabel = input$yAxisLabel,
                      sizeAxisX = input$sizeAxisX, sizeAxisY = input$sizeAxisY, secAxis = input$secAxis)
      savedPlot(p)
      savedXAxisData(getXAxisData(object = fits[[1]], oldXAxisData = oldXAxisData))
      return(p)
    } else {
      return(NULL)
    }
  })
  
  output$plotTime <- renderPlot({
    if(length(plotAdd() > 0) && plotAdd() == "add"){
      return(plotTimeAdd())
    } 
    if(length(plotAdd() > 0) && plotAdd() == "new"){
      return(plotTimeFunc())
    } 
    
  })
  
  minimum <- eventReactive(input$timeVars, {
    input$dataMatrix[, input$timeVars[1]] %>% min
  })
  
  maximum <- eventReactive(input$timeVars, {
    input$dataMatrix[, input$timeVars[1]] %>% max
  })
  
  observe({
    updateNumericInput(session, "from", value = minimum())
    updateNumericInput(session, "to", value = maximum())
    updateNumericInput(session, "from2", value = minimum())
    updateNumericInput(session, "to2", value = maximum())
    
  })
  
  estimates <- eventReactive(input$estSpecTimePoint, {
    if (is.null(input$savedModelsTime)) {
      return("Please select a fitted a model.")
    }
    
    fits <- savedModels()[input$savedModelsTime]
    
    tryCatch(
      withCallingHandlers(
        {
          ret <- lapply(fits, function(x) 
            estimateTimePoint(x, time = seq(input$from, input$to,
                                            by = input$by))
          )
          do.call("rbind", lapply(1:length(ret), function(x)
            data.frame(Individual = names(ret)[x], ret[[x]])
            ))
        },
        message = function(m) showNotification(m$message, type = "message"),
        warning = function(w) showNotification(w$message, type = "warning")
      ),
      error = function(e) alert(e$message)
    )
    })
  
  output$timePointEstimates <- renderPrint({
    estimates()
  })
  
  # observeEvent({ if(input$checkTimePointEst == 1) TRUE } , {
  #   showTab(inputId = "modTabs",
  #           target = "timePointEstimatesTab"
  #   )
  # })
  
  # observeEvent({ if(input$checkTimePointEst == 0) TRUE } , {
  #   hideTab(inputId = "modTabs",
  #           target = "timePointEstimatesTab"
  #   )
  # })
  
  
  observeEvent(input$exportTimePointEst, {
    req(estimates())
    req(!is.character(estimates()))
    showModal(modalDialog(
      "Export Data",
      easyClose = TRUE,
      footer = modalButton("OK"),
      selectInput(
        "exportType",
        "File type",
        choices = c("csv", "xlsx", "json"),
        selected = "xlsx"
      ),
      conditionalPanel(
        condition = "input['exportType'] == 'csv'",
        div(style = "display: inline-block;horizontal-align:top; width: 80px;",
            textInput("colseparator", "column separator:", value = ",")),
        div(style = "display: inline-block;horizontal-align:top; width: 80px;",
            textInput("decseparator", "decimal separator:", value = "."))
      ),
      downloadButton("exportExecuteTimePointEst", "Export")
    ))

  colseparator <- reactive({
    input$colseparator
  })
  decseparator <- reactive({
    input$decseparator
  })
  
  output$exportExecuteTimePointEst <- downloadHandler(
    filename = function(){
      paste("timePointEstimates", input$exportType, sep = ".")
    },
    content = function(file){
      exportData <- estimates()
      switch(
        input$exportType,
        csv = exportCSV(file, exportData, colseparator(), decseparator()),
        xlsx = exportXLSX(file, exportData),
        json = exportJSON(file, exportData)
      )
    }
  )
  })
  
  observeEvent(input$exportCredIntDat, {
    showModal(modalDialog(
      "Export Data",
      easyClose = TRUE,
      footer = modalButton("OK"),
      selectInput(
        "exportType",
        "File type",
        choices = c("csv", "xlsx", "json"),
        selected = "xlsx"
      ),
      conditionalPanel(
        condition = "input['exportType'] == 'csv'",
        div(style = "display: inline-block;horizontal-align:top; width: 80px;",
            textInput("colseparator", "column separator:", value = ",")),
        div(style = "display: inline-block;horizontal-align:top; width: 80px;",
            textInput("decseparator", "decimal separator:", value = "."))
      ),
      downloadButton("exportExecuteCredIntDat", "Export")
    ))
    
    colseparator <- reactive({
      input$colseparator
    })
    decseparator <- reactive({
      input$decseparator
    })
    
    output$exportExecuteCredIntDat <- downloadHandler(
      filename = function(){
        paste("credibilityIntervals", input$exportType, sep = ".")
      },
      content = function(file){
        exportData <- as.data.frame(fit())
        switch(
          input$exportType,
          csv = exportCSV(file, exportData, colseparator(), decseparator()),
          xlsx = exportXLSX(file, exportData),
          json = exportJSON(file, exportData)
        )
      }
    )
  })

  # Downloads Plots -------------------------------------------------
  
  observeEvent(input$exportCredIntPlot, {
    
    plotOutputElement <- renderPlot({ OsteoBioR::plot(fit(), prop = input$modCredInt) })
    exportTypeChoices <- c("png", "pdf", "svg", "tiff")
    
    showModal(modalDialog(
      title = "Export Graphic",
      footer = modalButton("OK"),
      plotOutputElement,
      selectInput(
        "exportType", "Filetype",
        choices = exportTypeChoices
      ),
      numericInput("width", "Width (px)", value = 1280),
      numericInput("height", "Height (px)", value = 800),
      downloadButton("exportExecute", "Export"),
      easyClose = TRUE
    ))
    
    output$exportExecute <- downloadHandler(
      filename = function(){
        paste0(gsub("-", "", Sys.Date()), "_", "Credibility_Intervals", ".", input$exportType)
      },
      content = function(file){
        switch(
          input$exportType,
          png = png(file, width = input$width, height = input$height),
          pdf = pdf(file, width = input$width / 72, height = input$height / 72),
          tiff = tiff(file, width = input$width, height = input$height),
          svg = svg(file, width = input$width / 72, height = input$height / 72)
        )
        print( OsteoBioR::plot(fit(), prop = input$modCredInt) )
        
        dev.off()
      }
    )
  })
  
  observeEvent(input$exportCredIntTimePlot, {

    plotOutputElement <- renderPlot({ savedPlot() })
    exportTypeChoices <- c("png", "pdf", "svg", "tiff")

    showModal(modalDialog(
      title = "Export Graphic",
      footer = modalButton("OK"),
      plotOutputElement,
      selectInput(
        "exportType", "Filetype",
        choices = exportTypeChoices
      ),
      numericInput("width", "Width (px)", value = 1280),
      numericInput("height", "Height (px)", value = 800),
      downloadButton("exportExecute", "Export"),
      easyClose = TRUE
    ))
  
  output$exportExecute <- downloadHandler(
    filename = function(){
      paste0(gsub("-", "", Sys.Date()), "_", "Credibility_Intervals_Over_Time", ".", input$exportType)
    },
    content = function(file){
      switch(
        input$exportType,
        png = png(file, width = input$width, height = input$height),
        pdf = pdf(file, width = input$width / 72, height = input$height / 72),
        tiff = tiff(file, width = input$width, height = input$height),
        svg = svg(file, width = input$width / 72, height = input$height / 72)
      )
      print( savedPlot() )

      dev.off()
    }
  )
  })
  
  
  ### RESIDING TIME ------------------------------------------
  datStayTime <- reactiveValues()
  
  datStayTime$dataExample <- eventReactive(input$loadStayTimeData, ignoreNULL = TRUE, {
    as.matrix(data.frame(
      siteMeans = c(-8, -10),
      siteSigma = c(1, 1.5)))
  })
  
  datStayTime$dataFile <- eventReactive(input$stayTimeData, ignoreNULL = TRUE, {
    inFile <- input$stayTimeData
    if (is.null(inFile)) return(NULL)
    file <- inFile$datapath
    if(grepl(".csv$", file)){
      name <- read.csv(file, sep = input$colseparatorStay, dec = input$decseparatorStay)
    } else if(grepl(".xlsx$", file)){
      name <- readxl::read_xlsx(file)
    } 
    if(any(sapply(as.matrix(name), is.character))) { shinyjs::alert("Please provide a dataset with all numeric variables.") }
    as.matrix(name)
    
  })
  observeEvent(input$loadStayTimeData, {
    updateMatrixInput(session, "stayTimeMatrix", value = datStayTime$dataExample() )
  })
  
  observeEvent(input$stayTimeData, {
    updateMatrixInput(session, "stayTimeMatrix", value = datStayTime$dataFile() )
  })
  
  stayTimeDat <- eventReactive(input$stayTimeMatrix, {
    input$stayTimeMatrix %>%
      data.frame() %>%
      .[colSums(!is.na(.)) > 0] %>%
      filter(complete.cases(.))
  })
  estimatedStayTimes <- eventReactive(input$stayingTime, {
    getSiteStayTimes(object = fit(),
                     siteMeans = stayTimeDat()[, 1] %>% unlist(),
                     siteSigma = stayTimeDat()[, 2] %>% unlist(),
                     print = FALSE)
  })
  output$estimatedStayTimes <- renderPrint({ estimatedStayTimes() })
  
  observeEvent(input$exportStayTimeDat, {
    showModal(modalDialog(
      "Export Data",
      easyClose = TRUE,
      footer = modalButton("OK"),
      selectInput(
        "exportType",
        "File type",
        choices = c("csv", "xlsx", "json"),
        selected = "xlsx"
      ),
      conditionalPanel(
        condition = "input['exportType'] == 'csv'",
        div(style = "display: inline-block;horizontal-align:top; width: 80px;",
            textInput("colseparator", "column separator:", value = ",")),
        div(style = "display: inline-block;horizontal-align:top; width: 80px;",
            textInput("decseparator", "decimal separator:", value = "."))
      ),
      downloadButton("exportExecuteStayTimeDat", "Export")
    ))
    
    colseparator <- reactive({
      input$colseparator
    })
    decseparator <- reactive({
      input$decseparator
    })
    
    output$exportExecuteStayTimeDat <- downloadHandler(
      filename = function(){
        paste("stayTimeLengths", input$exportType, sep = ".")
      },
      content = function(file){
        resTime <- estimatedStayTimes()
        exportData <- as.data.frame(resTime$stayTimes)
        switch(
          input$exportType,
          csv = exportCSV(file, exportData, colseparator(), decseparator()),
          xlsx = exportXLSX(file, exportData),
          json = exportJSON(file, exportData)
        )
      }
    )
  })
  
  
  ### COMPUTE ISOTOPIC VALUES ------------------------------
  datIso <- reactiveValues()
  
  datIso$dataExample <- eventReactive(input$loadHistData, ignoreNULL = TRUE, {
    as.matrix(data.frame(
      t = 1:3,
      bone1 = c(100, 50, 10, 10, 10, 10),
      bone2 = c(0, 20, 20, 20, 20, 20),
      teeth1 = c(0, 0, 0, 10, 50, 60),
      mean = c(1, 3, 3,0, -1, - 5),
      sd = c(1, 3, 1, 1.5, 2, 3)
    )) 
  })
  
  datIso$dataFile <- eventReactive(input$fileHistData, ignoreNULL = TRUE, {
    inFile <- input$fileHistData
    if (is.null(inFile)) return(NULL)
    file <- inFile$datapath
    if(grepl(".csv$", file)){
      name <- read.csv(file, sep = input$colseparatorHistData, dec = input$decseparatorHistData)
    } else if(grepl(".xlsx$", file)){
      name <- read.xlsx(file, sheetIndex = 1)
    } 
    if(any(sapply(as.matrix(name), is.character))) { shinyjs::alert("Please provide a dataset with all numeric variables.") }
    as.matrix(name)
    
  })
  
  observeEvent(input$loadHistData, {
    updateMatrixInput(session, "historicData", value = datIso$dataExample() )
  })
  
  observeEvent(input$fileHistData, {
    updateMatrixInput(session, "historicData", value = datIso$dataFile() )
  })
  
  isoHistDat <- eventReactive(input$historicData, {
    input$historicData %>% 
      data.frame() %>% 
      .[colSums(!is.na(.)) > 0] %>% 
      filter(complete.cases(.))
  })
  
  observe({
    updatePickerInput(session, "timeVarHist", choices = isoHistDat() %>% colnames() )
    updatePickerInput(session, "boneVarsHist", choices = isoHistDat() %>% colnames() )
    updatePickerInput(session, "meanVarHist", choices = isoHistDat() %>% colnames() )
    updatePickerInput(session, "sdVarHist", choices = isoHistDat() %>% colnames() )
  })
  
  isotopicValues <- eventReactive(input$calcIsotopicValues, {
    cbind(computeResult(
      data = isoHistDat(),
      timeVars = input$timeVarHist,
      boneVars = input$boneVarsHist,
      meanVar = input$meanVarHist,
      sdVar = input$sdVarHist
    ),
    sapply(isoHistDat() %>% select(input$boneVarsHist), quantile, probs = c(0.025, 0.975)) %>% t()
    
    )
  })
  
  output$isotopicValues <- renderTable({ isotopicValues() }, rownames = TRUE)
  
  observeEvent(input$exportResultsDat, {
    showModal(modalDialog(
      "Export Data",
      easyClose = TRUE,
      footer = modalButton("OK"),
      selectInput(
        "exportType",
        "File type",
        choices = c("csv", "xlsx", "json"),
        selected = "xlsx"
      ),
      conditionalPanel(
        condition = "input['exportType'] == 'csv'",
        div(style = "display: inline-block;horizontal-align:top; width: 80px;",
            textInput("colseparator", "column separator:", value = ",")),
        div(style = "display: inline-block;horizontal-align:top; width: 80px;",
            textInput("decseparator", "decimal separator:", value = "."))
      ),
      downloadButton("exportExportResultsDat", "Export")
    ))
    
    colseparator <- reactive({
      input$colseparator
    })
    decseparator <- reactive({
      input$decseparator
    })
    
    output$exportExportResultsDat <- downloadHandler(
      filename = function(){
        paste("isotopicValues", input$exportType, sep = ".")
      },
      content = function(file){
        exportData <- isotopicValues()
        switch(
          input$exportType,
          csv = exportCSV(file, exportData, colseparator(), decseparator()),
          xlsx = exportXLSX(file, exportData),
          json = exportJSON(file, exportData)
        )
      }
    )
  })

  observeEvent(input$getHelp, {
    showModal(modalDialog(
      title = "Help",
      easyClose = TRUE,
      getHelp(input$tab)
    ))
  })
})
