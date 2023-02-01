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
  dat <- reactiveValues()
  uploadedDataMatrix <- reactiveVal()
  uploadedIsotope <- reactiveVal()
  uploadedModelSpecInputs <- reactiveVal()
  
  importedData <- DataTools::importDataServer(
    "fileData",
    defaultSource = "file",
    customErrorChecks = list(reactive(DataTools::checkAnyNonNumericColumns))
  )
  
  observe({
    req(length(importedData()) > 0)
    dat$dataFile <- importedData()[[1]] %>%
      as.matrix()
  }) %>%
    bindEvent(importedData())
  
  importedIso <- DataTools::importDataServer(
    "fileIso",
    defaultSource = "file",
    customErrorChecks = list(reactive(DataTools::checkAnyNonNumericColumns))
  )
  
  observe({
    req(length(importedIso()) > 0)
    dat$fileIso <- importedIso()[[1]] %>%
      as.matrix()
  }) %>%
    bindEvent(importedIso())
  
  
  observeEvent(input$exampleData, {
    updateMatrixInput(session, "dataMatrix", value = getExampleDataMatrix() )
    updateMatrixInput(session, "isotope", value = getExampleIsotopeVals())
    
    # reset values storing data from uploaded models
    uploadedDataMatrix(NULL)
    uploadedIsotope(NULL)
    uploadedModelSpecInputs(NULL)
  })
  
  observeEvent(dat$dataFile, {
    updateMatrixInput(session, "dataMatrix", value = dat$dataFile)
    
    # reset values storing data from uploaded models
    uploadedDataMatrix(NULL)
    uploadedIsotope(NULL)
    uploadedModelSpecInputs(NULL)
  })
  
  observeEvent(dat$fileIso, {
    updateMatrixInput(session, "isotope", value = dat$fileIso)
    
    # reset values storing data from uploaded models
    uploadedDataMatrix(NULL)
    uploadedIsotope(NULL)
    uploadedModelSpecInputs(NULL)
  })
  
  observeEvent(uploadedDataMatrix(), {
    req(uploadedDataMatrix())
    updateMatrixInput(session, "dataMatrix", value = uploadedDataMatrix() )
  })
  
  observeEvent(uploadedIsotope(), {
    req(uploadedIsotope())
    updateMatrixInput(session, "isotope", value = uploadedIsotope() )
  })
  
  ### MODEL -------------------------------------------------
  
  modelSpecInputs <- modelSpecificationsServer(id = "modelSpecification", 
                                               dataMatrix = reactive(input$dataMatrix),
                                               uploadedModelSpecInputs = uploadedModelSpecInputs)
  
  modDat <- eventReactive(input$dataMatrix, {
    req(modelSpecInputs()$indVar)
    ret <- input$dataMatrix %>%
      data.frame() %>%
      .[colSums(!is.na(.)) > 0] #%>%
      #filter(complete.cases(.))
    lapply(split(ret, ret[, modelSpecInputs()$indVar]),
           function(x) x[, !apply(x, 2, function(y) all(is.na(y)))])
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
  
  savedModels <- reactiveVal(list())
  intervalTimePlot <- reactiveVal()
  
  observeEvent(input$fitModel, {
    if(!(all(length(nIndividuals1())==length(nIndividuals2())) && all(nIndividuals1()==nIndividuals2()))){
      shinyjs::alert("Number of individuals must be the same in both renewal and measurements table")
    }
    req(modDat())
    fit(NULL)
    fitted <- try({
      lapply(1:length(modDat()), function(x){
        withProgress({
          boneVars <- modelSpecInputs()$boneVars[
            which(modelSpecInputs()$boneVars %in% colnames(modDat()[[x]]))
          ]
          estimateIntervals(renewalRates = data.frame(modDat()[[x]]),
                            timeVars = modelSpecInputs()$timeVars,
                            boneVars = boneVars,
                            isoMean = unlist(isoMean()[[x]]),
                            isoSigma = unlist(isoSigma()[[x]]),
                            iter = modelSpecInputs()$iter,
                            burnin = modelSpecInputs()$burnin,
                            chains = modelSpecInputs()$chains)
        }, value = x / length(modDat()),
        message = paste0("Calculate model for individual nr.", x))
      })
    }, silent = TRUE)
    if (inherits(fitted, "try-error")) {
      shinyjs::alert(fitted[[1]])
    } else {
      allModels <- savedModels()
      allModels <- allModels[!grepl("Current", names(allModels))] # kept for the case if models 
      # from versions before 22.11.1 were uploaded
      for(i in 1:length(fitted)){
        newName <- paste0(modelSpecInputs()$indVar, "_", names(modDat())[i])
        allModels <- allModels[!grepl(newName, names(allModels))]
        newModel <- setNames(list(
          list(modelSpecifications = reactiveValuesToList(modelSpecInputs()),
               inputDataMatrix = input$dataMatrix,
               inputIsotope = input$isotope,
               fit = fitted[[i]])
        ), newName)
        allModels <- c(allModels, newModel)
      }
      
      savedModels(allModels)
    }
  })
  
  allXAxisData <- reactiveVal(data.frame())
  
  observeEvent(savedModels(), {
    req(length(savedModels()) > 0)
    
    modelChoices <- names(savedModels())
    selectedModel <- names(savedModels())[length(savedModels())]
    updateSelectInput(session, "savedModels", choices = modelChoices, selected = selectedModel)
    
    fit(savedModels()[[length(savedModels())]]$fit)
    
    # inputs in tab "Credibility intervals over time"
    updateSelectizeInput(session, "credIntTimePlot", 
                         choices = modelChoices, selected = selectedModel)
    
    updateNumericInput(session, "xmin", 
                       value = getDefaultPlotRange(savedModels(), deriv = "1")$xmin)
    updateNumericInput(session, "xmax", 
                       value = getDefaultPlotRange(savedModels(), deriv = "1")$xmax)
    updateNumericInput(session, "ymin",
                       value = getDefaultPlotRange(savedModels(), deriv = "1")$ymin)
    updateNumericInput(session, "ymax",
                       value = getDefaultPlotRange(savedModels(), deriv = "1")$ymax)
    
    # to draw x axis ticks and labels at all possible points in time present in savedModls()
    for (i in 1:length(savedModels())) {
      allXAxisData(getXAxisData(savedModels()[[i]]$fit, oldXAxisData = allXAxisData()))
    }
    
    # other tabs
    updatePickerInput(session, "savedModelsShift", 
                      choices = modelChoices, selected = selectedModel)
    updatePickerInput(session, "savedModelsTime", 
                      choices = modelChoices, selected = selectedModel)
    updatePickerInput(session, "savedModelsUserDefined", 
                      choices = modelChoices, selected = selectedModel)
  })
  
  output$shiftTimePoints <- renderPrint({
    req(input$savedModelsShift)
    fits <- getEntry(savedModels()[input$savedModelsShift], "fit")
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
  
  observeEvent(input$savedModelsUserDefined, {
    req(savedModels())
    
    fits <- getEntry(savedModels()[input$savedModelsUserDefined], "fit")
    req(!all(sapply(fits, is.null)))
    fits <- fits[!sapply(fits, is.null)]
    
    timeMin <- min(sapply(fits, function(fit) {min(fit@timeLower)}))
    timeMax <- max(sapply(fits, function(fit) {max(fit@timeUpper)}))
    
    updateNumericInput(session, "from2", value = timeMin)
    updateNumericInput(session, "to2", value = timeMax)
  })
  
  output$userDefined <- renderPrint({ 
    req(input$savedModelsUserDefined)
    fits <- getEntry(savedModels()[input$savedModelsUserDefined], "fit")
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
  
  output$summary <- renderPrint({ 
    print(fit(), pars = c("interval", "mu", "rho", "alpha")) 
  })
  
  observeEvent(input$saveModel, {
    if (trimws(input$modelName) == "") {
      shinyjs::alert("Please provide a model name")
      return()
    }
    if (input$modelName %in% names(savedModels())) {
      shinyjs::alert("Model name already present. Please choose another name")
      return()
    }
    
    if (is.null(fit())) {
      fitToSave <- NULL 
    } else {
      fitToSave <- reactiveValuesToList(fit())
    }
    newModel <- setNames(list(
      list(modelSpecifications = reactiveValuesToList(modelSpecInputs()),
           inputDataMatrix = input$dataMatrix,
           inputIsotope = input$isotope,
           fit = fitToSave)
    ), input$modelName)
    allModels <- c(savedModels(), newModel)
    savedModels(allModels)
  })
  
  observeEvent(input$loadModel, {
    req(savedModels())
    
    currentModel <- savedModels()[[input$savedModels]]
    uploadedDataMatrix(currentModel$inputDataMatrix)
    uploadedIsotope(currentModel$inputIsotope)
    uploadedModelSpecInputs(currentModel$modelSpecifications)
    fit(currentModel$fit)
    
    updateSelectizeInput(session, "credIntTimePlot", selected = input$savedModels)
    updatePickerInput(session, "savedModelsShift", selected = input$savedModels)
    updatePickerInput(session, "savedModelsTime", selected = input$savedModels)
    updatePickerInput(session, "savedModelsUserDefined", selected = input$savedModels)
    
  })
  
  uploadedNotes <- reactiveVal(NULL)
  callModule(downloadModel, "modelDownload", 
             savedModels = savedModels, uploadedNotes = uploadedNotes)
  callModule(uploadModel, "modelUpload", 
             savedModels = savedModels, uploadedNotes = uploadedNotes, 
             fit = fit, uploadedModelSpecInputs = uploadedModelSpecInputs,
             uploadedDataMatrix = uploadedDataMatrix, uploadedIsotope = uploadedIsotope
             )
  
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
      req(fit())
      exportData <- as.data.frame(extract(fit())$interval)
      namesStan <- names(fit())
      intervalNamesStan <- namesStan[grepl(pattern = "interval", namesStan)]
      colnames(exportData) <- intervalNamesStan
      switch(
        input$exportType,
        csv = exportCSV(file, exportData, colseparator(), decseparator()),
        xlsx = exportXLSX(file, exportData),
        json = exportJSON(file, exportData)
      )
    }
  )
  
  observeEvent(input$deriv,{
    req(input$savedModels, input$deriv)
    req((savedModels()[[input$savedModels]])$fit)
    
    updateNumericInput(session, "xmin", value = getDefaultPlotRange(savedModels(), deriv = input$deriv)$xmin)
    updateNumericInput(session, "xmax", value = getDefaultPlotRange(savedModels(), deriv = input$deriv)$xmax)
    updateNumericInput(session, "ymin", value = getDefaultPlotRange(savedModels(), deriv = input$deriv)$ymin)
    updateNumericInput(session, "ymax", value = getDefaultPlotRange(savedModels(), deriv = input$deriv)$ymax)
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
  
  output$plot <- renderPlot({ 
    req(fit())
    #OsteoBioR::plot(fit(), prop = input$modCredInt) 
    plot(fit(), prop = input$modCredInt) 
    })
    
  fitForTimePlot <- reactiveVal()
  savedPlot <- reactiveVal(list())
  #savedXAxisData <- reactiveVal(data.frame())
  
  observeEvent(input$credIntTimePlot, {
    req(savedModels(), input$credIntTimePlot)
    fits <- getEntry(savedModels()[input$credIntTimePlot], "fit")
    req(length(fits) > 0)
    fitForTimePlot(fits[[length(fits)]])
    
    # show default plot if plot is empty but data received
    req(is.null(intervalTimePlot()), fitForTimePlot())
    p <- plotTime(fitForTimePlot(), prop = input$modCredInt, yLim = c(input$ymin, input$ymax),
                  xLim = c(input$xmin, input$xmax), deriv = input$deriv,
                  oldXAxisData = allXAxisData(), # draws ticks at all data's times of x axis
                  colorL = input$colorL, colorU = input$colorU, alphaL = input$alphaL, alphaU =  input$alphaU,
                  xAxisLabel = input$xAxisLabel, yAxisLabel = input$yAxisLabel,
                  sizeTextY =  input$sizeTextY , sizeTextX = input$sizeTextX,
                  sizeAxisX = input$sizeAxisX, sizeAxisY = input$sizeAxisY)
    intervalTimePlot(p)
    savedPlot(p)
    #savedXAxisData(getXAxisData(fitForTimePlot()))
  })
  
  observeEvent(fit(), {
    # not as default plot, when fit() is ready, xLim & yLim are not
    req(fit(), intervalTimePlot())

    p <- plotTime(fit(), prop = input$modCredInt, yLim = c(input$ymin, input$ymax),
                  xLim = c(input$xmin, input$xmax), deriv = input$deriv,
                  oldXAxisData = allXAxisData(), # draws ticks at all data's times of x axis
                  colorL = input$colorL, colorU = input$colorU, alphaL = input$alphaL, alphaU =  input$alphaU,
                  xAxisLabel = input$xAxisLabel, yAxisLabel = input$yAxisLabel,
                  sizeTextY =  input$sizeTextY , sizeTextX = input$sizeTextX,
                  sizeAxisX = input$sizeAxisX, sizeAxisY = input$sizeAxisY)
    intervalTimePlot(p)
    savedPlot(p)
    #savedXAxisData(getXAxisData(fitForTimePlot()))
  })
  
  observeEvent(input$newPlot, {
    req(fitForTimePlot())
    p <- plotTime(fitForTimePlot(), prop = input$modCredInt, yLim = c(input$ymin, input$ymax),
                  xLim = c(input$xmin, input$xmax), deriv = input$deriv,
                  oldXAxisData = allXAxisData(), # draws ticks at all data's times of x axis
                  colorL = input$colorL, colorU = input$colorU, alphaL = input$alphaL, alphaU =  input$alphaU,
                  xAxisLabel = input$xAxisLabel, yAxisLabel = input$yAxisLabel,
                  sizeTextY =  input$sizeTextY , sizeTextX = input$sizeTextX,
                  sizeAxisX = input$sizeAxisX, sizeAxisY = input$sizeAxisY)
    intervalTimePlot(p)
    savedPlot(p)
    #savedXAxisData(getXAxisData(fitForTimePlot()))
  })
  
  observeEvent(input$addPlot, {
    req(fitForTimePlot(), length(savedPlot()) > 0)
    oldPlot <- savedPlot()
    #oldXAxisData <- savedXAxisData()
    p <- plotTime(fitForTimePlot(), prop = input$modCredInt, yLim = c(input$ymin, input$ymax),
                  xLim = c(input$xmin, input$xmax), deriv = input$deriv, oldPlot = oldPlot, 
                  #oldXAxisData = oldXAxisData,
                  oldXAxisData = allXAxisData(), # draws ticks at all data's times of x axis
                  colorL = input$colorL, colorU = input$colorU,
                  alphaL = input$alphaL, alphaU =  input$alphaU,
                  sizeTextY =  input$sizeTextY , sizeTextX = input$sizeTextX,
                  xAxisLabel = input$xAxisLabel, yAxisLabel = input$yAxisLabel,
                  sizeAxisX = input$sizeAxisX, sizeAxisY = input$sizeAxisY, secAxis = input$secAxis)
    intervalTimePlot(p)
    savedPlot(p)
    #savedXAxisData(getXAxisData(object = fitForTimePlot(), oldXAxisData = oldXAxisData))
  })
  
  output$plotTime <- renderPlot({
    req(intervalTimePlot())
    intervalTimePlot()
  })
  
  observe({
    if (is.null(modelSpecInputs()$timeMinimum) || is.null(modelSpecInputs()$timeMaximum)) {
      updateNumericInput(session, "from", value = defaultInputsForUI()$from)
      updateNumericInput(session, "to", value = defaultInputsForUI()$to)
      updateNumericInput(session, "from2", value = defaultInputsForUI()$from2)
      updateNumericInput(session, "to2", value = defaultInputsForUI()$to2)
    }
    
    req(modelSpecInputs()$timeMinimum, modelSpecInputs()$timeMaximum)
    updateNumericInput(session, "from", value = modelSpecInputs()$timeMinimum)
    updateNumericInput(session, "to", value = modelSpecInputs()$timeMaximum)
    updateNumericInput(session, "from2", value = modelSpecInputs()$timeMinimum)
    updateNumericInput(session, "to2", value = modelSpecInputs()$timeMaximum)
  })
  
  observeEvent(input$savedModelsTime, {
    req(savedModels())
    
    fits <- getEntry(savedModels()[input$savedModelsTime], "fit")
    req(!all(sapply(fits, is.null)))
    fits <- fits[!sapply(fits, is.null)]
    
    timeMin <- max(sapply(fits, function(fit) {min(fit@timeLower)}))
    timeMax <- min(sapply(fits, function(fit) {max(fit@timeUpper)}))
    
    updateNumericInput(session, "from", value = timeMin)
    updateNumericInput(session, "to", value = timeMax)
  })
  
  estimates <- eventReactive(input$estSpecTimePoint, {
    if (is.null(input$savedModelsTime)) {
      return("Please select a fitted a model.")
    }
    
    fits <- getEntry(savedModels()[input$savedModelsTime], "fit")
    
    tryCatch(
      withCallingHandlers(
        {
          ret <- lapply(fits, function(x) 
            if (!is.null(x)) estimateTimePoint(x, time = seq(input$from, input$to, by = input$by))
          )
          do.call("rbind", lapply(1:length(ret), function(x)
            if (!is.null(x)) data.frame(Individual = names(ret)[x], ret[[x]])
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
        req(fit())
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
    
    plotOutputElement <- renderPlot({ 
      req(fit())
      plot(fit(), prop = input$modCredInt) 
      #OsteoBioR::plot(fit(), prop = input$modCredInt) 
      })
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
        print({
          req(fit())
          #OsteoBioR::plot(fit(), prop = input$modCredInt) 
          plot(fit(), prop = input$modCredInt) 
          })
        
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
  
  importedStayTime <- DataTools::importDataServer(
    "stayTimeData",
    defaultSource = "file",
    customErrorChecks = list(reactive(DataTools::checkAnyNonNumericColumns))
  )
  
  observe({
    req(length(importedStayTime()) > 0)
    datStayTime$dataFile <- importedStayTime()[[1]] %>%
      as.matrix()
  }) %>%
    bindEvent(importedStayTime())
  
  observeEvent(datStayTime$dataFile, {
    updateMatrixInput(session, "stayTimeMatrix", value = datStayTime$dataFile)
  })
  
  observeEvent(input$loadStayTimeData, {
    updateMatrixInput(session, "stayTimeMatrix", value = datStayTime$dataExample() )
  })
  
  stayTimeDat <- eventReactive(input$stayTimeMatrix, {
    input$stayTimeMatrix %>%
      data.frame() %>%
      .[colSums(!is.na(.)) > 0] %>%
      filter(complete.cases(.))
  })
  estimatedStayTimes <- eventReactive(input$stayingTime, {
    req(fit())
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
