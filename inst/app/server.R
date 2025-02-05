library(shiny)
library(shinyWidgets)
library(OsteoBioR)
library(colourpicker)
library(shinyMatrix)
library(dplyr)
library(ggplot2)
library(rstan)
library(stats)

options(shiny.maxRequestSize = 200*1024^2
        # Set mc.cores option
        #mc.cores = parallel::detectCores() # the number of cores is set directly inside the estimation function
        )

rstan_options(auto_write = TRUE)

shinyServer(function(input, output, session) {
  # DATA -------------------------------------------------
  dat <- reactiveValues()
  uploadedDataMatrix <- reactiveVal()
  uploadedDataMatrixSD <- reactiveVal()
  uploadedIsotope <- reactiveVal()
  uploadedModelSpecInputs <- reactiveVal()
  
  ## Upload Renewal rates ----
  importedData <- DataTools::importDataServer(
    "fileData",
    customErrorChecks = list(reactive(DataTools::checkAnyNonNumericColumns)),
    defaultSource = config()[["defaultSourceData"]],
    ckanFileTypes = config()[["ckanFileTypes"]],
    options = DataTools::importOptions(
      rPackageName = config()[["rPackageName"]],
      customHelpText = helpText("The first column in your file should contain the IDs for individuals.")
    )
  )
  
  observe({
    req(length(importedData()) > 0)
    dat$dataFile <- importedData()[[1]] %>%
      as.matrix()
  }) %>%
    bindEvent(importedData())

  ## Upload Renewal rates uncertainty (optional) ----
  importedDataSD <- DataTools::importDataServer(
    "fileDataSD",
    customErrorChecks = list(reactive(DataTools::checkAnyNonNumericColumns)),
    defaultSource = config()[["defaultSourceData"]],
    ckanFileTypes = config()[["ckanFileTypes"]],
    options = DataTools::importOptions(rPackageName = config()[["rPackageName"]])
  )

  observe({
    req(length(importedDataSD()) > 0)
    dat$dataFileSD <- importedDataSD()[[1]] %>%
      as.matrix()
  }) %>%
    bindEvent(importedDataSD())
  
  ## Upload Measurements ----
  importedIso <- DataTools::importDataServer(
    "fileIso",
    customErrorChecks = list(reactive(DataTools::checkAnyNonNumericColumns)),
    defaultSource = config()[["defaultSourceData"]],
    ckanFileTypes = config()[["ckanFileTypes"]],
    options = DataTools::importOptions(
      rPackageName = config()[["rPackageName"]],
      customHelpText = helpText("The first column in your file should contain the IDs for individuals."))
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
    updateMatrixInput(session, "dataMatrixSD", value = getExampleDataMatrix(sd=TRUE))

    # reset values storing data from uploaded models
    uploadedDataMatrix(NULL)
    uploadedDataMatrixSD(NULL)
    uploadedIsotope(NULL)
    uploadedModelSpecInputs(NULL)
  })
  
  observeEvent(input$dataMatrix, {
    # use row- and column names from dataMatrix but content from dataMatrixSD
    updateMatrixNamesInput(session, "dataMatrixSD", value = input$dataMatrix, value2 = input$dataMatrixSD)
  })

  observeEvent(dat$dataFile, {
    updateMatrixInput(session, "dataMatrix", value = dat$dataFile)
    
    if (!input$renewUnc) {
      zeroSD <- setVarsForUncMatrix(renewalRates = dat$dataFile,
                                    renewalRatesUnc = NULL,
                                    timeVars = input[["modelSpecification-timeVars"]],
                                    indVar = input[["modelSpecification-indVar"]])
      
      # use row- and column names from dataMatrix but content from dataMatrixSD
      updateMatrixNamesInput(session, "dataMatrixSD", value = input$dataMatrix, value2 = zeroSD)
    }
    
    # reset values storing data from uploaded models
    uploadedDataMatrix(NULL)
    uploadedDataMatrixSD(NULL)
    uploadedIsotope(NULL)
    uploadedModelSpecInputs(NULL)
  })
  
  observeEvent(dat$dataFileSD, {
    updateMatrixInput(session, "dataMatrixSD", value = dat$dataFileSD)
    
    # reset values storing data from uploaded models
    uploadedDataMatrixSD(NULL)
    uploadedIsotope(NULL)
    uploadedModelSpecInputs(NULL)
  })
  
  observeEvent(dat$fileIso, {
    updateMatrixInput(session, "isotope", value = dat$fileIso)
    
    # reset values storing data from uploaded models
    uploadedDataMatrix(NULL)
    uploadedDataMatrixSD(NULL)
    uploadedIsotope(NULL)
    uploadedModelSpecInputs(NULL)
  })
  
  observeEvent(uploadedDataMatrix(), {
    req(uploadedDataMatrix())
    updateMatrixInput(session, "dataMatrix", value = uploadedDataMatrix() )
  })
  
  observeEvent(uploadedDataMatrixSD(), {
    req(uploadedDataMatrixSD())
    updateMatrixInput(session, "dataMatrixSD", value = uploadedDataMatrixSD() )
  })
  
  observeEvent(uploadedIsotope(), {
    req(uploadedIsotope())
    updateMatrixInput(session, "isotope", value = uploadedIsotope() )
  })
  
  # MODEL -------------------------------------------------
  
  modelSpecInputs <- modelSpecificationsServer(id = "modelSpecification", 
                                               dataMatrix = reactive(input$dataMatrix),
                                               uploadedModelSpecInputs = uploadedModelSpecInputs)
  
  isoMean <- eventReactive(input$isotope, {
    splitVar <- extractIndividuals(matrix = input$isotope, indVar = input[["modelSpecification-indVar"]])
    ret <- (input$isotope %>%
      data.frame() %>%
      .[colSums(!is.na(.)) > 0] #%>%
      #filter(complete.cases(.))
     )
    # use pre-last columns to get mean, because ind column does not exists when rownames are used
    lapply(split(ret[, length(ret)-1], splitVar), function(x) as.numeric(na.omit(x)))
  })
  
  isoSigma <- eventReactive(input$isotope, {
    splitVar <- extractIndividuals(matrix = input$isotope, indVar = input[["modelSpecification-indVar"]])
    ret <- (input$isotope %>%
      data.frame() %>%
      .[colSums(!is.na(.)) > 0] #%>%
      #filter(complete.cases(.))
     )
    # use last columns to get sigma, because ind column does not exists when rownames are used
    lapply(split(ret[, length(ret)], splitVar), function(x) as.numeric(na.omit(x)))
  })
  
  fit <- reactiveVal(NULL)
  
  nIndividuals1 <- reactive({
    extractIndividuals(matrix = input$isotope, 
                       indVar = input[["modelSpecification-indVar"]]) %>%
      na.omit() %>%
      unique()
    })
  nIndividuals2 <- reactive({
    extractIndividuals(matrix = input$dataMatrix, 
                       indVar = input[["modelSpecification-indVar"]]) %>%
      na.omit() %>%
      unique()
    })
  
  savedModels <- reactiveVal(list())
  intervalTimePlot <- reactiveVal()
  
  modelFittingTimeTxt <- reactiveVal()
  
  ## Fit Model ----
  observeEvent(input$fitModel, {
    if(!(all(length(nIndividuals1())==length(nIndividuals2())) && all(nIndividuals1()==nIndividuals2()))){
      shinyjs::alert("Number of individuals must be the same in both renewal and measurements table")
    }
    
    if (!input$renewUnc) {
      # update zero SD matrix regarding selected vars
      renewalRatesUnc <- setVarsForUncMatrix(renewalRates = input$dataMatrix,
                                             renewalRatesUnc = NULL,
                                             timeVars = input[["modelSpecification-timeVars"]],
                                             indVar = input[["modelSpecification-indVar"]])
    } else {
      renewalRatesUnc <- input$dataMatrixSD
    }
    
    splittedData <- cleanAndSplitData(indVar = input[["modelSpecification-indVar"]], 
                                      renewalRates = input$dataMatrix,
                                      renewalRatesUnc = renewalRatesUnc)
    modDat <- splittedData$renewalRatesPerInd
    modDatSD <- splittedData$renewalRatesUncPerInd
    
    req(modDat)
    fit(NULL)
    modelFittingTimeTxt(NULL)
    
    elapsedTime <- system.time({
      set.seed(getSeed(fixSeed = !input[["modelSpecification-rndmSeed"]],
                       seedValue = input[["modelSpecification-fixedSeed"]]))
      
      fitted <- try({
        lapply(1:length(modDat), function(x){
          withProgress({
            boneVars <- input[["modelSpecification-boneVars"]][
              which(input[["modelSpecification-boneVars"]] %in% colnames(modDat[[x]]))
            ]
            estimateIntervals(renewalRates = data.frame(modDat[[x]]),
                              timeVars = input[["modelSpecification-timeVars"]],
                              boneVars = boneVars,
                              indVar = names(modDat)[x],
                              isoMean = unlist(isoMean()[[x]]),
                              isoSigma = unlist(isoSigma()[[x]]),
                              renewalRatesSD = data.frame(modDatSD[[x]]),
                              iter = input[["modelSpecification-iter"]],
                              burnin = input[["modelSpecification-burnin"]],
                              chains = input[["modelSpecification-chains"]])
          }, value = x / length(modDat),
          message = paste0("Calculate model for individual nr.", x))
        })
      }, silent = TRUE)
    })[3]
    
    fittingTime <- sprintf("Elapsed time of model fitting for all %s individuals:  %5.2f minutes\n", 
                           length(modDat),
                           elapsedTime / 60)
    modelFittingTimeTxt(fittingTime)
    cat(fittingTime)
    
    if (inherits(fitted, "try-error")) {
      shinyjs::alert(fitted[[1]])
    } else {
      allModels <- savedModels()
      allModels <- allModels[!grepl("Current", names(allModels))] # kept for the case if models 
      # from versions before 22.11.1 were uploaded
      for(i in 1:length(fitted)){
        if (length(input[["modelSpecification-indVar"]]) == 0 || input[["modelSpecification-indVar"]] == "") {
          prefix <- "individual"
        } else {
          prefix <- input[["modelSpecification-indVar"]]
        }
        
        newName <- paste0(prefix, "_", names(modDat)[i])
        allModels <- allModels[!grepl(newName, names(allModels))]
        newModel <- setNames(list(
          list(modelSpecifications = reactiveValuesToList(modelSpecInputs()),
               inputDataMatrix = input$dataMatrix,
               inputDataMatrixSD = input$inputDataMatrixSD,
               inputIsotope = input$isotope,
               fit = fitted[[i]])
        ), newName)
        allModels <- c(allModels, newModel)
      }
      
      savedModels(allModels)
    }
  })
  
  output$fittingTimeTxt <- renderUI(HTML(modelFittingTimeTxt()))
  
  observeEvent(savedModels(), {
    req(length(savedModels()) > 0)
    
    modelChoices <- names(savedModels())
    
    selectedModel <- names(savedModels())[length(savedModels())]
    updateSelectInput(session, "savedModels", choices = modelChoices, selected = selectedModel)
    
    fit(savedModels()[[selectedModel]]$fit)
    
    updatePickerInput(session, "savedModelsShift", choices = modelChoices, selected = selectedModel)
    updatePickerInput(session, "savedModelsTime", choices = modelChoices, selected = selectedModel)
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
    
    ifelse(nrow(shiftTime) == 0, return("No shifts were found"), return(shiftTime))
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
  
  ## Save / Load a Model ----
  observeEvent(input$saveModel, {
    logging("Save model")
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
      fitToSave <- fit()
    }
    # plot format cannot be saved, this would require refactoring of select inputs to save/load a
    # single model, or download a list of models, selecting models for the timePlot
    newModel <- setNames(list(
      list(modelSpecifications = reactiveValuesToList(modelSpecInputs()),
           inputDataMatrix = input$dataMatrix,
           inputDataMatrixSD = input$inputDataMatrixSD,
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
    uploadedDataMatrixSD(currentModel$inputDataMatrixSD)
    
    uploadedIsotope(currentModel$inputIsotope)
    uploadedModelSpecInputs(currentModel$modelSpecifications)
    fit(currentModel$fit)
    
    updateSelectizeInput(session, "timePlotFormat-plotTimeModels", selected = input$savedModels)
    updateSelectizeInput(session, "timePlotFormat-formatTimePlot", selected = input$savedModels)
    updatePickerInput(session, "savedModelsShift", selected = input$savedModels)
    updatePickerInput(session, "savedModelsTime", selected = input$savedModels)
    updatePickerInput(session, "savedModelsUserDefined", selected = input$savedModels)
    
  })
  
  ## Down- / Upload Models ----
  observe({
    updateSelectInput(session, "selectedModels", choices = names(savedModels()),
                      selected = names(savedModels())[length(savedModels())])
  })
  
  uploadedNotes <- reactiveVal(NULL)
  DataTools::downloadModelServer("modelDownload",
                                 dat = reactive(savedModels()[input$selectedModels] %>%
                                                  removeModelOutputs()),
                                 inputs = reactiveValues(),
                                 model = reactive(savedModels()[input$selectedModels] %>%
                                                    extractModelOutputs()),
                                 rPackageName = config()[["rPackageName"]],
                                 fileExtension = config()[["fileExtension"]],
                                 modelNotes = uploadedNotes,
                                 triggerUpdate = reactive(TRUE))
  
  uploadedValues <- DataTools::importServer("modelUpload",
                                            title = "Import Model",
                                            importType = "model",
                                            ckanFileTypes = config()[["ckanModelTypes"]],
                                            ignoreWarnings = TRUE,
                                            defaultSource = config()[["defaultSourceModel"]],
                                            fileExtension = config()[["fileExtension"]],
                                            options = DataTools::importOptions(
                                              rPackageName = config()[["rPackageName"]]
                                            ))
  
  observe({
    req(length(uploadedValues()) > 0)
    # update notes in tab down-/upload ----
    uploadedNotes(uploadedValues()[[1]][["notes"]])
    
    # extract model object(s)
    uploadedData <- extractSavedModels(upload = uploadedValues()[[1]])
    
    # rename model if name already exists
    uploadedData <- uploadedData %>%
      DataTools::renameExistingNames(oldList = savedModels())
    
    # load model object(s)
    savedModels(c(savedModels(), uploadedData))
    
    currentModel <- savedModels()[[length(savedModels())]]
    
    if (!is.null(uploadedDataMatrix())) {
      showNotification("Updating input data under 'Data' ...",
                       duration = 10,
                       closeButton = TRUE,
                       type = "message")
    }
    uploadedDataMatrix(currentModel$inputDataMatrix)
    uploadedDataMatrixSD(currentModel$inputDataMatrixSD)
    uploadedIsotope(currentModel$inputIsotope)
    
    if (!is.null(uploadedModelSpecInputs())) {
      showNotification("Updating model input values under 'Model' ...", 
                       duration = 10,
                       closeButton = TRUE,
                       type = "message")
    }
    uploadedModelSpecInputs(currentModel$modelSpecifications)
    
    fit(currentModel$fit)
    showNotification("Model loaded", duration = 10, closeButton = TRUE, type = "message")
  }) %>%
    bindEvent(uploadedValues())
  
  ## Export Summary ----
  shinyTools::dataExportServer("exportSummary", filename = "isotopeData", dataFun = reactive({
    function() {
      if (is.null(fit()))
        return(NULL)
      
      exportData <- as.data.frame(extract(fit())$interval)
      namesStan <- names(fit())
      intervalNamesStan <- namesStan[grepl(pattern = "interval", namesStan)]
      colnames(exportData) <- intervalNamesStan
      
      exportData
    }
  }))
  
  # custom points ----
  custom_points <- reactiveVal(list())
  shinyTools::customPointsServer("customPoints", plot_type = "ggplot", custom_points = custom_points)
  
  # render "Credibility Intervals" plot ----
  plotToExport <- reactiveVal(NULL)
  output$plot <- renderPlot({ 
    req(fit())
    #OsteoBioR::plot(fit(), prop = input$modCredInt) 
    p <- plot(fit(), prop = input$modCredInt) %>%
      shinyTools::addCustomPointsToGGplot(custom_points = custom_points()) %>%
      shinyTools::shinyTryCatch(errorTitle = "Plotting failed")
    plotToExport(p)
    p
  })
    
  # create plotTime ----
  timePlotFormattingServer(id = "timePlotFormat", savedModels = savedModels)
  
  observe({
    updateNumericInput(session, "from", 
                       value = getTimeMin(mtrx = input$dataMatrix, 
                                          timeVars = input[["modelSpecification-timeVars"]]))
    updateNumericInput(session, "to", 
                       value = getTimeMax(mtrx = input$dataMatrix, 
                                          timeVars = input[["modelSpecification-timeVars"]])
      )
    updateNumericInput(session, "from2", 
                       value = getTimeMin(mtrx = input$dataMatrix, 
                                          timeVars = input[["modelSpecification-timeVars"]]))
    updateNumericInput(session, "to2", 
                       value = getTimeMax(mtrx = input$dataMatrix, 
                                          timeVars = input[["modelSpecification-timeVars"]]))
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
  
  shinyTools::dataExportServer("exportTimePointEst",
                               filename = "timePointEstimates",
                               dataFun = reactive({
                                 function() {
                                   if (length(input$savedModelsTime) == 0 ||
                                       any(input$savedModelsTime == "") ||
                                       length(input$estSpecTimePoint) == 0 ||
                                       input$estSpecTimePoint == 0 ||
                                       is.character(estimates()))
                                     return(NULL)
                                   
                                   estimates()
                                 }
                               }))
  
  shinyTools::dataExportServer("exportCredIntDat",
                               filename = "credibilityIntervals",
                               dataFun = reactive({
                                 function() {
                                   if (is.null(fit()))
                                     return(NULL)
                                   
                                   as.data.frame(fit())
                                 }
                               }))
  
  # Downloads Plots -------------------------------------------------
  shinyTools::plotExportServer("exportCredIntPlot",
                               plotFun = reactive({
                                 function() {
                                   plotToExport()
                                 }
                               }),
                               plotType = "none", #"ggplot", #<- fix issue with labels first
                               filename = sprintf("%s_Credibility_Intervals", gsub("-", "", Sys.Date())))
  
  # RESIDING TIME ------------------------------------------
  datStayTime <- reactiveValues()
  
  datStayTime$dataExample <- eventReactive(input$loadStayTimeData, ignoreNULL = TRUE, {
    as.matrix(data.frame(
      siteMeans = c(-8, -10),
      siteSigma = c(1, 1.5)))
  })
  
  importedStayTime <- DataTools::importDataServer(
    "stayTimeData",
    customErrorChecks = list(reactive(DataTools::checkAnyNonNumericColumns)),
    defaultSource = config()[["defaultSourceData"]],
    ckanFileTypes = config()[["ckanFileTypes"]],
    options = DataTools::importOptions(rPackageName = config()[["rPackageName"]])
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
                     print = FALSE) %>%
      shinyTools::shinyTryCatch(errorTitle = "Calculation failed", alertStyle = "shinyalert")
  })
  output$estimatedStayTimes <- renderPrint({ estimatedStayTimes() })
  
  shinyTools::dataExportServer("exportStayTimeDat",
                               filename = "stayTimeLengths",
                               dataFun = reactive({
                                 function() {
                                   if (length(fit()) == 0 ||
                                       is.null(input$stayingTime) ||
                                       input$stayingTime == 0 || length(estimatedStayTimes()) == 0)
                                     return(NULL)
                                   
                                   resTime <- estimatedStayTimes()
                                   as.data.frame(resTime$stayTimes)
                                 }
                               }))
  
  # COMPUTE ISOTOPIC VALUES ------------------------------
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
  
  observeEvent(input$loadHistData, {
    updateMatrixInput(session, "historicData", value = datIso$dataExample() )
  })
  
  importedHistData <- DataTools::importDataServer(
    "fileHistData",
    customErrorChecks = list(reactive(DataTools::checkAnyNonNumericColumns)),
    defaultSource = config()[["defaultSourceData"]],
    ckanFileTypes = config()[["ckanFileTypes"]],
    options = DataTools::importOptions(rPackageName = config()[["rPackageName"]])
  )
  
  observe({
    req(length(importedHistData()) > 0)
    datIso$dataFile <- importedHistData()[[1]] %>%
      as.matrix()
  }) %>%
    bindEvent(importedHistData())
  
  observeEvent(datIso$dataFile, {
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
    ) %>%
      shinyTools::shinyTryCatch(errorTitle = "Calculation failed", alertStyle = "shinyalert"),
    sapply(isoHistDat() %>% select(input$boneVarsHist), quantile, probs = c(0.025, 0.975)) %>% t()
    
    )
  })
  
  output$isotopicValues <- renderTable({ isotopicValues() }, rownames = TRUE)
  
  shinyTools::dataExportServer("exportResultsDat",
                               filename = "isotopicValues",
                               dataFun = reactive({
                                 function() {
                                   if (length(input$historicData) == 0 ||
                                       any(input$historicData == "") ||
                                       length(input$calcIsotopicValues) == 0 ||
                                       input$calcIsotopicValues == 0)
                                     return(NULL)
                                   
                                   isotopicValues()
                                 }
                               }))

  observeEvent(input$getHelp, {
    showModal(modalDialog(
      title = "Help",
      easyClose = TRUE,
      getHelp(input$tab)
    ))
  })
})
