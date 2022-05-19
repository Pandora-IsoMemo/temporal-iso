test_that("Test module modelSpecifications", {
  testServer(modelSpecificationsServer,
             args = list(dataMatrix = reactive({
               getExampleDataMatrix()
             })),
             {
               # Arrange
               print("test model specifications")
               # Act & Assert
               session$setInputs(
                 timeVars = c("intStart", "intEnd"),
                 boneVars = c("bone1", "bone2", "tooth1", "tooth2"),
                 indVar = "individual",
                 iter = 1000,
                 burnin = 200,
                 chains = 3
               )
               
               expect_true(all(
                 names(session$returned()) %in%
                   c(
                     "chains",
                     "timeMinimum",
                     "timeVars",
                     "indVar",
                     "boneVars",
                     "burnin",
                     "timeMaximum",
                     "iter"
                   )
               ))
             })
})