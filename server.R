library(shiny)
options(shiny.deprecation.messages=FALSE)

load("mikSLIPT-small.RData")

#input<-list("Breast", "CDH1", 10)
#names(input)<-c("dataset", "query", "obs")

# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {
  
  # By declaring databaseInput as a reactive expression we ensure that:
  #
  #  1) It is only called when the inputs it depends on changes
  #  2) The computation and result are shared by all the callers (it 
  #     only executes a single time)
  #  3) When the inputs change and the expression is re-executed, the
  #     new result is compared to the previous result; if the two are
  #     identical, then the callers are not notified
  #
  
  #names(kp)<-c("Breast", "Colon", "Ovarian", "Rectum", "Stomach", "Colorectal")
  datasetInput <- reactive({
  switch(input$dataset,
# Remove some of these options for now to reduce the data file being uploaded to github
#         "Brain - Glioblastoma" = DataMatrixBrainNorm,
         "Breast - Invasive Carcinoma" = DataMatrixBreastNorm,
#         "Colon - Adenocarcinoma" = DataMatrixColonNorm,
#         "Kidney - Clear Cell Carcinoma" = DataMatrixKidneyNorm,
#         "Leukemia (AML)"= DataMatrixLeukemiaNorm,
#         "Lung - Adenocarcinoma" = DataMatrixLungNorm,
#         "Lung - Squamous Cell Carcinoma" = DataMatrixLungSquamousCellNorm,
#         "Brain - Lower Grade Glioma" = DataMatrixLowGradeBrainNorm,
#         "Ovarian - Serious Cystadenocarcinoma" = DataMatrixOvarianNorm,
#         "Rectum - Adenocarcinoma" = DataMatrixRectumNorm,
#         "Uterine Corpus Endometrioid Carcinoma" = DataMatrixUterineEndometrialNorm,
#         "Stomach - Adenocarinoma" = DataMatrixStomachNorm,
         "Colorectal - Adenocarcinoma" = DataMatrixColorectalNorm
    )
  })
  #Take Columns of chosen Samples
  subsetDataInput <- reactive({
    switch(input$subset,
           "All Samples" = datasetInput(),
           "Tumour Only" = datasetInput()[,sort(c(grep("01A", colnames(datasetInput())),grep("01B", colnames(datasetInput())),grep("01C", colnames(datasetInput())),grep("01D", colnames(datasetInput())),grep("06A", colnames(datasetInput()))))],
           "Normal Only" = datasetInput()[,sort(c(grep("11A", colnames(datasetInput())),grep("11B", colnames(datasetInput()))))]
    )
  })
  #Take rows of chosen Genes
  subsetInput <- reactive({
    switch(as.character(input$choosegenes),
           "TRUE" = subsetDataInput()[match(c(input$query, strsplit(input$geneset, " ")[[1]]),rownames(subsetDataInput())),],
           "FALSE" = subsetDataInput()
    )
  })
  
  #Define a function to generate SL detection
  detectSL<-
    function(query, dataset){
      if(query %in% rownames(dataset)){
      datasetx<-apply(dataset,1,function(y) ifelse(y<quantile(as.numeric(y),1/3, na.rm=T),0,
                                                   ifelse(y>quantile(as.numeric(y),2/3, na.rm=T),2,1)))
      query.datax<-datasetx[,match(query, colnames(datasetx))]
      aa<-apply(datasetx[,(1:ncol(datasetx) %in% match(query, colnames(datasetx))==F)],2, function(x) chisq.test(table(query.datax, x)))
      chi.pv<-lapply(aa, function(x) x$p.value)
      ee<-lapply(aa, function(x) x$expected)
      oo<-lapply(aa, function(x) x$observed)
      synleth<-lapply(aa, function(x) ifelse(x$observed[1,1]<x$expected[1,1] & x$observed[1,3]>x$expected[1,3],1,0))
    #Format Data for Output in CSV  
    kp<-cbind(names(chi.pv),
                   rownames(dataset)[match(names(chi.pv),rownames(dataset))],
                   unlist(lapply(oo,function(x) x[1,1])),
                   unlist(lapply(ee,function(x) x[1,1])),
                   unlist(lapply(oo,function(x) x[1,3])),
                   unlist(lapply(ee,function(x) x[1,3])),
                   unlist(synleth),unlist(chi.pv),
                   unlist(p.adjust(chi.pv)),
                   unlist(p.adjust(chi.pv, method="BH"))
                   )[order(unlist(chi.pv)),]
    colnames(kp)<-c("Probe","Gene","ObsLow","ExpLow","ObsHigh",
                         "ExpHigh","SynLethal","rawPval","adjPval_holm", "adjPval_FDR")
   return(kp)
   }
    else return(list(head(rownames(dataset), n=nrow(dataset)), "Query gene not contained in the dataset. Please enter the gene symbol in UPPER CASE, examples are shown below. Please contact the curator, Tom Kelly, for more information: kelsi602@student.otago.ac.nz."))
  }
  
  
  #generate parameters
  query <- reactive(input$query)

  dataset <- reactive(subsetInput())
    
  #Use function to Produce Output
  SLtable<-reactive({detectSL(query(), dataset())})
  
  # The output$caption is computed based on a reactive expression that
  # returns input$caption. When the user changes the "caption" field:
  #
  #  1) This function is automatically called to recompute the output 
  #  2) The new caption is pushed back to the browser for re-display
  # 
  # Note that because the data-oriented reactive expressions below don't 
  # depend on input$caption, those expressions are NOT called when 
  # input$caption changes.
  output$caption <- renderText({
    input$caption
  })
  
  number1<-function(x) sum(as.numeric(x[,7]))
  number2<-function(x) sum(as.numeric((x[(x[,9]<0.05),7])))
  number3<-function(x) sum(as.numeric((x[(x[,10]<0.05),7])))
  
  output$info <-renderText(function(){
    paste("Predictions generated generated from The Cancer Genome Atlas expression dataset of", as.numeric(ncol(subsetInput())), "tumour/normal samples and", as.numeric(nrow(datasetInput())), "genes.")
  })
  
  output$summary1 <- renderText(function(){if(is.matrix(SLtable())){
    paste(number1(SLtable()),"synthetic lethal interations with", input$query, "detected.")
  }})
  output$summary2 <- renderText(function(){if(is.matrix(SLtable())){
    paste(number2(SLtable()),"significant synthetic lethal interations with", input$query,  "detected in", input$dataset, "cancer with Holm p-value adjustment.")
  }})
  output$summary3 <- renderText(function(){if(is.matrix(SLtable())){
    paste(number3(SLtable()),"significant synthetic lethal interations with", input$query, "detected in", input$dataset, "cancer with FDR p-value adjustment.")
  }})
  output$view <- renderTable(function(){if(is.matrix(SLtable())){
    head(SLtable()[grep(1, SLtable()[,7]),8:10][(SLtable()[grep(1, SLtable()[,7]),10]<0.05),], n=input$obs, scipen=-2)
  }})
  
  output$error <- renderText(function(){if(is.list(SLtable())){
    SLtable()[[2]]
  }})
  output$genes <- renderText(function(){if(is.list(SLtable())){
    head(SLtable()[[1]], n=input$obs)
  }})
      
  output$currentTime <- renderText({
    paste("Data Generated on", format(Sys.time(), "%a %b %d %X %Y"))
  })  
  
  output$downloadData <- downloadHandler(
    filename = function() { paste("TCGA", input$dataset, Sys.Date(), '.csv', sep='') },
    content = function(file) {
      write.csv(datasetInput(), file)
    })
  output$downloadSLgenes <- downloadHandler(
    filename = function() { paste("SLgenes", input$dataset, input$query, Sys.Date(), '.csv', sep='') },
    content = function(file) {
      write.csv(SLtable()[grep(1, SLtable()[,7]),2:10][(SLtable()[grep(1, SLtable()[,7]),10]<0.05),], file)
    })
  output$downloadSLtable <- downloadHandler(
    filename = function() { paste("SLtable", input$dataset, input$query, Sys.Date(), '.csv', sep='') },
    content = function(file) {
      write.csv(SLtable(), file)
    })
  
})
