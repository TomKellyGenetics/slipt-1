library(shiny)

# Define UI for dataset viewer application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Synthetic Lethal Interaction Discovery Pipeline"),
  
  # Sidebar with controls to provide a caption, select a dataset, and 
  # specify the number of observations to view. Note that changes made
  # to the caption in the textInput control are updated in the output
  # area immediately as you type
  sidebarPanel(
    textInput("caption", "Caption:", "Predicted Synthetic Lethal Interactions"),

  # Remove some of these options for now to reduce the data file being uploaded to github    
    selectInput("dataset", "Choose a cancer type for dataset:", 
                choices = c(#"Brain - Glioblastoma", 
                            #"Brain - Lower Grade Glioma", 
                            "Breast - Invasive Carcinoma", 
                            #"Colon - Adenocarcinoma",
                            #"Kidney - Clear Cell Carcinoma", "Leukemia (AML)", 
                            #"Lung - Adenocarcinoma", "Lung - Squamous Cell Carcinoma",
                            #"Low Grade Brain", 
                            #"Ovarian - Serious Cystadenocarcinoma",
                            #"Stomach - Adenocarinoma" ,
                            #"Rectum - Adenocarcinoma", 
                            #"Uterine Corpus Endometrioid Carcinoma", 
                            "Colorectal - Adenocarcinoma"),
                selected="Breast - Invasive Carcinoma"),
    
    checkboxInput("choosesubset", "Elect to subset dataset:"),
      conditionalPanel(
        condition = "input.choosesubset == true",
        selectInput("subset", "Choose subset of dataset samples:", 
                  choices = c("All Samples", "Tumour Only", "Normal Only")),
        checkboxInput("choosegenes", "Choose subset of dataset genes:"),
        conditionalPanel(
            condition = "input.choosegenes == true",
            textInput("geneset", "Enter Gene Subset (space separated):")
      )
    ),
    
    textInput("query", "Select Query Gene", value = "CDH1"),
        
    numericInput("obs", "Number of observations to view:", 20),
    
    submitButton("Run Synthetic Lethal Discovery Tool or Select Subset"),
    
    downloadButton('downloadSLgenes', 'Download Significant Genes Table'),
    
    downloadButton('downloadSLtable', 'Download Complete Analysis Output'),
    
    downloadButton('downloadData', 'Download Normalised TCGA Dataset')
  ),
  
  
  # Show the caption, a summary of the dataset and an HTML table with
  # the requested number of observations
  mainPanel(
      h3(textOutput("caption")),
      textOutput("info"),
      textOutput("summary1"),
      textOutput("summary2"),
      textOutput("summary3"),
      tableOutput("view"),
      textOutput("error"),
      textOutput("genes"),
      textOutput("currentTime")
    )
    
))
