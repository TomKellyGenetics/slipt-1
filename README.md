slipt (Synthetic Lethal Interaction Prediction Tool)
====================================================

This code provides a simple R Shiny interface to an R-based script for identifying potential synthetic lethal 
interactions in cancer between pairs of genes.  

In order to run the tool within R, users need to install the "Shiny" package, and place the ui.R and server.R scripts 
together in a folder (e.g., a directory called "SLIPT") along with the data file ("mikSLIPT-small.RData").  The following commands can then be issued from within R to initialize the interface (note that the SLIPT folder needs to be within the working directory for this code to work):

```
library(shiny)
runApp('SLIPT')
```

Note that the data file includes a reduced data set (colorectal and breast cancer microarray data from TCGA) to shrink the file size below the 50MB limit imposed by Github.  The TCGA data are made available here as per the data access terms at:

http://cancergenome.nih.gov/publications/publicationguidelines

Specifically:

 - "There are no restrictions on the use of TCGA data in publications or presentations after the initial TCGA global analysis publication." (Global analyses of the Colorectal and Breast data sets were published in 2012: Nature, 487(7407), 330–337; and Nature, 490(7418), 61–70).
 - "There are no restrictions on the use of TCGA data for legitimate research purposes not involving publication or presentation."
