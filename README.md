slipt (Synthetic Lethal Interaction Prediction Tool)
====================================================

This code provides a simple R Shiny interface to an R-based script for identifying potential synthetic lethal 
interactions in cancer between pairs of genes.  

In order to run the tool within R, users need to install the "Shiny" package, and place the ui.R and server.R scripts 
together in a folder (e.g., a directory called "SLIPT").  The following commands can then be issued from within R
to initialize the interface (note that the SLIPT folder needs to be within the working directory for this code to
work):

```
library(shiny)
runApp('SLIPT')
```
