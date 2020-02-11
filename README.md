# Litterati-Challenge-Visualization
Source code to visualize litter data + instructions to run 

You should have R and RStudio installed. 

Download the zip folder from this repository. 

Open your file finder, find the downloaded zip, and unzip it.

Drag and drop the unzipped folder into the RStudio icon. 

It should open a new RStudio window with the files loaded.


Switch the code view to the app.R file. The file has this at the top

library(ggplot2)

library(lubridate)

library(shiny)

library(shinydashboard)

library(dplyr)

library(comprehenr)

library(hashmap)

library(leaflet)

library(readr)

library(DT)

These are the necessary libraries needed to run the program. 

If you don't have them, install it on the RStudio REPL with this command 

install.packages("library_name") or install.packages(c("library1", "library2", "libraryN"))


Once the packages are installed, click the green "Run App" button at the top right of the code editor. 

A new window with the visualization will appear. Then you can interact with it and explore.

