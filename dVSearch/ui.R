library(shiny)
library(shinyjs)
library(DBI)
library(data.table)
library(odbc)
library(DT)
library(leaflet)
library(rintrojs)
library(shinycssloaders)
library(shinyalert)
library(shinyWidgets)
library(shinythemes)
#library(boxr)


jscode <- "$(function() {
  var $els = $('[data-proxy-click]');
$.each(
$els,
function(idx, el) {
var $el = $(el);
var $proxy = $('#' + $el.data('proxyClick'));
$el.keydown(function (e) {
if ((e.keyCode || e.which) == 13) {
$proxy.click();
}
});
}
);
});"

ui <- fluidPage(
  
  tags$head(
    tags$link(
      type = "text/css",
      rel="stylesheet",
      href = "tester1.css"), 
    tags$script(HTML(jscode)), 
    
    tags$link(
      type = "text/css",
      rel="stylesheet",
      href ="https://fonts.googleapis.com/css?family=Montserrat"),
    
    tags$link(
      type = "text/css",
      rel="stylesheet",
      href ="https://fonts.googleapis.com/css?family=PT+Sans+Narrow"),
    tags$script(src = "matomo.js")
  ),
  
  img(src='logo.png', align = "right", width = "150px", height = "60px"),
  introjsUI(),
  useShinyjs(),


  inlineCSS(list(.activated = "display: inline-block")),
  useShinyalert(),
introBox(
    titlePanel("dV Search"), data.step = 1, data.intro = "Welcome to dVSearch - Intuitive's very own search engine for robotics literature! 
This virtual tour will take you through some of the basics of searching our database.")
  ,
  tabsetPanel(
    id = "inTabset", type = "tabs",
    tabPanel(
      "Home",   
shiny::textInput("distinct_id", label = HTML('<div>Welcome to the </div><div> da Vinci Clinical Evidence Search</div>'), value = "", width = NULL, placeholder = "Search for..."), `data-proxy-click` = "btn",
      align = "center",
      actionButton("btn", icon("search"))    
    ),

    tabPanel(
      "Search", value = "tag",
      dropdownButton(circle = FALSE,  label ="Help",
      actionButton("helpButton", "User Guide", onclick = "window.open('DVLibUserManual.pdf')"),
      actionButton("help", "Virtual Tour", width = '100%'),

      actionButton("defs", "LOE?", onclick = "window.open('defs.pdf')"),
	actionButton("askus", "Ask Us", onclick ="window.open('https://docs.google.com/forms/d/1Ze8Cc4fO5U63GvDDApeaYdL3AYP-b5dprfr2JsKAqKc/', '_blank')")),
     introBox(
 actionButton("favButton", "Add to Favorites", onclick = "print(hey)"),  data.step = 5, data.intro = "If you want to save your 
'selected' articles while you make another search, simply click on 'Add to Favorites' and your articles will be saved under the 'Favorites' tab."
, style = "display: inline-block;"),
           introBox(actionButton("clear", "Clear Selected"), data.step = 6, data.intro = "If you've selected too many rows, you can simply clear
 your selected rows by hitting the 'Clear' button.", style = "display: inline-block;"),
  introBox(actionButton("selectAll", "Select All"), data.step = 7, data.intro = "Alternatively, if you want to interact with all of your results at once, you can 
simply press select all and it will highlight all of your filtered search results.", style = "display: inline-block;"),

    introBox( actionButton("advanced", "Advanced")
, data.step = 8, data.intro = "If you want to export results from your results table (without adding them to your favorites), change which columns
 are visible, or alter the number of rows visible, then press the 'Advanced' button.", style = "display: inline-block;"),

     	introBox( introBox( introBox(
      withSpinner(DT::dataTableOutput("tbl", width = '100%')), data.step = 2, data.intro = "Once you've made a search, you will
 see all of your results pop up in this table!", data.position = "top"
), data.step = 3, data.intro = "To view the abstract of an article, simply press the + button to the left of the title", data.position = "top"
), data.step = 4, data.intro = "To 'select' an article, simply click anywhere on the row and the entire section will become highlighted",
 data.position = "top"
)


 
    ),    tabPanel(
      "Favorites",
      actionButton("delete", "Delete Selected Rows"),
      actionButton("selectAllFav", "Select All"),
      actionButton("advanced2", "Advanced"),

      withSpinner(DT::dataTableOutput("favorites", width = "100%"))

     #actionButton("box", "Export to Box"),
     #downloadButton("downloadData", "Download")


      
    )
  )
)

