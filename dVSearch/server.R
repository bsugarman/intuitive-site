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
library(XML)
library(methods)
#library(rcrossref)

#library(boxr)

server <- function(input, output, session) {
  
  test <- reactiveValues()
  
  output$tbl <- DT::renderDataTable({
   # conn <- dbConnect(odbc::odbc(),"clinicalaffairs")
    # on.exit(dbDisconnect(conn), add = TRUE)
   # res <-
    #  DBI::dbSendQuery(
     #   conn,
      #  "SELECT  Custom5, Custom1, Custom6, Custom7, Custom8, Translated_Author, Journal, Reviewed_Item, Year, Author, Title, Abstract, Reprint_Edition FROM EndNote2"
     # )
    #dt <- DBI::dbFetch(res)
    


    
    # Added by Ashok BALARAMAN on 2018-08-23
    # Read the stored DataFrame created by XML to DataFrame process
    
    EndNoteDF <- readRDS("EndNoteFull.rds")
    
    # Convert Factors as Characters
    EndNoteDF <- data.frame(lapply(EndNoteDF, as.character), stringsAsFactors=FALSE)
    
    dt <- data.frame(Custom5 = EndNoteDF$custom5,          
                      Custom1 = EndNoteDF$custom1,          
                      Custom6 = EndNoteDF$custom6,         
                      Custom7 = EndNoteDF$custom7,         
                      Custom8 = EndNoteDF$custom7,       
                      Translated_Author = EndNoteDF$translated.authors,
                      Journal = EndNoteDF$alt.periodical.full.title,         
                      Reviewed_Item = EndNoteDF$reviewed.item,  
                     Year = EndNoteDF$dates.year,          
                     Author = EndNoteDF$authors,       
                     Title = EndNoteDF$title,         
                     Abstract = EndNoteDF$abstract,        
                     Reprint_Edition = EndNoteDF$reprint.edition
                  #   , DOI = EndNoteDF$electronic.resource.num
                     , stringsAsFactors=FALSE)



    # dt[, input$show_vars, drop = FALSE]
    # dt3 <- transform(dt, doi_reviews = (lapply(dt$DOI,  function(x){
    #           a <- try(print(cr_citation_count(x)))
    #       # force an error
    #          if (inherits(a, 'try-error'))  return(print("NULL"))
    #           }) ))

      
    #  print(cr_citation_count(dt$DOI[1]))
    #  lapply(dt$DOI,  function(x){ 
    #        a <- try(print(cr_citation_count(x)))
    #    # force an error 
    #         if (inherits(a, 'try-error'))  return(print("NULL")) 
    #         }) 
    #     
    # #      WORKS BUT IS VERY SLOW  
    # 
    
    file <-"www/output.txt"
    
    Citations <- scan(file, character(), quote = "")
    
    Citations <- as.numeric(Citations)
    dt <- cbind(dt, Citations)
    
    setnames(
      dt,
      old = c(
        "Reviewed_Item",
        "Custom1",
        "Custom5",
        "Translated_Author",
        "Reprint_Edition",
	"Citations"
	
      ),
      new = c("LOE", "Procedure", "Robotic", "Country", "Arm", "Cited By")
    )
    
    
    
    setcolorder(
      dt,
      c(
        "Title",
        "Author",
        "Year",
        "LOE",
        "Country",
        "Procedure",
        "Journal",
        "Robotic",
        "Abstract",
        "Arm"
      )
    )
    

    dt$Abstract <- enc2native(dt$Abstract)
    dt$Journal <- enc2native(dt$Journal)
    dt$Robotic <- as.factor(dt$Robotic)
    dt$Title <- enc2native(dt$Title)
    dt$Year <- as.integer(dt$Year)
    dt$LOE <- as.factor(dt$LOE)
    dt$Procedure <- stringi::stri_sub(dt$Procedure, from = 2)
    dt$Procedure <-
      stringi::stri_replace_all_regex(dt$Procedure, "[|]", ", ")
    
    dt$Arm <- stringi::stri_sub(dt$Arm, from = 2)
    
    dt$Arm<-
      stringi::stri_replace_all_regex(dt$Arm, "[|]", ", ")
    dt$AuthorFull <- enc2native(dt$Author)
    dt$Author <-
      stringi::stri_extract_first_regex(enc2native(dt$Author), "^(.+?),|[^,]+")
    dt$Author <-
      stringi::stri_replace_all_regex(dt$Author, "[,]", " ")

    dt$Procedure <-
      stringi::stri_replace_all_regex(dt$Procedure, "[-]", "None")
    test$a <- jsonlite::toJSON(dt)
    
    
    test$textInput <- input$distinct_id
    print(colnames(dt))
    
    
    
    datatable(
      width = '100%',
      extensions = c('Buttons'),
      cbind(' ' = '&oplus;', dt),
      escape = -2,
      options = list(
         order = list(list(9, 'dsc'),list(4, 'dsc'), list(15, 'dsc')), 
        columnDefs = list(
list(className = 'dt-left', targets = c(4, 15)),
list(width = '38%', targets = c(2)),
list(width = '5px', targets = c(1)),
list(label = " ", targets = c(1)),
          list(visible = FALSE, targets = c(0, 16, 6, 8, 9, 10, 11, 12, 13, 14)),
          list(
            className = 'details-control',
            targets = 1
          )
        ),
         selector = 'td:not(:first-child)',
        language = list(search = "Search All Fields:"),
        search = list(regex = TRUE, search = test$textInput),
        server = FALSE,
        dom = 'Bfrtip',
        searchHighlight = TRUE,
        deferRender = TRUE,
        server = FALSE,
        scrollY = 500,
        scroller = TRUE,

        pageLength = 50,
        #update excel button to be sekected
        buttons = list(
          list(extend = 'colvis', columns = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 11)),
          list(extend = 'collection',
                    buttons =
list(                   
          list(
            extend = "pdf",
            text = "Export Selected PDF",
            exportOptions = list(
              stripHtml = FALSE,
              rows = ".selected",    
			columns = c(2,3,4,8,10)          
              
            ),
            
            orientation = 'landscape',
            customize = JS("function(doc){console.dir(doc);}")
          ),
          
          list(
            extend = "excel",
            text = "Export Selected Excel",
            exportOptions = list(
              stripHtml = FALSE,
              rows = ".selected"
            ))),  text = 'Export Selected Data '
            
          ),
          
          
          
          c('pageLength')
        )


      ),
      
      filter = list(position = 'top'),
      
      
      
      callback = JS(
        "
        table.column(1).nodes().to$().css({cursor: 'pointer'});
        var format = function(d) {
        return '<div style=\"background-color:#eee; padding: .5em;\"> <b>Abstract:</b> ' +
        d[10] + '<br></br>' + '</div>';
        };
        table.on('click', 'td.details-control', function() {
        var td = $(this), row = table.row(td.closest('tr'));
        if (row.child.isShown()) {
        row.child.hide();
        td.html('&oplus;');
        } else {
        row.child(format(row.data())).show();
        td.html('&CircleMinus;');
        }
        });
        
        "
      )
      ) %>% formatStyle(
  'Year',
   textAlign ="left")

})



  observeEvent(input$help,
               introjs(session, options = list("nextLabel"="Next",
                                               "prevLabel"="Previous",
                                               "skipLabel"="Skip"),
                       events = list("oncomplete" = I('swal("Congratulations!", "You are ready to use dVSearch like a pro!", "success", {
  button: "Aww yiss!",
});

')
))
  )
  
  observeEvent(input$btn, {
     shinyjs::toggleClass(class = "activated", selector = "div.dt-buttons")
     shinyjs::toggleClass(class = "activated", selector = "div.dataTables_filter")

    updateTabsetPanel(session, "inTabset",
                      selected = "tag")
    
    
  })
  
  
  observeEvent(input$box, {
    #table <- jsonlite::fromJSON(test$a)
   #ids <- test$b
    #ids <- unique(ids)
    #box_auth(client_id = "vky06dal94puks1wqh1fdiblvqgqe0vl", client_secret = "RenZnri5utxS1SLXRd7RyuZYNEnaPbpd")
    #box_write(table[ids, ], "Data.xlsx")
  })
  
   
  

  

  observeEvent(input$helpButton,{
    tags$iframe(src = "DVLibUserManual.pdf"
                , style="width:100%;",  frameborder="0"
                ,id="iframe"
                , height = "500px")
    
  })

  observeEvent(input$advanced,{
     shinyjs::toggleClass(class = "activated", selector = "div.dt-buttons")
     shinyjs::toggleClass(class = "activated", selector = "div.dataTables_filter")

  })
 observeEvent(input$advanced2,{
     shinyjs::toggleClass(class = "activated", selector = "div.dt-buttons")
     shinyjs::toggleClass(class = "activated", selector = "div.dataTables_filter")

  })


 
 
  observeEvent(input$favButton, {
    if (is.null(input$tbl_rows_selected)) {
      print("Nothing")
    } else {
      
      
 
      table <- jsonlite::fromJSON(test$a)
      print(input$tbl_rows_selected)
      test$b <- append(test$b, input$tbl_rows_selected)   
      print(test$b)
      
      ids <- test$b
      ids <- unique(ids)
      table <- jsonlite::fromJSON(test$a)
      
      test$c <- jsonlite::toJSON(table[unique(ids), ])
      print(colnames(table[ids, ]))
      
      shinyalert("Added to Favorites", type = "success", showConfirmButton = FALSE,  timer = 1000)
      
      newTable<-table[ids, ]
      setcolorder(
        newTable,
        c(
          "Title",
          "Author",
          "Year",
          "LOE",
          "Country",
          "Procedure",
          "Journal",
          "Robotic",
          "Abstract",
          "Arm"
        )
      )
      
      output$favorites <- DT::renderDataTable({
        datatable(
          
          newTable,
          width = '100%',
          extensions = list('Buttons' = NULL, 'ColReorder' = NULL),
          options = list(
      
            columnDefs = list(
              list(visible = FALSE, targets = c(0, 5, 7,8,9,10, 11, 12, 13, 15)),
              list(
                className = 'details-control',
                targets = 1
              )
            ),
            
            search = list(regex = TRUE),
            server = FALSE,
            dom = 'Bfrtip',
            searchHighlight = TRUE,
            deferRender = TRUE,
            server = FALSE,
            scrollY = 500,
            scroller = TRUE,
            pageLength = 100, 
            lengthMenu = list(c('10', '20', '50','100', '-1'), c('10', '20', '50', '100', "All")),
            buttons = list(
              list(extend = 'colvis', columns = c(1, 2, 3, 4, 5, 6, 7, 8, 14)),
              
              list(
                extend = "pdf",
                text = "Export Selected PDF",
                exportOptions = list(
			columns = c(1,15,3,6,14 ,9),      
                  stripHtml = FALSE,
                  rows = ".selected"
                ),
                orientation = 'landscape',
                customize = JS("function(doc){console.dir(doc);}")
              ),
              list(
                extend = "excel",
                text = "Export Selected Excel",
                exportOptions = list(
                  stripHtml = FALSE,
                  rows = ".selected"
                )
              ),
              
              c(  'pageLength')
            )
          ),
          
          filter = list(position = 'top')
          
        )
        
      })
    }
    
  })
  
  proxy = dataTableProxy('tbl')
  proxyTwo = dataTableProxy('favorites')
  
  
  observeEvent(input$clear, {
      shinyalert("Cleared all rows", type = "success", showConfirmButton = FALSE,  timer = 1000)

    proxy %>% selectRows(NULL)
    
  })
  
  
  observeEvent(input$selectAll, {
      shinyalert("Selected all rows", type = "success", showConfirmButton = FALSE,  timer = 1000)

    proxy %>% selectRows(input$tbl_rows_all)
    
  })
  
  observeEvent(input$selectAllFav, {      

shinyalert("Selected all rows", type = "success", showConfirmButton = FALSE,  timer = 1000)

    proxyTwo %>% selectRows(input$favorites_rows_all)
    
  })
  
  
  
  observeEvent(input$delete,{
      shinyalert("Deleted selected rows",type = "success", showConfirmButton = FALSE,  timer = 1000)

    
    table <- jsonlite::fromJSON(test$c)
    ids <- input$favorites_rows_selected
    test$b <- test$b[-ids]
    table <- table[-ids, ]
    
    
    setcolorder(
      table,
      c(
        "Title",
        "Author",
        "Year",
        "LOE",
        "Country",
        "Procedure",
        "Journal",
        "Robotic",
        "Abstract",
        "Arm"
      )
    )
    test$c <- jsonlite::toJSON(table)
    
    if (!is.null(input$favorites_rows_selected)) {
      output$favorites <- DT::renderDataTable({
        datatable(
          table,
          width = '1000px',
          extensions = list('Buttons' = NULL, 'ColReorder' = NULL),
          options = list(
            colReorder = list(realtime = TRUE),
            columnDefs = list(

              list(visible = FALSE, targets = c(0, 5, 7,8,9,10, 11, 12, 13, 15)),
              list(
                className = 'details-control',
                targets = 1
              )
            ),
            
            search = list(regex = TRUE),
            server = FALSE,
            dom = 'Bfrtip',
            searchHighlight = TRUE,
            deferRender = TRUE,
            server = FALSE,
            scrollY = 500,
            scroller = TRUE,
            pageLength = 50,
            lengthMenu = list(c('10', '20', '50','100', '-1'), c('10', '20', '50', '100', "All")),
            
            buttons = list(
              list(extend = 'colvis', columns = c(1, 2, 3, 4, 5, 6, 7, 8, 14)),
              
              list(
                extend = "pdf",
                text = "Export Selected",
                exportOptions = list(
			columns = c(1,2,3,4,7,14, 9),   

                  stripHtml = FALSE,
                  rows = ".selected"
                ),
                orientation = 'landscape',
                customize = JS("function(doc){console.dir(doc);}")
              ),
              list(
                extend = "excel",
                text = "Export Selected Excel",
                exportOptions = list(
                  stripHtml = FALSE,
                  rows = ".selected"
                )
              ),
              
              c('copy', 'pageLength')
            )
          ),
          
          filter = list(position = 'top')
          
          
        )
        
      })
    }
  })
  
}




