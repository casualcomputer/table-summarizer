# Part 0: Install the required R packages 
list.of.packages <- c("odbc", "DBI","scales", "DT","shiny", "shinyWidgets")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, type="binary",dependencies=TRUE)


# Part 1: Create helper functions that generate SQL queries 
# most up-to-date codes: https://github.com/casualcomputer/sql.mechanic

#' Generate SQL codes used to summarize tables
#' @param target_path Path to the table
#' @param show_codes character string: option to print the codes
#' @param type character string: choice of basic or advanced summary
#' @param dbtype character string: choice of database type
#' @return character vector containing the SQL scripts

get_summary_codes <- function(target_path, show_codes=FALSE, type="basic", dbtype="Netezza", 
                              quote_table_name=FALSE, clipboard_enabled=TRUE){
    
    target_path <- gsub("\\[|\\]","",target_path) #get rid of opening/closing square brackets in the [db].[schema].[table] notation.
    db_path <- unlist(strsplit(target_path,"\\.")) #split on period to make a vector of length 3
    if (length(db_path)!=3){
        
        stop("The path of the table has an invalid format. Example of a valid format: 'DATABASE_NAME.SCHEMA_NAME.TABLE_NAME'")
        
    }
    
    
    if (!(dbtype %in% c("Netezza", "MSSQL"))){
        
        stop("Currently, you can only generate SQL codes for 'Netezza' and 'MSSQL' databases. More updates are coming...")
        
    }
    
    if (!(type %in% c("basic","advanced"))){
        
        stop("Input 'type' is invalid.\nReminder: type='basic' is the short summary and type='advanced' is the more detailed summary.")
        
    }
    
    
    db_name <- db_path[1] #database name
    schema_name <- db_path[2] #schema name
    table_name <-  db_path[3] #table name
    
    
    if(type=="basic"& dbtype=="Netezza"){
        
        
        if (quote_table_name){ table_name_quoted = paste0("\'\"",table_name,"\"\'")} else {
            table_name_quoted = paste0("\'",table_name,"\'")}
        
        sql_codes <- paste0(" SELECT REPLACE(REPLACE(REPLACE(
                        '<start> SELECT ''<col>'' as colname,
                        COUNT(*) as numvalues,
                        MAX(freqnull) as freqnull,
                        CAST(MIN(minval) as CHAR(100)) as minval,
                        SUM(CASE WHEN <col> = minval THEN freq ELSE 0 END) as numminvals,
                        CAST(MAX(maxval) as CHAR(100)) as maxval,
                        SUM(CASE WHEN <col> = maxval THEN freq ELSE 0 END) as nummaxvals,
                        SUM(CASE WHEN freq =1 THEN 1 ELSE 0 END) as numuniques
                        FROM (SELECT <col>, COUNT(*) as freq
                        FROM ",schema_name,".<tab> GROUP BY <col>) osum
                        CROSS JOIN (SELECT MIN(<col>) as minval, MAX(<col>) as maxval, SUM(CASE WHEN <col> IS NULL THEN 1 ELSE 0 END) as freqnull
                        FROM (SELECT <col> FROM ",schema_name,".<tab>) osum
                        ) summary',
                               '<col>', column_name),
                        '<tab>', ", table_name_quoted, "),
                        '<start>',
                        (CASE WHEN ordinal_position = 1 THEN ''
                        ELSE 'UNION ALL' END)) as codes_data_summary
                        FROM (SELECT table_name, case when regexp_like(column_name,'[a-z.]|GROUP')  then \'\"\'||column_name||\'\"\'
                        else column_name end as column_name  , ordinal_position
                        FROM information_schema.columns
                        WHERE table_name =","'",table_name,"'",") a;")
    }
    
    
    if(type=="advanced"&dbtype=="Netezza"){
        
        
        if (quote_table_name){ table_name_quoted = paste0("\'\"",table_name,"\"\'")} else {
            table_name_quoted = paste0("\'",table_name,"\'")}
        
        sql_codes <-  paste0("SELECT REPLACE(REPLACE(REPLACE(
                         '<start> SELECT ''<col>'' as colname,
                         COUNT(*) as numvalues,
                         MAX(freqnull) as freqnull,
                         CAST(MIN(minval) AS VARCHAR(250)) as minval,
                         SUM(CASE WHEN <col>  = minval THEN freq ELSE 0 END) as numminvals,
                         CAST(MAX(maxval) AS VARCHAR(250)) as maxval,
                         SUM(CASE WHEN <col>  = maxval THEN freq ELSE 0 END) as nummaxvals,
                         CAST(MIN(CASE WHEN freq = maxfreq THEN <col>  END) AS VARCHAR(250)) as mode,
                         SUM(CASE WHEN freq = maxfreq THEN 1 ELSE 0 END) as nummodes,
                         MAX(maxfreq) as modefreq,
                         CAST(MIN(CASE WHEN freq = minfreq THEN <col>  END) AS VARCHAR(250)) as antimode,
                         SUM(CASE WHEN freq = minfreq THEN 1 ELSE 0 END) as numantimodes,
                         MAX(minfreq) as antimodefreq,
                         SUM(CASE WHEN freq = 1 THEN freq ELSE 0 END) as numuniques
                         FROM (SELECT <col> , COUNT(*) as freq
                         FROM ",schema_name,".<tab>
                         GROUP BY <col> ) osum CROSS JOIN
                         (SELECT MIN(freq) as minfreq, MAX(freq) as maxfreq,
                         MIN(<col> ) as minval, MAX(<col> ) as maxval,
                         SUM(CASE WHEN <col>  IS NULL THEN freq ELSE 0 END) as freqnull
                         FROM (SELECT <col> , COUNT(*) as freq
                         FROM ",schema_name,".<tab>
                         GROUP BY <col> ) osum) summary',
                         '<col>', column_name),
                         '<tab>',", table_name_quoted,"),
                         '<start>',
                         (CASE WHEN ordinal_position = 1 THEN ''
                         ELSE 'UNION ALL' END)) as CODES_DATA_SUMMARY
                         FROM (SELECT table_name, ordinal_position,
                         case when regexp_like(column_name,'[a-z.]|GROUP')  then \'\"\'||column_name||\'\"\'
                         else column_name end as column_name
                         FROM information_schema.columns
                         WHERE table_name = ","'",table_name,"'",") a;")
        
    }
    
    
    if(type=="basic"& dbtype=="MSSQL"){
        
        if (quote_table_name){ table_name_quoted = paste0("\'\"",table_name,"\"\'")} else {
            table_name_quoted = paste0("\'",table_name,"\'")}
        
        sql_codes <-  paste0("
                         SELECT REPLACE(REPLACE(REPLACE('<start> SELECT ''<col>'' as colname,
                         COUNT(*) as numvalues, MAX(freqnull) as freqnull, CAST(MIN(minval) as
                         VARCHAR) as minval, SUM(CASE WHEN <col> = minval THEN freq ELSE 0 END)
                         as numminvals, CAST(MAX(maxval) as VARCHAR) as maxval, SUM(CASE WHEN
                         <col> = maxval THEN freq ELSE 0 END) as nummaxvals, SUM(CASE WHEN freq =
                         1 THEN 1 ELSE 0 END) as numuniques FROM (SELECT <col>, COUNT(*) as freq
                         FROM ", schema_name, ".<tab> GROUP BY <col>) osum CROSS JOIN (SELECT MIN(<col>) as minval,
                         MAX(<col>) as maxval, SUM(CASE WHEN <col> IS NULL THEN 1 ELSE 0 END) as
                         freqnull FROM (SELECT <col> FROM ", schema_name, ".<tab>) osum) summary',
                         '<col>', column_name),
                         '<tab>', ", table_name_quoted,"),
                         '<start>',
                         (CASE WHEN ordinal_position = 1 THEN ''
                         ELSE 'UNION ALL' END)) as CODES_DATA_SUMMARY
                         FROM (", "SELECT table_name, case when column_name like ","'",'%[\\.]%',"'",
                             " then concat(","'",'"',"'",",column_name,","'", '"' ,"'",")",
                             " else column_name end as column_name, ordinal_position",
                             " FROM information_schema.columns
                         WHERE table_name = ", "'",table_name,"'",") a;")
    }
    
    
    if(type=="advanced" & dbtype=="MSSQL"){
        
        if (quote_table_name){ table_name_quoted = paste0("\'\"",table_name,"\"\'")} else {
            table_name_quoted = paste0("\'",table_name,"\'")}
        
        sql_codes <-  paste0("
                         SELECT REPLACE(REPLACE(REPLACE(
                         '<start> SELECT ''<col>'' as colname,
                         COUNT(*) as numvalues,
                         MAX(freqnull) as freqnull,
                         CAST(MIN(minval) AS VARCHAR) as minval,
                         SUM(CASE WHEN <col>  = minval THEN freq ELSE 0 END) as numminvals,
                         CAST(MAX(maxval) AS VARCHAR) as maxval,
                         SUM(CASE WHEN <col>  = maxval THEN freq ELSE 0 END) as nummaxvals,
                         CAST(MIN(CASE WHEN freq = maxfreq THEN <col>  END) AS VARCHAR) as mode,
                         SUM(CASE WHEN freq = maxfreq THEN 1 ELSE 0 END) as nummodes,
                         MAX(maxfreq) as modefreq,
                         CAST(MIN(CASE WHEN freq = minfreq THEN <col>  END) AS VARCHAR) as antimode,
                         SUM(CASE WHEN freq = minfreq THEN 1 ELSE 0 END) as numantimodes,
                         MAX(minfreq) as antimodefreq,
                         SUM(CASE WHEN freq = 1 THEN freq ELSE 0 END) as numuniques
                         FROM (SELECT <col> , COUNT(*) as freq
                         FROM ",schema_name,".<tab>
                         GROUP BY <col> ) osum CROSS JOIN
                         (SELECT MIN(freq) as minfreq, MAX(freq) as maxfreq,
                         MIN(<col> ) as minval, MAX(<col> ) as maxval,
                         SUM(CASE WHEN <col>  IS NULL THEN freq ELSE 0 END) as freqnull
                         FROM (SELECT <col> , COUNT(*) as freq
                         FROM ",schema_name,".<tab>
                         GROUP BY <col> ) osum) summary',
                         '<col>', column_name),
                         '<tab>', ", table_name_quoted, "),
                         '<start>',
                         (CASE WHEN ordinal_position = 1 THEN ''
                         ELSE 'UNION ALL' END)) as CODES_DATA_SUMMARY
                         FROM ( ", "SELECT table_name, case when column_name like ","'",'%[\\.]%',"'",
                             " then concat(","'",'"',"'",",column_name,","'", '"' ,"'",")",
                             " else column_name end as column_name, ordinal_position" ,
                             " FROM information_schema.columns
                         WHERE table_name = ","'",table_name,"'",") a;")
        
    }
    
    if(show_codes==TRUE){
        cat(sql_codes)
        cat(rep('\n',5))#print codes
    }
    
    if (clipboard_enabled) {writeClipboard(sql_codes)} #copy to clipboard, if the clipboard option is enabled
    
    return(sql_codes) #return SQL texts
    
}



# Part 2: Create a RShiny app 

library(shiny)
library(shinyWidgets)


# Define UI for application that draws a histogram
ui = fluidPage(
    
    # Note the wrapping of the string in HTML()
    tags$style(HTML("
                  /* Change input texts*/
                  label,h3 {
                  font-family: monospace;
                  font-size: 15px;
                  font-weight: bold;
                  }
                  
                  
                  /*left, right margins*/
                  .row {
                  margin-right: 2px;
                  margin-left: 2px;
                  margin-top: 10px;
                  }    
                  
                  
                  /*left, right, top margins*/
                  #DataTables_Table_0_wrapper {
                  margin-top: 10px;
                  margin-right: 5px;
                  margin-left: 5px;
  }     
                  
                  
                  /* Leave space to top*/
                  .col-sm-2 {
                  margin-top: 17px;
                  }              
                  
                  .dt-buttons{
                  margin: 4px 0 0 0;
                  }
                  
                  #row1 {background-color: #b8c6db;
                  border-radius: 1em;
                  #margin-top: 10px;
                  background: rgb(255,255,255);
                  background: linear-gradient(90deg, rgba(255,255,255,1) 0%, rgba(208,208,208,0.3309698879551821) 74%);
  }
                  
                  .selectize-control { 
                  width: 45em;
                  }

                  .selectize-dropdown-content { 
                  font-size: 0.85em;
                  }
                  
                  ")), 
    
    
    # dynamic title: https://stackoverflow.com/questions/47896844/shiny-dynamically-change-tab-names 
    
    fluidRow(id= "row1", column(1, textInput("odbc_source", h3("ODBC Input"))),
             column(5, uiOutput("schema_name_tables")),
             column(2,  radioButtons("checkMode", "Mode", list("Basic"="basic" ,"Advanced"="advanced"))),
             column(2,  radioButtons("checkQuote", "\"Table Name\"", list("No" = FALSE,  "Yes"=TRUE ))),
             column(2,  radioButtons("checkDbtype", "Database",
                                     list("Netezza" = "Netezza", "SQL Server" = "MSSQL")
             )
             )
    ),
    fluidRow(addSpinner(DT::dataTableOutput("summary_table"), spin = "circle", color = "#3498db")) 
    
)

# Define server logic 

server = function(input, output,session) {
    
    library(odbc)
    library(DBI)
    library(scales)
    library(DT)
    
    # number of rows
    output$num_rows =  renderText({
        con =   dbConnect(odbc::odbc(),  input$odbc_source, encoding = 'windows-1252') 
        res = dbSendQuery(con, paste("select count(*) as cnt from", input$odbc_table_name))
        num_rows = dbFetch(res)
        print(num_rows)
        num_rows= as.data.frame(num_rows)$CNT 
        print(num_rows)
        dbClearResult(res)
        dbDisconnect(con)
        scales::comma(num_rows) 
    }) 
    
    # database name, schema name and table name 
    output$schema_name_tables = renderUI({
        if (nchar(input$odbc_source)>0){
            
            con =   tryCatch({ dbConnect(odbc::odbc(),  input$odbc_source, encoding = 'windows-1252')
            }, error = function(e) {message(paste("Unable to connect to ODBC!")) })
            
            res =   tryCatch({ dbSendQuery(con, "select distinct TABLE_CATALOG, TABLE_SCHEMA, TABLE_NAME from INFORMATION_SCHEMA.TABLES")
            }, error = function(e) {message(paste("Cannot load information schemas!")) })
            
            list_of_schemas =   tryCatch({ as.data.frame(dbFetch(res));
            }, error = function(e) {message(paste("Cannot load information schemas!")) })
            
            if (!is.null(res)){
                dbClearResult(res)
            }
            
            if (!is.null(con)){
                dbDisconnect(con)
            }
            
            selectInput(inputId = "odbc_table_name", label = h3("Table Input"), 
                        choices = c("'No tables selected'",paste0(list_of_schemas$TABLE_CATALOG,".",list_of_schemas$TABLE_SCHEMA,"." ,list_of_schemas$TABLE_NAME)),
                        selected = NULL)
            
        } 
        
    })
    
    
    #the output table 
    output$summary_table = DT::renderDataTable({
        
        # connect to ODBC data source 
        con =   tryCatch({ dbConnect(odbc::odbc(),  input$odbc_source, encoding = 'windows-1252') 
        }, error = function(e) {message(paste("Unable to connect to ODBC!")) })
        
        
        # list available data sources 
        list_of_tables =  tryCatch({ dbListTables(con)}, error = function(e){message("Unable to list tables!")})
        
        
        # table name
        curr_table =  tryCatch({ unlist(strsplit(input$odbc_table_name,"\\."))[3]}, 
                               error = function(e){return(data.frame(Messages="Loading interface..."))},
                               warning = function(e){return(data.frame(Messages="Loading interface..."))})
        
        # check if the current table exists in metadata 
        if (curr_table %in% list_of_tables){
            
            
            sql_query = get_summary_codes(input$odbc_table_name,type=input$checkMode, dbtype=input$checkDbtype, 
                                          quote_table_name = input$checkQuote, clipboard_enabled = FALSE) #create SQL queries for summary
            
            res = tryCatch({dbSendQuery(con, sql_query)},
                           error = function(e){return(data.frame(Messages="Wrong database type selection, or bad choice of quotation"))},
                           warning = function(e){return(data.frame(Messages="Wrong database type selection, or bad choice of quotation"))}) 
            
            #get results
            
            output_table = tryCatch({dbFetch(res)},
                                    error = function(e){return(data.frame(Messages = "Wrong database type selection, or bad choice of quotation"))},
                                    warning = function(e){return(data.frame(Messages = "Wrong database type selection, or bad choice of quotation"))})  #fetch the sql queries to run, which will generate summary query  
            
            tryCatch({
                
                dbClearResult(res)  #clear outputs   
                
                summary_query = paste(output_table$CODES_DATA_SUMMARY, collapse = ' ')  #copy the summary query 1
                print(paste("Table:",input$odbc_table_name))
                print("Started Summarizing...")
                
                res =   dbSendQuery(con, summary_query)  #run the summary query to fetch the summary table 
                output_table = dbFetch(res); #fetch summary output
                cat("Results fetched!\n\n\n")
                
                names(output_table) = toupper(names(output_table))
                
                dbClearResult(res) #clear outputs 
                dbDisconnect(con)
                
                output_tableNUMVALUES=scales::comma(as.integer(output_table$NUMVALUES))
                output_tableFREQNULL=scales::comma(as.integer(output_table$FREQNULL))
                output_tableNUMMINVALS=scales::comma(as.integer(output_table$NUMMINVALS))
                output_tableNUMMAXVALS=scales::comma(as.integer(output_table$NUMMAXVALS))
                output_tableNUMUNIQUES=scales::comma(as.integer(output_table$NUMUNIQUES))
                
                if (input$checkMode == "advanced"){
                    
                    output_table$NUMMODES=scales::comma(as.integer(output_table$NUMMODES))
                    output_table$MODEFREQ=scales::comma(as.integer(output_table$MODEFREQ))
                    output_table$NUMANTIMODES=scales::comma(as.integer(output_table$NUMANTIMODES))
                    output_table$ANTIMODEFREQ=scales::comma(as.integer(output_table$ANTIMODEFREQ))
                    
                }
                
                output_table[order(output_table$NUMVALUES,decreasing = TRUE),]  #output: summary table 
                print(output_table[order(output_table$NUMVALUES,decreasing = TRUE),])
                
            }, 
            error = function(e){data.frame(message="Wrong database type selection, or bad choice of quotation");},
            warning = function(e){data.frame(message="Wrong database type selection, or bad choice of quotation")}) 
            
        } else {
            if (nchar(input$odbc_source)>0){
                data.frame(Messages = paste("The current table you are looking for doesn't exist in", input$odbc_source,"!", "Enter a valid table name."))
            } else {
                data.frame(Messages = "Please enter your ODBC connection name!")
            }
            
            
        }
    }, 
    server=FALSE, 
    extensions = "Buttons",
    rownames= FALSE,
    options = list(  info = FALSE,  
                     lengthMenu = list(c( 50 ,100,-1), c(  "50","100","All" )),
                     dom = 'lfrtiBp', 
                     buttons = list(list(extend ="csv", text ='Download .CSV', 
                                         filename = paste0(gsub("\\.","_",input$odbc_table_name),gsub("-| ","_",Sys.time()),'EST')),
                                    list(extend ="pdf", text ='Download .PDF', 
                                         title = paste0(gsub("\\.","_",input$odbc_table_name),gsub("-| ","_",Sys.time()),'EST'),
                                         orientation = 'landscape',pageSize='A3'))
    )
    )  
    
    
}     


# Run the application 
options(shiny.host = '127.0.0.1')
options(shiny.port = 4809)
shinyApp(ui = ui, server = server)