library(shiny)
library(DT)
library(rbitcoind)
library(RSQLite)

# config

options(rpchost = "127.0.0.1",
        rpcuser = "bitcoinduser",
        rpcpassword = "userpassbitcoind",
        rpcport = "18332")

db_file = "rbitcoind.db"
wd = getwd()
db_filepath = path.expand(paste(wd, db_file,sep="/"))
if(!file.exists(db_filepath)) stop(paste0("No historical database file or invalid working directory ",wd,". Be aware 'Run App' button in RStudio may change working directory of app."), call. = FALSE)
conn = dbConnect(SQLite(), db_filepath)
tbls = dbListTables(conn)
if(!length(tbls)) stop("No tables in database", call. = FALSE)

# ui ----------------------------------------------------------------------

ui = fluidPage(
    fluidRow(column(12L, align="left",
                    titlePanel("rbitcoind"),
                    br())),
    fluidRow(column(3L, align="left",
                    selectInput("tbl", label = "tables", choices = tbls)),
             column(9L, align="center",
                    DT::dataTableOutput("DT")))
)

# server ------------------------------------------------------------------

server = function(input, output, session){
    
    output$DT = DT::renderDataTable({
        validate(need(is.character(input$tbl) && length(input$tbl), "Select table name"))
        dbReadTable(conn, input$tbl)
    }, rownames=FALSE, options = list(scrollX = TRUE))
    
}

# shinyApp ----------------------------------------------------------------

shinyApp(ui, server)
