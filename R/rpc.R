#' @name bitcoind.rpc
#' @title Bitcoin daemon curl call wrapper.
#' @param host character.
#' @param user character.
#' @param password character.
#' @param port character.
#' @param id any.
#' @param method character.
#' @param params list.
#' @return jsonlite decoded response from json-rpc bitcoind. If \code{option("use.data.table"=TRUE)} then it will recursively turn data.frames to data.tables.
#' @examples \dontrun{
#' bitcoind.rpc(host = "127.0.0.1",
#'              user = "rpcuser", 
#'              password = "rpcpassword", 
#'              port = "18332", # regtest
#'              method = "getinfo")
#' }
bitcoind.rpc <- function(host = getOption("rpchost","127.0.0.1"),
                         user = getOption("rpcuser"),
                         password = getOption("rpcpassword"),
                         port = getOption("rpcport","8332"),
                         id = NA_integer_, method, params = list()){
    if(!is.character(user)) stop("Provided `user` to `bitcoind.rpc` is invalid.")
    if(!is.character(password)) stop("Provided `password` to `bitcoind.rpc` is invalid.")
    if(!is.character(method)) stop("Provided `method` to `bitcoind.rpc` is invalid.")
    rpcurl <- paste0("http://",user,":",password,"@",host,":",port)
    req <- httr::POST(rpcurl, body = jsonlite::toJSON(list(jsonrpc = "1.0", id = id, method = method, params = params), auto_unbox=TRUE))
    if(httr::http_status(req)$category != "success"){
        message(jsonlite::fromJSON(httr::content(req, "text"))$error$message)
        httr::stop_for_status(req)
    }
    use.data.table(jsonlite::fromJSON(httr::content(req, "text")))
}

#' @name use.data.table
#' @title Auto recursive setDT on data.frames
#' @param x object to recursively transform.
#' @param use logical, default will keep the data.frames.
#' @details Requires data.table package.
use.data.table <- function(x, use = getOption("use.data.table")){
    if(isTRUE(use)){
        if(!requireNamespace("data.table", quietly = TRUE)) stop("Cannot use 'use.data.table' without having data.table package installed.")
        if(data.table::is.data.table(x)) x
        else if(is.data.frame(x)) data.table::setDT(x)
        else if(is.list(x)) lapply(x, use.data.table, use = use)
        else x
    }
    else x
}
