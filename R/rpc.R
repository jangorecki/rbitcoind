#' @name bitcoind.rpc
#' @title Bitcoin daemon curl call wrapper.
#' @param connect character.
#' @param user character.
#' @param password character.
#' @param port character.
#' @param id any.
#' @param method character.
#' @param params list.
#' @return jsonlite decoded response from json-rpc bitcoind. If \code{option("use.data.table"=TRUE)} then it will recursively turn data.frames to data.tables.
#' @examples \dontrun{
#' bitcoind.rpc(user = "username", 
#'              password = "password", 
#'              port = "18332",
#'              method = "getinfo")
#' }
bitcoind.rpc <- function(connect = getOption("rpchost","127.0.0.1"),
                         user = getOption("rpcuser"),
                         password = getOption("rpcpassword"),
                         port = getOption("rpcport","8332"),
                         id = NA_integer_, method, params = list()){
    if(!is.character(user)) stop("Provided `user` to `bitcoind.rpc` is invalid.")
    if(!is.character(password)) stop("Provided `password` to `bitcoind.rpc` is invalid.")
    if(!is.character(method)) stop("Provided `method` to `bitcoind.rpc` is invalid.")
    rpcurl <- paste0("http://",user,":",password,"@",connect,":",port)
    req <- POST(rpcurl, body = toJSON(list(jsonrpc = "1.0", id = id, method = method, params = params), auto_unbox=TRUE))
    if(http_status(req)$category != "success"){
        message(fromJSON(content(req, "text"))$error$message)
        stop_for_status(req)
    }
    use.data.table(fromJSON(content(req, "text")))
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
