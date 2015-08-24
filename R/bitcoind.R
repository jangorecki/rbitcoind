#' @name rbitcoind
#' @title R API to bitcoin daemon
#' @docType package
#' @author Jan Gorecki
#' @description You need to setup/connect to bitcoin node. Unit tests are run on \code{regtest} network which uses private blockchain and doesn't need to be synchronized.
#' @seealso \link{bitcoind.rpc}
#' @examples \dontrun{
#' options(rpchost = "127.0.0.1",
#'         rpcuser = "bitcoinduser",
#'         rpcpassword = "userpassbitcoind",
#'         rpcport = "18332") # regtest
#' getinfo()
#' }
NULL

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
#'              user = "bitcoinduser", 
#'              password = "userpassbitcoind", 
#'              port = "18332", # regtest
#'              method = "getinfo")
#' }
bitcoind.rpc <- function(host = getOption("rpchost","127.0.0.1"),
                         user = getOption("rpcuser"),
                         password = getOption("rpcpassword"),
                         port = getOption("rpcport","8332"),
                         id = NA_integer_, method, params = list()){
    stopifnot(is.character(user), is.character(password), is.character(method))
    rpcurl <- paste0("http://",user,":",password,"@",host,":",port)
    req <- httr::POST(rpcurl, body = jsonlite::toJSON(list(jsonrpc = "1.0", id = id, method = method, params = params), auto_unbox=TRUE))
    if(httr::http_status(req)$category != "success"){
        message(httr::content(req, "text"))
        message(jsonlite::fromJSON(httr::content(req, "text"))$error$message)
        httr::stop_for_status(req)
    }
    use.data.table(jsonlite::fromJSON(httr::content(req, "text")))
}

#' @name makepaymenturi
#' @title Generate payment uri.
#' @param address character. Target address.
#' @param amount numeric. Amount to be hardcoded in payment uri.
#' @return Character, payment uri link \code{bitcoin:ADDRESS&amount=0.1}.
makepaymenturi <- function(address, amount) paste0("bitcoin:",address,"?amount=",amount)

#' @name plotQR
#' @title Plot QR code for payment uri.
#' @param to_encode any accepted by qrencoder pkg.
#' @details Requires qrencoder package.
plotQR <- function(to_encode){
    mar <- par(mar=c(0,0,0,0))
    on.exit(par(mar))
    if(requireNamespace("qrencoder", quietly = TRUE)){
        suppressPackageStartupMessages(require("raster")) # cannot image without attaching raster package
        image(qrencoder::qrencode_raster(to_encode), asp=1, col=c("white", "black"), axes=FALSE, xlab="", ylab="")
    } else {
        plot(c(0, 1), c(0, 1), ann=FALSE, bty="n", type="n", xaxt="n", yaxt="n")
        text(x = 0.5, y = 0.5, "To display QR Code install hrbrmstr/qrencoder package", col = "black")
    }
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
