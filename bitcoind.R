# json-rpc to bitcoind
bitcoind.rpc <- function(host = getOption("rpchost","127.0.0.1"),
                         user = getOption("rpcuser"),
                         password = getOption("rpcpassword"),
                         port = getOption("rpcport","8332"),
                         id = NA_integer_, method, params = list()){
    stopifnot(is.character(user), is.character(password), is.character(method))
    rpcurl <- paste0("http://",user,":",password,"@",host,":",port)
    req <- httr::POST(rpcurl, body = jsonlite::toJSON(list(jsonrpc = "1.0", id = id, method = method, params = params), auto_unbox=TRUE))
    if(httr::http_status(req)$category != "success"){
        message(jsonlite::fromJSON(httr::content(req, "text"))$error$message)
        httr::stop_for_status(req)
    }
    use.data.table(jsonlite::fromJSON(httr::content(req, "text")))
}

# wrappers
decodescript <- function(hex) bitcoind.rpc(method = "decodescript", params = list(hex))$result
getaccount <- function(bitcoinaddress) bitcoind.rpc(method = "getaccount", params = list(bitcoinaddress))$result
getaccountaddress <- function(account) bitcoind.rpc(method = "getaccountaddress", params = list(account))$result
getaddressesbyaccount <- function(account) bitcoind.rpc(method = "getaddressesbyaccount", params = list(account))$result
getbalance <- function(account) bitcoind.rpc(method = "getbalance", params = list(account))$result
getbestblockhash <-  function() bitcoind.rpc(method = "getbestblockhash")$result
getblock <- function(hash, verbose = TRUE) bitcoind.rpc(method = "getblock", params = list(hash, verbose))$result
getblockchaininfo <- function() bitcoind.rpc(method = "getblockchaininfo")$result
getblockcount <- function() bitcoind.rpc(method = "getblockcount")$result
getblockhash <- function(index) bitcoind.rpc(method = "getblockhash", params = list(index))$result
getchaintips <- function() bitcoind.rpc(method = "getchaintips")$result
getinfo <- function() bitcoind.rpc(method = "getinfo")$result
getnetworkinfo <- function() bitcoind.rpc(method = "getnetworkinfo")$result
getpeerinfo <- function() bitcoind.rpc(method = "getpeerinfo")$result
getnewaddress <- function() bitcoind.rpc(method = "getnewaddress")$result
getreceivedbyaccount <- function(account, minconf = 1L) bitcoind.rpc(method = "getreceivedbyaccount", params = list(account, minconf))$result
getreceivedbyaddress <- function(bitcoinaddress, minconf = 1L) bitcoind.rpc(method = "getreceivedbyaddress", params = list(bitcoinaddress, minconf))$result
gettransaction <- function(txid, includeWatchonly = FALSE) bitcoind.rpc(method = "gettransaction", params = list(txid, includeWatchonly))$result
getunconfirmedbalance <- function() bitcoind.rpc(method = "getunconfirmedbalance")$result
getwalletinfo <- function() bitcoind.rpc(method = "getwalletinfo")$result
help.bitcoind <- function(command) if(missing(command)) bitcoind.rpc(method = "help")$result else bitcoind.rpc(method = "help", params = list(command))$result
listaccounts <- function(minconf = 1L) bitcoind.rpc(method = "listaccounts", params = list(minconf))$result
listaddressgroupings <- function() bitcoind.rpc(method = "listaddressgroupings")$result
listreceivedbyaccount <- function(minconf = 1L, includeempty = FALSE) bitcoind.rpc(method = "listreceivedbyaccount", params = list(minconf, includeempty))$result
listsinceblock <- function(blockhash, target.confirmations = 1L, includeWatchonly = FALSE) bitcoind.rpc(method = "listsinceblock", params = list(blockhash, target.confirmations, includeWatchonly))$result
listtransactions <- function(account, count = 10L, from = 0L) bitcoind.rpc(method = "listtransactions", params = list(account, count, from))$result
listunspent <- function(miconf = 1L, maxconf = 9999999L, addresses) bitcoind.rpc(method = "listunspent", params = list(miconf, maxconf, addresses))$result
stop.bitcoind <- function() bitcoind.rpc(method = "stop")$result
validateaddress <- function(bitcoinaddress) bitcoind.rpc(method = "validateaddress", params = list(bitcoinaddress))$result

# helpers
makepaymenturi <- function(address, amount) paste0("bitcoin:",address,"?amount=",amount)
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

# technical
use.data.table <- function(x, use = getOption("use.data.table")){
    if(isTRUE(use)){
        if(!requireNamespace("data.table", quietly = TRUE)) stop("Cannot use 'use.data.table' without having data.table package installed.")
        else {
            if(is.data.table(x)) x
            else if(is.data.frame(x)) data.table::setDT(x)
            else if(is.list(x)) lapply(x, use.data.table, use = use)
            else x
        }
    } else x
}
