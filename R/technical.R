#' @name makepaymenturi
#' @title Generate payment uri.
#' @param address character. Target address.
#' @param amount numeric. Amount to be hardcoded in payment uri.
#' @return Character, payment uri link \code{bitcoin:ADDRESS&amount=0.1}.
makepaymenturi <- function(address, amount) paste0("bitcoin:",address,"?amount=",amount)

#' @name plotQR
#' @title Plot QR code for payment uri.
#' @param dataString any accepted by qrcode pkg.
#' @details Requires qrcode package.
plotQR <- function(dataString){
    if(requireNamespace("qrcode", quietly = TRUE)){
        qrcode::qrcode_gen(dataString)
    } else {
        plot(c(0, 1), c(0, 1), ann=FALSE, bty="n", type="n", xaxt="n", yaxt="n")
        text(x = 0.5, y = 0.5, "To display QR Code install qrcode package", col = "black")
    }
}

#' @name defaultports
#' @title Returns matrix of defaults port used by bitcoin daemon modes.
defaultports <- function(){
    matrix(c(8333L, 18333L, 18444L, 8332L, 18332L, 18332L), 3L, 2L, 
           dimnames = list(chain = c("main","test","regtest"), 
                           interface = c("connect","rpc")))
}
