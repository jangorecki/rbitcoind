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