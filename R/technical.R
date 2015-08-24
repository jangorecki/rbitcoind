#' @title run.bitcoind
#' @details Simply calls \code{system("bitcoind -daemon")}. Works only when bitcoin daemon available on localhost.
run.bitcoind <- function(){
    rpchost = getOption("rpchost")
    if(!is.null(rpchost) && !rpchost %in% c("127.0.0.1","localhost")) stop(paste0("start.bitcoind function can only start locally hosted bitcoin node, adjust your 'rpchost' option, current value: ", toString(rpchost)))
    bitcoind_datadir = path.expand(paste("~",getOption("bitcoind.datadir", ".bitcoin"), sep="/"))
    bitcoind_pid = pid.bitcoind()
    if(length(bitcoind_pid)) stop(paste0("bitcoind is already running in ", paste(names(bitcoind_pid),collapse=", "), " mode. The bitcoind.pid and regtest/bitcoind.pid should not exists. For custom pid file use 'bitcoind.pid' option."))
    run_res = capture.output(system("bitcoind -daemon"))
    run_res
}

#' @title pid.bitcoind
#' @details Checks for bitcoind.pid file (or value from option \code{"bitcoind.pid"}) in bitcoin datadir and subdir regtest.
pid.bitcoind <- function(){
    bitcoind_datadir = getOption("bitcoind.datadir", ".bitcoin")
    bitcoind_pid = getOption("bitcoind.pid", "bitcoind.pid")
    path_pid = c(
        mainnet = paste(path.expand("~"), bitcoind_datadir, bitcoind_pid, sep="/"),
        regtest = paste(path.expand("~"), bitcoind_datadir, "regtest", bitcoind_pid, sep="/")
    )
    which_pid = sapply(path_pid, file.exists)
    if(length(path_pid[which_pid])) sapply(path_pid[which_pid], readLines) else path_pid[which_pid]
}

#' @title kill.bitcoind
#' @details Kills bitcoind processes detected by \link{pid.bitcoind}.
kill.bitcoind <- function(){
    pid = pid.bitcoind()
    if(length(pid)) sapply(pid, function(pid) system(paste("kill",pid))) else pid
}