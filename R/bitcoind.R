#' @name rbitcoind
#' @title R interface to bitcoin daemon
#' @docType package
#' @author Jan Gorecki
#' @description You need to have to bitcoin node installed, blockchain is not necessary. Unit tests runs on \emph{regtest} network which uses private blockchain and doesn't need to be synchronized.
#' @seealso \link{bitcoind}, \link{bitcoind.rpc}
#' @references \url{https://bitcoin.org/en/glossary/regression-test-mode}
#' @aliases btcd rbtcd
#' @examples \dontrun{
#' # low level calls to json-rpc
#' bitcoind.rpc(host = "127.0.0.1",
#'              user = "rpcuser",
#'              password = "rpcpassword",
#'              port = "18332", # regtest
#'              method = "getinfo")
#' 
#' # bitcoin daemon object
#' btcd <- bitcoind$new(rpcuser = "rpcuser", 
#'                      rpcpassword = "rpcpassword", 
#'                      regtest = TRUE)
#' btcd$getinfo()
#' }
NULL

#' @title bitcoind class
#' @docType class
#' @format An R6 class object.
#' @name bitcoind
#' @details Initiate object to manage all RPC methods.
#' @seealso \link{bitcoind.rpc}
bitcoind <- R6Class(
    classname = "bitcoind",
    public = list(
        host = character(),
        port = character(),
        rpcuser = character(),
        rpcport = character(),
        datadir = character(),
        connect = character(),
        addnode = character(),
        testnet = logical(),
        regtest = logical(),
        net = character(),
        pid = character(), # file name
        initialize = function(host, port, rpcuser, rpcpassword, rpcport, datadir, connect, addnode, testnet, regtest, pid){
            private$rpcpassword = rpcpassword; rm(rpcpassword)
            self$rpcuser = rpcuser
            self$host = if(missing(host)) "127.0.0.1" else host
            self$testnet = if(missing(testnet)) NULL else as.logical(testnet)
            self$regtest = if(missing(regtest)) NULL else as.logical(regtest)
            net = c("test"[self$testnet], "regtest"[self$regtest])
            self$net = if(length(net)==0L) "main" else if(length(net)>1L) stop("Cannot use both testnet and regtest modes for single daemon instance.") else net
            self$port = if(missing(port)) defaultports()[self$net,"connect"] else port
            self$rpcport = if(missing(rpcport)) defaultports()[self$net,"rpc"] else rpcport
            self$datadir = if(missing(datadir)) "~/.bitcoin" else datadir
            self$connect = if(missing(connect)) NULL else connect
            self$addnode = if(missing(addnode)) NULL else addnode
            self$pid = if(missing(pid)) "bitcoind.pid" else pid
            if(was.running <- self$is.running()){
                if(!identical(self$net, self$get_network())) stop(paste0("Target daemon is ","already "[was.running],"running on ",self$get_network()," network. Adjust daemon's bitcoin.conf or args to bitcoind$new: ", 
                                                                         switch(self$get_network(),
                                                                                "main" = "do not use `testnet` or `regtest` TRUE",
                                                                                "test" = "`testnet=TRUE`",
                                                                                "regtest" = "`regtest=TRUE`"),
                                                                         "."),
                                                                  call. = FALSE)
            }
            invisible(self)
        },
        is.localhost = function() !length(self$host) || self$host %in% c("127.0.0.1","localhost"),
        is.running =  function() all(c("result","error","id") %in% names(tryCatch(bitcoind.rpc(host=self$host, user=self$rpcuser, password=private$rpcpassword, port=self$rpcport, method = "getinfo"), error = function(e) NULL))),
        get_network = function() self$getblockchaininfo()$chain,
        get_subdir = function() switch(self$net, "main"=NULL, "test"="testnet3", "regtest"="regtest"),
        get_pid = function() if(file.exists(pid_path <- paste(c(self$datadir, self$get_subdir(), self$pid), collapse="/"))) readLines(pid_path,warn=FALSE),
        print = function(getinfo = FALSE){
            cat("<bitcoind>\n")
            cat("  network: ", self$net, "\n", sep="")
            cat("  datadir: ", self$datadir,"\n", sep="")
            Sys.sleep(0.001)
            cat("  pid: ", self$get_pid(), "\n", sep="")
            if(getinfo){
                info = self$getinfo()
                cat("  block height: ", info$blocks, "\n", sep="")
                cat("  wallet balance: ", info$balance, "\n", sep="")
                cat("  accounts count: ", length(self$listaccounts()), "\n", sep="")
                cat("  connections count: ", info$connections, "\n", sep="")
            }
        },
        # system calls
        grep_pid = function(){
            cmd = "pgrep -f bitcoind.*"
            message(cmd)
            system(cmd, intern = TRUE)
        },
        run = function(wait = 3){
            if(!(was.running <- self$is.running())){
                dir = path.expand(self$datadir)
                if(!dir.exists(dir)) dir.create(dir, recursive = TRUE)
                else {
                    if(self$is.localhost()){
                        message(private$run_cmd(mask=TRUE))
                        system(private$run_cmd(mask=FALSE))
                        Sys.sleep(wait)
                    }
                    else {
                        cat("# Run method supports only localhost daemon, run remote daemon manually using:\n",sep="")
                        cat(private$run_cmd(mask=TRUE),"\n",sep="")
                    }
                }
            }
            if(!identical(self$net, self$get_network())) stop(paste0("Target daemon is ","already "[was.running],"running on ",self$get_network()," network. Adjust daemon's bitcoin.conf or args to bitcoind$new: ", 
                                                                     switch(self$get_network(),
                                                                            "main" = "do not use TRUE for `testnet` or `regtest`",
                                                                            "test" = "`testnet=TRUE`",
                                                                            "regtest" = "`regtest=TRUE`"),
                                                                     "."),
                                                              call. = FALSE)
            invisible(self)
        },
        term = function(){
            cmd = "killall -s SIGTERM --regex bitcoin.*"
            message(cmd)
            system(cmd)
        },
        kill = function(){
            cmd = "killall -s SIGKILL --regex bitcoin.*" 
            message(cmd)
            system(cmd)
        },
        # json-rpc
        decoderawtransaction = function(hex) bitcoind.rpc(host=self$host, user=self$rpcuser, password=private$rpcpassword, port=self$rpcport, method = "decoderawtransaction", params = list(hex))$result,
        decodescript = function(hex) bitcoind.rpc(host=self$host, user=self$rpcuser, password=private$rpcpassword, port=self$rpcport, method = "decodescript", params = list(hex))$result,
        generate = function(numblocks) bitcoind.rpc(host=self$host, user=self$rpcuser, password=private$rpcpassword, port=self$rpcport, method = "generate", params = list(numblocks))$result,
        getaccountaddress = function(account) bitcoind.rpc(host=self$host, user=self$rpcuser, password=private$rpcpassword, port=self$rpcport, method = "getaccountaddress", params = list(account))$result,
        getaccount = function(bitcoinaddress) bitcoind.rpc(host=self$host, user=self$rpcuser, password=private$rpcpassword, port=self$rpcport, method = "getaccount", params = list(bitcoinaddress))$result,
        getaddressesbyaccount = function(account) bitcoind.rpc(host=self$host, user=self$rpcuser, password=private$rpcpassword, port=self$rpcport, method = "getaddressesbyaccount", params = list(account))$result,
        getbalance = function(account) bitcoind.rpc(host=self$host, user=self$rpcuser, password=private$rpcpassword, port=self$rpcport, method = "getbalance", params = list(account))$result,
        getbestblockhash = function() bitcoind.rpc(host=self$host, user=self$rpcuser, password=private$rpcpassword, port=self$rpcport, method = "getbestblockhash")$result,
        getblock = function(hash, verbose = TRUE) bitcoind.rpc(host=self$host, user=self$rpcuser, password=private$rpcpassword, port=self$rpcport, method = "getblock", params = list(hash, verbose))$result,
        getblockchaininfo = function() bitcoind.rpc(host=self$host, user=self$rpcuser, password=private$rpcpassword, port=self$rpcport, method = "getblockchaininfo")$result,
        getblockcount = function() bitcoind.rpc(host=self$host, user=self$rpcuser, password=private$rpcpassword, port=self$rpcport, method = "getblockcount")$result,
        getblockhash = function(index) bitcoind.rpc(host=self$host, user=self$rpcuser, password=private$rpcpassword, port=self$rpcport, method = "getblockhash", params = list(index))$result,
        getchaintips = function() bitcoind.rpc(host=self$host, user=self$rpcuser, password=private$rpcpassword, port=self$rpcport, method = "getchaintips")$result,
        getconnectioncount = function() bitcoind.rpc(host=self$host, user=self$rpcuser, password=private$rpcpassword, port=self$rpcport, method = "getconnectioncount")$result,
        getinfo = function() bitcoind.rpc(host=self$host, user=self$rpcuser, password=private$rpcpassword, port=self$rpcport, method = "getinfo")$result,
        getnetworkinfo = function() bitcoind.rpc(host=self$host, user=self$rpcuser, password=private$rpcpassword, port=self$rpcport, method = "getnetworkinfo")$result,
        getpeerinfo = function() bitcoind.rpc(host=self$host, user=self$rpcuser, password=private$rpcpassword, port=self$rpcport, method = "getpeerinfo")$result,
        getnewaddress = function(account = "") bitcoind.rpc(host=self$host, user=self$rpcuser, password=private$rpcpassword, port=self$rpcport, method = "getnewaddress", params = list(account))$result,
        getrawtransaction = function(txid, verbose = 0) bitcoind.rpc(host=self$host, user=self$rpcuser, password=private$rpcpassword, port=self$rpcport, method = "getrawtransaction", params = list(txid, verbose))$result,
        getreceivedbyaccount = function(account, minconf = 1L) bitcoind.rpc(host=self$host, user=self$rpcuser, password=private$rpcpassword, port=self$rpcport, method = "getreceivedbyaccount", params = list(account, minconf))$result,
        getreceivedbyaddress = function(bitcoinaddress, minconf = 1L) bitcoind.rpc(host=self$host, user=self$rpcuser, password=private$rpcpassword, port=self$rpcport, method = "getreceivedbyaddress", params = list(bitcoinaddress, minconf))$result,
        gettransaction = function(txid, includeWatchonly = FALSE) bitcoind.rpc(host=self$host, user=self$rpcuser, password=private$rpcpassword, port=self$rpcport, method = "gettransaction", params = list(txid, includeWatchonly))$result,
        getunconfirmedbalance = function() bitcoind.rpc(host=self$host, user=self$rpcuser, password=private$rpcpassword, port=self$rpcport, method = "getunconfirmedbalance")$result,
        getwalletinfo = function() bitcoind.rpc(host=self$host, user=self$rpcuser, password=private$rpcpassword, port=self$rpcport, method = "getwalletinfo")$result,
        help = function(command) if(missing(command)) bitcoind.rpc(host=self$host, user=self$rpcuser, password=private$rpcpassword, port=self$rpcport, method = "help")$result else bitcoind.rpc(host=self$host, user=self$rpcuser, password=private$rpcpassword, port=self$rpcport, method = "help", params = list(command))$result,
        listaccounts = function(minconf = 1L) bitcoind.rpc(host=self$host, user=self$rpcuser, password=private$rpcpassword, port=self$rpcport, method = "listaccounts", params = list(minconf))$result,
        listaddressgroupings = function() bitcoind.rpc(host=self$host, user=self$rpcuser, password=private$rpcpassword, port=self$rpcport, method = "listaddressgroupings")$result,
        listreceivedbyaccount = function(minconf = 1L, includeempty = FALSE) bitcoind.rpc(host=self$host, user=self$rpcuser, password=private$rpcpassword, port=self$rpcport, method = "listreceivedbyaccount", params = list(minconf, includeempty))$result,
        listsinceblock = function(blockhash, target.confirmations = 1L, includeWatchonly = FALSE) bitcoind.rpc(host=self$host, user=self$rpcuser, password=private$rpcpassword, port=self$rpcport, method = "listsinceblock", params = list(blockhash, target.confirmations, includeWatchonly))$result,
        listtransactions = function(account, count = 10L, from = 0L) bitcoind.rpc(host=self$host, user=self$rpcuser, password=private$rpcpassword, port=self$rpcport, method = "listtransactions", params = list(account, count, from))$result,
        listunspent = function(miconf = 1L, maxconf = 9999999L, addresses) bitcoind.rpc(host=self$host, user=self$rpcuser, password=private$rpcpassword, port=self$rpcport, method = "listunspent", params = list(miconf, maxconf, addresses))$result,
        stop = function() bitcoind.rpc(host=self$host, user=self$rpcuser, password=private$rpcpassword, port=self$rpcport, method = "stop")$result,
        validateaddress = function(bitcoinaddress) bitcoind.rpc(host=self$host, user=self$rpcuser, password=private$rpcpassword, port=self$rpcport, method = "validateaddress", params = list(bitcoinaddress))$result
    ),
    private = list(
        run_cmd = function(mask=TRUE){
            paste0("bitcoind -server -listen -port=",self$port," -rpcuser=",self$rpcuser," -rpcpassword=", if(isTRUE(mask)) "***" else private$rpcpassword," -rpcport=",self$rpcport," -datadir=", if(isTRUE(mask)) self$datadir else path.expand(self$datadir), if(length(self$connect)) paste0(" -connect=",self$connect), if(length(self$addnode)) paste0(" -addnode=",self$addnode), if(isTRUE(self$regtest)) " -regtest"," -pid=",self$pid," -daemon")
        },
        rpcpassword = character()
    )
)