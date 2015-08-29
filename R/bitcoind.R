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
#' bitcoind.rpc(user = "username",
#'              password = "password",
#'              port = "18332",
#'              method = "getinfo")
#' 
#' # bitcoin daemon object
#' btcd <- bitcoind$new(rpcuser = "username", 
#'                      rpcpassword = "password", 
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
        listen = logical(),
        testnet = logical(),
        regtest = logical(),
        network = character(),
        pid = character(), # file name
        initialize = function(host="127.0.0.1", port, rpcuser, rpcpassword, rpcport, datadir="~/.bitcoin", listen, connect=NULL, addnode=NULL, testnet=FALSE, regtest=FALSE, pid="bitcoind.pid"){
            stopifnot(is.character(rpcuser), is.character(rpcpassword))
            private$rpcpassword = rpcpassword; rm(rpcpassword)
            self$rpcuser = rpcuser
            self$host = host
            self$testnet = testnet
            self$regtest = regtest
            net = c("test"[self$testnet], "regtest"[self$regtest])
            self$network = if(length(net)==0L) "main" else if(length(net)>1L) stop("Cannot use both testnet and regtest modes for single daemon instance at the same time.") else net
            self$port = if(missing(port)) defaultports()[self$network,"connect"] else port
            self$rpcport = if(missing(rpcport)) defaultports()[self$network,"rpc"] else rpcport
            self$connect = connect
            self$addnode = addnode
            self$datadir = datadir
            self$pid = pid
            self$listen = if(missing(listen)) as.integer(!length(self$connect)) else listen
            if(was.running <- self$is.running()){
                if(!identical(self$network, self$get_network())) stop(paste0("Target daemon is ","already "[was.running],"running on ",self$get_network()," network. Adjust daemon's bitcoin.conf or args to bitcoind$new: ", 
                                                                             switch(self$get_network(),
                                                                                    "main" = "do not use `testnet` or `regtest` TRUE",
                                                                                    "test" = "`testnet=TRUE`",
                                                                                    "regtest" = "`regtest=TRUE`"),
                                                                             "."),
                                                                      call. = FALSE)
            }
            invisible(self)
        },
        is.localhost = function() self$host %in% c("127.0.0.1","localhost"),
        is.running =  function() all(c("result","error","id") %in% names(tryCatch(bitcoind.rpc(host=self$host, user=self$rpcuser, password=private$rpcpassword, port=self$rpcport, method = "getinfo"), error = function(e) NULL))),
        get_network = function() self$getblockchaininfo()$chain,
        get_subdir = function() switch(self$network, "main"=NULL, "test"=getOption("testnet.subdir","testnet3"), "regtest"="regtest"),
        status = function(getinfo = FALSE){
            df = data.frame(host = self$host, rpcport = self$rpcport, network = self$network, datadir = self$datadir, pid = self$get_pid())
            if(getinfo){
                info = if(self$is.running()) self$getinfo() else info = list(blocks = NA_integer_, balance = NA_real_, connections = NA_integer_)
                df = cbind(df, data.frame(height = info$blocks, balance = info$balance, connections = info$connections))
            }
            use.data.table(df)
        },
        print = function(getinfo = FALSE){
            cat("<bitcoind>\n")
            cat("  instance: ", self$host, ":", self$rpcport, "\n", sep="")
            cat("  network: ", self$network, "\n", sep="")
            cat("  datadir: ", self$datadir,"\n", sep="")
            cat("  pid: ", self$get_pid(), "\n", sep="")
            if(getinfo){
                info = self$getinfo()
                cat("  height: ", info$blocks, "\n", sep="")
                cat("  balance: ", info$balance, "\n", sep="")
                cat("  connections: ", info$connections, "\n", sep="")
            }
        },
        # localhost only
        get_pid = function(){
            if(self$is.localhost()) if(file.exists(pid_path <- paste(c(self$datadir, self$get_subdir(), self$pid), collapse="/"))) readLines(pid_path,warn=FALSE) else NA_character_
        },
        # localhost only system calls
        grep_pid = function(){
            cmd = "pgrep -f bitcoind.*"
            if(!self$is.localhost()) stop(run_localhost_msg("grep_pid",cmd), call. = FALSE)
            message(cmd)
            system(cmd, intern = TRUE)
        },
        run = function(wait = 3){
            if(!(was.running <- self$is.running())){
                if(self$is.localhost()){
                    dir = path.expand(self$datadir)
                    if(!dir.exists(dir)) dir.create(dir, recursive = TRUE)
                    message(private$run_cmd(mask=TRUE))
                    system(private$run_cmd(mask=FALSE))
                    Sys.sleep(wait)
                }
                else {
                    if(!self$is.localhost()) stop(run_localhost_msg("run",private$run_cmd(mask=TRUE), call. = FALSE))
                }
            }
            if(self$is.running() && !identical(self$network, self$get_network()))
                stop(paste0("Target daemon is ","already "[was.running],"running on ",self$get_network()," network. Adjust daemon's bitcoin.conf or args to bitcoind$new: ", 
                            switch(self$get_network(),
                                   "main" = "do not use TRUE for `testnet` or `regtest`",
                                   "test" = "`testnet=TRUE`",
                                   "regtest" = "`regtest=TRUE`"),
                            "."),
                     call. = FALSE)
            invisible(self)
        },
        term = function(){
            cmd = "killall -s SIGTERM --regex bitcoind.*"
            if(!self$is.localhost()) stop(run_localhost_msg("term",cmd), call. = FALSE)
            message(cmd)
            system(cmd)
        },
        kill = function(){
            cmd = "killall -s SIGKILL --regex bitcoind.*"
            if(!self$is.localhost()) stop(run_localhost_msg("kill",cmd), call. = FALSE)
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
        getmempoolinfo = function() bitcoind.rpc(host=self$host, user=self$rpcuser, password=private$rpcpassword, port=self$rpcport, method = "getmempoolinfo")$result,
        getnetworkinfo = function() bitcoind.rpc(host=self$host, user=self$rpcuser, password=private$rpcpassword, port=self$rpcport, method = "getnetworkinfo")$result,
        getnewaddress = function(account = "") bitcoind.rpc(host=self$host, user=self$rpcuser, password=private$rpcpassword, port=self$rpcport, method = "getnewaddress", params = list(account))$result,
        getpeerinfo = function() bitcoind.rpc(host=self$host, user=self$rpcuser, password=private$rpcpassword, port=self$rpcport, method = "getpeerinfo")$result,
        getrawmempool = function(verbose = FALSE) bitcoind.rpc(host=self$host, user=self$rpcuser, password=private$rpcpassword, port=self$rpcport, method = "getrawmempool", params = list(verbose))$result,
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
            no_value_args = c("server", "daemon", "listen"[as.logical(self$listen)], "regtest"[as.logical(self$regtest)], "testnet"[as.logical(self$testnet)])
            value_args = c("port" = self$port,
                           "rpcuser" = self$rpcuser,
                           "rpcpassword" = if(mask) "***" else private$rpcpassword, 
                           "rpcport" = self$rpcport,
                           "datadir" = if(mask || !self$is.localhost()) self$datadir else path.expand(self$datadir),
                           "pid" = self$pid,
                           "connect" = if(length(self$connect)) self$connect,
                           "addnode" = if(length(self$addnode)) self$addnode)
            paste(
                "bitcoind",
                paste0("-",no_value_args,collapse=" "),
                paste0("-",names(value_args),"=",value_args,collapse=" ")
            )
        },
        rpcpassword = character()
    )
)