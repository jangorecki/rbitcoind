#' @name rbitcoind
#' @title R interface to bitcoin daemon
#' @docType package
#' @author Jan Gorecki
#' @description You need to have to bitcoin node installed, blockchain is not necessary. Unit tests runs on \emph{regtest} network which uses private blockchain and doesn't need to be synchronized.
#' @seealso \link{bitcoind}, \link{bitcoind.rpc}
#' @references \url{https://bitcoin.org/en/glossary/regression-test-mode}
#' @examples \dontrun{
#' # low level calls to json-rpc
#' bitcoind.rpc(host = "127.0.0.1",
#'              user = "rpcuser",
#'              password = "rpcpassword",
#'              port = "18332", # regtest
#'              method = "getinfo")
#' 
#' # bitcoin daemon object
#' btcd <- bitcoind$new(host = "127.0.0.1", 
#'                      rpcuser = "rpcuser", 
#'                      rpcpassword = "rpcpassword", 
#'                      rpcport = "18332")
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
        regtest = integer(),
        pid = character(), # file name
        initialize = function(host = "127.0.0.1", port = "8333", rpcuser, rpcpassword, rpcport = "8332",  datadir = "~/.bitcoin", connect = character(), addnode = character(), regtest = FALSE, pid = "bitcoind.pid"){
            self$host = host
            self$port = port
            self$rpcuser = rpcuser
            private$rpcpassword = rpcpassword
            self$rpcport = rpcport
            self$datadir = datadir
            self$connect = connect
            self$addnode = addnode
            self$regtest = regtest
            self$pid = pid
            self
        },
        is.localhost = function() !length(self$host) || self$host %in% c("127.0.0.1","localhost"),
        is.running =  function(){
            r = tryCatch(bitcoind.rpc(host=self$host, user=self$rpcuser, password=private$rpcpassword, port=self$rpcport, method = "getinfo"), error = function(e) FALSE)
            if(all(c("result","error","id") %in% names(r))) return(is.null(r[["error"]])) else FALSE
        },
        get_pid = function() if(file.exists(pid_path <- paste(self$datadir,if(self$regtest) "regtest", self$pid, sep="/"))) readLines(pid_path,warn=FALSE),
        print = function(details = FALSE){
            cat("<bitcoind>\n")
            cat("  datadir: ", self$datadir,"\n", sep="")
            cat("  pid: ", self$get_pid(), "\n", sep="")
            if(details){
                cat("  balance: ", NA, "\n", sep="")
                cat("  accounts: ", NA, "\n", sep="")
                cat("  connections: ", NA, "\n", sep="")
            }
        },
        # system calls
        grep_pid = function(){
            cmd = "pgrep -f bitcoin.*"
            message(cmd)
            system(cmd, intern = TRUE)
        },
        run = function(){
            dir = paste(path.expand(self$datadir), if(self$regtest) "regtest", sep="/")
            if(!dir.exists(dir)) dir.create(dir, recursive = TRUE)
            if(self$is.running()) warning("bitcoind instance already running.", call. = FALSE)
            else {
                if(self$is.localhost()){
                    message(private$run_cmd(mask=TRUE))
                    system(private$run_cmd(mask=FALSE))
                }
                else {
                    cat("# Run method supports only localhost daemon, run remote daemon manually using:\n",sep="")
                    cat(private$run_cmd(mask=TRUE),"\n",sep="")
                }
            }
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
        run_cmd = function(mask=TRUE) paste0("bitcoind -server -listen -port=",self$port," -rpcuser=",self$rpcuser," -rpcpassword=", if(isTRUE(mask)) "***" else private$rpcpassword," -rpcport=",self$rpcport," -datadir=", if(isTRUE(mask)) self$datadir else path.expand(self$datadir), if(length(self$connect)) paste0(" -connect=",self$connect), if(length(self$addnode)) paste0(" -addnode=",self$addnode), if(self$regtest) " -regtest"," -pid=",self$pid," -daemon"),
        rpcpassword = character()
    )
)