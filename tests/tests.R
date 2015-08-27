## rbitcoind CI tests
# confgurate new private blockchain using `regtest` mode
# mine some BTC
# dump some statitics blockchain, wallet, accounts
# iterate few times
# validate results

library(rbitcoind)
library(data.table)
library(RSQLite)

# deploy bitcoin.conf file from rbitcoind/conf/bitcoin.conf

bitcoind_datadir = path.expand("~/.bitcoin")
bitcoind_conf_pkg = system.file("conf","bitcoin.conf", package = "rbitcoind")
bitcoind_conf = paste(bitcoind_datadir, "bitcoin.conf", sep="/")
if(!dir.exists(bitcoind_datadir)){
    dir.create(bitcoind_datadir)
    file.copy(bitcoind_conf_pkg, bitcoind_conf)
} else {
    if(!file.exists(bitcoind_conf)){
        file.copy(bitcoind_conf_pkg, bitcoind_conf)
    } else {
        conf_match = identical(readLines(bitcoind_conf_pkg,warn=FALSE), readLines(bitcoind_conf,warn=FALSE))
        if(!conf_match) stop(paste0(bitcoind_conf, " should not exists or should match to ",bitcoind_conf_pkg,"."))
    }
}
cat(head(readLines(bitcoind_conf, warn = FALSE),-1),"rpcpassword=***", sep="\n")

# run daemon

btcd = bitcoind$new(host = "127.0.0.1",
                    rpcuser = "rpcuser",
                    rpcpassword = "rpcpassword",
                    rpcport = "18332")
btcd$run()
Sys.sleep(5L)
options("use.data.table" = TRUE)

# valid testing environment

if(!length(info <- btcd$getinfo())) stop("Testing connection to bitcoin daemon fails.")
setDT(info)[]
# if((blockcount <- btcd$getblockcount()) > 0L) stop(paste0("Connected to non fresh blockchain, already has ", blockcount, " blocks. For CI use fresh environment."))

# iterations on blockchain and wallet growing

if(!btcd$getblockchaininfo()$chain=="regtest") stop("generate method should be run only on regtest network.")

db_file = "rbitcoind.db"
conn = dbConnect(SQLite(), db_file)
keepAtomic = function(x) x[, .SD, .SDcols=sapply(x, is.atomic)]
dbWrite = function(name, value) dbWriteTable(conn, name, value = keepAtomic(value), append = TRUE)
dbRead = function(name) use.data.table(dbReadTable(conn, name))
selfName = function(x) setNames(x, x)
getRandString = function(len=12L) return(paste(sample(c(rep(0:9,each=5),LETTERS,letters),len,replace=TRUE),collapse='')) # https://ryouready.wordpress.com/2008/12/18/generate-random-string-name/#comment-38

set.seed(123)
ci_time = as.integer(Sys.time())
meta_cols = function(i, ts=Sys.time()) list(ci=ci_time, iteration=i, timestamp=ts)
for(i in 1:4){ # i = 1L
    
    # wait a second before each iteration except first
    if(i > 1L) Sys.sleep(time = 1L)
    
    # populate blockchain
    if((ver <- btcd$getinfo()$version) >= 110000L) btcd$generate(if(btcd$getblockcount() >= 100L) 1L else 100L)
    else cat("Update your bitcoin package, currently installed ", ver,", required 110000.\n",sep="")
    
    # populate accounts
    new_accounts = sapply(1:(i*2L), function(i) getRandString(6L))
    sapply(new_accounts, function(account) btcd$getnewaddress(account))
    
    # populate addresses
    accounts = names(btcd$listaccounts())
    new_address_accounts = sample(accounts, as.integer(length(accounts)/2L))
    sapply(new_address_accounts, function(account) btcd$getnewaddress(account))
    
    # make transactions
    if((ver <- btcd$getinfo()$version) >= 110000L) invisible()
    else cat("Update your bitcoin package, currently installed ", ver,", required 110000.\n",sep="")
    
    # collect info
    blockchaininfo = setDT(btcd$getblockchaininfo())[, c(meta_cols(i), .SD)]
    walletinfo = setDT(btcd$getwalletinfo())[, c(meta_cols(i), .SD)]
    # write to db
    dbWrite("blockchaininfo", blockchaininfo)
    dbWrite("walletinfo", walletinfo)
    lapply(accounts, function(account){
        transactions = btcd$listtransactions(account)
        if(length(transactions)) dbWrite("transactions", value = setDT(transactions)[, c(meta_cols(i), .SD)])
    })
    lapply(accounts, function(account){
        addresses = btcd$getaddressesbyaccount(account)
        if(length(addresses)) dbWrite("addressesbyaccount", value = data.table(account = account, addresses = addresses)[, c(meta_cols(i), .SD)])
    })
}
invisible(dbDisconnect(conn))

# read blockchain and wallet data

conn = dbConnect(SQLite(), db_file)
tbls = dbListTables(conn)

options("datatable.prettyprint.char"=20L)
DT = lapply(selfName(tbls), function(tbl) dbRead(tbl))
lapply(DT, head, 4L)
sapply(DT, nrow)

# stop and exit

btcd$stop()
q("no")
