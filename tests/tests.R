## rbitcoind CI tests
# confgurate new private blockchain using `regtest` mode
# mine some BTC
# dump some statitics blockchain, wallet, accounts
# iterate few times
# validate results

library(rbitcoind)
library(data.table)
library(RSQLite)

options(rpchost = "127.0.0.1",
        rpcuser = "bitcoinduser",
        rpcpassword = "userpassbitcoind",
        rpcport = "18332")

# deploy bitcoin.conf file from rbitcoind/tests/bitcoin.conf

bitcoind_datadir = path.expand(paste("~", getOption("bitcoind.datadir", ".bitcoin"), sep="/"))
bitcoind_conf_pkg = system.file("tests","bitcoin.conf", package = "rbitcoind")
bitcoind_conf = paste(bitcoind_datadir, "bitcoin.conf", sep="/")
if(!dir.exists(bitcoind_datadir)){
    dir.create(bitcoind_datadir)
    file.copy(bitcoind_conf_pkg, bitcoind_conf)
} else {
    if(!file.exists(bitcoind_conf)){
        file.copy(bitcoind_conf_pkg, bitcoind_conf)
    } else {
        conf_match = unname(tools::md5sum(bitcoind_conf_pkg))==unname(tools::md5sum(bitcoind_conf))
        if(!conf_match) stop(paste0(bitcoind_conf, " should not exists or should md5 match to ",bitcoind_conf_pkg,"."))
    }
}
cat(readLines(bitcoind_conf, warn = FALSE), sep="\n")

# run daemon

run.bitcoind()
Sys.sleep(10L)

# valid testing environment

if(!length(info <- getinfo())) stop("Testing connection to bitcoin daemon fails.")
print(setDT(info))
if((blockcount <- getblockcount()) > 1212L) stop(paste0("Connected to non fresh blockchain, already has ", blockcount, " blocks. For CI use fresh environment."))

# helpers

options("use.data.table" = TRUE)
stripCols = function(x) x[, .SD, .SDcols=sapply(x, is.atomic)]
dbWrite = function(name, value) dbWriteTable(conn, name, value = stripCols(value), append = TRUE)
dbRead = function(name) use.data.table(dbReadTable(conn, name))
selfName = function(x) setNames(x, x)
getRandString = function(len=12L) return(paste(sample(c(rep(0:9,each=5),LETTERS,letters),len,replace=TRUE),collapse='')) # https://ryouready.wordpress.com/2008/12/18/generate-random-string-name/#comment-38

# populate blockchain and wallet data in iterations

set.seed(123)
ci_time = as.integer(Sys.time())
meta_cols = function(i, ts=Sys.time()) list(ci=ci_time, iteration=i, timestamp=ts)
db_file = "rbitcoind.db"
suppressWarnings(file.remove(db_file))
conn = dbConnect(SQLite(), db_file)
for(i in 1:4){ # i = 1L
    # wait before each iteration
    if(i > 1L) Sys.sleep(time = 1L)
    # generate blocks
    if(version.bitcoind() >= 110000L){
        if(i == 1L) invisible(generate(100L))
        invisible(generate(i * 2L))
    }
    # create accounts
    new_accounts = sapply(1:(i*2L), function(i) getRandString(6L))
    invisible(sapply(new_accounts, getnewaddress))
    # addresses
    accounts = names(listaccounts())
    # addresses = lapply(accounts, as.null)
    # TO DO
    
    # make transactions
    if(version.bitcoind() >= 110000L){
        # TO DO
    }
    
    # collect info
    blockchaininfo = setDT(getblockchaininfo())[, c(meta_cols(i), .SD)]
    walletinfo = setDT(getwalletinfo())[, c(meta_cols(i), .SD)]
    # write to db
    dbWrite("blockchaininfo", blockchaininfo)
    dbWrite("walletinfo", walletinfo)
    lapply(accounts, function(account){
        transactions = listtransactions(account)
        if(length(transactions)) dbWrite("transactions", value = setDT(transactions)[, c(meta_cols(i), .SD)])
    })
    lapply(accounts, function(account){
        addresses = getaddressesbyaccount(account)
        if(length(addresses)) dbWrite("addressesbyaccount", value = data.table(account = account, addresses = addresses)[, c(meta_cols(i), .SD)])
    })
}
invisible(dbDisconnect(conn))
chain = version.bitcoind() >= 110000L
stop.bitcoind()

# read blockchain and wallet data

conn = dbConnect(SQLite(), db_file)
blockchain_tbls = c("blockchaininfo")
wallet_tbls = c("walletinfo")
accounts_tbls = c(if(chain) "transactions","addressesbyaccount")
blockchain_data = lapply(selfName(blockchain_tbls), dbRead)
wallet_data = lapply(selfName(wallet_tbls), dbRead)
accounts_data = lapply(selfName(accounts_tbls), function(name) dbRead(name))
invisible(dbDisconnect(conn))

# dump data to csv files

suppressWarnings(file.remove(paste0("rbitcoind_blockchain_data_",blockchain_tbls,".csv"), paste0("rbitcoind_wallet_data_",wallet_tbls,".csv"),  paste0("rbitcoind_accounts_data_",accounts_tbls,".csv")))
sapply(names(blockchain_data), function(blockchain_tbl) write.csv(blockchain_data[[blockchain_tbl]], file = paste0("rbitcoind_blockchain_data_",blockchain_tbl,".csv"), row.names = FALSE))
sapply(names(wallet_data), function(wallet_tbl) write.csv(wallet_data[[wallet_tbl]], file = paste0("rbitcoind_wallet_data_",wallet_tbl,".csv"), row.names = FALSE))
sapply(names(accounts_data), function(accounts_tbl) write.csv(accounts_data[[accounts_tbl]], file = paste0("rbitcoind_accounts_data_",accounts_tbl,".csv"), row.names = FALSE))

# # stats
# 
# tbls = c(blockchain_tbls, wallet_tbls, accounts_tbls)
# conn = dbConnect(SQLite(), db_file)
# ldata = lapply(selfName(tbls), dbRead)
# invisible(dbDisconnect(conn))
# 
# lprocess = list(
#     blockchaininfo = function(x) x[, timestamp := as.POSIXct(timestamp, origin="1970-01-01")][, .(from=min(timestamp), to=max(timestamp)), .(chain, blocks, headers)],
#     walletinfo = function(x) x[, timestamp := as.POSIXct(timestamp, origin="1970-01-01")][, .(timestamp, balance, unconfirmed_balance, txcount)],
#     transactions = function(x) x[, ts := as.POSIXct(time, origin="1970-01-01")][, .(.N, total_amount = sum(amount)), .(account, address, year(ts), month(ts), mday(ts))],
#     addressesbyaccount = function(x) x[, .(from=as.POSIXct(min(timestamp), origin="1970-01-01"), to=as.POSIXct(max(timestamp), origin="1970-01-01")), .(account, addresses)]
# )
# 
# stats = function(tbls, ldata, lprocess){
#     stopifnot(names(lprocess)==names(ldata))
#     mapply(function(process, data) process(data), 
#            lprocess[names(lprocess) %in% tbls], 
#            ldata[names(ldata) %in% tbls], 
#            SIMPLIFY = FALSE)
# }
# print(stats(tbls, ldata, lprocess))
