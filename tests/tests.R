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

if(!length(getinfo())) stop("Testing connection to bitcoin daemon fails.")
if((blockcount <- getblockcount()) > 1212L) stop(paste0("Connected to non fresh blockchain, already has ", blockcount, " blocks. For CI use fresh environment."))

# helpers

options("use.data.table" = TRUE)
stripCols = function(x) x[, .SD, .SDcols=sapply(x, is.atomic)]
dbWrite = function(name, value) dbWriteTable(conn, name, value = stripCols(value), append = TRUE)
dbRead = function(name) use.data.table(dbReadTable(conn, name))
selfName = function(x) setNames(x, x)

# populate blockchain and wallet data in iterations

ci_time = as.integer(Sys.time())
meta_cols = function(i, ts=Sys.time()) list(ci=ci_time, iteration=i, timestamp=ts)
db_file = "rbitcoind.db"
conn = dbConnect(SQLite(), db_file)
for(i in 1:4){ # i = 1L
    # wait before each iteration
    if(i > 1L) Sys.sleep(time = 1L)
    # generate blocks
    #if(blockcount < 300L) blocks = generate(101L)
    # create acounts
    #new_num = i * 2L
    #getnewaddress(acc)
    # addresses
    accounts = names(listaccounts())
    addresses = lapply(accounts, as.null)
    # TO DO
    # make transactions
    # TO DO
    # collect info
    blockchaininfo = setDT(getblockchaininfo())[, c(meta_cols(i), .SD)]
    walletinfo = setDT(getwalletinfo())[, c(meta_cols(i), .SD)]
    
    # write to db
    dbWrite("blockchaininfo", blockchaininfo)
    dbWrite("walletinfo", walletinfo)
    lapply(accounts, function(account) dbWrite("transactions", value = listtransactions(account)[, c(meta_cols(i), .SD)]))
    lapply(accounts, function(account) dbWrite("addressesbyaccount", value = data.table(addresses = getaddressesbyaccount(account))[, c(meta_cols(i), list(account = account), .SD)]))
}
invisible(dbDisconnect(conn))
stop.bitcoind()

# read blockchain and wallet data

conn = dbConnect(SQLite(), db_file)
blockchain_tbls = c("blockchaininfo")
wallet_tbls = c("walletinfo")
accounts_tbls = c("transactions","addressesbyaccount")
blockchain_data = lapply(selfName(blockchain_tbls), dbRead)
wallet_data = lapply(selfName(wallet_tbls), dbRead)
accounts_data = lapply(selfName(accounts_tbls), function(name) dbRead(name))
invisible(dbDisconnect(conn))

# dump data to csv files

suppressWarnings(file.remove(paste0("rbitcoind_blockchain_data_",blockchain_tbls,".csv"), paste0("rbitcoind_wallet_data_",wallet_tbls,".csv"),  paste0("rbitcoind_accounts_data_",accounts_tbls,".csv")))
sapply(names(blockchain_data), function(blockchain_tbl) write.csv(blockchain_data[[blockchain_tbl]], file = paste0("rbitcoind_blockchain_data_",blockchain_tbl,".csv"), row.names = FALSE))
sapply(names(wallet_data), function(wallet_tbl) write.csv(wallet_data[[wallet_tbl]], file = paste0("rbitcoind_wallet_data_",wallet_tbl,".csv"), row.names = FALSE))
sapply(names(accounts_data), function(accounts_tbl) write.csv(accounts_data[[accounts_tbl]], file = paste0("rbitcoind_accounts_data_",accounts_tbl,".csv"), row.names = FALSE))
