## rbitcoind CI tests
# confgurate new private blockchain using `regtest` mode
# mine some BTC
# dump some statitics blockchain, wallet, accounts
# iterate few times
# validate results

## tests should be execute on fresh CI process, with bitcoind installed but not started/configured.

library(rbitcoind)
library(data.table)
library(RSQLite)

options(rpchost = "127.0.0.1",
        rpcuser = "bitcoinduser",
        rpcpassword = "userpassbitcoind",
        rpcport = "18332")

# deploy bitcoin.conf file based on rbitcoind/tests/bitcoin.conf

bitcoin_dir = path.expand("~/.bitcoin")
bitcoin_conf_pkg = system.file("tests","bitcoin.conf", package = "rbitcoind")
if(!dir.exists(bitcoin_dir)) dir.create(bitcoin_dir) else stop(paste(bitcoin_dir, "should not exists in fresh CI builds."))
bitcoin_conf = paste(bitcoin_dir, "bitcoin.conf", sep="/")
if(!file.exists(bitcoin_conf)) file.copy(bitcoin_conf_pkg, bitcoin_conf) else stop(paste(bitcoin_conf, "should not exists in fresh CI builds."))

# run daemon
run.bitcoind()
Sys.sleep(10L)

# valid testing environment

if(!length(getinfo())) stop("Testing connection to bitcoin daemon fails.")
if((blockcount <- getblockcount()) > 1212L) stop(paste0("Connected to non fresh blockchain, already has ", blockcount, " blocks. For CI use fresh environment."))

# iterations of blockchain growning

options("use.data.table" = TRUE)
stripCols = function(x) x[, .SD, .SDcols=sapply(x, is.atomic)]
dbWrite = function(name, value) dbWriteTable(conn, name, value = stripCols(value), append = TRUE)
dbRead = function(name) use.data.table(dbReadTable(conn, name))
selfName = function(x) setNames(x, x)

ci_time = as.integer(Sys.time())
meta_cols = function(i, ts=Sys.time()) list(ci=ci_time, iteration=i, timestamp=ts)
db_file = "rbitcoind.db"
conn = dbConnect(SQLite(), db_file)
for(i in 1:4){
    # wait before each iteration
    if(i > 1L) Sys.sleep(time = 1L)
    # mine BTC
    if(blockcount < 300L) blocks = generate(101L)
    # create acounts, addresses
    # TO DO
    # transfer BTC
    # TO DO
    # collect info
    blockchaininfo = setDT(getblockchaininfo())[, c(meta_cols(i), .SD)]
    walletinfo = setDT(getwalletinfo())[, c(meta_cols(i), .SD)]
    accounts = names(listaccounts())
    # write to db
    dbWrite("blockchaininfo", blockchaininfo)
    dbWrite("walletinfo", walletinfo)
    lapply(accounts, function(account) dbWrite("transactions", value = listtransactions(account)[, c(meta_cols(i), .SD)]))
    lapply(accounts, function(account) dbWrite("addressesbyaccount", value = data.table(addresses = getaddressesbyaccount(account))[, c(meta_cols(i), list(account = account), .SD)]))
}
invisible(dbDisconnect(conn))

# get history

conn = dbConnect(SQLite(), db_file)
blockchain_tbls = c("blockchaininfo")
wallet_tbls = c("walletinfo")
accounts_tbls = c("transactions","addressesbyaccount")
blockchain_data = lapply(selfName(blockchain_tbls), dbRead)
wallet_data = lapply(selfName(wallet_tbls), dbRead)
accounts_data = lapply(selfName(accounts_tbls), function(name) dbRead(name))
invisible(dbDisconnect(conn))

sapply(names(blockchain_data), function(blockchain_tbl) write.csv(blockchain_data[[blockchain_tbl]], file = paste0("rbitcoind_blockchain_data_",blockchain_tbl,".csv"), row.names = FALSE))
sapply(names(wallet_data), function(wallet_tbl) write.csv(wallet_data[[wallet_tbl]], file = paste0("rbitcoind_wallet_data_",wallet_tbl,".csv"), row.names = FALSE))
sapply(names(accounts_data), function(accounts_tbl) write.csv(accounts_data[[accounts_tbl]], file = paste0("rbitcoind_accounts_data_",accounts_tbl,".csv"), row.names = FALSE))
