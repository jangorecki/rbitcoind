library(rbitcoind)
library(data.table)
library(RSQLite)
options(rpchost = "127.0.0.1",
        rpcuser = "bitcoinduser",
        rpcpassword = "userpassbitcoind",
        rpcport = "18332") # regtest

# valid testing environment

if(!length(getinfo())) stop("testing connection to bitcoin daemon fails")
if((blockcount <- getblockcount()) > 1212L) invisible() #stop(paste0("connected to non clean blockchain, already has ", blockcount, " blocks."))

# iterations of blockchain growning

options("use.data.table" = TRUE)
stripCols = function(x) x[, .SD, .SDcols=sapply(x, is.atomic)]
dbWrite = function(name, value) dbWriteTable(conn, name, value = stripCols(value), append = TRUE)
dbRead = function(name) use.data.table(dbReadTable(conn, name))
selfName = function(x) setNames(x, x)

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
    blockchaininfo = setDT(getblockchaininfo())[, c(list(iteration = i, timestamp=Sys.time()), .SD)]
    walletinfo = setDT(getwalletinfo())[, c(list(iteration = i, timestamp=Sys.time()), .SD)]
    accounts = names(listaccounts())
    # write to db
    dbWrite("blockchaininfo", blockchaininfo)
    dbWrite("walletinfo", walletinfo)
    lapply(accounts, function(account) dbWrite("transactions", value = listtransactions(account)[, c(list(iteration = i), .SD)]))
    lapply(accounts, function(account) dbWrite("addressesbyaccount", value = data.table(addresses = getaddressesbyaccount(account))[, c(list(iteration = i, account = account), .SD)]))
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
