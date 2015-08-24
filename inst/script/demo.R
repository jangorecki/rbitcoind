source("bitcoind.R")
options(rpchost = "192.168.56.103",
        rpcuser = "bitcoinduser",
        rpcpassword = "userpassbitcoind",
        rpcport = "18332") # regtest
# options(rpcport = NULL) # mainnet