library(rbitcoind)
options(rpchost = "192.168.56.103",
        rpcuser = "bitcoinduser",
        rpcpassword = "userpassbitcoind",
        rpcport = "18332") # regtest
getinfo()
