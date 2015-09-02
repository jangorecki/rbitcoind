# R interface to bitcoin daemon [![Build Status](https://travis-ci.org/jangorecki/rbitcoind.svg?branch=master)](https://travis-ci.org/jangorecki/rbitcoind)

Creates R6 class which allows to interact with official bitcoin daemon using json RPC methods.  
Using `sapply` or `lapply` you can easily manage a farm of bitcoin nodes and mockup a complete environment.  
Be aware each node requires own copy of blockchain unless you are using `regtest` mode which runs private empty blokchain.  
If you want to run multiple nodes of `testnet` or `mainnet` you should start single instance, fully synchronize it and copy blockchain data to rest of your nodes.  
Names of methods match to names listed in [official developer reference](https://bitcoin.org/en/developer-reference#rpcs). Not all methods are implemented, for those which aren't you can use `x$rpc(method, params)` method which allows any other json-rpc method.  
There are also many helper methods, some of them uses system calls and may not work on non-linux OS.  

# Usage

Example usage on localhost.  

```r
library(rbitcoind)
btcd = bitcoind$new(rpcuser = "username", 
                    rpcpassword = "password", 
                    regtest = TRUE)
# run daemon on localhost
btcd$run()
# print daemon summary
print(btcd)
# daemon summary as data.frame
btcd$status()

# instantly mine 5 blocks (in regtest mode only and v0.11+)
btcd$generate(5L)
# various info about node and network
btcd$getinfo()
# current state of your blockchain
btcd$getblockchaininfo()

# stop daemon instance
btcd$stop()
```

The CI process run tests which includes setting up new private blockchain using `regtest` mode and populating blocks. You can find the process in `.travis.yml` file and `tests/tests.R`.  

# Installation

Stable:

```r
install.packages("rbitcoind", repos="https://jangorecki.github.io/drat")
```

Devel:

```r
library(devtools) # install.packages("devtools")
install_github("jangorecki/rbitcoind")
```

## License

GPL-3  

## Contact

`J.Gorecki@wit.edu.pl`
