#' @title decodescript
#' @param hex character
#' @seealso \url{https://bitcoin.org/en/developer-reference#decodescript}
decodescript <- function(hex) bitcoind.rpc(method = "decodescript", params = list(hex))$result
#' @title generate
#' @param numblocks integer.
#' @seealso \url{https://bitcoin.org/en/developer-reference#generate}
generate <- function(numblocks) bitcoind.rpc(method = "generate", params = list(numblocks))$result
#' @title getaccountaddress
#' @param account character.
#' @seealso \url{https://bitcoin.org/en/developer-reference#getaccountaddress}
getaccountaddress <- function(account) bitcoind.rpc(method = "getaccountaddress", params = list(account))$result
#' @title getaccount
#' @param bitcoinaddress character.
#' @seealso \url{https://bitcoin.org/en/developer-reference#getaccount}
getaccount <- function(bitcoinaddress) bitcoind.rpc(method = "getaccount", params = list(bitcoinaddress))$result
#' @title getaddressesbyaccount
#' @param account character.
#' @seealso \url{https://bitcoin.org/en/developer-reference#getaddressesbyaccount}
getaddressesbyaccount <- function(account) bitcoind.rpc(method = "getaddressesbyaccount", params = list(account))$result
#' @title getbalance
#' @param account character.
#' @seealso \url{https://bitcoin.org/en/developer-reference#getbalance}
getbalance <- function(account) bitcoind.rpc(method = "getbalance", params = list(account))$result
#' @title getbestblockhash
#' @seealso \url{https://bitcoin.org/en/developer-reference#getbestblockhash}
getbestblockhash <-  function() bitcoind.rpc(method = "getbestblockhash")$result
#' @title getblock
#' @param hash character.
#' @param verbose logical.
#' @seealso \url{https://bitcoin.org/en/developer-reference#getblock}
getblock <- function(hash, verbose = TRUE) bitcoind.rpc(method = "getblock", params = list(hash, verbose))$result
#' @title getblockchaininfo
#' @seealso \url{https://bitcoin.org/en/developer-reference#getblockchaininfo}
getblockchaininfo <- function() bitcoind.rpc(method = "getblockchaininfo")$result
#' @title getblockcount
#' @seealso \url{https://bitcoin.org/en/developer-reference#getblockcount}
getblockcount <- function() bitcoind.rpc(method = "getblockcount")$result
#' @title getblockhash
#' @param index integer.
#' @seealso \url{https://bitcoin.org/en/developer-reference#getblockhash}
getblockhash <- function(index) bitcoind.rpc(method = "getblockhash", params = list(index))$result
#' @title getchaintips
#' @seealso \url{https://bitcoin.org/en/developer-reference#getchaintips}
getchaintips <- function() bitcoind.rpc(method = "getchaintips")$result
#' @title getconnectioncount
#' @seealso \url{https://bitcoin.org/en/developer-reference#getconnectioncount}
getconnectioncount <- function() bitcoind.rpc(method = "getconnectioncount")$result
#' @title getinfo
#' @seealso \url{https://bitcoin.org/en/developer-reference#getinfo}
getinfo <- function() bitcoind.rpc(method = "getinfo")$result
#' @title getnetworkinfo
#' @seealso \url{https://bitcoin.org/en/developer-reference#getnetworkinfo}
getnetworkinfo <- function() bitcoind.rpc(method = "getnetworkinfo")$result
#' @title getpeerinfo
#' @seealso \url{https://bitcoin.org/en/developer-reference#getpeerinfo}
getpeerinfo <- function() bitcoind.rpc(method = "getpeerinfo")$result
#' @title getnewaddress
#' @param account character.
#' @seealso \url{https://bitcoin.org/en/developer-reference#getnewaddress}
getnewaddress <- function(account = "") bitcoind.rpc(method = "getnewaddress", params = list(account))$result
#' @title getreceivedbyaccount
#' @param account character.
#' @param minconf integer.
#' @seealso \url{https://bitcoin.org/en/developer-reference#getreceivedbyaccount}
getreceivedbyaccount <- function(account, minconf = 1L) bitcoind.rpc(method = "getreceivedbyaccount", params = list(account, minconf))$result
#' @title getreceivedbyaddress
#' @param bitcoinaddress character.
#' @param minconf integer.
#' @seealso \url{https://bitcoin.org/en/developer-reference#getreceivedbyaddress}
getreceivedbyaddress <- function(bitcoinaddress, minconf = 1L) bitcoind.rpc(method = "getreceivedbyaddress", params = list(bitcoinaddress, minconf))$result
#' @title gettransaction
#' @param txid character.
#' @param includeWatchonly logical.
#' @seealso \url{https://bitcoin.org/en/developer-reference#gettransaction}
gettransaction <- function(txid, includeWatchonly = FALSE) bitcoind.rpc(method = "gettransaction", params = list(txid, includeWatchonly))$result
#' @title getunconfirmedbalance
#' @seealso \url{https://bitcoin.org/en/developer-reference#getunconfirmedbalance}
getunconfirmedbalance <- function() bitcoind.rpc(method = "getunconfirmedbalance")$result
#' @title getwalletinfo
#' @seealso \url{https://bitcoin.org/en/developer-reference#getwalletinfo}
getwalletinfo <- function() bitcoind.rpc(method = "getwalletinfo")$result
#' @title help.bitcoind
#' @param command character.
#' @seealso \url{https://bitcoin.org/en/developer-reference#help}
help.bitcoind <- function(command) if(missing(command)) bitcoind.rpc(method = "help")$result else bitcoind.rpc(method = "help", params = list(command))$result
#' @title listaccounts
#' @param minconf integer.
#' @seealso \url{https://bitcoin.org/en/developer-reference#listaccounts}
listaccounts <- function(minconf = 1L) bitcoind.rpc(method = "listaccounts", params = list(minconf))$result
#' @title listaddressgroupings
#' @seealso \url{https://bitcoin.org/en/developer-reference#listaddressgroupings}
listaddressgroupings <- function() bitcoind.rpc(method = "listaddressgroupings")$result
#' @title listreceivedbyaccount
#' @param minconf integer.
#' @param includeempty logical.
#' @seealso \url{https://bitcoin.org/en/developer-reference#listreceivedbyaccount}
listreceivedbyaccount <- function(minconf = 1L, includeempty = FALSE) bitcoind.rpc(method = "listreceivedbyaccount", params = list(minconf, includeempty))$result
#' @title listsinceblock
#' @param blockhash character.
#' @param target.confirmations integer.
#' @param includeWatchonly logical.
#' @seealso \url{https://bitcoin.org/en/developer-reference#listsinceblock}
listsinceblock <- function(blockhash, target.confirmations = 1L, includeWatchonly = FALSE) bitcoind.rpc(method = "listsinceblock", params = list(blockhash, target.confirmations, includeWatchonly))$result
#' @title listtransactions
#' @param account character.
#' @param count integer.
#' @param from integer.
#' @seealso \url{https://bitcoin.org/en/developer-reference#listtransactions}
listtransactions <- function(account, count = 10L, from = 0L) bitcoind.rpc(method = "listtransactions", params = list(account, count, from))$result
#' @title listunspent
#' @param miconf integer.
#' @param maxconf integer.
#' @param addresses character.
#' @seealso \url{https://bitcoin.org/en/developer-reference#listunspent}
listunspent <- function(miconf = 1L, maxconf = 9999999L, addresses) bitcoind.rpc(method = "listunspent", params = list(miconf, maxconf, addresses))$result
#' @title stop.bitcoind
#' @seealso \url{https://bitcoin.org/en/developer-reference#stop}
stop.bitcoind <- function() bitcoind.rpc(method = "stop")$result
#' @title validateaddress
#' @param bitcoinaddress character.
#' @seealso \url{https://bitcoin.org/en/developer-reference#validateaddress}
validateaddress <- function(bitcoinaddress) bitcoind.rpc(method = "validateaddress", params = list(bitcoinaddress))$result
