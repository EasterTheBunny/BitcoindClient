# BitcoindClient
Scala client for connection to bitcoind via JSON-RPC.

Usage:
val client = new BitcoindClient("username","password").setPort("9332").setSecure(true)

client.getBalance(None, Some(20)).result match {
	case Some(b) => println("balance is: " + b)
	case _ => println("error: no balance returned")
}

to unlock and do something in with the wallet:

val unlock = client.walletPassPhrase("walletpass", 120)
unlock.error match {
	case Some(error) if(error.code == -17) => doSomething
	case None => {
		DatabaseInvoices.getUnpaidAddressesByType("BTC").foreach(addr => {
      		val btcaddr = btc.getReceivedByAddress(addr.address.is, Some(10))
      		btcaddr.result match {
        		case Some(amount) =>
          			/**
          			*	Process settlement of bitcoin payment for each btc
          			*	receive address. 
          			*
          			*/
        		case None => 
          			// handle error
      		}
    	})
	}
}