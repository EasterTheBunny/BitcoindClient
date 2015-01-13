import BitcoindClient
import InvoicesFromYourDatabase

/**
*	This example assumes you have a database containing unpaid invoices
*	with an accessor class called InvoicesFromYourDatabase with the method
*	getUnpaidAddressesByType() and an instance of bitcoind running on 
*	127.0.0.1:9332
*
*/
object example {
    def main(args: Array[String]) {
    	val client = new BitcoindClient("username","password").setPort("9332").setSecure(true)

		client.getBalance(None, Some(20)).result match {
			case Some(b) => println("balance is: " + b)
			case _ => println("error: no balance returned")
		}


		val unlock = client.walletPassPhrase("walletpass", 120)
		unlock.error match {
			case Some(error) if(error.code == -17) => println(error.message)
			case None => {
				InvoicesFromYourDatabase.getUnpaidAddressesByType("BTC").foreach(addr => {
      				val btcaddr = client.getReceivedByAddress(addr.address.is, Some(10))
      				btcaddr.result match {
        				case Some(amount) =>
          					/**
          					*	Process settlement of bitcoin payment for each btc
          					*	receive address. 
          					*
          					*/
          					println(amount)
        				case None => 
          					// handle error
          					println("error")
      				}
    			})
			}
		}
    }
  }