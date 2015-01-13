package code
package lib

import net.liftweb.json._
import net.liftweb.util.Helpers._
import net.liftweb.common._
//import net.liftweb.json.JsonParser._
import java.net.Authenticator
import java.net.PasswordAuthentication
import java.io.InputStream
import scala.io.Source
import java.net.URL
import java.net.HttpURLConnection
import java.security.cert._
import javax.net.ssl._
import java.io.IOException

case class ReceiveByAddress(address: String, account: String, amount: BigDecimal, confirmations: Integer, txids: List[String])
case class Transaction(amount: BigDecimal, confirmations: Integer, blockhash: String, blockindex: Integer, blocktime: Long, txid: String, time: Long, timereceived: Long, details: List[TransactionDetail])
case class TransactionDetail(account: String, address: String, category: String, amount: BigDecimal)

case class BigDecimalDefault(result: Option[BigDecimal], error: Option[Error], id: Int)
case class StringDefault(result: Option[String], error: Option[Error], id: Int)
case class ListOfAddresses(result: Option[List[ReceiveByAddress]], error: Option[Error], id: Int)
case class TransactionResult(result: Option[Transaction], error: Option[Error], id: Int)
case class AddressesResult(result: Option[List[String]], error: Option[Error], id: Int)
case class Error(code: Int, message: String)
case class DefaultResult(result: Option[String], error: Option[Error], id: Int)

class MyHostnameVerifier extends HostnameVerifier {
  def verify(hostname: String, session: SSLSession): Boolean = {
    // verification of hostname is switched off
	true
  }
}

class MyTrustManager extends X509TrustManager {
  def checkClientTrusted(chain: Array[X509Certificate], authType: String) = {}
  def checkServerTrusted(chain: Array[X509Certificate], authType: String) = {}

  def getAcceptedIssuers: Array[X509Certificate] = {
	  new Array[X509Certificate](0)
  }
}

class Extractor[T](implicit man: Manifest[T]) {
  def dolt(json: JValue): T = {
    implicit val formats = net.liftweb.json.DefaultFormats + BigDecimalSerializer
    json.extract[T]
  }
}

class BasicAuthenticator(user: String, password: String) extends Authenticator {
  val baName = user
  val baPassword = password
  
  override def getPasswordAuthentication: PasswordAuthentication = {
    //println("Authenticating...")
    new PasswordAuthentication(baName, baPassword.toCharArray())
  }
}

/**
 * Still need wallet encryption
 * Work on error handling
 */
class BitcoindClient(userd: String, password: String) {
  val rpcuser = userd
  val rpcpassword = password
  
  private var port = "8332"
  def setPort(pt: String): BitcoindClient = {
    port = pt
    this
  }
  
  private var url = "127.0.0.1"
  def setURL(u: String): BitcoindClient = {
    url = u
    this
  }
  
  private var secure = false
  def setSecure(s: Boolean): BitcoindClient = {
    secure = s
    this
  }
  	  
  def postURL = this.url + ":" + port
  	  
  def listReceivedByAddress(minconf: Option[Integer], includeEmpty: Option[Boolean]) = {
  	val include = if(includeEmpty.getOrElse(false)) "true" else "false"
  	  
  	val params = """{"method":"listreceivedbyaddress", "params":[""" + minconf.getOrElse(1) + "," + include + """], "id":1}"""
    httpPost(postURL, params) match {
  	  case Some(r) => (new Extractor[ListOfAddresses]).dolt(parse(r))
  	  case None => new ListOfAddresses(None,Some(new Error(-10000,"Server contact error")),1)
  	}
  }
  
  def getReceivedByAddress(bitcoinAddress: String, minconf: Option[Integer]) = {
    val params = """{"method":"getreceivedbyaddress", "params":["""" + bitcoinAddress.trim + """",""" + minconf.getOrElse(1) + """], "id":1}"""
    httpPost(postURL, params) match {
      case Some(r) => (new Extractor[BigDecimalDefault]).dolt(parse(r))
      case None => new BigDecimalDefault(None,Some(new Error(-10000,"Server contact error")),1)
    }
  }
  
  def getTransaction(txid: String) = {
    val params = """{"method":"gettransaction", "params":["""" + txid.trim + """"], "id":1}"""
    httpPost(postURL, params) match {
      case Some(r) => (new Extractor[TransactionResult]).dolt(parse(r))
      case None => new TransactionResult(None,Some(new Error(-10000,"Server contact error")),1)
    }
  }
  
  def getNewAddress(account: Option[String]) = {
    val arg = account match {
      case Some(e) => """"""" + e.trim + """""""
      case _ => ""
    }
    
    val params = """{"method":"getnewaddress", "params":[""" + arg + """], "id":1}"""
    httpPost(postURL, params) match {
      case Some(r) => (new Extractor[StringDefault]).dolt(parse(r))
      case None => new StringDefault(None,Some(new Error(-10000,"Server contact error")),1)
    }
  }
  
  def getAddressesByAccount(account: String) = {
    val params = """{"method":"getaddressesbyaccount", "params":["""" + account + """"], "id":1}"""
    httpPost(postURL, params) match {
      case Some(r) => (new Extractor[AddressesResult]).dolt(parse(r))
      case None => new AddressesResult(None,Some(new Error(-10000,"Server contact error")),1)
    }
  }
  
  // return txid as single string value if successful
  def sendToAddress(bitcoinAddress: String, amount: BigDecimal, comment: Option[String], commentTo: Option[String]) = {
    val amt = amount.setScale(8, BigDecimal.RoundingMode.HALF_UP)
    val com = comment match {
      case Some(c) => ""","""" + c.trim() + """""""
      case _ => ""
    }
    val comto = commentTo match {
      case Some(c) => ""","""" + c.trim() + """""""
      case _ => ""
    }
    
    val params = """{"method":"sendtoaddress", "params":["""" + bitcoinAddress.trim + """"""" + "," + amt + com + comto + """], "id":1}"""
    httpPost(postURL, params) match {
      case Some(r) => (new Extractor[StringDefault]).dolt(parse(r))
      case None => new StringDefault(None,Some(new Error(-10000,"Server contact error")),1)
    }
  }
  
  def getBalance(account: Option[String], minconf: Option[Integer]) = {
    val min = minconf.getOrElse(1)
    val acc = account match {
      case Some(a) => """"""" + a.trim + """""""
      case _ => """"*""""
    }
    
    val params = """{"method":"getbalance", "params":[""" + acc + "," + min + """], "id":1}"""
    httpPost(postURL, params) match {
      case Some(r) => (new Extractor[BigDecimalDefault]).dolt(parse(r))
      case None => new BigDecimalDefault(None,Some(new Error(-10000,"Server contact error")),1)
    }
  }
  
  def walletPassPhrase(passphrase: String, timeout: Integer) = {
    val params = """{"method":"walletpassphrase", "params":["""" + passphrase + """",""" + timeout + """], "id":1}"""
    println("unlocking wallet ...")
    httpPost(postURL, params) match {
      case Some(r) => println("much unlock success"); (new Extractor[DefaultResult]).dolt(parse(r))
      case None => println("much unlock fail"); new DefaultResult(None,Some(new Error(-10000,"Server contact error")),1)
    }
  }
  
  def walletLock() = {
    val params = """{"method":"walletlock", "params":[], "id":1}"""
    println("locking wallet")
    httpPost(postURL, params) match {
      case Some(r) => println("much lock success"); (new Extractor[DefaultResult]).dolt(parse(r))
      case None => println("much lock fail"); new DefaultResult(None,Some(new Error(-10000,"Server contact error")),1)
    }
  }
  
  /**
   * opens a connection and posts data to URL
   * set up using https
   */
  private def httpPost(urlS: String, postdata: String): Option[String] = {
    var input: Option[InputStream] = None
    val urlI = if(secure) "https://" + urlS else "http://" + urlS
    
    try {
      Authenticator.setDefault(new BasicAuthenticator(rpcuser, rpcpassword))
      
      val sslctx: SSLContext = SSLContext.getInstance("SSL")
      sslctx.init(null, Array[TrustManager](new MyTrustManager()), null)
      HttpsURLConnection.setDefaultSSLSocketFactory(sslctx.getSocketFactory())
      
      val url: URL = new URL(urlI)
      val connection = if(!secure) url.openConnection().asInstanceOf[HttpURLConnection] else url.openConnection().asInstanceOf[HttpsURLConnection]
      
      if(secure) connection.asInstanceOf[HttpsURLConnection].setHostnameVerifier(new MyHostnameVerifier())
      connection.setDoOutput(true)
      connection.setDoInput(true)
      
      val out = connection.getOutputStream()
      out.write(postdata.getBytes())
      out.flush()
      out.close
        
        println(connection.getResponseCode() + ": ")
      if(connection.getResponseCode() == 200) {
      
        val in = connection.getInputStream()
      
        val str = Source.fromInputStream(in).foldLeft("")( (ch, ex) => {
        	ch + ex
      	})
      
      	in.close
      
      	Some(str)
      } else {
      //  println("[ERROR]: " + connection.getResponseMessage())
        None
      }
    } catch {
      case e: IOException => None
    }
  }
}

/**
 * A helper that will JSON serialize BigDecimal
 */
object BigDecimalSerializer extends Serializer[BigDecimal] {
  private val Class = classOf[BigDecimal]
  
  def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), BigDecimal] = {
    case (TypeInfo(Class, _), json) => json match {
      case JInt(iv) => BigDecimal(iv)
      case JDouble(dv) => BigDecimal(dv)
      case value => throw new MappingException("Can't convert " + value + " to " + Class)
    }
  }
  
  def serialize(implicit format: Formats): PartialFunction[Any, JValue] = {
    case d: BigDecimal => JDouble(d.doubleValue)
  }
}