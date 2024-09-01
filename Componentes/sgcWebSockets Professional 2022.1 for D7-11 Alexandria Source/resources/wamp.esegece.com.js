/*!
**************************************************************************
 sgcWebSocket component

 written by eSeGeCe
            copyright © 2021
            Email : info@esegece.com
            Web : http://www.esegece.com
**************************************************************************
*/

 
function sgcws_wamp() {
  if (arguments.length == 0) return;
  
  if (arguments.length == 1) {
    if (typeof arguments[0] == "object") {
      arguments[0]["subprotocol"] = "WAMP";
	  sgcWebSocket.call(this, arguments[0]);
    } else if (typeof arguments[0] == "string") { 
      arguments[1] = "WAMP";
	  sgcWebSocket.call(this, arguments[0], arguments[1]);
    }
  } else if (arguments.length == 2) {
    if (typeof arguments[0] == "object") {
      arguments[0]["subprotocol"] = arguments[1] + ".WAMP";
	  sgcWebSocket.call(this, arguments[0]);
    } else if (typeof arguments[0] == "string") { 
      arguments[1] = arguments[1] + ".WAMP";
	  sgcWebSocket.call(this, arguments[0], arguments[1]);
    }  
  } else if (arguments.length == 3) {
    if (typeof arguments[0] == "object") {
      arguments[0]["subprotocol"] = arguments[1] + ".WAMP";
	  arguments[0]["transport"] = arguments[2];
	  sgcWebSocket.call(this, arguments[0]);
    } else if (typeof arguments[0] == "string") { 
      arguments[1] = arguments[1] + ".WAMP";
	  sgcWebSocket.call(this, arguments[0], arguments[1], arguments[2]);
    }  
  }
  
  var send_override = this.send;
  this.prefix = function(prefix, URI) {
    arguments[0] = JSON.stringify([1, prefix, URI]);
    send_override.apply(this, arguments);
  }
  this.call = function(callID, procURI, params) {
    if (callID == "") {
	  callID = GUID();
	}
    arguments[0] = JSON.stringify([2, callID, procURI, params]);
    send_override.apply(this, arguments);
  }   
  this.cancelcall = function(callID) {
    if (callID == "") {
	  callID = GUID();
	}
    arguments[0] = JSON.stringify([1001, callID]);
    send_override.apply(this, arguments);
  }   
  this.subscribe = function(topicURI) {
    arguments[0] = JSON.stringify([5, topicURI]);
    send_override.apply(this, arguments);	
  }
  this.unsubscribe = function(topicURI) {
    arguments[0] = JSON.stringify([6, topicURI]);
    send_override.apply(this, arguments);	
  }
  this.publish = function(topicURI, event, excludeMe, eligible) {
    arguments[0] = JSON.stringify([7, topicURI, event, excludeMe, eligible]);
    send_override.apply(this, arguments);    
  }
  
  
  var evt_onsgcmessage = new event("onsgcmessage");     

  var evt_onwampwelcome = new event("onwampwelcome");   
  var evt_onwampcallresult = new event("onwampcallresult");
  var evt_onwampcallprogressresult = new event("onwampcallprogressresult");
  var evt_onwampcallerror = new event("onwampcallerror");
  var evt_onwampevent = new event("onwampevent");
   
  DoEventMessage = function(evt) { 
    var json = evt.data; 
    obj = JSON.parse(json);
    if (obj[0] == 0) {
      evt_onwampwelcome.fire(
      {
        name: 'onwampwelcome',
        SessionId: obj[1],
        ProtocolVersion: obj[2],
		ServerIdent: obj[3]
      }
	  )
	} else if (obj[0] == 3) {
	  evt_onwampcallresult.fire(
	  {
	    name: 'onwampcallresult',
		CallId: obj[1],
		CallResult: obj[2]
	  }
	  )
	} else if (obj[0] == 1000) {
	  evt_onwampcallprogressresult.fire(
	  {
	    name: 'onwampcallprogressresult',
		CallId: obj[1],
		CallResult: obj[2]
	  }
	  )	  
	} else if (obj[0] == 4) {
	  evt_onwampcallerror.fire(
	  { 
		name: 'onwampcallerror',
		CallId: obj[1],
		ErrorURI: obj[2],
		ErrorDesc: obj[3],
		ErrorDetails: obj[4]
	  }
	  )
	} else if (obj[0] == 8) {
	  evt_onwampevent.fire(
	  { 
		name: 'onwampevent',
		TopicURI: obj[1],
		Event: obj[2]
	  }
	  )
    } else {
	  evt_onsgcmessage.fire(
	  {
	    name: 'onsgmessage',
		message: event['message']
	  }
	  );
	}
  }
  
  
  var on_override = this.on; 
  this.on = function (evtname, fn) {
  if (evtname == "sgcmessage") {
    evt_onsgcmessage.subscribe(fn);
  } else if (evtname == "wampwelcome") {
    evt_onwampwelcome.subscribe(fn);
  } else if (evtname == "wampcallresult") {
    evt_onwampcallresult.subscribe(fn);
  } else if (evtname == "wampcallprogressresult") {
    evt_onwampcallprogressresult.subscribe(fn);	
  } else if (evtname == "wampcallerror") {
    evt_onwampcallerror.subscribe(fn);
  } else if (evtname == "wampevent") {
    evt_onwampevent.subscribe(fn);	
  } else {
    on_override.apply(this, arguments); 
  };
  };
  
}

sgcws_wamp.prototype = new sgcWebSocket();
sgcws_wamp.prototype.constructor = sgcws_wamp;

