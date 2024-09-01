/*!
**************************************************************************
 sgcWebSocket component

 written by eSeGeCe
            copyright © 2021
            Email : info@esegece.com
            Web : http://www.esegece.com
**************************************************************************
*/

function sgcws() {
  if (arguments.length == 0) return;
  
  if (arguments.length == 1) {
    if (typeof arguments[0] == "object") {
      arguments[0]["subprotocol"] = "esegece.com";
	  sgcWebSocket.call(this, arguments[0]);
    } else if (typeof arguments[0] == "string") { 
      arguments[1] = "esegece.com";
	  sgcWebSocket.call(this, arguments[0], arguments[1]);
    }
  } else if (arguments.length == 2) {
    if (typeof arguments[0] == "object") {
      arguments[0]["subprotocol"] = arguments[1] + ".esegece.com";
	  sgcWebSocket.call(this, arguments[0]);
    } else if (typeof arguments[0] == "string") { 
      arguments[1] = arguments[1] + ".esegece.com";
	  sgcWebSocket.call(this, arguments[0], arguments[1]);
    }  
  } else if (arguments.length == 3) {
    if (typeof arguments[0] == "object") {
      arguments[0]["subprotocol"] = arguments[1] + ".esegece.com";
	  arguments[0]["transport"] = arguments[2];
	  sgcWebSocket.call(this, arguments[0]);
    } else if (typeof arguments[0] == "string") { 
      arguments[1] = arguments[1] + ".esegece.com";
	  sgcWebSocket.call(this, arguments[0], arguments[1], arguments[2]);
    }  
  }

  var QoS = '';
  this.qoslevel0 = function () {
    QoS = 'qosLevel0';
  }
  this.qoslevel1 = function () {
    QoS = 'qosLevel1';
  }
  this.qoslevel2 = function () {
    QoS = 'qosLevel2';
  }
  
  var Queue = '';
  this.queuelevel0 = function () {
    Queue = 'queueLevel0';
  }
  this.queuelevel1 = function () {
    Queue = 'queueLevel1';
  }
  this.queuelevel2 = function () {
    Queue = 'queueLevel2';
  }  

  
  var send_override = this.send;
  this.send = function(message, method, channel, id) {
    _qos = '';
    if ((QoS !== '') && (QoS !== undefined)) {
	  _qos = ', "qos":"' + QoS + '"';
	}
    _queue = '';
    if ((Queue !== '') && (Queue !== undefined) && (method == 'sgc@publish')) {
	  _queue = ', "queue":"' + Queue + '"';	  
	}
	_message = message
	if ((_message.substr(0, 1) !== "{") && (_message.substr(0, 1) !== "[") && (_message.substr(0, 1) !== '"')) {
	  _message = '"' + _message + '"';
	}
    if (arguments.length == 1) {	
	  arguments[0] = '{"jsonrpc":"2.0", "method":"sgc@message", "params":{"message":' + _message + _qos + _queue + '}, "id":"' + GUID() + '"}'
	} else if (arguments.length == 2) {
	  arguments[0] = '{"jsonrpc":"2.0", "method":"' + method + '", "params":{"message":' + _message + _qos + _queue + '}, "id":"' + GUID() + '"}'	
	} else if ((id !== '') && (id !== undefined)) {
	  arguments[0] = '{"jsonrpc":"2.0", "method":"' + method + '", "params":{"channel":"' + channel + '", "message":' + _message + _qos + _queue + '}, "id":"' + id + '"}'
	} else {
	  arguments[0] = '{"jsonrpc":"2.0", "method":"' + method + '", "params":{"channel":"' + channel + '", "message":' + _message + '}}'
	};
    send_override.apply(this, arguments);
  }  

  
  
  
  this.broadcast = function() {
	if (arguments.length == 1) {
      this.send(arguments[0], 'sgc@broadcast', '');
	} else if (arguments.length == 2) {
	  this.send(arguments[0], 'sgc@broadcast', arguments[1]);
	}
  }
  
  this.publish = function() {
    this.send(arguments[0], 'sgc@publish', arguments[1], GUID());
  }  
   
  this.subscribe = function(channel) {
    this.send('', 'sgc@subscribe', channel, GUID());
  }
  
  this.unsubscribe = function(channel) {
    this.send('', 'sgc@unsubscribe', channel, GUID());
  }
  
  this.rpc = function(id, method, params) {
    arguments[0] = '{"jsonrpc":"2.0", "method":"' + method + '", "params":"' + params + '", "id":"' + id + '"}';
    send_override.apply(this, arguments); 
  }
  
  this.notify = function(method) {
    arguments[0] = '{"jsonrpc":"2.0", "method":"' + method + '", "params":""}';
    send_override.apply(this, arguments); 
  }  
  
  this.request = function(method) {
	arguments[0] = '{"jsonrpc":"2.0", "method":"' + method + '"}';
	send_override.apply(this, arguments);
  }  
  
  this.getsession = function() {
    this.request('sgc@session');  
  }   
  
  this.starttransaction = function(channel){
    if (channel == undefined) {
		channel = "";
	}
    this.send('', 'sgc@transaction', channel, '');	
  }
  
  this.commit = function(channel){
    if (channel == undefined) {
		channel = "";
	}  
    this.send('', 'sgc@commit', channel, '');	
  }

  this.rollback = function(channel){
    if (channel == undefined) {
		channel = "";
	}  
    this.send('', 'sgc@rollback', channel, '');	
  }
  
  this.pubrel = function(id) {
    this.send('', 'sgc@pubrel', '', id);
  }
  
  
  var evt_onsgcmessage = new event("onsgcmessage");   
  var evt_onsgcevent = new event("onsgcevent");   
  var evt_onsgcsubscribe = new event("onsgcsubscribe");
  var evt_onsgcunsubscribe = new event("onsgcunsubscribe");
  var evt_onsgcrpcresult = new event("onsgcrpcresult");
  var evt_onsgcrpcerror = new event("onsgcrpcerror");
  var evt_onsgcacknowledgment = new event("onsgcacknowledgment");
  var evt_onsgcpubrec = new event("onsgcpubrec");  
  var evt_onsgcsession = new event("onsgcsession");
  
   
  DoEventMessage = function(evt) { 
    var json = evt.data; 
    obj = JSON.parse(json);
	if (obj.result !== undefined){
      if (obj.result['method'] == "sgc@subscribe") {
        evt_onsgcsubscribe.fire(
        {
          name: 'onsgcsubscribe',
          channel: obj.result['channel']
        }
	    )
  	  } else if (obj.result['method'] == "sgc@unsubscribe") {
	    evt_onsgcunsubscribe.fire(
	    {
	      name: 'onsgcunsubscribe',
		  channel: obj.result['channel']
	    }
	    )
  	  } else if (obj.result['method'] == "sgc@pubrec") {
        evt_onsgcpubrec.fire(
		{
          name: 'onsgcpubrec',
          id: obj.id		  
	    }
	    )
  	  } else if (obj.result['method'] == "sgc@event") {
        evt_onsgcevent.fire(
		{
          name: 'onsgcevent',
	      channel: obj.result['channel'],
          message: obj.result['message']			  
	    }
	    )		
  	  } else if (obj.result['method'] == "sgc@session") {
        evt_onsgcsession.fire(
		{
          name: 'onsgcsession',
          guid: obj.result['message']			  
	    }
	    )		
  	  } else if (obj.result['method'] == "sgc@message") {
        evt_onsgcmessage.fire(
		{
          name: 'onsgcmessage',
          message: obj.result['message']		  
	    }
	    )	
	  } else if (obj.result['method'] == "sgc@acknowledgment") {
        evt_onsgcacknowledgment.fire(
		{
          name: 'onsgcacknowledgment',
          id: obj.id
	    }
	    )
	  } else {
	  evt_onsgcrpcresult.fire(
	    {
	      name: 'onsgcrpcresult',
		  result: obj.result,
		  id: obj.id
	    }
	    )  
	  }
	} else if (obj.error !== undefined){
	  evt_onsgcrpcerror.fire(
	  {
	    name: 'onsgcrpcerror',
		code: obj.error["code"],
		message: obj.error["message"],
		data: obj.error["data"]
	  }
	  )
	}
  }
  
  
  var on_override = this.on; 
  this.on = function (evtname, fn) {
  if (evtname == "sgcmessage") {
    evt_onsgcmessage.subscribe(fn);
  } else if (evtname == "sgcevent") {
    evt_onsgcevent.subscribe(fn);	
  } else if (evtname == "sgcsession") {
    evt_onsgcsession.subscribe(fn);		
  } else if (evtname == "sgcsubscribe") {
    evt_onsgcsubscribe.subscribe(fn);
  } else if (evtname == "sgcunsubscribe") {
    evt_onsgcunsubscribe.subscribe(fn);
  } else if (evtname == "sgcacknowledgment") {
    evt_onsgcacknowledgment.subscribe(fn);	
  } else if (evtname == "sgcpubrec") {
    evt_onsgcpubrec.subscribe(fn);	
  } else if (evtname == "sgcrpcresult") {
    evt_onsgcrpcresult.subscribe(fn);		
  } else if (evtname == "sgcrpcerror") {
    evt_onsgcrpcerror.subscribe(fn);	
  } else {
    on_override.apply(this, arguments); 
  };
  };
 
  
}

sgcws.prototype = new sgcWebSocket();
sgcws.prototype.constructor = sgcws;


