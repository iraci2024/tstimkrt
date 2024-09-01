/*!
**************************************************************************
 sgcWebSocket component

 written by eSeGeCe
            copyright © 2021
            Email : info@esegece.com
            Web : http://www.esegece.com
**************************************************************************
*/

function sgcws_dataset() {
  if (arguments.length == 0) return;
  
  if (arguments.length == 1) {
    if (typeof arguments[0] == "object") {
      arguments[0]["subprotocol"] = "dataset.esegece.com";
	  sgcWebSocket.call(this, arguments[0]);
    } else if (typeof arguments[0] == "string") { 
      arguments[1] = "dataset.esegece.com";
	  sgcWebSocket.call(this, arguments[0], arguments[1]);
    }
  } else if (arguments.length == 2) {
    if (typeof arguments[0] == "object") {
      arguments[0]["subprotocol"] = arguments[1] + ".dataset.esegece.com";
	  sgcWebSocket.call(this, arguments[0]);
    } else if (typeof arguments[0] == "string") { 
      arguments[1] = arguments[1] + ".dataset.esegece.com";
	  sgcWebSocket.call(this, arguments[0], arguments[1]);
    }  
  } else if (arguments.length == 3) {
    if (typeof arguments[0] == "object") {
      arguments[0]["subprotocol"] = arguments[1] + ".dataset.esegece.com";
	  arguments[0]["transport"] = arguments[2];
	  sgcWebSocket.call(this, arguments[0]);
    } else if (typeof arguments[0] == "string") { 
      arguments[1] = arguments[1] + ".dataset.esegece.com";
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
  
  this.dataset = function (method, dataset) {
	arguments[0] = '{"jsonrpc":"2.0", "result":{"method":"sgc@dataset","channel":"sgc@dataset@' + method + '","dataset":' + dataset + '}}'
	send_override.apply(this, arguments);
  }
  
  this.dataset_new = function (dataset) {
	this.dataset('new', dataset);
  } 
  
  this.dataset_update = function (dataset) {
	this.dataset('update', dataset);
  }   
  
  this.dataset_delete = function (dataset) {
	this.dataset('delete', dataset);
  }  
  
  this.request = function(method) {
	arguments[0] = '{"jsonrpc":"2.0", "method":"' + method + '"}';
	send_override.apply(this, arguments);
  }
  
  this.synchronize = function() {
    this.request('sgc@dataset@req@records');
  }

  this.getmetadata = function() {
	this.request('sgc@dataset@req@metadata');
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
  
  this.publish = function() {
    this.send(arguments[0], 'sgc@publish', arguments[1], GUID());
  }    
  
  this.rpc = function(id, method, params) {
    arguments[0] = '{"jsonrpc":"2.0", "method":"' + method + '", "params":' + params + ', "id":"' + id + '"}';
    send_override.apply(this, arguments); 
  }
  
  this.notify = function(method, params) {
    arguments[0] = '{"jsonrpc":"2.0", "method":"' + method + '", "params":' + params + '"}';
    send_override.apply(this, arguments); 
  }  
 
  this.subscribe = function(channel) {
    this.send('', 'sgc@subscribe', channel, GUID());
  }
  
  this.unsubscribe = function(channel) {
    this.send('', 'sgc@unsubscribe', channel, GUID());
  }    
 
  this.subscribe_all = function() {
	this.subscribe("sgc@dataset@new");
	this.subscribe("sgc@dataset@update");
	this.subscribe("sgc@dataset@delete");
  }  
   
  this.unsubscribe_all = function() {
	this.unsubscribe("sgc@dataset@new");
	this.unsubscribe("sgc@dataset@update");
	this.unsubscribe("sgc@dataset@delete");
  }   
  
  this.pubrel = function(id) {
    this.send('', 'sgc@pubrel', '', id);
  }  
   
   
  var evt_onsgcmessage = new event("onsgcmessage");   
  var evt_onsgcsubscribe = new event("onsgcsubscribe");
  var evt_onsgcunsubscribe = new event("onsgcunsubscribe");   
  var evt_onsgcdataset = new event("onsgcdataset"); 
  var evt_onsgcevent = new event("onsgcevent");   
  var evt_onsgcrpcresult = new event("onsgcrpcresult");
  var evt_onsgcrpcerror = new event("onsgcrpcerror");
  var evt_onsgcacknowledgment = new event("onsgcacknowledgment");
  var evt_onsgcpubrec = new event("onsgcpubrec");  
  var evt_onsgcsession = new event("onsgcsession");  
  var evt_onsgcmetadata = new event("onsgcmetadata");
  var evt_onsgcbeforesynchronize = new event("onsgcbeforesynchronize");
  var evt_onsgcaftersynchronize = new event("onsgcaftersynchronize");
  
  
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
  	  } else if (obj.result['method'] == "sgc@dataset") {
	    evt_onsgcdataset.fire(
	    {
	      name: 'onsgcdataset',
		  channel: obj.result['channel'],
          dataset: obj.result['dataset']		  
	    }
	    )		
  	  } else if (obj.result['method'] == "sgc@metadata") {
	    evt_onsgcmetadata.fire(
	    {
	      name: 'onsgcmetadata',
          metadata: obj.result['metadata']		  
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
          id: obj.result['id']		  
	    }
	    )		
	  } else if (obj.result['method'] == "sgc@dataset@req@records@start") {
        evt_onsgcbeforesynchronize.fire(
		{
          name: 'onsgcbeforesynchronize',
          dataset_name: obj.result['dataset_name'],
		  dataset_recordcount: obj.result['dataset_recordcount']		  		  
	    }
	    )		
	  } else if (obj.result['method'] == "sgc@dataset@req@records@end") {
        evt_onsgcaftersynchronize.fire(
		{
          name: 'onsgcaftersynchronize',
          dataset_name: obj.result['dataset_name'],
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
    if (evtname == "sgcdataset") {
      evt_onsgcdataset.subscribe(fn);
    } else if (evtname == "sgcmessage") {
      evt_onsgcmessage.subscribe(fn);
    } else if (evtname == "sgcevent") {
	  evt_onsgcevent.subscribe(fn);	
	} else if (evtname == "sgcsession") {
      evt_onsgcsession.subscribe(fn);		  
    } else if (evtname == "sgcsubscribe") {
      evt_onsgcsubscribe.subscribe(fn);
    } else if (evtname == "sgcunsubscribe ") {
      evt_onsgcunsubscribe.subscribe(fn);
    } else if (evtname == "sgcacknowledgment") {
	  evt_onsgcacknowledgment.subscribe(fn);	
	} else if (evtname == "sgcpubrec") {
	  evt_onsgcpubrec.subscribe(fn);	
    } else if (evtname == "sgcbeforesynchronize") {
	  evt_onsgcbeforesynchronize.subscribe(fn);			  
    } else if (evtname == "sgcaftersynchronize") {
	  evt_onsgcaftersynchronize.subscribe(fn);			  	  
    } else if (evtname == "sgcrpcresult") {
      evt_onsgcrpcresult.subscribe(fn);		
    } else if (evtname == "sgcrpcerror") {
      evt_onsgcrpcerror.subscribe(fn);		  
    } else if (evtname == "sgcmetadata") {
      evt_onsgcmetadata.subscribe(fn);		  	  
    } else {
      on_override.apply(this, arguments); 
  };
  };  

}

sgcws_dataset.prototype = new sgcWebSocket();
sgcws_dataset.prototype.constructor = sgcws_dataset;

