/*!
**************************************************************************
 sgcWebSocket component

 written by eSeGeCe
            copyright © 2021
            Email : info@esegece.com
            Web : http://www.esegece.com
**************************************************************************
*/

function GUID ()
{
    var S4 = function ()
    {
        return Math.floor(
                Math.random() * 0x10000
            ).toString(16);
    };

    return (
            S4() + S4() + 
            S4() + 
            S4() + 
            S4() + 
            S4() + S4() + S4()
        );
}

function event(name) {
  this.name = name;
  //function to call on event fire
  this.eventAction = null;
  //subscribe a function to the event
  this.subscribe = function(fn) {
    this.eventAction = fn;
  };
  //fire the event
  this.fire = function(sender, eventArgs) {
    if(this.eventAction != null) {
      this.eventAction(sender, eventArgs);
    };
  };
}

function sgcStreamToString(stream, callback) {
  var reader = new FileReader(); 
  reader.readAsText(stream);          
  reader.onload = function() { 
	vResult = reader.result; 
	callback(vResult);
  }	
}

function sgcWSStreamRead(stream, callback) {
  var vStreamSize = stream.slice(0, 10);
  sgcStreamToString(vStreamSize, function(value){
      var vSize = parseInt(value);
      var vStreamHeader = stream.slice(10, 10 + vSize);
      sgcStreamToString(vStreamHeader, function(value) {
	    var vHeader = value;
		var vStream = stream.slice(10 + vSize, stream.size);
		callback(vHeader, vStream);
	  });
  });     
}

var sgcEncodeBase64 = false;
function sgcEncodeB64(text){
	if (text.length > 0)
	{
		if (sgcEncodeBase64 == true)
		{
			return "sgc@b64:" + btoa(text);
		}
		else
		{
			return text;
		}
	}
	else
	{
		return "";
	}
}

function sgcDecodeB64(text){
	if (((typeof arguments[0] == "object") == false) && (text.substring(0, 8) == "sgc@b64:"))
	{
		return atob(text.substring(8, text.length));
	}
	else
	{
		return text;
	}
}


function sgcWebSocket() {  

  if (arguments.length == 0) return;
    
  // ... decode constructor arguments	
  if (typeof arguments[0] == "object") {
    this.host = arguments[0]["host"]; 
	this.subprotocol = arguments[0]["subprotocol"];
    this.user = arguments[0]["user"]; 
	this.password = arguments[0]["password"];	
    this.transport = arguments[0]["transport"];	
  } else if (typeof arguments[0] == "string") { 
    this.host = arguments[0];
    this.subprotocol = arguments[1];
	this.transport = arguments[2];
  } else {
	return;
  }
  
  if (this.host == undefined) {
    this.host = "127.0.0.1";
  }
  if (this.subprotocol == undefined) {
    this.subprotocol = "";
  }
  if (this.transport == undefined) {
    this.transport = ["websocket", "sse"];
  }
  
  var evt_onopen = new event("onopen");
  var evt_onclose = new event("onclose");
  var evt_onmessage = new event("onmessage");
  var evt_onerror = new event("onerror");  
  
  DoEventBinary = function(evt)
  {
	  evt_onstream.fire(
	  { 
		name: 'onstream',
		stream: evt.data
	  });			
  }
  
  DoEventMessage = function(evt)
  {	
	  evt_onmessage.fire(
	  { 
		name: 'onmessage',
		message: evt.data
	  });  
  }
  
  // ... WebSocket  
  if ((window.WebSocket) && ((!this.transport.indexOf) || (this.transport.indexOf("websocket") > -1))) {  
	var evt_onstream = new event("onstream");

    this.open = function () {   
      if ((this.host !== "") && (this.user !== "") && (this.user !== undefined)) {
	    if (this.password == undefined) {
		  this.password == "";
		}		
		if ((this.subprotocol !== "") && (this.subprotocol !== undefined)) {
		  this.websocket = new WebSocket(this.host + "/sgc/auth/url/" + this.user + "/" + this.password, this.subprotocol);
		} else {
		  this.websocket = new WebSocket(this.host + "/sgc/auth/url/" + this.user + "/" + this.password);		
		}	    
	  } else if ((this.host !== "") && (this.subprotocol !== "") && (this.subprotocol !== undefined)) {
	    this.websocket = new WebSocket(this.host, this.subprotocol);
	  } else if (this.host !== "") {  
	    this.websocket = new WebSocket(this.host);
	  }
	  
	  this.websocket.onopen = function() { 
	    evt_onopen.fire(
		{ 
		  name: 'onopen',
		  message: ''
		});	   
	  }; 

	  this.websocket.onmessage = function(evt) { 
		if (typeof evt.data === "object") {
			DoEventBinary(evt);
		} else {
			DoEventMessage(evt);
		}
	  };	  
	  
	  this.websocket.onclose = function(evt) { 
	    evt_onclose.fire(
		{ 
		  name: 'onclose',
		  message: '',
		  code: evt.code,
		  reason: evt.reason,
		  clean: evt.wasClean
		});	   
	  }; 	

	  this.websocket.onerror = function(evt) { 
	    evt_onerror.fire(
		{ 
		  name: 'onerror',
		  message: evt.data
		});	   
	  };
    }     
	
    if (this.websocket == undefined) {
      this.open();
    };	
	
	this.send = function(message) {
	  this.websocket.send(message);
	}
	
	this.sendbin = function(buffer) {
	  this.websocket.send(buffer);
	}	
	
	this.close = function () {
	  this.websocket.close();     
	};
	
	this.state = function () {
		switch (this.websocket.readyState) {
		  case 0:
			return "connecting";
			break;
		  case 1:
			return "open";
			break;
		  case 2:
			return "closing";
			break;
		  case 3:
			return "closed";
			break;
		  default:
			return "undefined";
			break;
		}
	}	
	
	this.on = function (evtname, fn) {
	  if (evtname == "open") {
	    evt_onopen.subscribe(fn);
	  } 
	  else if (evtname == "close") {
	    evt_onclose.subscribe(fn);
	  }
	  else if (evtname == "message") {
	    evt_onmessage.subscribe(fn);
	  }	
	  else if (evtname == "stream") {
	    evt_onstream.subscribe(fn);
	  }		  
	  else if (evtname == "error") {
	    evt_onerror.subscribe(fn);
	  }	
	}
  
  // ... EventSource  
  } else if ((window.EventSource) && (this.transport.indexOf("sse") > -1)) {    
    
	var vGUID = "";
	
    this.open = function () {   
      if ((this.host !== "") && (this.user !== "") && (this.user !== undefined)) {
	    if (this.password == undefined) {
		  this.password == "";
		}		
		if ((this.subprotocol !== "") && (this.subprotocol !== undefined)) {
		  this.EventSource = new EventSource(this.host.replace (/^[a-z]{2,3}\:\/{2}[a-z,0-9,.]{1,}\:[0-9]{1,4}.(.*)/, '$1') + "/sgc/auth/url/" + this.user + "/" + this.password + "/" + this.subprotocol);
		} else {
		  this.EventSource = new EventSource(this.host.replace (/^[a-z]{2,3}\:\/{2}[a-z,0-9,.]{1,}\:[0-9]{1,4}.(.*)/, '$1') + "/sgc/auth/url/" + this.user + "/" + this.password);		
		}	    
	  } else if ((this.host !== "") && (this.subprotocol !== "") && (this.subprotocol !== undefined)) {
	    this.EventSource = new EventSource(this.subprotocol);
	  } else if (this.host !== "") {  
	    this.EventSource = new EventSource('/');
	  }
	  
	  this.EventSource.onopen = function() { 
	    evt_onopen.fire(
		{ 
		  name: 'onopen',
		  message: ''
		});	   
	  }; 

	  this.EventSource.onmessage = function(evt) { 
	    if (vGUID == "") {
		  vGUID = evt.data;
		} else {
		  DoEventMessage(evt);
		};		
	  };	  
	  

	  this.EventSource.onerror = function(evt) { 	
	    evt_onerror.fire(
		{ 
		  name: 'onerror',
		  message: evt.data
		});	   
	  };
    }     
	
    if (this.EventSource == undefined) {
      this.open();
    };	
	
	this.send = function(message) {
	  if (vGUID !== "") {
		if (window.XMLHttpRequest) {
		  xhr = new XMLHttpRequest();
		} else if (window.ActiveXObject) {  
		  xhr = new ActiveXObject("Microsoft.XMLHTTP");  
		} else {
		  return
		};
		
		xhr.open("POST", '/sgc/xhr/' + vGUID, true);
		xhr.send(message);
	  }
	}
	
	this.close = function () {
	  this.EventSource.close();     
	  
	  evt_onclose.fire(
		{ 
		  name: 'onclose',
		  message: '',
		  code: 1000,
		  reason: '',
		  clean: true
		});	  
	};
	
	this.state = function () {
		switch (this.EventSource.readyState) {
		  case 0:
			return "connecting";
			break;
		  case 1:
			return "open";
			break;
		  case 2:
			return "closed";
			break;
		  default:
			return "undefined";
			break;
		}
	}	
	
	this.on = function (evtname, fn) {
	  if (evtname == "open") {
	    evt_onopen.subscribe(fn);
	  } 
	  else if (evtname == "close") {
	    evt_onclose.subscribe(fn);
	  }
	  else if (evtname == "message") {
	    evt_onmessage.subscribe(fn);
	  }	  
	  else if (evtname == "stream") {
	    evt_onstream.suscribe(fn);
	  }
	  else if (evtname == "error") {
	    evt_onerror.subscribe(fn);
	  }	
	}    
  } else {
    alert("WebSockets not supported by your Browser.");     
  }
}
