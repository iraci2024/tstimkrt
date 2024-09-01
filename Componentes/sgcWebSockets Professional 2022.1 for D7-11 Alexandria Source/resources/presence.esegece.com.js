/*!
**************************************************************************
 sgcWebSocket component

 written by eSeGeCe
            copyright © 2021
            Email : info@esegece.com
            Web : http://www.esegece.com
**************************************************************************
*/

function sgcws_presence() {
  if (arguments.length == 0) return;
  
  if (arguments.length == 1) {
    if (typeof arguments[0] == "object") {
      arguments[0]["subprotocol"] = "presence.esegece.com";
	  sgcWebSocket.call(this, arguments[0]);
    } else if (typeof arguments[0] == "string") { 
      arguments[1] = "presence.esegece.com";
	  sgcWebSocket.call(this, arguments[0], arguments[1]);
    }
  } else if (arguments.length == 2) {
    if (typeof arguments[0] == "object") {
      arguments[0]["subprotocol"] = arguments[1] + "presence.esegece.com";
	  sgcWebSocket.call(this, arguments[0]);
    } else if (typeof arguments[0] == "string") { 
      arguments[1] = arguments[1] + "presence.esegece.com";
	  sgcWebSocket.call(this, arguments[0], arguments[1]);
    }  
  } else if (arguments.length == 3) {
    if (typeof arguments[0] == "object") {
      arguments[0]["subprotocol"] = arguments[1] + "presence.esegece.com";
	  arguments[0]["transport"] = arguments[2];
	  sgcWebSocket.call(this, arguments[0]);
    } else if (typeof arguments[0] == "string") { 
      arguments[1] = arguments[1] + "presence.esegece.com";
	  sgcWebSocket.call(this, arguments[0], arguments[1], arguments[2]);
    }  
  }
   
  var member = new Object();
  member.id = "";
  member.name = "";
  member.info = "";  
  
  var channel = new Object();
  channel.name = "";
  
  var message = new Object();
  message.text = "";  
  
  var error = new Object();
  error.text = "";

  var send_override = this.send;
  this.send = function(method, member, channel, message, id) {
    arguments[0] = '{"jsonrpc":"2.0", "method":"' + sgcEncodeB64(method) + '"';
	if (member !== undefined) {
	  arguments[0] = arguments[0] + ', "member":{"id":"' + sgcEncodeB64(member.id) + '","name":"' +  sgcEncodeB64(member.name) + '","info":"' + sgcEncodeB64(member.info) + '"}';
	}
	if (channel !== undefined) {
	  arguments[0] = arguments[0] + ', "channel":{"name":"' + sgcEncodeB64(channel.name) + '"}';
	}	
	if (message !== undefined) {
	  arguments[0] = arguments[0] + ', "message":{"text":"' + sgcEncodeB64(message.text) + '"}';
	}	
	arguments[0] = arguments[0] + '}';
    send_override.apply(this, arguments);
  }  

  
  this.newmember = function(member_id, member_name, member_info) {    
    member.id = member_id;
    member.name = member_name;
	if (member_info !== undefined) {
	  member.info = member_info;
	} else {
	  member.info = '';
	}	
    this.send('sgc@presence@member@new', member);
  }
  
  this.removemember = function() {    
    this.send('sgc@presence@member@remove', member);
  }  
   
  this.publish = function(message_text, channel_name) {
    channel.name = "";
	if (channel_name !== undefined) {
      channel.name = channel_name;
	}
	message.text = "";
	if (message_text !== undefined) {
      message.text = message_text;
	}	
    this.send('sgc@presence@publish@msg', member, channel, message);
  }  
   
  this.subscribe = function(channel_name) {
    channel.name = channel_name;
    this.send('sgc@presence@channel@member@new', member, channel);
  }
  
  this.unsubscribe = function(channel_name) {
    channel.name = channel_name;
    this.send('sgc@presence@channel@member@remove', member, channel);
  }
  
  this.getmembers = function(channel_name) {
    channel.name = "";
	if (channel_name !== undefined) {
      channel.name = channel_name;
	}    
    this.send('sgc@presence@get@members', member, channel);
  }    
  
  var evt_onsgcsession = new event("onsgcsession");
  var evt_onsgcnewmember = new event("onsgcnewmember");   
  var evt_onsgcremovemember = new event("onsgcremovemember");     
  var evt_onsgcnewchannelmember = new event("onsgcnewchannelmember");   
  var evt_onsgcremovechannelmember = new event("onsgcremovechannelmember");     
  var evt_onsgcpublishmsg = new event("onsgcpublishmsg");
  var evt_onsgcgetmembers = new event("onsgcgetmembers");
  var evt_onsgcerrormemberchannel = new event("onsgcerrormemberchannel");
  var evt_onsgcerrorpublishmsg = new event("onsgcerrorpublishmsg");
  
    
  function sgcDecodeJSON(data) {
    for(var i in data) {
      if(typeof data[i] == 'object') sgcDecodeJSON(data[i]);
        if(data[i].length > 0) data[i] = sgcDecodeB64(data[i]);
    }
    return data
  }	
	
  DoEventMessage = function(evt) { 
    var json = evt.data;
    obj = sgcDecodeJSON(JSON.parse(json));
	if (obj.method !== undefined){
  	  if (obj.method == "sgc@presence@session") {
        evt_onsgcsession.fire(
		{
          name: 'onsgcsession',
          id: obj.message['text']		  
	    }
	    )
  	  } else if (obj.method == "sgc@presence@member@new") {
	    evt_onsgcnewmember.fire(
	    {
	      name: 'onsgcnewmember',
		  member: obj.member
	    }
	    )
  	  } else if (obj.method == "sgc@presence@member@remove") {
        evt_onsgcremovemember.fire(
		{
	      name: 'onsgcremovemember',
		  member: obj.member		  
	    }
	    )		
  	  } else if (obj.method == "sgc@presence@channel@member@new") {
        evt_onsgcnewchannelmember.fire(
		{
          name: 'onsgcnewchannelmember',
          member: obj.member,
		  channel: obj.channel
	    }
	    )	
  	  } else if (obj.method == "sgc@presence@channel@member@remove") {
        evt_onsgcremovechannelmember.fire(
		{
          name: 'onsgcremovechannelmember',
          member: obj.member,
		  channel: obj.channel
	    }
	    )
  	  } else if (obj.method == "sgc@presence@publish@msg") {
		evt_onsgcpublishmsg.fire(
	    {
	      name: 'onsgcpublishmsg',
          member: obj.member,
		  channel: obj.channel,
		  message: obj.message
	    }
	    )  
  	  } else if (obj.method == "sgc@presence@send@members") {
		evt_onsgcgetmembers.fire(
	    {
	      name: 'onsgcgetmembers',
          members: obj.members,
		  member: obj.member,
		  channel: obj.channel
	    }
	    )  
  	  } else if (obj.method == "sgc@presence@error@member@channel") {
		evt_onsgcerrormemberchannel.fire(
	    {
	      name: 'onsgcgetmembers',
		  error: obj.error,
          member: obj.member,
		  channel: obj.channel
	    }
	    )  
  	  } else if (obj.method == "sgc@presence@error@publish@msg") {
		evt_onsgcerrorpublishmsg.fire(
	    {
	      name: 'onsgcerrorpublishmsg',
		  error: obj.error,
		  message: obj.message,
          member: obj.member,
		  channel: obj.channel
	    }
	    )  
	  }		  
	}
  }
  
   
  var on_override = this.on; 
  this.on = function (evtname, fn) {
  if (evtname == "sgcsession") {
    evt_onsgcsession.subscribe(fn);
  } else if (evtname == "sgcnewmember") {
    evt_onsgcnewmember.subscribe(fn);	
  } else if (evtname == "sgcremovemember") {
    evt_onsgcremovemember.subscribe(fn);		
  } else if (evtname == "sgcnewchannelmember") {
    evt_onsgcnewchannelmember.subscribe(fn);
  } else if (evtname == "sgcremovechannelmember") {
    evt_onsgcremovechannelmember.subscribe(fn);
  } else if (evtname == "sgcpublishmsg") {
    evt_onsgcpublishmsg.subscribe(fn);	
  } else if (evtname == "sgcgetmembers") {
    evt_onsgcgetmembers.subscribe(fn);		
  } else if (evtname == "sgcerrormemberchannel") {
    evt_onsgcerrormemberchannel.subscribe(fn);	
  } else if (evtname == "sgcerrorpublishmsg") {
    evt_onsgcerrorpublishmsg.subscribe(fn);		
  } else {
    on_override.apply(this, arguments); 
  };
  };
 
  
}

sgcws_presence.prototype = new sgcWebSocket();
sgcws_presence.prototype.constructor = sgcws_presence;
