/*!
**************************************************************************
 sgcWebSocket component

 written by eSeGeCe
            copyright © 2021
            Email : info@esegece.com
            Web : http://www.esegece.com
**************************************************************************
*/

function sgcws_webrtc() {
  var localvideo;
  var remoteVideo;
  var localStream;
  var remoteStream;
  var rtcpc;
  var rtc_channel;
  var ice_servers = {"iceServers": [{"url": "stun:stun.l.google.com:19302"}]};
    
  // Set up audio and video regardless of what devices are present.
  var mediaConstraints = {'mandatory': {
                            'OfferToReceiveAudio':true, 
                            'OfferToReceiveVideo':true }};
  var isVideoMuted = false;
  var isAudioMuted = false;
  
  self = this;
  
  if (arguments.length == 0) return;
   
  if (arguments.length == 1) {
    if (typeof arguments[0] == "object") {
      arguments[0]["subprotocol"] = "webrtc.esegece.com";
	  sgcWebSocket.call(this, arguments[0]);
    } else if (typeof arguments[0] == "string") { 
      arguments[1] = "webrtc.esegece.com";
	  sgcWebSocket.call(this, arguments[0], arguments[1]);
    }
  } else if (arguments.length == 2) {
    if (typeof arguments[0] == "object") {
      arguments[0]["subprotocol"] = arguments[1] + ".webrtc.esegece.com";
	  sgcWebSocket.call(this, arguments[0]);
    } else if (typeof arguments[0] == "string") { 
      arguments[1] = arguments[1] + ".webrtc.esegece.com";
	  sgcWebSocket.call(this, arguments[0], arguments[1]);
    }  
  } else if (arguments.length == 3) {
    if (typeof arguments[0] == "object") {
      arguments[0]["subprotocol"] = arguments[1] + ".webrtc.esegece.com";
	  arguments[0]["transport"] = arguments[2];
	  sgcWebSocket.call(this, arguments[0]);
    } else if (typeof arguments[0] == "string") { 
      arguments[1] = arguments[1] + ".webrtc.esegece.com";
	  sgcWebSocket.call(this, arguments[0], arguments[1], arguments[2]);
    }  
  }
  

  var send_override = this.send;
  this.send = function(message, method, channel, id) {
    if (arguments.length == 1) {
	  arguments[0] = '{"jsonrpc":"2.0", "method":"sgc@message", "params":{"message":"' + message + '"}, "id":"' + GUID() + '", "webrtc":""}'
	} else if (arguments.length == 2) {
	  arguments[0] = '{"jsonrpc":"2.0", "method":"' + method + '", "params":{"message":"' + message + '"}, "id":"' + GUID() + '", "webrtc":""}'	
	} else if ((id !== '') && (id !== undefined)) {
	  arguments[0] = '{"jsonrpc":"2.0", "method":"' + method + '", "params":{"channel":"' + channel + '", "message":"' + message + '"}, "id":"' + id + '", "webrtc":""}'
	} else {
	  arguments[0] = '{"jsonrpc":"2.0", "method":"' + method + '", "params":{"channel":"' + channel + '", "message":"' + message + '"}, "webrtc":""}'
	};
    send_override.apply(this, arguments);
  }   
  
  var close_override = this.close;
  this.close = function () {
	webrtc_send(rtc_channel, {type: 'bye'});
	close_override.apply(this, arguments);
  }
  
  function webrtc_send(channel, message) {
    var msgString = JSON.stringify(message);
    arguments[0] =  '{"jsonrpc":"2.0", "method":"sgc@webrtc", "params":{"channel":"' + channel + '", "message":""}, "webrtc":' + msgString + '}'
    send_override.apply(self, arguments);
  }
  
  this.broadcast = function() {
	if (arguments.length == 1) {
      this.send(arguments[0], 'sgc@broadcast', '');
	} else if (arguments.length == 2) {
	  this.send(arguments[0], 'sgc@broadcast', arguments[1]);
	}
  }
    
  function doStop() {
    isAudioMuted = false;
    isVideoMuted = false;
    rtcpc.close();
    rtcpc = null;
	
	onSessionClose();
  }	
	
  this.webrtc_connect = function(channel) {
    rtc_channel = channel;
	this.send('', 'sgc@subscribe', channel);
  }
  
  this.webrtc_disconnect = function(channel) {
    rtc_channel = channel;
    this.send('', 'sgc@unsubscribe', channel);
	doStop();
  }   
   
 
  function createPeerConnection() {   
    try {
	  RTCPeerConnection = window.RTCPeerConnection || window.mozRTCPeerConnection || 
                       window.webkitRTCPeerConnection || window.msRTCPeerConnection;
      rtcpc = new RTCPeerConnection(ice_servers);
      rtcpc.onicecandidate = onIceCandidate;
    } catch (e) {
      alert("Cannot create RTCPeerConnection object; WebRTC is not supported by this browser.");
        return;
    }

    rtcpc.onconnecting = onSessionConnecting;
    rtcpc.onopen = onSessionOpened;
    rtcpc.ontrack = onRemoteStreamAdded;
    rtcpc.onremovestream = onRemoteStreamRemoved;
  }
  
  function doCall() {
    rtcpc.createOffer(setLocalAndSendMessage, writeConsoleLog, mediaConstraints);
  }
 
  function writeConsoleLog(error)
  {
	console.log(error.name + ':' + error.message); 
  }

  function setLocalAndSendMessage(sessionDescription) {
    rtcpc.setLocalDescription(sessionDescription, function () {
	  webrtc_send(rtc_channel, sessionDescription);
	}, function(e) {
	  writeConsoleLog(e);
	});   
  }


  function processSignalingMessage(message) {
    var msg = JSON.parse(message);
    if (msg.type === 'offer') {
      rtcpc.setRemoteDescription(new RTCSessionDescription(msg), function () {
        rtcpc.createAnswer(setLocalAndSendMessage, writeConsoleLog, mediaConstraints);
	  }, function (e) {
	    console.error(e);
	  });
    } else if (msg.type === 'answer') {
      rtcpc.setRemoteDescription(new RTCSessionDescription(msg));
    } else if (msg.type === 'candidate') {
      var candidate = new RTCIceCandidate({sdpMLineIndex:msg.label,
                                         candidate:msg.candidate});
      rtcpc.addIceCandidate(candidate);
    } else if (msg.type === 'bye') {
      onRemoteHangup();
    }
  }


  function onIceCandidate(event) {
    if (event.candidate) {
      webrtc_send(rtc_channel, {type: 'candidate',
                   label: event.candidate.sdpMLineIndex,
                   id: event.candidate.sdpMid,
                   candidate: event.candidate.candidate});
    } else {
      // end of candidates
    }
  }

  function onSessionConnecting(message) {
	evt_onsgcsessionconnecting.fire(
	{
		name: 'onsgcsessionconnecting',
		message: message
	});
  }
  function onSessionOpened(message) {
	evt_onsgcsessionopen.fire(
	{
		name: 'onsgcsessionopen',
		message: message
	});
  }
  
  function onSessionClose(){
	evt_onsgcsessionclose.fire(
	{
		name: 'onsgcsessionclose'
	});
  }  

  function onRemoteStreamAdded(event) {
	evt_onsgcaddstream.fire(
	{
		name: 'onsgcaddstream',
		stream: event.streams[0]
	});
    remoteStream = event.streams[0];
  }
  
  function onRemoteStreamRemoved(event) {
	evt_onsgcremovestream.fire(
	{
		name: 'onsgcremovestream'
	});	
  }

  function onHangup() {
    transitionToDone();
    doStop();
	
    this.close();	
  }
   
  function onRemoteHangup() {
    transitionToWaiting();
    doStop();
  }

  function transitionToWaiting() {
    setTimeout(function() {
                 remoteVideo.src = "" }, 500);
    localVideo.style.opacity = 0;
    remoteVideo.style.opacity = 0;
  }
  function transitionToDone() {
    localVideo.style.opacity = 0;
    remoteVideo.style.opacity = 0;
  }


  // Send BYE on refreshing(or leaving) a demo page
  // to ensure the room is cleaned for next session.
  window.onbeforeunload = function() {
    webrtc_send(rtc_channel, {type: 'bye'});
  }


  // Set Opus as the default audio codec if it's present.
  function preferOpus(sdp) {
    var sdpLines = sdp.split('\r\n');

    // Search for m line.
    for (var i = 0; i < sdpLines.length; i++) {
        if (sdpLines[i].search('m=audio') !== -1) {
          var mLineIndex = i;
          break;
        } 
    }
    if (mLineIndex === null)
      return sdp;

    // If Opus is available, set it as the default in m line.
    for (var i = 0; i < sdpLines.length; i++) {
      if (sdpLines[i].search('opus/48000') !== -1) {        
        var opusPayload = extractSdp(sdpLines[i], /:(\d+) opus\/48000/i);
        if (opusPayload)
          sdpLines[mLineIndex] = setDefaultCodec(sdpLines[mLineIndex], opusPayload);
        break;
      }
    }

    // Remove CN in m line and sdp.
    sdpLines = removeCN(sdpLines, mLineIndex);

    sdp = sdpLines.join('\r\n');
    return sdp;
  }

  function extractSdp(sdpLine, pattern) {
    var result = sdpLine.match(pattern);
    return (result && result.length == 2)? result[1]: null;
  }

  // Set the selected codec to the first in m line.
  function setDefaultCodec(mLine, payload) {
    var elements = mLine.split(' ');
    var newLine = new Array();
    var index = 0;
    for (var i = 0; i < elements.length; i++) {
      if (index === 3) // Format of media starts from the fourth.
        newLine[index++] = payload; // Put target payload to the first.
      if (elements[i] !== payload)
        newLine[index++] = elements[i];
    }
    return newLine.join(' ');
  }

  // Strip CN from sdp before CN constraints is ready.
  function removeCN(sdpLines, mLineIndex) {
    var mLineElements = sdpLines[mLineIndex].split(' ');
    // Scan from end for the convenience of removing an item.
    for (var i = sdpLines.length-1; i >= 0; i--) {
      var payload = extractSdp(sdpLines[i], /a=rtpmap:(\d+) CN\/\d+/i);
      if (payload) {
        var cnPos = mLineElements.indexOf(payload);
        if (cnPos !== -1) {
          // Remove CN payload from m line.
          mLineElements.splice(cnPos, 1);
        }
        // Remove CN line in sdp
        sdpLines.splice(i, 1);
      }
    }

    sdpLines[mLineIndex] = mLineElements.join(' ');
    return sdpLines;
  }
  
    
  var evt_onsgcopen = new event("open");
  var evt_onsgcclose = new event("close");
  
  var evt_onsgcmediastart = new event("onsgcmediastart");
  var evt_onsgcmediaerror = new event("onsgcmediaerror");
  var evt_onsgcmediastop = new event("onsgcmediastop");
   
  var evt_onsgcsessionconnecting = new event("onsgcsessionconnecting");
  var evt_onsgcsessionopen = new event("onsgcsessionopen");
  var evt_onsgcsessionclose = new event("onsgcsessionclose");  
  var evt_onsgcaddstream = new event("onsgcaddstream");
  var evt_onsgcremovestream = new event("onsgcremovestream");  
  
  var evt_onsgcmessage = new event("onsgcmessage");   
  var evt_onsgcsubscribe = new event("onsgcsubscribe");
  var evt_onsgcunsubscribe = new event("onsgcunsubscribe");    
  
  
  this.on('open', function(event) {
    evt_onsgcopen.fire(
    {
	  name: 'onopen',
	  message: ''
    });  
  
    card = document.getElementById("card");
    remoteVideo = document.getElementById("remoteVideo"); 
	navigator.getUserMedia = navigator.getUserMedia || navigator.webkitGetUserMedia ||
		navigator.mozGetUserMedia || navigator.msGetUserMedia;

	navigator.getUserMedia({audio: true, video: true}, 
		function (stream) {
			evt_onsgcmediastart.fire(
			{
			  name: 'onsgcmediastart',
			  message: '',
			  stream: stream
			});
			localStream = stream;

			createPeerConnection();
			rtcpc.addStream(localStream);			
		}, 
		function(error) {
			evt_onsgcmediaerror.fire(
			{
			  name: 'onsgcmediaerror',
			  code: error.name,
			  message: error.message
			});		
		});  
  }); 
  
  
  this.on('close', function(event) {
     evt_onsgcmediastop.fire(
    {
	  name: 'onsgcmediastop',
	  message: ''
    });
  
	evt_onsgcclose.fire(
	{ 
	  name: 'onclose',
	  message: '',
	  code: event.code,
	  reason: event.reason,
	  clean: event.wasClean
	}); 
  
    rtcpc.close();
    rtcpc = null; 
  });   
  
  DoEventMessage = function(evt) { 
    var json = evt.data; 
    obj = JSON.parse(json);
    if (obj.method == "sgc@subscribe") {
	  if (obj.params["message"] == '2') {
		doCall();
	  }
	
      evt_onsgcsubscribe.fire(
      {
        name: 'onsgcsubscribe',
        channel: obj.params["channel"],
		users: obj.params["message"]
      }
	  )
	} else if (obj.method == "sgc@unsubscribe") {
	  evt_onsgcunsubscribe.fire(
	  {
	    name: 'onsgcunsubscribe',
		channel: obj.channel,
		guid: obj.guid
	  }
	  ) 
	} else if (obj.method == "sgc@webrtc") {
	    var mymessage = JSON.stringify(obj.webrtc);
		mymessage = mymessage.toString();
		processSignalingMessage(mymessage);
	} else if (obj.method == "sgc@iceservers") {
	    ice_servers = obj.iceservers;
	} else {		  
	  evt_onsgcmessage.fire(
	  { 
		name: 'onsgcmessage',
		method: obj.result["method"],
		channel: obj.result["channel"],
		message: obj.result["message"],
		guid: obj.guid
	  }
	  );
    }
  }  
  
  var on_override = this.on; 
  this.on = function (evtname, fn) {  
    if (evtname == "sgcmessage") {
      evt_onsgcmessage.subscribe(fn);
    } else if (evtname == "sgcsubscribe") {
      evt_onsgcsubscribe.subscribe(fn);
    } else if (evtname == "sgcunsubscribe") {
      evt_onsgcunsubscribe.subscribe(fn);
	} else if (evtname == "open") {	  
	  evt_onsgcopen.subscribe(fn);
	} else if (evtname == "close") {	  
	  evt_onsgcclose.subscribe(fn);	  
	} else if (evtname == "sgcmediastart") {	  
	  evt_onsgcmediastart.subscribe(fn);
	} else if (evtname == "sgcmediastop") {	  
	  evt_onsgcmediastop.subscribe(fn);
	} else if (evtname == "sgcmediaerror") {	  
	  evt_onsgcmediaerror.subscribe(fn);	  
	} else if (evtname == "sgcsessionconnecting") {	  
	  evt_onsgcsessionconnecting.subscribe(fn);
	} else if (evtname == "sgcsessionopen") {	  
	  evt_onsgcsessionopen.subscribe(fn);
	} else if (evtname == "sgcsessionclose") {	  
	  evt_onsgcsessionclose.subscribe(fn);	  
	} else if (evtname == "sgcaddstream") {	  
	  evt_onsgcaddstream.subscribe(fn);
	} else if (evtname == "sgcremovestream") {	  
	  evt_onsgcremovestream.subscribe(fn);	 
    } else {
      on_override.apply(this, arguments); 
  };
  };  

}

sgcws_webrtc.prototype = new sgcWebSocket();
sgcws_webrtc.prototype.constructor = sgcws_webrtc;

