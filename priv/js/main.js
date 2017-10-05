'use strict';

var isChannelReady = false;
var isInitiator = false;
var isStarted = false;
var localStream;
var pc;
var remoteStream;
var turnReady;

var pcConfig = {
  'iceServers': [{
    'urls': 'stun:stun.l.google.com:19302'
  }]
};

// Set up audio and video regardless of what devices are present.
var sdpConstraints = {
  offerToReceiveAudio: true,
  offerToReceiveVideo: true
};

/////////////////////////////////////////////

// TODO multiple rooms
const room = 'foo';
const wsUrl = 'ws://' + window.location.host + '/websocket';
const socket = new WebSocket(wsUrl);

function sendMessage(event, message) {
  const payload = {
    event,
    data: message
  };
  console.log('Client sending message: ', event, message);
  socket.send(JSON.stringify(payload));
}

// fake event listeners ala socket.io
const listeners = {
  created,
  joined,
  candidate,
  offer,
  answer,
  gotMedia,
  bye
};

socket.onopen = function(event) {
  console.log('socket connected');

  socket.onmessage = function(e) {
    const data = JSON.parse(e.data);
    console.log('Client received message:', data);
    listeners[data.event](data.data);
  };
};

function created () {
  console.log('Created room ');
  isInitiator = true;
}

function joined () {
  console.log('joined: ' + room);
  isChannelReady = true;
}

function candidate(data) {
  if (isStarted) {
    console.log("CONSTRUCTING CANDIDATE", {
      sdpMLineIndex: data.label,
      candidate: data.candidate
    }, data);
    var candidate = new RTCIceCandidate({
      sdpMLineIndex: data.label,
      candidate: data.candidate
    });
    pc.addIceCandidate(candidate);
  }
}

function offer (data) {
  if (!isInitiator && !isStarted) {
    maybeStart();
  }
  console.log("SETTING DESCR", data)
  pc.setRemoteDescription(new RTCSessionDescription(data));
  doAnswer();
}

function answer (data) {
  if (isStarted) {
    pc.setRemoteDescription(new RTCSessionDescription(data));
  }
}

function bye () {
  if (isStarted) {
    handleRemoteHangup();
  }
}


function gotMedia () {
  maybeStart();
}

////////////////////////////////////////////////////

var localVideo = document.querySelector('#localVideo');
var remoteVideo = document.querySelector('#remoteVideo');

navigator.mediaDevices.getUserMedia({
  audio: false, // FIXME use audio
  video: true
})
.then(gotStream)
.catch(function(e) {
  alert('getUserMedia() error: ' + e.name);
});

function gotStream(stream) {
  console.log('Adding local stream.');
  localVideo.src = window.URL.createObjectURL(stream);
  localStream = stream;
  sendMessage('gotMedia');
  if (isInitiator) {
    maybeStart();  }
}

var constraints = {
  video: true
};

console.log('Getting user media with constraints', constraints);

function maybeStart() {
  console.log('>>>>>>> maybeStart() ', isStarted, localStream, isChannelReady);
  if (!isStarted && typeof localStream !== 'undefined' && isChannelReady) {
    console.log('>>>>>> creating peer connection');
    createPeerConnection();
    pc.addStream(localStream);
    isStarted = true;
    console.log('isInitiator', isInitiator);
    if (isInitiator) {
      doCall();
    }
  }
}

window.onbeforeunload = function() {
  sendMessage('bye');
};

/////////////////////////////////////////////////////////

function createPeerConnection() {
  try {
    pc = new RTCPeerConnection(null);
    pc.onicecandidate = handleIceCandidate;
    pc.onaddstream = handleRemoteStreamAdded;
    pc.onremovestream = handleRemoteStreamRemoved;
    console.log('Created RTCPeerConnnection');
  } catch (e) {
    console.log('Failed to create PeerConnection, exception: ' + e.message);
    alert('Cannot create RTCPeerConnection object.');
    return;
  }
}

function handleIceCandidate(event) {
  console.log('icecandidate event: ', event);
  if (event.candidate) {
    sendMessage('candidate', {
      label: event.candidate.sdpMLineIndex,
      id: event.candidate.sdpMid,
      candidate: event.candidate.candidate
    });
  } else {
    console.log('End of candidates.');
  }
}

function handleRemoteStreamAdded(event) {
  console.log('Remote stream added.');
  remoteVideo.src = window.URL.createObjectURL(event.stream);
  remoteStream = event.stream;
}

function handleCreateOfferError(event) {
  console.log('createOffer() error: ', event);
}

function doCall() {
  console.log('Sending offer to peer');
  pc.createOffer(setLocalAndSendMessage, handleCreateOfferError);
}

function doAnswer() {
  console.log('Sending answer to peer.');
  pc.createAnswer().then(
    setLocalAndSendMessage,
    onCreateSessionDescriptionError
  );
}

function setLocalAndSendMessage(sessionDescription) {
  pc.setLocalDescription(sessionDescription);
  console.log('setLocalAndSendMessage sending message', sessionDescription);

  //use sessionDescription.type (offer/answer) as the event
  sendMessage(sessionDescription.type, sessionDescription);
}

function onCreateSessionDescriptionError(error) {
  trace('Failed to create session description: ' + error.toString());
}

function handleRemoteStreamAdded(event) {
  console.log('Remote stream added.');
  remoteVideo.src = window.URL.createObjectURL(event.stream);
  remoteStream = event.stream;
}

function handleRemoteStreamRemoved(event) {
  console.log('Remote stream removed. Event: ', event);
}

function hangup() {
  console.log('Hanging up.');
  stop();
  sendMessage('bye');
}

function handleRemoteHangup() {
    console.log('Session terminated.');
    stop();
    isInitiator = false;
}

function stop() {
  isStarted = false;
  // isAudioMuted = false;
  // isVideoMuted = false;
  pc.close();
  pc = null;
}
