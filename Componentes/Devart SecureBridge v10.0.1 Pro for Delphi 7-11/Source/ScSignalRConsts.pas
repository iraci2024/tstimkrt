//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2018-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I SB.inc}
unit ScSignalRConsts;

interface

resourcestring

/// Log messages
  SInvocationRequest_InvocationCreated = 'Invocation %s created.';
  SInvocationRequest_InvocationDisposed = 'Invocation %s disposed.';
  SInvocationRequest_InvocationCompleted = 'Invocation %s marked as completed.';
  SInvocationRequest_ReceivedUnexpectedComplete = 'Invocation %s received a completion result, but was invoked as a streaming invocation.';
  SInvocationRequest_InvocationFailed = 'Invocation %s marked as failed.';
  SInvocationRequest_ErrorWritingStreamItem = 'Invocation %s caused an error trying to write a stream item.';
  SInvocationRequest_StreamItemOnNonStreamInvocation = 'Invocation %s received stream item but was invoked as a non-streamed invocation.';

  SHttpConnection_SkippingStart = 'Skipping start, connection is already started.';
  SHttpConnection_Starting = 'Starting HttpConnection.';
  SHttpConnection_Started = 'HttpConnection Started.';
  SHttpConnection_DisposingHttpConnection = 'Disposing HttpConnection.';
  SHttpConnection_TransportThrewExceptionOnStop = 'The transport threw an exception while stopping.';
  SHttpConnection_Disposed = 'HttpConnection Disposed.';
  SHttpConnection_SkippingDispose = 'Skipping dispose, connection is already disposed.';
  SHttpConnection_StartingTransport = 'Starting transport ''%s'' with Url: %s.';
  SHttpConnection_TransportNotSupported = 'Skipping transport %s because it is not supported by this client.';
  SHttpConnection_TransportDisabledByClient = 'Skipping transport %s because it was disabled by the client.';
  SHttpConnection_TransportDoesNotSupportTransferFormat = 'Skipping transport {TransportName} because it does not support the requested transfer format ''{TransferFormat}''.';
  SHttpConnection_TransportFailed = 'Skipping transport %s because it failed to initialize.'#13#10'%s';
  SHttpConnection_EstablishingConnection = 'Establishing connection with server at ''%s''.';
  SHttpConnection_ConnectionEstablished = 'Established connection ''%s'' with the server.';
  SHttpConnection_ErrorWithNegotiation = 'Failed to start connection. Error getting negotiation response from ''%s''.';
  SHttpConnection_ErrorStartingTransport = 'Failed to start connection. Error starting transport ''%s''.';
  SHttpConnection_TransportStarted = 'Transport ''%s'' started.';

  SHubConnection_InvokingClosedEventHandler = 'Invoking the Closed event handler.';
  SHubConnection_ErrorDuringClosedEvent = 'An exception was thrown in the handler for the Closed event.';
  SHubConnection_ErrorDuringReconnectingEvent = 'An exception was thrown in the handler for the Reconnecting event.';
  SHubConnection_ErrorDuringReconnectedEvent = 'An exception was thrown in the handler for the Reconnected event.';
  SHubConnection_RegisteringHandler = 'Registering handler for client method ''%s''.';
  SHubConnection_RemovingHandlers = 'Removing handlers for client method ''%s''.';
  SHubConnection_Starting = 'Starting HubConnection.';
  SHubConnection_HubProtocol = 'Using HubProtocol ''%s v%d''.';
  SHubConnection_ErrorStartingConnection = 'Error starting connection.';
  SHubConnection_Started = 'HubConnection started.';
  SHubConnection_RegisteringInvocation = 'Registering Invocation ID ''%s'' for tracking.';
  SHubConnection_PreparingBlockingInvocation = 'Preparing blocking invocation ''%s'' of ''%s'', with return type ''%s'' and %d argument(s).';
  SHubConnection_PreparingStreamingInvocation = 'Preparing streaming invocation ''%s'' of ''%s'', with return type ''%s'' and %d argument(s).';
  SHubConnection_IssuingInvocation = 'Issuing Invocation ''%s'': %s %s(%d).';
  SHubConnection_FailedToSendInvocation = 'Sending Invocation ''%s'' failed.';
  SHubConnection_SendingMessage = 'Sending %s message.';
  SHubConnection_MessageSent = 'Sending %s message completed.';
  SHubConnection_PreparingNonBlockingInvocation = 'Preparing non-blocking invocation of ''%s'', with %d argument(s).';
  SHubConnection_ResettingKeepAliveTimer = 'Resetting keep-alive timer, received a message from the server.';
  SHubConnection_ArgumentBindingFailure = 'Failed to bind arguments received in invocation ''%s'' of ''%s''.';
  SHubConnection_ReceivedInvocation = 'Received Invocation ''%s'': %s(%d).';
  SHubConnection_DroppedCompletionMessage = 'Dropped unsolicited Completion message for invocation ''%s''.';
  SHubConnection_DroppedStreamMessage = 'Dropped unsolicited StreamItem message for invocation ''%s''.';
  SHubConnection_ReceivedClose = 'Received close message.';
  SHubConnection_ReceivedCloseWithError = 'Received close message with an error: %s';
  SHubConnection_ReceivedPing = 'Received a ping message.';
  SHubConnection_MissingHandler = 'Failed to find handler for ''%s'' method.';
  SHubConnection_ErrorInvokingClientSideMethod = 'Invoking client side method ''%s'' failed.';
  SHubConnection_ReceivedStreamItem = 'Received StreamItem for Invocation %s.';
  SHubConnection_CancelingStreamItem = 'Canceling dispatch of StreamItem message for Invocation %s. The invocation was canceled.';
  SHubConnection_ReceivedStreamItemAfterClose = 'Invocation %s received stream item after channel was closed.';
  SHubConnection_ReceivedInvocationCompletion = 'Received Completion for Invocation %s.';
  SHubConnection_CancelingInvocationCompletion = 'Canceling dispatch of Completion message for Invocation %s. The invocation was canceled.';
  SHubConnection_SendingHubHandshake = 'Sending Hub Handshake.';
  SHubConnection_ErrorReceivingHandshakeResponse = 'The underlying connection closed while processing the handshake response. See exception for details.';
  SHubConnection_HandshakeServerError = 'Server returned handshake error: %s';
  SHubConnection_HandshakeComplete = 'Handshake with server complete.';
  SHubConnection_ErrorInvalidHandshakeResponse = 'Received an invalid handshake response.';
  SHubConnection_ErrorHandshakeTimedOut = 'The handshake timed out after %d seconds.';
  SHubConnection_ErrorHandshakeCanceled = 'The handshake was canceled by the client.';
  SHubConnection_ReceiveLoopStarting = 'Receive loop starting.';
  SHubConnection_ProcessingMessage = 'Processing %d byte message from server.';
  SHubConnection_ServerDisconnectedWithError = 'The server connection was terminated with an error.';
  SHubConnection_ShutdownWithError = 'Connection is shutting down due to an error.';
  SHubConnection_ShutdownConnection = 'Shutting down connection.';
  SHubConnection_FirstReconnectRetryDelayNull = 'Connection not reconnecting because the IRetryPolicy returned null on the first reconnect attempt.';
  SHubConnection_ReconnectingWithError = 'HubConnection reconnecting due to an error.';
  SHubConnection_Reconnecting = 'HubConnection reconnecting.';
  SHubConnection_AwaitingReconnectRetryDelay = 'Reconnect attempt number %d will start in %d.';
  SHubConnection_ReconnectingStoppedDuringRetryDelay = 'Connection stopped during reconnect delay. Done reconnecting.';
  SHubConnection_Reconnected = 'HubConnection reconnected successfully after %d attempts and %d elapsed."';
  SHubConnection_ReconnectAttemptFailed = 'Reconnect attempt failed.';
  SHubConnection_ReconnectingStoppedDuringReconnectAttempt = 'Connection stopped during reconnect attempt. Done reconnecting.';
  SHubConnection_ReconnectAttemptsExhausted = 'Reconnect retries have been exhausted after %d failed attempts and %d elapsed. Disconnecting.';
  SHubConnection_ErrorDuringNextRetryDelay = 'An exception was thrown from GetNextRetryDelay().';
  SHubConnection_InvocationAlreadyInUse = 'Invocation ID ''%s'' is already in use.';
  SHubConnection_CancelingOutstandingInvocations = 'Canceling all outstanding invocations.';
  SHubConnection_RemovingInvocation = 'Removing pending invocation %s.';
  SHubConnection_Stopping = 'Stopping HubConnection.';
  SHubConnection_TerminatingReceiveLoop = 'Terminating receive loop.';
  SHubConnection_WaitingForReceiveLoopToTerminate = 'Waiting for the receive loop to terminate.';
  SHubConnection_Stopped = 'HubConnection stopped.';
  SHubConnection_UnableToAcquireConnectionLockForPing = 'Skipping ping because a send is already in progress.';
  SHubConnection_AcquiredConnectionLockForPing = 'Acquired the Connection Lock in order to ping the server.';
  SHubConnection_ReceivedUnexpectedResponse = 'Unsolicited response received for invocation ''%s''.';
  SHubConnection_StateTransitionFailed = 'The HubConnection failed to transition from the %s state to the %s state because it was actually in the %s state.';
  SHubConnection_AttemptingStateTransition = 'The HubConnection is attempting to transition from the %s state to the %s state.';
  SHubConnection_WaitingOnConnectionLock = 'Waiting on Connection Lock.';
  SHubConnection_ReleasingConnectionLock = 'Releasing Connection Lock.';

  STransport_StartedTransport = 'Started transport.';
  STransport_TransportStopping = 'Transport is stopping.';
  STransport_TransportStopped = 'Transport stopped.';
  STransport_ReceiveCanceled = 'Receive loop canceled.';
  STransport_ReceiveStopped = 'Receive loop stopped.';
  STransport_SendStopped = 'Send loop stopped.';

  SLongPollingTransport_StartTransport = 'Starting transport. Transfer mode: %s.';
  SLongPollingTransport_StartReceive = 'Starting receive loop.';
  SLongPollingTransport_PollResponseReceived = 'Poll response with status code %s received from server. Content length: %d.';
  SLongPollingTransport_ClosingConnection = 'The server is closing the connection.';
  SLongPollingTransport_ReceivedMessages = 'Received messages from the server.';
  SLongPollingTransport_ErrorPolling = 'Error while polling ''%s''.';
  SLongPollingTransport_SendingDeleteRequest = 'Sending DELETE request to ''%s''.';
  SLongPollingTransport_ConnectionAlreadyClosedSendingDeleteRequest = 'A 404 response was returned from sending DELETE request to ''%s'', likely because the transport was already closed on the server.';
  SLongPollingTransport_DeleteRequestAccepted = 'DELETE request to ''%s'' accepted.';
  SLongPollingTransport_ErrorSendingDeleteRequest = 'Error sending DELETE request to ''%s''.';
  SLongPollingTransport_SendStarted = 'Starting the send loop.';
  SLongPollingTransport_SendCanceled = 'Send loop canceled.';
  SLongPollingTransport_SendingMessages = 'Sending %d bytes to the server using url: %s.';
  SLongPollingTransport_SentSuccessfully = 'Message(s) sent successfully.';
  SLongPollingTransport_NoMessages = 'No messages in batch to send.';
  SLongPollingTransport_ErrorSending = 'Error while sending to ''%s''.';

  SWebSocketsTransport_StartTransport = 'Starting transport. Transfer mode: %s. Url: %s.';
  SWebSocketsTransport_ErrorSendingMessage = 'Error while sending a message.';
  SWebSocketsTransport_ReceivedFromApp = 'Received message from application. Payload size: %d.';
  SWebSocketsTransport_WebSocketClosed = 'WebSocket closed by the server. Close status %s.';
  SWebSocketsTransport_MessageReceived = 'Message received. Type: %s, size: %d.';

/// Exception messages
  SUnexpectedJsonObjectTokenType = 'Unexpected JSON token type ''%s''. Expected a JSON Object';
  SUnexpectedJsonArrayTokenType = 'Unexpected JSON token type ''%s''. Expected a JSON Array';
  SExpectedHandshakeResponse = 'Expected a handshake response from the server';
  SUnexpectedHandshakeResponseToken = 'Unexpected token ''%s'' when reading handshake response JSON';
  SUnexpectedRequestToken = 'Unexpected token ''%s'' when reading handshake request JSON. Message content: %s';
  SUnexpectedNegotiationResponseToken = 'Unexpected token ''%s'' when reading negotiation response JSON';
  SUnexpectedReadingTransferToken = 'Unexpected token ''%s'' when reading transfer formats JSON';
  SUnexpectedReadingTransportToken = 'Unexpected token ''%s'' when reading available transport JSON';
  SMissingRequiredProperty = 'Missing required property ''%s''. ';
  SMessageContent = 'Message content: %s';
  SExpectedErrorOrResult = 'Expected either "Error" or "Result" to be provided, but not both';
  SUnexpectedJsonEnd = 'Unexpected end when reading JSON';
  SExpectedPropertyOfType = 'Expected ''%s'' to be of type %s';
  SErrorReadingJson = 'Error reading JSON: ';
  SExpectedHeaderOfType = 'Expected header ''%s'' to be of type %s';
  SUnexpectedHeadersEnd = 'Unexpected end when reading message headers';
  SUnsupportedMessageType = 'Unsupported message type: %s';
  SUnexpectedMessageType = 'Unexpected message type: %s';
  SInvocationProvidesUnexpectedArgument = 'Invocation provides %d argument(s) but target expects %d';
  SErrorBindingArguments = 'Error binding arguments. Make sure that the types of the provided values match the types of the hub method being invoked';
  STransferFormatNotSupported = 'The ''%s'' transfer format is not supported by this transport';
  SNoRequestedTransports = 'No requested transports available on the server';
  SCannotAccessTransportPipeBeforeConnecting = 'Cannot access the Transport pipe before the connection has started';
  SNegotiationCanNotBeSkipped = 'Negotiation can only be skipped when using the WebSocket transport directly';
  SNegotiateRedirectionLimitExceeded = 'Negotiate redirection limit exceeded';
  SUnableConnectWithAnyTransports = 'Unable to connect to the server with any of the available transports.';
  SNoneTransportsNotSupported = 'None of the transports supported by the client are supported by the server';
  SInvalidConnectionId = 'Invalid connection id';
  STransportStarted = 'Transport is already started';
  SHttpConnectionDisposed = 'THttpConnection is disposed';
  SOnlyCoreSignalRServerSupported = 'Detected a connection attempt to an ASP.NET SignalR Server. This client only supports connecting to an ASP.NET Core SignalR Server. See https://aka.ms/signalr-core-differences for details';
  SInvalidNegotiationResponse = 'Invalid negotiation response received.';
  SOperationCanceled = 'Operation canceled';
  SUnexpectedResultType = 'Unexpected result type. Expected the TObject variant type';
  SInvalidInputArg = 'Invalid the %s input argument';

  SHubConnectionCanNotBeStartedIfNotDisconnected = 'The HubConnection cannot be started if it is not in the Disconnected state';
  SHubConnectionCanNotBeStartedWhileStop = 'The HubConnection cannot be started while Stop is running';
  SHubConnectionAlreadyConnected = 'The HubConnection is already connected';
  SUnableToCompleteHandshake = 'Unable to complete handshake with the server due to an error: %s';
  SServerDisconnectedBeforeSendingHandshakeResponse = 'The server disconnected before sending a handshake response';
  SConnectionTerminatedWhileReadingMessage = 'Connection terminated while reading a message';
  SSomeoneClearedConnectionState = 'Someone other than ReceiveLoop cleared the connection state';
  SSomeoneSetConnectionState = 'Someone other than Reconnect set the connection state';
  SInvocationIdUsed = 'Invocation ID ''%d'' is already in use';
  SSomethingResetConnectionState = 'Something reset the connection state before the timer loop completed';
  SNoRegisteredCallbacks = 'There are no callbacks registered for the method "%s"';
  SHubConnectionFailedToTransition = 'The HubConnection failed to transition from the ''%s'' state to the ''%s'' state because it was actually in the ''%s'' state';
  SMethodCannotBeCalledIfConnectionNotActive = 'The method cannot be called if the connection is not active';
  SServerProvidedCompletionResult = 'Server provided a result in a completion response to a streamed invocation';
  SStreamingMustBeInvokeWithStreamAsChannel = 'Streaming hub methods must be invoked with the ''HubConnection.StreamAsChannel'' method';
  SServerClosedWithError = 'The server closed the connection with the following error: %s';
  SServerDisconnectedBeforeStarted = 'The server disconnected before the handshake could be started';
  SConnectionStoppedDuringReconnectDelay = 'Connection stopped during reconnect delay. Done reconnecting.';
  SConnectionStoppedDuringReconnectAttempt = 'Connection stopped during reconnect attempt. Done reconnecting.';
  SReconnectRetriesExhausted = 'Reconnect retries have been exhausted after %d failed attempts and %d elapsed. Disconnecting';
  SServerTimeoutWithoutReceivingMessage = 'Server timeout (%d ms) elapsed without receiving a message from the server';
  SNotInConnectionLock = 'We are not in the Connection Lock';
  SNotHaveConnection = 'We do not have a connection';
  SWebsocketClosedWithError = 'Websocket closed with error: %s';

implementation

end.
