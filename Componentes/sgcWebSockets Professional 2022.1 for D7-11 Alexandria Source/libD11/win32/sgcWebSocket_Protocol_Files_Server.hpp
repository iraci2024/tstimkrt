// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_Protocol_Files_Server.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_protocol_files_serverHPP
#define Sgcwebsocket_protocol_files_serverHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Contnrs.hpp>
#include <Winapi.Windows.hpp>
#include <IdContext.hpp>
#include <sgcBase_Classes.hpp>
#include <sgcWebSocket_Types.hpp>
#include <sgcWebSocket_Classes.hpp>
#include <sgcWebSocket_Protocol_Files_Message.hpp>
#include <sgcWebSocket_Protocol_Base_Server.hpp>
#include <sgcWebSocket_HTTPResponse.hpp>
#include <sgcWebSocket_Protocol_sgc_Message.hpp>
#include <sgcWebSocket_Helpers.hpp>
#include <sgcWebSocket_Classes_SyncObjs.hpp>
#include <sgcSocket_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_protocol_files_server
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSFilesServerQoS_Options;
class DELPHICLASS TsgcWSFilesServer_Options;
class DELPHICLASS TsgcWSProtocol_Files_Server;
class DELPHICLASS TsgcWSProtocol_JS_Files;
class DELPHICLASS TsgcWSProtocol_HTML_Files;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSFilesServerQoS_Options : public Sgcwebsocket_protocol_files_message::TsgcWSFilesQoS_Options
{
	typedef Sgcwebsocket_protocol_files_message::TsgcWSFilesQoS_Options inherited;
	
public:
	/* TsgcWSFilesQoS_Options.Create */ inline __fastcall virtual TsgcWSFilesServerQoS_Options() : Sgcwebsocket_protocol_files_message::TsgcWSFilesQoS_Options() { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSFilesServerQoS_Options() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSFilesServer_Options : public Sgcwebsocket_protocol_files_message::TsgcWSFiles_Options
{
	typedef Sgcwebsocket_protocol_files_message::TsgcWSFiles_Options inherited;
	
private:
	TsgcWSFilesServerQoS_Options* FQoS;
	void __fastcall SetQoS(TsgcWSFilesServerQoS_Options* const Value);
	
public:
	__fastcall virtual TsgcWSFilesServer_Options();
	__fastcall virtual ~TsgcWSFilesServer_Options();
	
__published:
	__property TsgcWSFilesServerQoS_Options* QoS = {read=FQoS, write=SetQoS};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcWSProtocol_Files_Server : public Sgcwebsocket_protocol_base_server::TsgcWSProtocol_Subscription_Server_Base
{
	typedef Sgcwebsocket_protocol_base_server::TsgcWSProtocol_Subscription_Server_Base inherited;
	
protected:
	virtual void __fastcall Loaded();
	virtual void __fastcall DoEventMessage(Sgcwebsocket_classes::TsgcWSConnection* aConnection, const System::UnicodeString Text);
	virtual void __fastcall DoEventBinary(Sgcwebsocket_classes::TsgcWSConnection* const aConnection, System::Classes::TMemoryStream* Data);
	virtual void __fastcall DoEventFragmented(Sgcwebsocket_classes::TsgcWSConnection* const aConnection, System::Classes::TMemoryStream* Data, Sgcwebsocket_types::TOpcode OpCode, bool Continuation);
	virtual void __fastcall DoEventDisconnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection, int Code);
	virtual void __fastcall DoRawWriteData(System::UnicodeString aGuid, System::UnicodeString aMessage)/* overload */;
	virtual void __fastcall DoRawWriteData(System::UnicodeString aGuid, System::Classes::TStream* aStream)/* overload */;
	
public:
	virtual bool __fastcall WriteData(System::UnicodeString aGuid, System::UnicodeString aMessage)/* overload */;
	
protected:
	virtual void __fastcall DoFinalize(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	
private:
	TsgcWSFilesServer_Options* FFiles;
	void __fastcall SetFiles(TsgcWSFilesServer_Options* const Value);
	
public:
	__property TsgcWSFilesServer_Options* Files = {read=FFiles, write=SetFiles};
	__fastcall virtual TsgcWSProtocol_Files_Server(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWSProtocol_Files_Server();
	
private:
	System::UnicodeString FWSMessageId;
	
protected:
	Sgcwebsocket_protocol_sgc_message::TsgcWSMessage* __fastcall GetWSMessageByConnection(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	bool __fastcall DoWriteMessageText(System::UnicodeString aGuid, System::UnicodeString aMessage)/* overload */;
	bool __fastcall DoWriteMessageText(Sgcwebsocket_classes::TsgcWSConnection* aConnection, System::UnicodeString aMessage)/* overload */;
	
private:
	System::UnicodeString FWSMessageFileId;
	
protected:
	Sgcwebsocket_protocol_files_message::TsgcWSMessageFile* __fastcall GetWSMessageFileByConnection(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	Sgcwebsocket_protocol_files_message::TsgcWSMessageFile* __fastcall GetWSMessageFileByConnectionId(System::UnicodeString aGuid);
	virtual void __fastcall DoWriteMessage(System::UnicodeString aGuid, System::UnicodeString aText);
	virtual void __fastcall DoProcessMessage(Sgcwebsocket_classes::TsgcWSConnection* aConnection, System::Classes::TMemoryStream* Data);
	
private:
	Sgcbase_classes::TsgcTimer* FQoSTimer;
	Sgcwebsocket_protocol_files_message::TsgcWSFilesQoSList* FQoSList;
	Sgcwebsocket_protocol_files_message::TsgcWSFilesQoSList* __fastcall GetQoSList();
	
protected:
	virtual void __fastcall DoStartQoS();
	virtual void __fastcall DoStopQoS();
	virtual void __fastcall DoQoSList();
	void __fastcall DoProcessFileAcknowledgment(Sgcwebsocket_protocol_files_message::TsgcWSMessageFile* aMessageFile);
	virtual void __fastcall DoWriteAcknowledgment(Sgcwebsocket_classes::TsgcWSConnection* aConnection)/* overload */;
	virtual void __fastcall DoWriteAcknowledgment(System::UnicodeString aGuid)/* overload */;
	virtual void __fastcall DoWriteFileRec(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Sgcwebsocket_protocol_files_message::TsgcWSMessageFile* aMessageFile);
	virtual void __fastcall DoWriteFileQoS(System::UnicodeString aGuid, Sgcwebsocket_protocol_files_message::TsgcWSMessageFile* aMessageFile, bool aDuplicate = false);
	virtual void __fastcall DoQueueQoSLevel2(System::UnicodeString aGuid, Sgcwebsocket_protocol_files_message::TsgcWSMessageFile* aMessageFile);
	void __fastcall DoDeleteQoSByFileId(Sgcwebsocket_protocol_files_message::TsgcWSMessageFile* aMessageFile);
	virtual void __fastcall OnQoSEvent(System::TObject* Sender);
	virtual void __fastcall OnQoSExceptionEvent(System::TObject* Sender, System::Sysutils::Exception* E);
	__property Sgcwebsocket_protocol_files_message::TsgcWSFilesQoSList* QoSList = {read=GetQoSList};
	
private:
	Sgcwebsocket_protocol_files_message::TsgcWSFileStreams* FFileReceivedStreams;
	Sgcwebsocket_protocol_files_message::TsgcWSFileStreams* FFileSentStreams;
	Sgcwebsocket_protocol_files_message::TsgcWSFileStreams* __fastcall GetFileReceivedStreams();
	Sgcwebsocket_protocol_files_message::TsgcWSFileStreams* __fastcall GetFileSentStreams();
	
protected:
	virtual Sgcwebsocket_protocol_files_message::TsgcWSFileStream* __fastcall DoGetFileReceivedStream(System::UnicodeString aId);
	virtual void __fastcall DoDeleteFileReceivedStream(System::UnicodeString aId);
	virtual void __fastcall DoDeleteFileReceivedStreams(System::UnicodeString aGuid);
	virtual Sgcwebsocket_protocol_files_message::TsgcWSFileStream* __fastcall DoGetFileSentStream(System::UnicodeString aId);
	virtual void __fastcall DoDeleteFileSentStream(System::UnicodeString aId);
	virtual void __fastcall DoDeleteFileSentStreams(System::UnicodeString aGuid);
	
public:
	__property Sgcwebsocket_protocol_files_message::TsgcWSFileStreams* FileReceivedStreams = {read=GetFileReceivedStreams, write=FFileReceivedStreams};
	__property Sgcwebsocket_protocol_files_message::TsgcWSFileStreams* FileSentStreams = {read=GetFileSentStreams, write=FFileSentStreams};
	
protected:
	virtual void __fastcall DoWriteFileReceived(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Sgcwebsocket_protocol_files_message::TsgcWSMessageFile* aMessageFile);
	virtual void __fastcall DoWriteFileReceivedError(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Sgcwebsocket_protocol_files_message::TsgcWSMessageFile* aMessageFile, System::UnicodeString aError);
	virtual void __fastcall DoDeleteFileReceived(System::UnicodeString aFileName);
	virtual void __fastcall DoFileReceivedStream(Sgcwebsocket_classes::TsgcWSConnection* aConnection, System::Classes::TMemoryStream* Data);
	virtual void __fastcall DoWriteFileStream(System::UnicodeString aGuid, Sgcwebsocket_helpers::TsgcFileStream* aFileStream, int aSize);
	virtual void __fastcall DoWriteHeaderStream(System::UnicodeString aGuid, System::UnicodeString aId, System::UnicodeString aFileId, Sgcwebsocket_helpers::TsgcFileStream* aFileStream, int aSize, Sgcwebsocket_types::TwsQoS aQoS, System::UnicodeString aData, System::UnicodeString aChannel = System::UnicodeString());
	virtual void __fastcall DoWriteWSFileMessage(System::Classes::TMemoryStream* &Stream, Sgcwebsocket_protocol_files_message::TsgcWSMessageFile* aMessageFile, __int64 aFilePosition, Sgcwebsocket_types::TwsStreaming aStreaming, bool aDuplicate = false);
	virtual void __fastcall DoSendFile(System::UnicodeString aGuid, System::UnicodeString aFileName, int aSize, Sgcwebsocket_types::TwsQoS aQoS, System::UnicodeString aData, System::UnicodeString aFileId);
	virtual void __fastcall DoSendFileQoSLevel2(System::UnicodeString aGuid, System::UnicodeString aFileName, int aSize, System::UnicodeString aData, System::UnicodeString aChannel = System::UnicodeString(), System::UnicodeString aFileId = System::UnicodeString());
	virtual void __fastcall DoWriteFileSentError(System::UnicodeString aGuid, Sgcwebsocket_protocol_files_message::TsgcWSMessageFile* aMessageFile, System::UnicodeString aError);
	
public:
	virtual void __fastcall SendFile(System::UnicodeString aGuid, System::UnicodeString aFileName, int aSize, Sgcwebsocket_types::TwsQoS aQoS, System::UnicodeString aData, System::UnicodeString aFileId = System::UnicodeString())/* overload */;
	virtual void __fastcall SendFile(System::UnicodeString aGuid, System::UnicodeString aFileName)/* overload */;
	virtual void __fastcall SendFile(System::UnicodeString aGuid, System::UnicodeString aFileName, System::UnicodeString aData, System::UnicodeString aFileId = System::UnicodeString())/* overload */;
	
protected:
	virtual void __fastcall DoBroadCast(Sgcwebsocket_protocol_sgc_message::TsgcWSMessage* aMessage, System::UnicodeString aChannel = System::UnicodeString(), System::UnicodeString Exclude = System::UnicodeString(), System::UnicodeString Include = System::UnicodeString())/* overload */;
	virtual void __fastcall DoBroadCast(System::Classes::TStream* aStream, System::UnicodeString aChannel = System::UnicodeString(), System::UnicodeString Exclude = System::UnicodeString(), System::UnicodeString Include = System::UnicodeString(), int aSize = 0x0, Sgcwebsocket_types::TwsStreaming aStreaming = (Sgcwebsocket_types::TwsStreaming)(0x0))/* overload */;
	virtual void __fastcall DoBroadcastFile(System::UnicodeString aFileName, int aSize, System::UnicodeString aData, Sgcwebsocket_types::TwsQoS aQoS, System::UnicodeString aChannel = System::UnicodeString(), System::UnicodeString Exclude = System::UnicodeString(), System::UnicodeString Include = System::UnicodeString(), System::UnicodeString aFileId = System::UnicodeString())/* overload */;
	virtual void __fastcall DoBroadcastFileQoSLevel2(System::UnicodeString aFileName, int aSize, System::UnicodeString aData = System::UnicodeString(), System::UnicodeString aChannel = System::UnicodeString(), System::UnicodeString Exclude = System::UnicodeString(), System::UnicodeString Include = System::UnicodeString(), System::UnicodeString aFileId = System::UnicodeString())/* overload */;
	
public:
	void __fastcall BroadcastFile(System::UnicodeString aFileName, int aSize, System::UnicodeString aData, System::UnicodeString aChannel = System::UnicodeString(), System::UnicodeString Exclude = System::UnicodeString(), System::UnicodeString Include = System::UnicodeString(), System::UnicodeString aFileId = System::UnicodeString())/* overload */;
	void __fastcall BroadcastFile(System::UnicodeString aFileName, int aSize, System::UnicodeString aData, Sgcwebsocket_types::TwsQoS aQoS, System::UnicodeString aChannel = System::UnicodeString(), System::UnicodeString Exclude = System::UnicodeString(), System::UnicodeString Include = System::UnicodeString(), System::UnicodeString aFileId = System::UnicodeString())/* overload */;
	void __fastcall BroadcastFile(System::UnicodeString aFileName, System::UnicodeString aData = System::UnicodeString(), System::UnicodeString aChannel = System::UnicodeString(), System::UnicodeString Exclude = System::UnicodeString(), System::UnicodeString Include = System::UnicodeString(), System::UnicodeString aFileId = System::UnicodeString())/* overload */;
	
private:
	Sgcwebsocket_protocol_files_message::TsgcWSFileFragmentEvent FOnFileReceivedFragment;
	Sgcwebsocket_protocol_files_message::TsgcWSFileEvent FOnFileReceived;
	Sgcwebsocket_protocol_files_message::TsgcWSFileErrorEvent FOnFileReceivedError;
	Sgcwebsocket_protocol_files_message::TsgcWSFileAuthorizationEvent FOnFileReceivedAuthorization;
	Sgcwebsocket_protocol_files_message::TsgcWSFileEvent FOnFileSentAcknowledgment;
	Sgcwebsocket_protocol_files_message::TsgcWSFileErrorEvent FOnFileSentError;
	Sgcwebsocket_protocol_files_message::TsgcWSFileEvent FOnFileSent;
	Sgcwebsocket_protocol_files_message::TsgcWSFileBeforeEvent FOnFileBeforeSent;
	Sgcwebsocket_protocol_files_message::TsgcWSFileFragmentRequestEvent FOnFileSentFragmentRequest;
	
protected:
	virtual void __fastcall DoEventFileReceived(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Sgcwebsocket_protocol_files_message::TsgcWSMessageFile* aMessage);
	virtual void __fastcall DoEventFileReceivedFragment(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Sgcwebsocket_protocol_files_message::TsgcWSMessageFile* aMessage);
	virtual void __fastcall DoEventFileReceivedError(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Sgcwebsocket_protocol_files_message::TsgcWSMessageFile* aMessage, System::UnicodeString aError);
	virtual void __fastcall DoEventFileReceivedAuthorization(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Sgcwebsocket_protocol_files_message::TsgcWSMessageFile* aMessage, System::UnicodeString &aFileName);
	virtual void __fastcall DoEventFileSent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Sgcwebsocket_protocol_files_message::TsgcWSMessageFile* aMessage);
	virtual void __fastcall DoEventFileSentAcknowledgment(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Sgcwebsocket_protocol_files_message::TsgcWSMessageFile* aMessage);
	virtual void __fastcall DoEventFileSentError(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Sgcwebsocket_protocol_files_message::TsgcWSMessageFile* aMessage, System::UnicodeString aError);
	virtual void __fastcall DoEventFileBeforeSent(Sgcwebsocket_protocol_files_message::TsgcWSMessageFile* aMessage);
	virtual void __fastcall DoEventFileSentFragmentRequest(Sgcwebsocket_protocol_files_message::TsgcWSMessageFile* aMessage);
	
public:
	__property Sgcwebsocket_protocol_files_message::TsgcWSFileEvent OnFileReceived = {read=FOnFileReceived, write=FOnFileReceived};
	__property Sgcwebsocket_protocol_files_message::TsgcWSFileErrorEvent OnFileReceivedError = {read=FOnFileReceivedError, write=FOnFileReceivedError};
	__property Sgcwebsocket_protocol_files_message::TsgcWSFileFragmentEvent OnFileReceivedFragment = {read=FOnFileReceivedFragment, write=FOnFileReceivedFragment};
	__property Sgcwebsocket_protocol_files_message::TsgcWSFileAuthorizationEvent OnFileReceivedAuthorization = {read=FOnFileReceivedAuthorization, write=FOnFileReceivedAuthorization};
	__property Sgcwebsocket_protocol_files_message::TsgcWSFileEvent OnFileSentAcknowledgment = {read=FOnFileSentAcknowledgment, write=FOnFileSentAcknowledgment};
	__property Sgcwebsocket_protocol_files_message::TsgcWSFileErrorEvent OnFileSentError = {read=FOnFileSentError, write=FOnFileSentError};
	__property Sgcwebsocket_protocol_files_message::TsgcWSFileEvent OnFileSent = {read=FOnFileSent, write=FOnFileSent};
	__property Sgcwebsocket_protocol_files_message::TsgcWSFileBeforeEvent OnFileBeforeSent = {read=FOnFileBeforeSent, write=FOnFileBeforeSent};
	__property Sgcwebsocket_protocol_files_message::TsgcWSFileFragmentRequestEvent OnFileSentFragmentRequest = {read=FOnFileSentFragmentRequest, write=FOnFileSentFragmentRequest};
	/* Hoisted overloads: */
	
protected:
	inline bool __fastcall  WriteData(System::UnicodeString aGuid, System::Classes::TStream* aStream, int aSize = 0x0, Sgcwebsocket_types::TwsStreaming aStreaming = (Sgcwebsocket_types::TwsStreaming)(0x0)){ return Sgcwebsocket_protocol_base_server::TsgcWSProtocol_Server_Base::WriteData(aGuid, aStream, aSize, aStreaming); }
	inline bool __fastcall  WriteData(Sgcwebsocket_classes::TsgcWSConnection* aConnection, System::UnicodeString aMessage){ return Sgcwebsocket_protocol_base_server::TsgcWSProtocol_Server_Base::WriteData(aConnection, aMessage); }
	inline void __fastcall  WriteData(Sgcwebsocket_classes::TsgcWSConnection* aConnection, System::Classes::TStream* aStream){ Sgcwebsocket_protocol_base_server::TsgcWSProtocol_Server_Base::WriteData(aConnection, aStream); }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSProtocol_JS_Files : public Sgcwebsocket_httpresponse::TsgcWSHTTPResponse_Base
{
	typedef Sgcwebsocket_httpresponse::TsgcWSHTTPResponse_Base inherited;
	
protected:
	virtual System::UnicodeString __fastcall GetResponse();
	
public:
	__classmethod virtual System::UnicodeString __fastcall GetFileName();
public:
	/* TObject.Create */ inline __fastcall TsgcWSProtocol_JS_Files() : Sgcwebsocket_httpresponse::TsgcWSHTTPResponse_Base() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcWSProtocol_JS_Files() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSProtocol_HTML_Files : public Sgcwebsocket_httpresponse::TsgcWSHTTPResponse_Base
{
	typedef Sgcwebsocket_httpresponse::TsgcWSHTTPResponse_Base inherited;
	
protected:
	virtual System::UnicodeString __fastcall GetResponse();
	
public:
	__classmethod virtual System::UnicodeString __fastcall GetFileName();
public:
	/* TObject.Create */ inline __fastcall TsgcWSProtocol_HTML_Files() : Sgcwebsocket_httpresponse::TsgcWSHTTPResponse_Base() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcWSProtocol_HTML_Files() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_protocol_files_server */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_PROTOCOL_FILES_SERVER)
using namespace Sgcwebsocket_protocol_files_server;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_protocol_files_serverHPP
