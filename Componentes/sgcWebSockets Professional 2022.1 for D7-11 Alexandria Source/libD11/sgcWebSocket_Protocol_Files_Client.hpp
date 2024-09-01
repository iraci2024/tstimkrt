// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_Protocol_Files_Client.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_protocol_files_clientHPP
#define Sgcwebsocket_protocol_files_clientHPP

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
#include <sgcBase_Classes.hpp>
#include <sgcBase_Helpers.hpp>
#include <sgcWebSocket_Types.hpp>
#include <sgcWebSocket_Protocol_Files_Message.hpp>
#include <sgcWebSocket_Classes.hpp>
#include <sgcWebSocket_Protocol_Base_Client.hpp>
#include <sgcWebSocket_HTTPResponse.hpp>
#include <sgcWebSocket_Protocol_sgc_Message.hpp>
#include <sgcWebSocket_Helpers.hpp>
#include <sgcWebSocket_Classes_SyncObjs.hpp>
#include <sgcTCP_Classes.hpp>
#include <sgcSocket_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_protocol_files_client
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSFilesClientQoS_Options;
class DELPHICLASS TsgcWSFilesClient_Options;
class DELPHICLASS TsgcWSProtocol_Files_Client;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSFilesClientQoS_Options : public Sgcwebsocket_protocol_files_message::TsgcWSFilesQoS_Options
{
	typedef Sgcwebsocket_protocol_files_message::TsgcWSFilesQoS_Options inherited;
	
public:
	/* TsgcWSFilesQoS_Options.Create */ inline __fastcall virtual TsgcWSFilesClientQoS_Options() : Sgcwebsocket_protocol_files_message::TsgcWSFilesQoS_Options() { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSFilesClientQoS_Options() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSFilesClient_Options : public Sgcwebsocket_protocol_files_message::TsgcWSFiles_Options
{
	typedef Sgcwebsocket_protocol_files_message::TsgcWSFiles_Options inherited;
	
private:
	TsgcWSFilesClientQoS_Options* FQoS;
	void __fastcall SetQoS(TsgcWSFilesClientQoS_Options* const Value);
	
public:
	__fastcall virtual TsgcWSFilesClient_Options();
	__fastcall virtual ~TsgcWSFilesClient_Options();
	
__published:
	__property TsgcWSFilesClientQoS_Options* QoS = {read=FQoS, write=SetQoS};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcWSProtocol_Files_Client : public Sgcwebsocket_protocol_base_client::TsgcWSProtocol_Subscription_Client_Base
{
	typedef Sgcwebsocket_protocol_base_client::TsgcWSProtocol_Subscription_Client_Base inherited;
	
private:
	Sgcwebsocket_classes::TsgcWSConnection* FWSConnection;
	
protected:
	virtual void __fastcall DoEventConnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoEventMessage(Sgcwebsocket_classes::TsgcWSConnection* aConnection, const System::UnicodeString Text);
	virtual void __fastcall DoEventBinary(Sgcwebsocket_classes::TsgcWSConnection* const aConnection, System::Classes::TMemoryStream* Data);
	virtual void __fastcall DoEventFragmented(Sgcwebsocket_classes::TsgcWSConnection* const aConnection, System::Classes::TMemoryStream* Data, Sgcwebsocket_types::TOpcode OpCode, bool Continuation);
	virtual void __fastcall DoEventDisconnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection, int Code);
	virtual void __fastcall DoInitialize(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoFinalize(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoClear(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoWriteRawData(const System::UnicodeString aText)/* overload */;
	virtual void __fastcall DoWriteRawData(System::Classes::TStream* aStream)/* overload */;
	
public:
	virtual void __fastcall WriteData(const System::UnicodeString aText)/* overload */;
	
private:
	System::UnicodeString FWSMessageId;
	
protected:
	Sgcwebsocket_protocol_sgc_message::TsgcWSMessage* FWSMessage;
	Sgcwebsocket_protocol_sgc_message::TsgcWSMessage* __fastcall GetWSMessage();
	Sgcwebsocket_protocol_sgc_message::TsgcWSMessage* __fastcall GetWSMessageByConnection(Sgcwebsocket_classes::TsgcWSConnection* const aConnection);
	virtual void __fastcall DoWriteMessageText();
	
public:
	virtual void __fastcall Subscribe(const System::UnicodeString aChannel, const System::UnicodeString aGuid = System::UnicodeString());
	virtual void __fastcall UnSubscribe(const System::UnicodeString aChannel, const System::UnicodeString aGuid = System::UnicodeString());
	
protected:
	__property Sgcwebsocket_protocol_sgc_message::TsgcWSMessage* WSMessage = {read=GetWSMessage, write=FWSMessage};
	
private:
	System::UnicodeString FWSMessageFileId;
	Sgcwebsocket_protocol_files_message::TsgcWSMessageFile* FWSMessageFile;
	Sgcwebsocket_protocol_files_message::TsgcWSMessageFile* __fastcall GetWSMessageFile();
	
protected:
	Sgcwebsocket_protocol_files_message::TsgcWSMessageFile* __fastcall GetWSMessageFileByConnection(Sgcwebsocket_classes::TsgcWSConnection* const aConnection);
	virtual void __fastcall DoWriteMessage(const System::UnicodeString aText);
	virtual void __fastcall DoProcessMessage(Sgcwebsocket_classes::TsgcWSConnection* const aConnection, System::Classes::TMemoryStream* Data);
	
public:
	__property Sgcwebsocket_protocol_files_message::TsgcWSMessageFile* WSMessageFile = {read=GetWSMessageFile, write=FWSMessageFile};
	
private:
	Sgcwebsocket_protocol_files_message::TsgcWSFileStreams* FFileReceivedStreams;
	Sgcwebsocket_protocol_files_message::TsgcWSFileStreams* FFileSentStreams;
	Sgcwebsocket_protocol_files_message::TsgcWSFileStreams* __fastcall GetFileReceivedStreams();
	Sgcwebsocket_protocol_files_message::TsgcWSFileStreams* __fastcall GetFileSentStreams();
	
protected:
	virtual Sgcwebsocket_protocol_files_message::TsgcWSFileStream* __fastcall DoGetFileReceivedStream(const System::UnicodeString aId);
	virtual void __fastcall DoDeleteFileReceivedStream(const System::UnicodeString aId);
	virtual Sgcwebsocket_protocol_files_message::TsgcWSFileStream* __fastcall DoGetFileSentStream(const System::UnicodeString aId);
	virtual void __fastcall DoDeleteFileSentStream(const System::UnicodeString aId);
	
public:
	__property Sgcwebsocket_protocol_files_message::TsgcWSFileStreams* FileReceivedStreams = {read=GetFileReceivedStreams, write=FFileReceivedStreams};
	__property Sgcwebsocket_protocol_files_message::TsgcWSFileStreams* FileSentStreams = {read=GetFileSentStreams, write=FFileSentStreams};
	
private:
	TsgcWSFilesClient_Options* FFiles;
	void __fastcall SetFiles(TsgcWSFilesClient_Options* const Value);
	
public:
	__property TsgcWSFilesClient_Options* Files = {read=FFiles, write=SetFiles};
	
private:
	Sgcbase_classes::TsgcTimer* FQoSTimer;
	Sgcwebsocket_protocol_files_message::TsgcWSFilesQoSList* FQoSList;
	Sgcwebsocket_protocol_files_message::TsgcWSFilesQoSList* __fastcall GetQoSList();
	
protected:
	virtual void __fastcall DoStartQoS();
	virtual void __fastcall DoStopQoS();
	virtual void __fastcall DoQoSList();
	void __fastcall DoProcessFileAcknowledgment(Sgcwebsocket_protocol_files_message::TsgcWSMessageFile* const aMessageFile);
	virtual void __fastcall DoWriteAcknowledgment();
	virtual void __fastcall DoWriteFileRec();
	virtual void __fastcall DoWriteFileQoS(Sgcwebsocket_protocol_files_message::TsgcWSMessageFile* const aMessageFile, const bool aDuplicate = false);
	virtual void __fastcall DoQueueQoSLevel2(Sgcwebsocket_protocol_files_message::TsgcWSMessageFile* const aMessageFile);
	void __fastcall DoDeleteQoSByFileId(Sgcwebsocket_protocol_files_message::TsgcWSMessageFile* const aMessageFile);
	virtual void __fastcall OnQoSEvent(System::TObject* Sender);
	virtual void __fastcall OnQoSExceptionEvent(System::TObject* Sender, System::Sysutils::Exception* E);
	__property Sgcwebsocket_protocol_files_message::TsgcWSFilesQoSList* QoSList = {read=GetQoSList};
	
public:
	__fastcall virtual TsgcWSProtocol_Files_Client(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWSProtocol_Files_Client();
	
protected:
	virtual void __fastcall DoWriteFileReceived();
	virtual void __fastcall DoWriteFileReceivedError(const System::UnicodeString aError);
	virtual void __fastcall DoDeleteFileReceived(const System::UnicodeString aFileName);
	virtual void __fastcall DoFileReceivedStream(Sgcwebsocket_classes::TsgcWSConnection* const aConnection, System::Classes::TMemoryStream* Data);
	virtual void __fastcall DoWriteHeaderStream(const System::UnicodeString aId, const System::UnicodeString aFileId, Sgcwebsocket_helpers::TsgcFileStream* const aFileStream, const int aSize, const Sgcwebsocket_types::TwsQoS aQoS, const System::UnicodeString aData);
	virtual void __fastcall DoWriteFileStream(Sgcwebsocket_helpers::TsgcFileStream* const aFileStream, const int aSize);
	virtual void __fastcall DoWriteWSFileMessage(System::Classes::TMemoryStream* &Stream, Sgcwebsocket_protocol_files_message::TsgcWSMessageFile* const aMessageFile, const __int64 aFilePosition, const Sgcwebsocket_types::TwsStreaming aStreaming, const bool aDuplicate = false);
	virtual void __fastcall DoWriteFileSentError(const System::UnicodeString aError);
	virtual void __fastcall DoSendFile(const System::UnicodeString aFileName, const int aSize, const Sgcwebsocket_types::TwsQoS aQoS, const System::UnicodeString aData, const System::UnicodeString aFileId);
	virtual void __fastcall DoSendFileQoSLevel2(const System::UnicodeString aFileName, const int aSize, const System::UnicodeString aData, const System::UnicodeString aFileId);
	
public:
	void __fastcall SendFile(System::UnicodeString aFileName, int aSize, Sgcwebsocket_types::TwsQoS aQoS, System::UnicodeString aData, System::UnicodeString aFileId = System::UnicodeString())/* overload */;
	void __fastcall SendFile(System::UnicodeString aFileName)/* overload */;
	void __fastcall SendFile(System::UnicodeString aFileName, System::UnicodeString aData, System::UnicodeString aFileId = System::UnicodeString())/* overload */;
	
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
	virtual void __fastcall DoEventFileReceived(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Sgcwebsocket_protocol_files_message::TsgcWSMessageFile* const aMessage);
	virtual void __fastcall DoEventFileReceivedError(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Sgcwebsocket_protocol_files_message::TsgcWSMessageFile* const aMessage, const System::UnicodeString aError);
	virtual void __fastcall DoEventFileReceivedFragment(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Sgcwebsocket_protocol_files_message::TsgcWSMessageFile* const aMessage);
	virtual void __fastcall DoEventFileReceivedAuthorization(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Sgcwebsocket_protocol_files_message::TsgcWSMessageFile* const aMessage, System::UnicodeString &aFileName);
	virtual void __fastcall DoEventFileSentAcknowledgment(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Sgcwebsocket_protocol_files_message::TsgcWSMessageFile* const aMessage);
	virtual void __fastcall DoEventFileSentError(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Sgcwebsocket_protocol_files_message::TsgcWSMessageFile* const aMessage, const System::UnicodeString aError);
	virtual void __fastcall DoEventFileSent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Sgcwebsocket_protocol_files_message::TsgcWSMessageFile* const aMessage);
	virtual void __fastcall DoEventFileBeforeSent(Sgcwebsocket_protocol_files_message::TsgcWSMessageFile* const aMessage);
	virtual void __fastcall DoEventFileSentFragmentRequest(Sgcwebsocket_protocol_files_message::TsgcWSMessageFile* const aMessage);
	
public:
	__property Sgcwebsocket_protocol_files_message::TsgcWSFileErrorEvent OnFileReceivedError = {read=FOnFileReceivedError, write=FOnFileReceivedError};
	__property Sgcwebsocket_protocol_files_message::TsgcWSFileEvent OnFileReceived = {read=FOnFileReceived, write=FOnFileReceived};
	__property Sgcwebsocket_protocol_files_message::TsgcWSFileFragmentEvent OnFileReceivedFragment = {read=FOnFileReceivedFragment, write=FOnFileReceivedFragment};
	__property Sgcwebsocket_protocol_files_message::TsgcWSFileAuthorizationEvent OnFileReceivedAuthorization = {read=FOnFileReceivedAuthorization, write=FOnFileReceivedAuthorization};
	__property Sgcwebsocket_protocol_files_message::TsgcWSFileEvent OnFileSentAcknowledgment = {read=FOnFileSentAcknowledgment, write=FOnFileSentAcknowledgment};
	__property Sgcwebsocket_protocol_files_message::TsgcWSFileErrorEvent OnFileSentError = {read=FOnFileSentError, write=FOnFileSentError};
	__property Sgcwebsocket_protocol_files_message::TsgcWSFileEvent OnFileSent = {read=FOnFileSent, write=FOnFileSent};
	__property Sgcwebsocket_protocol_files_message::TsgcWSFileBeforeEvent OnFileBeforeSent = {read=FOnFileBeforeSent, write=FOnFileBeforeSent};
	__property Sgcwebsocket_protocol_files_message::TsgcWSFileFragmentRequestEvent OnFileSentFragmentRequest = {read=FOnFileSentFragmentRequest, write=FOnFileSentFragmentRequest};
	/* Hoisted overloads: */
	
public:
	inline void __fastcall  WriteData(System::Classes::TStream* aStream, int aSize = 0x0, const Sgcwebsocket_types::TwsStreaming aStreaming = (Sgcwebsocket_types::TwsStreaming)(0x0)){ Sgcwebsocket_protocol_base_client::TsgcWSProtocol_Client_Base::WriteData(aStream, aSize, aStreaming); }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_protocol_files_client */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_PROTOCOL_FILES_CLIENT)
using namespace Sgcwebsocket_protocol_files_client;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_protocol_files_clientHPP
