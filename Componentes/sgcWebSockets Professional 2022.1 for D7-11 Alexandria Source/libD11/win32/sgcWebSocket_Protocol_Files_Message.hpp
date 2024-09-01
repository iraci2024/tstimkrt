// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_Protocol_Files_Message.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_protocol_files_messageHPP
#define Sgcwebsocket_protocol_files_messageHPP

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
#include <sgcWebSocket_Classes.hpp>
#include <sgcWebSocket_Types.hpp>
#include <sgcWebSocket_Helpers.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_protocol_files_message
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSMessageFile;
class DELPHICLASS TsgcWSFileStream;
class DELPHICLASS TsgcWSFileStreams;
class DELPHICLASS TsgcWSFiles_Options;
class DELPHICLASS TsgcWSFilesQoS_Options;
class DELPHICLASS TsgcWSFilesQoSItem;
class DELPHICLASS TsgcWSFilesQoSList;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TsgcWSFileEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, TsgcWSMessageFile* const aMessage);

typedef void __fastcall (__closure *TsgcWSFileFragmentEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, TsgcWSMessageFile* const aMessage, bool &Cancel);

typedef void __fastcall (__closure *TsgcWSFileErrorEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, TsgcWSMessageFile* const aMessage, const System::UnicodeString Error);

typedef void __fastcall (__closure *TsgcWSFileAuthorizationEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, TsgcWSMessageFile* const aMessage, System::UnicodeString &aFileName, bool &Accept);

typedef void __fastcall (__closure *TsgcWSFileBeforeEvent)(TsgcWSMessageFile* const aMessage);

typedef void __fastcall (__closure *TsgcWSFileFragmentRequestEvent)(TsgcWSMessageFile* const aMessage, bool &Cancel);

class PASCALIMPLEMENTATION TsgcWSMessageFile : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	int FBufferSize;
	System::UnicodeString FChannel;
	System::UnicodeString FConnectionId;
	System::UnicodeString FMethod;
	System::UnicodeString FData;
	System::UnicodeString FFileName;
	__int64 FFilePosition;
	__int64 FFileSize;
	System::UnicodeString FFileId;
	System::UnicodeString FId;
	System::UnicodeString FQoS;
	System::UnicodeString FStreaming;
	System::UnicodeString FText;
	unsigned FThreadId;
	
public:
	void __fastcall Read(const System::UnicodeString aText);
	System::UnicodeString __fastcall Write();
	void __fastcall Clear();
	__property int BufferSize = {read=FBufferSize, write=FBufferSize, nodefault};
	__property System::UnicodeString Channel = {read=FChannel, write=FChannel};
	__property System::UnicodeString Method = {read=FMethod, write=FMethod};
	__property System::UnicodeString FileId = {read=FFileId, write=FFileId};
	__property System::UnicodeString Data = {read=FData, write=FData};
	__property System::UnicodeString FileName = {read=FFileName, write=FFileName};
	__property __int64 FilePosition = {read=FFilePosition, write=FFilePosition};
	__property __int64 FileSize = {read=FFileSize, write=FFileSize};
	__property System::UnicodeString Id = {read=FId, write=FId};
	__property System::UnicodeString QoS = {read=FQoS, write=FQoS};
	__property System::UnicodeString Streaming = {read=FStreaming, write=FStreaming};
	__property System::UnicodeString Text = {read=FText, write=FText};
	__property System::UnicodeString ConnectionId = {read=FConnectionId, write=FConnectionId};
	__property unsigned ThreadId = {read=FThreadId, write=FThreadId, nodefault};
public:
	/* TComponent.Create */ inline __fastcall virtual TsgcWSMessageFile(System::Classes::TComponent* AOwner) : System::Classes::TComponent(AOwner) { }
	/* TComponent.Destroy */ inline __fastcall virtual ~TsgcWSMessageFile() { }
	
};


class PASCALIMPLEMENTATION TsgcWSFileStream : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::UnicodeString FFileName;
	System::UnicodeString FGuid;
	System::UnicodeString FId;
	System::TDateTime FLast;
	Sgcwebsocket_helpers::TsgcFileStream* FStream;
	TsgcWSMessageFile* FMessageFile;
	TsgcWSMessageFile* __fastcall GetMessageFile();
	void __fastcall SetMessageFile(TsgcWSMessageFile* const Value);
	void __fastcall SetStream(Sgcwebsocket_helpers::TsgcFileStream* const Value);
	
public:
	__fastcall virtual ~TsgcWSFileStream();
	__property System::UnicodeString FileName = {read=FFileName, write=FFileName};
	__property System::UnicodeString Guid = {read=FGuid, write=FGuid};
	__property System::UnicodeString Id = {read=FId, write=FId};
	__property System::TDateTime Last = {read=FLast, write=FLast};
	__property TsgcWSMessageFile* MessageFile = {read=GetMessageFile, write=SetMessageFile};
	__property Sgcwebsocket_helpers::TsgcFileStream* Stream = {read=FStream, write=SetStream};
public:
	/* TObject.Create */ inline __fastcall TsgcWSFileStream() : System::TObject() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSFileStreams : public System::Contnrs::TObjectList
{
	typedef System::Contnrs::TObjectList inherited;
	
public:
	HIDESBASE TsgcWSFileStream* __fastcall Add(const System::UnicodeString aId)/* overload */;
	HIDESBASE void __fastcall Delete(const System::UnicodeString aId)/* overload */;
	TsgcWSFileStream* __fastcall Item(const System::UnicodeString aId);
	TsgcWSFileStream* __fastcall GetById(const System::UnicodeString aId);
public:
	/* TObjectList.Create */ inline __fastcall TsgcWSFileStreams()/* overload */ : System::Contnrs::TObjectList() { }
	/* TObjectList.Create */ inline __fastcall TsgcWSFileStreams(bool AOwnsObjects)/* overload */ : System::Contnrs::TObjectList(AOwnsObjects) { }
	
public:
	/* TList.Destroy */ inline __fastcall virtual ~TsgcWSFileStreams() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSFiles_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	int FBufferSize;
	bool FClearReceivedStreamsOnDisconnect;
	bool FClearSentStreamsOnDisconnect;
	System::UnicodeString FSaveDirectory;
	System::UnicodeString __fastcall GetSaveDirectory();
	
public:
	__fastcall virtual TsgcWSFiles_Options();
	
__published:
	__property int BufferSize = {read=FBufferSize, write=FBufferSize, nodefault};
	__property bool ClearReceivedStreamsOnDisconnect = {read=FClearReceivedStreamsOnDisconnect, write=FClearReceivedStreamsOnDisconnect, nodefault};
	__property bool ClearSentStreamsOnDisconnect = {read=FClearSentStreamsOnDisconnect, write=FClearSentStreamsOnDisconnect, nodefault};
	__property System::UnicodeString SaveDirectory = {read=GetSaveDirectory, write=FSaveDirectory};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSFiles_Options() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSFilesQoS_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	int FInterval;
	Sgcwebsocket_types::TwsQoS FLevel;
	int FTimeout;
	
public:
	__fastcall virtual TsgcWSFilesQoS_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property int Interval = {read=FInterval, write=FInterval, nodefault};
	__property Sgcwebsocket_types::TwsQoS Level = {read=FLevel, write=FLevel, nodefault};
	__property int Timeout = {read=FTimeout, write=FTimeout, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSFilesQoS_Options() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcWSFilesQoSItem : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::TDateTime FDate;
	System::UnicodeString FGuid;
	TsgcWSMessageFile* FMessageFile;
	TsgcWSMessageFile* __fastcall GetMessageFile();
	void __fastcall SetMessageFile(TsgcWSMessageFile* const Value);
	
public:
	__fastcall virtual ~TsgcWSFilesQoSItem();
	__property System::TDateTime Date = {read=FDate, write=FDate};
	__property System::UnicodeString Guid = {read=FGuid, write=FGuid};
	__property TsgcWSMessageFile* MessageFile = {read=GetMessageFile, write=SetMessageFile};
public:
	/* TObject.Create */ inline __fastcall TsgcWSFilesQoSItem() : System::TObject() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSFilesQoSList : public System::Contnrs::TObjectList
{
	typedef System::Contnrs::TObjectList inherited;
	
public:
	/* TObjectList.Create */ inline __fastcall TsgcWSFilesQoSList()/* overload */ : System::Contnrs::TObjectList() { }
	/* TObjectList.Create */ inline __fastcall TsgcWSFilesQoSList(bool AOwnsObjects)/* overload */ : System::Contnrs::TObjectList(AOwnsObjects) { }
	
public:
	/* TList.Destroy */ inline __fastcall virtual ~TsgcWSFilesQoSList() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_protocol_files_message */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_PROTOCOL_FILES_MESSAGE)
using namespace Sgcwebsocket_protocol_files_message;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_protocol_files_messageHPP
