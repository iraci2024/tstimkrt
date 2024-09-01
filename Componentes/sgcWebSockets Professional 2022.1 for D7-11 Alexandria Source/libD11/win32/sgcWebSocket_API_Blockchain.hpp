// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_API_Blockchain.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_api_blockchainHPP
#define Sgcwebsocket_api_blockchainHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <sgcWebSocket_Classes.hpp>
#include <sgcJSON.hpp>
#include <sgcSocket_Classes.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_api_blockchain
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWS_API_Blockchain;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TsgcWSBitfinexNewTransactionEvent)(System::TObject* Sender, System::UnicodeString Transaction);

typedef void __fastcall (__closure *TsgcWSBitfinexNewBlockEvent)(System::TObject* Sender, System::UnicodeString Block);

class PASCALIMPLEMENTATION TsgcWS_API_Blockchain : public Sgcwebsocket_classes::TsgcWSAPI_client
{
	typedef Sgcwebsocket_classes::TsgcWSAPI_client inherited;
	
protected:
	virtual void __fastcall DoEventMessage(Sgcwebsocket_classes::TsgcWSConnection* aConnection, const System::UnicodeString Text);
	virtual System::UnicodeString __fastcall GetURL();
	
private:
	Sgcjson::TsgcJSON* FJSON;
	Sgcjson::TsgcJSON* __fastcall GetJSON();
	
protected:
	__property Sgcjson::TsgcJSON* JSON = {read=GetJSON};
	
public:
	void __fastcall Ping();
	void __fastcall SubscribeTransactions();
	void __fastcall UnSubscribeTransactions();
	void __fastcall SubscribeAddress(const System::UnicodeString aAddress);
	void __fastcall UnSubscribeAddress(const System::UnicodeString aAddress);
	void __fastcall SubscribeBlocks();
	void __fastcall UnSubscribeBlocks();
	__fastcall virtual TsgcWS_API_Blockchain(System::Classes::TComponent* aOwner);
	
private:
	System::Classes::TNotifyEvent FOnBlockchainPong;
	TsgcWSBitfinexNewBlockEvent FOnBlockchainNewBlock;
	TsgcWSBitfinexNewTransactionEvent FOnBlockchainNewTransaction;
	
protected:
	void __fastcall DoEventPong();
	void __fastcall DoEventNewTransaction(const System::UnicodeString aTransaction);
	void __fastcall DoEventNewBlock(System::UnicodeString aBlock);
	
__published:
	__property TsgcWSBitfinexNewBlockEvent OnBlockchainNewBlock = {read=FOnBlockchainNewBlock, write=FOnBlockchainNewBlock};
	__property TsgcWSBitfinexNewTransactionEvent OnBlockchainNewTransaction = {read=FOnBlockchainNewTransaction, write=FOnBlockchainNewTransaction};
	__property System::Classes::TNotifyEvent OnBlockchainPong = {read=FOnBlockchainPong, write=FOnBlockchainPong};
public:
	/* TsgcWSComponent_Base.Destroy */ inline __fastcall virtual ~TsgcWS_API_Blockchain() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_api_blockchain */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_API_BLOCKCHAIN)
using namespace Sgcwebsocket_api_blockchain;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_api_blockchainHPP
