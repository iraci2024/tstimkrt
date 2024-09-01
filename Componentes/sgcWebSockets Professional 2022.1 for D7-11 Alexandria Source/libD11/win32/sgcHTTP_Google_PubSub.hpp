// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcHTTP_Google_PubSub.pas' rev: 35.00 (Windows)

#ifndef Sgchttp_google_pubsubHPP
#define Sgchttp_google_pubsubHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <sgcHTTP_Google_Cloud.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgchttp_google_pubsub
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcHTTP_Google_Cloud_PubSub_Client;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TsgcHTTP_Google_Cloud_PubSub_Client : public Sgchttp_google_cloud::TsgcHTTP_Google_Cloud_Client
{
	typedef Sgchttp_google_cloud::TsgcHTTP_Google_Cloud_Client inherited;
	
protected:
	virtual System::UnicodeString __fastcall DoGetScope();
	
public:
	__fastcall virtual TsgcHTTP_Google_Cloud_PubSub_Client(System::Classes::TComponent* aOwner);
	System::UnicodeString __fastcall GetIamPolicy(const System::UnicodeString aResource);
	System::UnicodeString __fastcall SetIamPolicy(const System::UnicodeString aResource);
	System::UnicodeString __fastcall TestIamPermissions(const System::UnicodeString aResource, System::Classes::TStrings* aPermissions);
	System::UnicodeString __fastcall CreateSnapshot(const System::UnicodeString aProject, const System::UnicodeString aSnapshot, const System::UnicodeString aSubscription, System::Classes::TStrings* const aLabels = (System::Classes::TStrings*)(0x0));
	System::UnicodeString __fastcall DeleteSnapshot(const System::UnicodeString aProject, const System::UnicodeString aSnapshot);
	System::UnicodeString __fastcall ListSnapshots(const System::UnicodeString aProject, int aPageSize = 0x0, System::UnicodeString aPageToken = System::UnicodeString());
	System::UnicodeString __fastcall Acknowledge(const System::UnicodeString aProject, const System::UnicodeString aSubscription, const System::UnicodeString aAckId);
	System::UnicodeString __fastcall CreateSubscription(const System::UnicodeString aProject, const System::UnicodeString aSubscription, const System::UnicodeString aTopic, const System::UnicodeString aPushEndPoint = System::UnicodeString(), int aAckDeadlineSeconds = 0x0, bool aRetainAckedMessages = false, System::UnicodeString aMessageRetentionDuration = L"604800s", System::Classes::TStrings* const aLabels = (System::Classes::TStrings*)(0x0), System::UnicodeString aExpirationPolicy = L"2678400s");
	System::UnicodeString __fastcall DeleteSubscripton(const System::UnicodeString aProject, const System::UnicodeString aSubscription);
	System::UnicodeString __fastcall GetSubscription(const System::UnicodeString aProject, const System::UnicodeString aSubscription);
	System::UnicodeString __fastcall ListSubscriptions(const System::UnicodeString aProject, int aPageSize = 0x0, System::UnicodeString aPageToken = System::UnicodeString());
	System::UnicodeString __fastcall ModifyAckDeadlineSubscription(const System::UnicodeString aProject, const System::UnicodeString aSubscription, System::Classes::TStrings* aAckIds, int aAckDeadlineSeconds = 0x0);
	System::UnicodeString __fastcall ModifyPushConfigSubscription(const System::UnicodeString aProject, const System::UnicodeString aSubscription, const System::UnicodeString aPushEndPoint = System::UnicodeString(), System::Classes::TStrings* const aAttributes = (System::Classes::TStrings*)(0x0), const System::UnicodeString aServiceAccountEmail = System::UnicodeString(), System::UnicodeString aAudience = System::UnicodeString());
	System::UnicodeString __fastcall Pull(const System::UnicodeString aProject, const System::UnicodeString aSubscription, int aMaxMessages = 0x7fffffff);
	System::UnicodeString __fastcall Seek(const System::UnicodeString aProject, const System::UnicodeString aSubscription, System::UnicodeString aTimeUTC, const System::UnicodeString aSnapshot);
	System::UnicodeString __fastcall CreateTopic(const System::UnicodeString aProject, const System::UnicodeString aTopic);
	System::UnicodeString __fastcall DeleteTopic(const System::UnicodeString aProject, const System::UnicodeString aTopic);
	System::UnicodeString __fastcall GetTopic(const System::UnicodeString aProject, const System::UnicodeString aTopic);
	System::UnicodeString __fastcall ListTopics(const System::UnicodeString aProject, int aPageSize = 0x0, System::UnicodeString aPageToken = System::UnicodeString());
	System::UnicodeString __fastcall Publish(const System::UnicodeString aProject, const System::UnicodeString aTopic, const System::UnicodeString aMessage, System::Classes::TStrings* const aAttributes = (System::Classes::TStrings*)(0x0), const System::UnicodeString aOrderingKey = System::UnicodeString());
	System::UnicodeString __fastcall ListTopicSubscriptions(const System::UnicodeString aProject, const System::UnicodeString aTopic, int aPageSize = 0x0, System::UnicodeString aPageToken = System::UnicodeString());
public:
	/* TsgcHTTP_Google_Cloud_Client.Destroy */ inline __fastcall virtual ~TsgcHTTP_Google_Cloud_PubSub_Client() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgchttp_google_pubsub */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCHTTP_GOOGLE_PUBSUB)
using namespace Sgchttp_google_pubsub;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgchttp_google_pubsubHPP
