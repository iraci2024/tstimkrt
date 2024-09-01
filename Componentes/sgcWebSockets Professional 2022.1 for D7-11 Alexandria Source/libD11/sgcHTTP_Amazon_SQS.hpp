// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcHTTP_Amazon_SQS.pas' rev: 35.00 (Windows)

#ifndef Sgchttp_amazon_sqsHPP
#define Sgchttp_amazon_sqsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <sgcHTTP_Amazon_AWS.hpp>
#include <sgcWebSocket_Classes_Queues.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgchttp_amazon_sqs
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcSQSAttribute;
class DELPHICLASS TsgcSQSAttributes;
class DELPHICLASS TsgcSQSMessageAttributeItem;
class DELPHICLASS TsgcSQSMessageAttributes;
class DELPHICLASS TsgcSQSSendMessageRequest;
class DELPHICLASS TsgcSQSSendMessageRequests;
class DELPHICLASS TsgcSQSSendMessageResponse;
class DELPHICLASS TsgcSQSSendMessageResponses;
class DELPHICLASS TsgcSQSSendMessage;
class DELPHICLASS TsgcSQSReceiveMessageRequest;
class DELPHICLASS TsgcSQSReceiveMessageResponse;
class DELPHICLASS TsgcSQSReceiveMessageResponses;
class DELPHICLASS TsgcSQSReceiveMessage;
class DELPHICLASS TsgcSQSDeleteMessageBatchRequestItem;
class DELPHICLASS TsgcSQSDeleteMessageBatchRequestItems;
class DELPHICLASS TsgcSQSChangeMessageVisibilityBatchRequest;
class DELPHICLASS TsgcSQSChangeMessageVisibilityBatchRequests;
class DELPHICLASS TsgcSQSTag;
class DELPHICLASS TsgcSQSTags;
class DELPHICLASS TsgcSQSPermission;
class DELPHICLASS TsgcSQSPermissions;
class DELPHICLASS TsgcSQSQueueItem;
class DELPHICLASS TsgcSQSQueue;
class DELPHICLASS TsgcSQSCreateQueue;
class DELPHICLASS TsgcHTTP_Amazon_AWS_SQS_Client;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TsgcOnAWSSQSResponse)(System::TObject* Sender, const System::UnicodeString RawResponse, bool &Handled);

typedef void __fastcall (__closure *TsgcOnAWSSQSBeforeRequest)(System::TObject* Sender, System::UnicodeString &URL, bool &Handled);

typedef void __fastcall (__closure *TsgcOnAWSSQSError)(System::TObject* Sender, const System::UnicodeString Error_Code, const System::UnicodeString Error_Description, const System::UnicodeString RawError);

enum DECLSPEC_DENUM TsgcAWSSQSAttribute : unsigned char { sqsatAll, sqsatPolicy, sqsatVisibilityTimeout, sqsatMaximumMessageSize, sqsatMessageRetentionPeriod, sqsatApproximateNumberOfMessages, sqsatApproximateNumberOfMessagesNotVisible, sqsatCreatedTimestamp, sqsatLastModifiedTimestamp, sqsatQueueArn, sqsatApproximateNumberOfMessagesDelayed, sqsatDelaySeconds, sqsatReceiveMessageWaitTimeSeconds, sqsatRedrivePolicy, sqsatFifoQueue, sqsatContentBasedDeduplication, sqsatKmsMasterKeyId, sqsatKmsDataKeyReusePeriodSeconds };

typedef System::DynamicArray<TsgcAWSSQSAttribute> TsgcAWSSQSAttributes;

enum DECLSPEC_DENUM TsgcAWSSQSActionName : unsigned char { sqsanAddPermission, sqsanChangeMessageVisibility, sqsanDeleteMessage, sqsanDeleteQueue, sqsanGetQueueAttributes, sqsanGetQueueUrl, sqsanListDeadLetterSourceQueues, sqsanPurgeQueue, sqsanReceiveMessage, sqsanRemovePermission, sqsanSendMessage, sqsanSetQueueAttributes };

typedef System::DynamicArray<TsgcAWSSQSActionName> TsgcAWSSQSActionNames;

class PASCALIMPLEMENTATION TsgcSQSAttribute : public Sgcwebsocket_classes_queues::TsgcQueueItemBase
{
	typedef Sgcwebsocket_classes_queues::TsgcQueueItemBase inherited;
	
private:
	System::UnicodeString FAttributeValue;
	System::UnicodeString __fastcall GetAttributeName();
	void __fastcall SetAttributeName(const System::UnicodeString Value);
	
public:
	__property System::UnicodeString AttributeName = {read=GetAttributeName, write=SetAttributeName};
	__property System::UnicodeString AttributeValue = {read=FAttributeValue, write=FAttributeValue};
public:
	/* TsgcQueueItemBase.Create */ inline __fastcall virtual TsgcSQSAttribute() : Sgcwebsocket_classes_queues::TsgcQueueItemBase() { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcSQSAttribute() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSQSAttributes : public Sgcwebsocket_classes_queues::TsgcQueue
{
	typedef Sgcwebsocket_classes_queues::TsgcQueue inherited;
	
public:
	void __fastcall AddSQSAttribute(const System::UnicodeString aName, const System::UnicodeString aValue)/* overload */;
	void __fastcall AddSQSAttribute(const TsgcAWSSQSAttribute aAttribute, System::UnicodeString aValue)/* overload */;
public:
	/* TsgcQueueCommon.Create */ inline __fastcall virtual TsgcSQSAttributes() : Sgcwebsocket_classes_queues::TsgcQueue() { }
	/* TsgcQueueCommon.Destroy */ inline __fastcall virtual ~TsgcSQSAttributes() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcSQSMessageAttributeItem : public Sgcwebsocket_classes_queues::TsgcQueueItemBase
{
	typedef Sgcwebsocket_classes_queues::TsgcQueueItemBase inherited;
	
private:
	System::UnicodeString FAttributeType;
	System::UnicodeString FAttributeValue;
	System::UnicodeString __fastcall GetAttributeName();
	void __fastcall SetAttributeName(const System::UnicodeString Value);
	
public:
	__property System::UnicodeString AttributeName = {read=GetAttributeName, write=SetAttributeName};
	__property System::UnicodeString AttributeType = {read=FAttributeType, write=FAttributeType};
	__property System::UnicodeString AttributeValue = {read=FAttributeValue, write=FAttributeValue};
public:
	/* TsgcQueueItemBase.Create */ inline __fastcall virtual TsgcSQSMessageAttributeItem() : Sgcwebsocket_classes_queues::TsgcQueueItemBase() { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcSQSMessageAttributeItem() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSQSMessageAttributes : public Sgcwebsocket_classes_queues::TsgcQueue
{
	typedef Sgcwebsocket_classes_queues::TsgcQueue inherited;
	
public:
	void __fastcall AddSQSAttribute(const System::UnicodeString aName, const System::UnicodeString aType, const System::UnicodeString aValue);
public:
	/* TsgcQueueCommon.Create */ inline __fastcall virtual TsgcSQSMessageAttributes() : Sgcwebsocket_classes_queues::TsgcQueue() { }
	/* TsgcQueueCommon.Destroy */ inline __fastcall virtual ~TsgcSQSMessageAttributes() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcSQSSendMessageRequest : public Sgcwebsocket_classes_queues::TsgcQueueItemBase
{
	typedef Sgcwebsocket_classes_queues::TsgcQueueItemBase inherited;
	
private:
	int FDelaySeconds;
	System::UnicodeString FMessageBody;
	System::UnicodeString FMessageDeduplicationId;
	System::UnicodeString FMessageGroupId;
	TsgcSQSMessageAttributes* FMessageAttributes;
	TsgcSQSMessageAttributes* FMessageSystemAttributes;
	TsgcSQSMessageAttributes* __fastcall GetMessageAttributes();
	TsgcSQSMessageAttributes* __fastcall GetMessageSystemAttributes();
	void __fastcall SetMessageAttributes(TsgcSQSMessageAttributes* const Value);
	void __fastcall SetMessageSystemAttributes(TsgcSQSMessageAttributes* const Value);
	
public:
	__fastcall virtual TsgcSQSSendMessageRequest();
	__fastcall virtual ~TsgcSQSSendMessageRequest();
	__property int DelaySeconds = {read=FDelaySeconds, write=FDelaySeconds, nodefault};
	__property TsgcSQSMessageAttributes* MessageAttributes = {read=GetMessageAttributes, write=SetMessageAttributes};
	__property TsgcSQSMessageAttributes* MessageSystemAttributes = {read=GetMessageSystemAttributes, write=SetMessageSystemAttributes};
	__property System::UnicodeString MessageBody = {read=FMessageBody, write=FMessageBody};
	__property System::UnicodeString MessageDeduplicationId = {read=FMessageDeduplicationId, write=FMessageDeduplicationId};
	__property System::UnicodeString MessageGroupId = {read=FMessageGroupId, write=FMessageGroupId};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSQSSendMessageRequests : public Sgcwebsocket_classes_queues::TsgcQueue
{
	typedef Sgcwebsocket_classes_queues::TsgcQueue inherited;
	
public:
	/* TsgcQueueCommon.Create */ inline __fastcall virtual TsgcSQSSendMessageRequests() : Sgcwebsocket_classes_queues::TsgcQueue() { }
	/* TsgcQueueCommon.Destroy */ inline __fastcall virtual ~TsgcSQSSendMessageRequests() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcSQSSendMessageResponse : public Sgcwebsocket_classes_queues::TsgcQueueItemBase
{
	typedef Sgcwebsocket_classes_queues::TsgcQueueItemBase inherited;
	
private:
	System::UnicodeString FMD5OfMessageAttributes;
	System::UnicodeString FMD5OfMessageBody;
	System::UnicodeString FMD5OfMessageSystemAttributes;
	System::UnicodeString FMessageId;
	System::UnicodeString FSequenceNumber;
	
public:
	__property System::UnicodeString MD5OfMessageAttributes = {read=FMD5OfMessageAttributes, write=FMD5OfMessageAttributes};
	__property System::UnicodeString MD5OfMessageBody = {read=FMD5OfMessageBody, write=FMD5OfMessageBody};
	__property System::UnicodeString MD5OfMessageSystemAttributes = {read=FMD5OfMessageSystemAttributes, write=FMD5OfMessageSystemAttributes};
	__property System::UnicodeString MessageId = {read=FMessageId, write=FMessageId};
	__property System::UnicodeString SequenceNumber = {read=FSequenceNumber, write=FSequenceNumber};
public:
	/* TsgcQueueItemBase.Create */ inline __fastcall virtual TsgcSQSSendMessageResponse() : Sgcwebsocket_classes_queues::TsgcQueueItemBase() { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcSQSSendMessageResponse() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSQSSendMessageResponses : public Sgcwebsocket_classes_queues::TsgcQueue
{
	typedef Sgcwebsocket_classes_queues::TsgcQueue inherited;
	
public:
	/* TsgcQueueCommon.Create */ inline __fastcall virtual TsgcSQSSendMessageResponses() : Sgcwebsocket_classes_queues::TsgcQueue() { }
	/* TsgcQueueCommon.Destroy */ inline __fastcall virtual ~TsgcSQSSendMessageResponses() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcSQSSendMessage : public Sgcwebsocket_classes_queues::TsgcQueueItemBase
{
	typedef Sgcwebsocket_classes_queues::TsgcQueueItemBase inherited;
	
private:
	TsgcSQSSendMessageRequest* FRequest;
	TsgcSQSSendMessageResponse* FResponse;
	TsgcSQSSendMessageRequest* __fastcall GetRequest();
	TsgcSQSSendMessageResponse* __fastcall GetResponse();
	
public:
	__fastcall virtual ~TsgcSQSSendMessage();
	__property TsgcSQSSendMessageRequest* Request = {read=GetRequest, write=FRequest};
	__property TsgcSQSSendMessageResponse* Response = {read=GetResponse, write=FResponse};
public:
	/* TsgcQueueItemBase.Create */ inline __fastcall virtual TsgcSQSSendMessage() : Sgcwebsocket_classes_queues::TsgcQueueItemBase() { }
	
};


class PASCALIMPLEMENTATION TsgcSQSReceiveMessageRequest : public Sgcwebsocket_classes_queues::TsgcQueueItemBase
{
	typedef Sgcwebsocket_classes_queues::TsgcQueueItemBase inherited;
	
private:
	TsgcAWSSQSAttributes FAttributes;
	int FMaxNumberOfMessages;
	System::Classes::TStringList* FMessageAttributeName;
	System::UnicodeString FReceiveRequestAttemptId;
	int FVisibilityTimeout;
	int FWaitTimeSeconds;
	System::Classes::TStringList* __fastcall GetMessageAttributeName();
	
public:
	__fastcall virtual TsgcSQSReceiveMessageRequest();
	__fastcall virtual ~TsgcSQSReceiveMessageRequest();
	__property TsgcAWSSQSAttributes Attributes = {read=FAttributes, write=FAttributes};
	__property int MaxNumberOfMessages = {read=FMaxNumberOfMessages, write=FMaxNumberOfMessages, nodefault};
	__property System::Classes::TStringList* MessageAttributeName = {read=GetMessageAttributeName, write=FMessageAttributeName};
	__property System::UnicodeString ReceiveRequestAttemptId = {read=FReceiveRequestAttemptId, write=FReceiveRequestAttemptId};
	__property int VisibilityTimeout = {read=FVisibilityTimeout, write=FVisibilityTimeout, nodefault};
	__property int WaitTimeSeconds = {read=FWaitTimeSeconds, write=FWaitTimeSeconds, nodefault};
};


class PASCALIMPLEMENTATION TsgcSQSReceiveMessageResponse : public Sgcwebsocket_classes_queues::TsgcQueueItemBase
{
	typedef Sgcwebsocket_classes_queues::TsgcQueueItemBase inherited;
	
private:
	TsgcSQSAttributes* FAttributes;
	System::UnicodeString FBody;
	System::UnicodeString FMD5OfBody;
	System::UnicodeString FMD5OfMessageAttributes;
	TsgcSQSMessageAttributes* FMessageAttributes;
	System::UnicodeString FReceiptHandle;
	TsgcSQSAttributes* __fastcall GetAttributes();
	TsgcSQSMessageAttributes* __fastcall GetMessageAttributes();
	System::UnicodeString __fastcall GetMessageId();
	void __fastcall SetMessageId(const System::UnicodeString Value);
	void __fastcall SetReceiptHandle(const System::UnicodeString Value);
	
public:
	__fastcall virtual ~TsgcSQSReceiveMessageResponse();
	__property TsgcSQSAttributes* Attributes = {read=GetAttributes, write=FAttributes};
	__property System::UnicodeString Body = {read=FBody, write=FBody};
	__property System::UnicodeString MD5OfBody = {read=FMD5OfBody, write=FMD5OfBody};
	__property System::UnicodeString MD5OfMessageAttributes = {read=FMD5OfMessageAttributes, write=FMD5OfMessageAttributes};
	__property TsgcSQSMessageAttributes* MessageAttributes = {read=GetMessageAttributes, write=FMessageAttributes};
	__property System::UnicodeString MessageId = {read=GetMessageId, write=SetMessageId};
	__property System::UnicodeString ReceiptHandle = {read=FReceiptHandle, write=SetReceiptHandle};
public:
	/* TsgcQueueItemBase.Create */ inline __fastcall virtual TsgcSQSReceiveMessageResponse() : Sgcwebsocket_classes_queues::TsgcQueueItemBase() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSQSReceiveMessageResponses : public Sgcwebsocket_classes_queues::TsgcQueue
{
	typedef Sgcwebsocket_classes_queues::TsgcQueue inherited;
	
public:
	/* TsgcQueueCommon.Create */ inline __fastcall virtual TsgcSQSReceiveMessageResponses() : Sgcwebsocket_classes_queues::TsgcQueue() { }
	/* TsgcQueueCommon.Destroy */ inline __fastcall virtual ~TsgcSQSReceiveMessageResponses() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcSQSReceiveMessage : public Sgcwebsocket_classes_queues::TsgcQueueItemBase
{
	typedef Sgcwebsocket_classes_queues::TsgcQueueItemBase inherited;
	
private:
	TsgcSQSReceiveMessageRequest* FRequest;
	TsgcSQSReceiveMessageResponse* FResponse;
	TsgcSQSReceiveMessageRequest* __fastcall GetRequest();
	TsgcSQSReceiveMessageResponse* __fastcall GetResponse();
	
public:
	__fastcall virtual ~TsgcSQSReceiveMessage();
	__property TsgcSQSReceiveMessageRequest* Request = {read=GetRequest, write=FRequest};
	__property TsgcSQSReceiveMessageResponse* Response = {read=GetResponse, write=FResponse};
public:
	/* TsgcQueueItemBase.Create */ inline __fastcall virtual TsgcSQSReceiveMessage() : Sgcwebsocket_classes_queues::TsgcQueueItemBase() { }
	
};


class PASCALIMPLEMENTATION TsgcSQSDeleteMessageBatchRequestItem : public Sgcwebsocket_classes_queues::TsgcQueueItemBase
{
	typedef Sgcwebsocket_classes_queues::TsgcQueueItemBase inherited;
	
private:
	System::UnicodeString FReceiptHandle;
	
public:
	__property System::UnicodeString ReceiptHandle = {read=FReceiptHandle, write=FReceiptHandle};
public:
	/* TsgcQueueItemBase.Create */ inline __fastcall virtual TsgcSQSDeleteMessageBatchRequestItem() : Sgcwebsocket_classes_queues::TsgcQueueItemBase() { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcSQSDeleteMessageBatchRequestItem() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSQSDeleteMessageBatchRequestItems : public Sgcwebsocket_classes_queues::TsgcQueue
{
	typedef Sgcwebsocket_classes_queues::TsgcQueue inherited;
	
public:
	void __fastcall AddSQSDeleteMessage(const System::UnicodeString aId, const System::UnicodeString aReceiptHandle);
public:
	/* TsgcQueueCommon.Create */ inline __fastcall virtual TsgcSQSDeleteMessageBatchRequestItems() : Sgcwebsocket_classes_queues::TsgcQueue() { }
	/* TsgcQueueCommon.Destroy */ inline __fastcall virtual ~TsgcSQSDeleteMessageBatchRequestItems() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcSQSChangeMessageVisibilityBatchRequest : public Sgcwebsocket_classes_queues::TsgcQueueItemBase
{
	typedef Sgcwebsocket_classes_queues::TsgcQueueItemBase inherited;
	
private:
	System::UnicodeString FReceiptHandle;
	int FVisibilityTimeout;
	
public:
	__property System::UnicodeString ReceiptHandle = {read=FReceiptHandle, write=FReceiptHandle};
	__property int VisibilityTimeout = {read=FVisibilityTimeout, write=FVisibilityTimeout, nodefault};
public:
	/* TsgcQueueItemBase.Create */ inline __fastcall virtual TsgcSQSChangeMessageVisibilityBatchRequest() : Sgcwebsocket_classes_queues::TsgcQueueItemBase() { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcSQSChangeMessageVisibilityBatchRequest() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSQSChangeMessageVisibilityBatchRequests : public Sgcwebsocket_classes_queues::TsgcQueue
{
	typedef Sgcwebsocket_classes_queues::TsgcQueue inherited;
	
public:
	void __fastcall AddSQSChangeVisibility(const System::UnicodeString aId, const System::UnicodeString aReceiptHandle, int aVisibilityTimeout);
public:
	/* TsgcQueueCommon.Create */ inline __fastcall virtual TsgcSQSChangeMessageVisibilityBatchRequests() : Sgcwebsocket_classes_queues::TsgcQueue() { }
	/* TsgcQueueCommon.Destroy */ inline __fastcall virtual ~TsgcSQSChangeMessageVisibilityBatchRequests() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcSQSTag : public Sgcwebsocket_classes_queues::TsgcQueueItemBase
{
	typedef Sgcwebsocket_classes_queues::TsgcQueueItemBase inherited;
	
private:
	System::UnicodeString FValue;
	System::UnicodeString __fastcall GetKey();
	void __fastcall SetKey(const System::UnicodeString Value);
	
public:
	__property System::UnicodeString Key = {read=GetKey, write=SetKey};
	__property System::UnicodeString Value = {read=FValue, write=FValue};
public:
	/* TsgcQueueItemBase.Create */ inline __fastcall virtual TsgcSQSTag() : Sgcwebsocket_classes_queues::TsgcQueueItemBase() { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcSQSTag() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSQSTags : public Sgcwebsocket_classes_queues::TsgcQueue
{
	typedef Sgcwebsocket_classes_queues::TsgcQueue inherited;
	
public:
	void __fastcall AddSQSTag(const System::UnicodeString Key, const System::UnicodeString Value);
public:
	/* TsgcQueueCommon.Create */ inline __fastcall virtual TsgcSQSTags() : Sgcwebsocket_classes_queues::TsgcQueue() { }
	/* TsgcQueueCommon.Destroy */ inline __fastcall virtual ~TsgcSQSTags() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcSQSPermission : public Sgcwebsocket_classes_queues::TsgcQueueItemBase
{
	typedef Sgcwebsocket_classes_queues::TsgcQueueItemBase inherited;
	
private:
	System::UnicodeString FActionName;
	System::UnicodeString FAWSAccountId;
	
public:
	__property System::UnicodeString ActionName = {read=FActionName, write=FActionName};
	__property System::UnicodeString AWSAccountId = {read=FAWSAccountId, write=FAWSAccountId};
public:
	/* TsgcQueueItemBase.Create */ inline __fastcall virtual TsgcSQSPermission() : Sgcwebsocket_classes_queues::TsgcQueueItemBase() { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcSQSPermission() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSQSPermissions : public Sgcwebsocket_classes_queues::TsgcQueue
{
	typedef Sgcwebsocket_classes_queues::TsgcQueue inherited;
	
public:
	void __fastcall AddSQSPermission(const System::UnicodeString ActionName, const System::UnicodeString AWSAccountId)/* overload */;
	void __fastcall AddSQSPermission(const TsgcAWSSQSActionName ActionName, const System::UnicodeString AWSAccountId)/* overload */;
public:
	/* TsgcQueueCommon.Create */ inline __fastcall virtual TsgcSQSPermissions() : Sgcwebsocket_classes_queues::TsgcQueue() { }
	/* TsgcQueueCommon.Destroy */ inline __fastcall virtual ~TsgcSQSPermissions() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcSQSQueueItem : public Sgcwebsocket_classes_queues::TsgcQueueItemBase
{
	typedef Sgcwebsocket_classes_queues::TsgcQueueItemBase inherited;
	
private:
	System::UnicodeString FURL;
	
public:
	__property System::UnicodeString URL = {read=FURL, write=FURL};
public:
	/* TsgcQueueItemBase.Create */ inline __fastcall virtual TsgcSQSQueueItem() : Sgcwebsocket_classes_queues::TsgcQueueItemBase() { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcSQSQueueItem() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSQSQueue : public Sgcwebsocket_classes_queues::TsgcQueue
{
	typedef Sgcwebsocket_classes_queues::TsgcQueue inherited;
	
public:
	void __fastcall AddSQSQueue(const System::UnicodeString aQueue, const System::UnicodeString aURL);
	TsgcSQSQueueItem* __fastcall GetSQSQueue(const System::UnicodeString aQueue);
	System::UnicodeString __fastcall GetURLFromQueue(const System::UnicodeString aQueueName);
public:
	/* TsgcQueueCommon.Create */ inline __fastcall virtual TsgcSQSQueue() : Sgcwebsocket_classes_queues::TsgcQueue() { }
	/* TsgcQueueCommon.Destroy */ inline __fastcall virtual ~TsgcSQSQueue() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcSQSCreateQueue : public Sgcwebsocket_classes_queues::TsgcQueueItemBase
{
	typedef Sgcwebsocket_classes_queues::TsgcQueueItemBase inherited;
	
private:
	TsgcSQSAttributes* FAttributes;
	TsgcSQSTags* FTags;
	TsgcSQSAttributes* __fastcall GetAttributes();
	System::UnicodeString __fastcall GetQueueName();
	TsgcSQSTags* __fastcall GetTags();
	void __fastcall SetQueueName(const System::UnicodeString Value);
	
public:
	__fastcall virtual ~TsgcSQSCreateQueue();
	__property TsgcSQSAttributes* Attributes = {read=GetAttributes, write=FAttributes};
	__property System::UnicodeString QueueName = {read=GetQueueName, write=SetQueueName};
	__property TsgcSQSTags* Tags = {read=GetTags, write=FTags};
public:
	/* TsgcQueueItemBase.Create */ inline __fastcall virtual TsgcSQSCreateQueue() : Sgcwebsocket_classes_queues::TsgcQueueItemBase() { }
	
};


class PASCALIMPLEMENTATION TsgcHTTP_Amazon_AWS_SQS_Client : public Sgchttp_amazon_aws::TsgcHTTP_Amazon_AWS_Client
{
	typedef Sgchttp_amazon_aws::TsgcHTTP_Amazon_AWS_Client inherited;
	
protected:
	virtual bool __fastcall DoBeforeGET_Request(System::UnicodeString &aURL);
	
private:
	System::Classes::TStringList* FParams;
	void __fastcall AddParam(const System::UnicodeString aParam, const System::UnicodeString aValue)/* overload */;
	void __fastcall AddParam(const System::UnicodeString aParam, int aValue)/* overload */;
	void __fastcall ClearParams(const System::UnicodeString aAction);
	System::Classes::TStringList* __fastcall GetParams();
	System::UnicodeString __fastcall GetFullURL(const System::UnicodeString aEndPoint = System::UnicodeString());
	
protected:
	__property System::Classes::TStringList* Params = {read=GetParams};
	
public:
	__fastcall virtual TsgcHTTP_Amazon_AWS_SQS_Client(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcHTTP_Amazon_AWS_SQS_Client();
	
private:
	TsgcSQSQueue* FQueues;
	TsgcSQSQueue* __fastcall GetQueues();
	
protected:
	__property TsgcSQSQueue* Queues = {read=GetQueues, write=FQueues};
	
private:
	void __fastcall DoProcessError(const System::UnicodeString aError);
	
protected:
	virtual bool __fastcall DoSendMessage(const System::UnicodeString aURL, TsgcSQSSendMessageRequest* const aRequest, TsgcSQSSendMessageResponse* &aResponse);
	virtual bool __fastcall DoSendMessageBatch(const System::UnicodeString aURL, TsgcSQSSendMessageRequests* const aRequests, TsgcSQSSendMessageResponses* &aResponses);
	virtual bool __fastcall DoDeleteMessage(const System::UnicodeString aURL, const System::UnicodeString aReceiptHandle);
	virtual void __fastcall DoDeleteMessageBatch(const System::UnicodeString aURL, TsgcSQSDeleteMessageBatchRequestItems* aDeleteBatchItems);
	virtual bool __fastcall DoChangeMessageVisibility(const System::UnicodeString aURL, const System::UnicodeString aReceiptHandle, int VisibilityTimeout);
	bool __fastcall DoChangeMessageVisibilityBatch(const System::UnicodeString aURL, TsgcSQSChangeMessageVisibilityBatchRequests* const aRequests, System::Classes::TStringList* &aResponseIds);
	virtual bool __fastcall DoReceiveMessage(const System::UnicodeString aURL, TsgcSQSReceiveMessageRequest* const aRequest, TsgcSQSReceiveMessageResponses* &aResponses);
	virtual System::UnicodeString __fastcall DoGetQueueURL(const System::UnicodeString aQueueName);
	virtual System::UnicodeString __fastcall DoCreateQueue(TsgcSQSCreateQueue* const aQueue);
	virtual bool __fastcall DoDeleteQueue(const System::UnicodeString aURL);
	virtual bool __fastcall DoListQueues(System::Classes::TStringList* &Queues, const System::UnicodeString aQueueNamePrefix);
	virtual bool __fastcall DoListQueueTags(const System::UnicodeString aURL, TsgcSQSTags* &Tags);
	virtual bool __fastcall DoGetQueueAttributes(const System::UnicodeString aURL, TsgcSQSAttributes* &Attributes);
	virtual bool __fastcall DoSetQueueAttributes(const System::UnicodeString aURL, TsgcSQSAttributes* const aAttributes);
	virtual bool __fastcall DoPurgueQueue(const System::UnicodeString aURL);
	virtual bool __fastcall DoTagQueue(const System::UnicodeString aURL, TsgcSQSTags* aTags);
	virtual bool __fastcall DoUnTagQueue(const System::UnicodeString aURL, System::Classes::TStringList* aTags);
	virtual bool __fastcall DoAddPermission(const System::UnicodeString aURL, const System::UnicodeString aLabel, TsgcSQSPermissions* const aPermissions);
	virtual bool __fastcall DoRemovePermission(const System::UnicodeString aURL, const System::UnicodeString aLabel);
	bool __fastcall DoListDeadLetterSourceQueues(const System::UnicodeString aURL, System::Classes::TStringList* aQueueURLs);
	
public:
	bool __fastcall SendMessage(const System::UnicodeString aQueueName, const System::UnicodeString aMessageBody, int aDelaySeconds = 0x0)/* overload */;
	bool __fastcall SendMessage(const System::UnicodeString aQueueName, TsgcSQSSendMessageRequest* aRequest, TsgcSQSSendMessageResponse* &aResponse)/* overload */;
	void __fastcall SendMessageBatch(const System::UnicodeString aQueueName, TsgcSQSSendMessageRequests* aRequests, TsgcSQSSendMessageResponses* &aResponses);
	bool __fastcall ReceiveMessage(const System::UnicodeString aQueueName, TsgcSQSReceiveMessageResponses* &aResponses)/* overload */;
	bool __fastcall ReceiveMessage(const System::UnicodeString aQueueName, TsgcSQSReceiveMessageRequest* const aRequest, TsgcSQSReceiveMessageResponses* &aResponses)/* overload */;
	bool __fastcall ChangeMessageVisibility(const System::UnicodeString aQueueName, const System::UnicodeString aReceiptHandle, int VisibilityTimeout);
	bool __fastcall ChangeMessageVisibilityBatch(const System::UnicodeString aQueueName, TsgcSQSChangeMessageVisibilityBatchRequests* const aRequests, System::Classes::TStringList* &aResponseIds);
	bool __fastcall DeleteMessage(const System::UnicodeString aQueueName, const System::UnicodeString aReceiptHandle);
	void __fastcall DeleteMessageBatch(const System::UnicodeString aQueueName, TsgcSQSDeleteMessageBatchRequestItems* aDeleteBatchItems);
	System::UnicodeString __fastcall GetQueueURL(const System::UnicodeString aQueueName);
	System::UnicodeString __fastcall CreateQueue(const System::UnicodeString aQueueName)/* overload */;
	System::UnicodeString __fastcall CreateQueue(TsgcSQSCreateQueue* const aQueue)/* overload */;
	bool __fastcall DeleteQueue(const System::UnicodeString aQueueName);
	bool __fastcall ListQueues(System::Classes::TStringList* &Queues, const System::UnicodeString aQueueNamePrefix = System::UnicodeString());
	bool __fastcall ListQueueTags(const System::UnicodeString aQueueName, TsgcSQSTags* &Tags);
	bool __fastcall GetQueueAttributes(const System::UnicodeString aQueueName, TsgcSQSAttributes* &Attributes);
	bool __fastcall SetQueueAttributes(const System::UnicodeString aQueueName, TsgcSQSAttributes* const aAttributes);
	bool __fastcall PurgueQueue(const System::UnicodeString aQueueName);
	bool __fastcall TagQueue(const System::UnicodeString aQueueName, TsgcSQSTags* aTags)/* overload */;
	bool __fastcall TagQueue(const System::UnicodeString aQueueName, System::UnicodeString aKey, System::UnicodeString aValue)/* overload */;
	bool __fastcall UnTagQueue(const System::UnicodeString aQueueName, System::Classes::TStringList* aTags)/* overload */;
	bool __fastcall UnTagQueue(const System::UnicodeString aQueueName, System::UnicodeString aKey)/* overload */;
	bool __fastcall AddPermission(const System::UnicodeString aQueueName, const System::UnicodeString aLabel, TsgcSQSPermissions* const aPermissions);
	bool __fastcall RemovePermission(const System::UnicodeString aQueueName, const System::UnicodeString aLabel);
	bool __fastcall ListDeadLetterSourceQueues(const System::UnicodeString aQueueName, System::Classes::TStringList* aQueueURLs);
	
private:
	TsgcOnAWSSQSBeforeRequest FOnSQSBeforeRequest;
	TsgcOnAWSSQSError FOnSQSError;
	TsgcOnAWSSQSResponse FOnSQSResponse;
	
protected:
	virtual bool __fastcall DoSQSResponse(const System::UnicodeString aRawResponse);
	virtual bool __fastcall DoSQSBeforeRequest(System::UnicodeString &aURL);
	virtual void __fastcall DoSQSErrorEvent(const System::UnicodeString aError_Code, const System::UnicodeString aError_Description, const System::UnicodeString aRawError);
	
public:
	__property TsgcOnAWSSQSResponse OnSQSResponse = {read=FOnSQSResponse, write=FOnSQSResponse};
	__property TsgcOnAWSSQSError OnSQSError = {read=FOnSQSError, write=FOnSQSError};
	__property TsgcOnAWSSQSBeforeRequest OnSQSBeforeRequest = {read=FOnSQSBeforeRequest, write=FOnSQSBeforeRequest};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgchttp_amazon_sqs */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCHTTP_AMAZON_SQS)
using namespace Sgchttp_amazon_sqs;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgchttp_amazon_sqsHPP
