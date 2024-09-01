// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcAMQP_Const.pas' rev: 35.00 (Windows)

#ifndef Sgcamqp_constHPP
#define Sgcamqp_constHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcamqp_const
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
static const System::Int8 CS_AMQP_MAJOR_VERSION = System::Int8(0x0);
static const System::Int8 CS_AMQP_MINOR_VERSION = System::Int8(0x9);
static const System::Int8 CS_AMQP_RELEASE_VERSION = System::Int8(0x1);
static const System::Int8 CS_AMQP_FRAME_HEADER_LENGTH = System::Int8(0x7);
static const System::Int8 CS_AMQP_FRAME_END_LENGTH = System::Int8(0x1);
static const System::Int8 CS_AMQP_FRAME_HEADER_SIZE = System::Int8(0x8);
static const System::Int8 CS_AMQP_FRAME_METHOD = System::Int8(0x1);
static const System::Int8 CS_AMQP_FRAME_HEADER = System::Int8(0x2);
static const System::Int8 CS_AMQP_FRAME_BODY = System::Int8(0x3);
static const System::Int8 CS_AMQP_FRAME_HEARTBEAT = System::Int8(0x8);
static const System::Word CS_AMQP_FRAME_MIN_SIZE = System::Word(0x1000);
static const System::Byte CS_AMQP_FRAME_END = System::Byte(0xce);
static const System::Byte CS_AMQP_REPLY_SUCCESS = System::Byte(0xc8);
static const System::Word CS_AMQP_ERROR_CONTENT_TOO_LARGE = System::Word(0x137);
static const System::Word CS_AMQP_ERROR_NO_CONSUMERS = System::Word(0x139);
static const System::Word CS_AMQP_ERROR_CONNECTION_FORCED = System::Word(0x140);
static const System::Word CS_AMQP_ERROR_INVALID_PATH = System::Word(0x192);
static const System::Word CS_AMQP_ERROR_ACCESS_REFUSED = System::Word(0x193);
static const System::Word CS_AMQP_ERROR_NOT_FOUND = System::Word(0x194);
static const System::Word CS_AMQP_ERROR_RESOURCE_LOCKED = System::Word(0x195);
static const System::Word CS_AMQP_ERROR_PRECONDITION_FAILED = System::Word(0x196);
static const System::Word CS_AMQP_ERROR_FRAME = System::Word(0x1f5);
static const System::Word CS_AMQP_ERROR_SYNTAX = System::Word(0x1f6);
static const System::Word CS_AMQP_ERROR_COMMAND_INVALID = System::Word(0x1f7);
static const System::Word CS_AMQP_ERROR_CHANNEL = System::Word(0x1f8);
static const System::Word CS_AMQP_ERROR_UNEXPECTED_FRAME = System::Word(0x1f9);
static const System::Word CS_AMQP_ERROR_RESOURCE = System::Word(0x1fa);
static const System::Word CS_AMQP_ERROR_NOT_ALLOWED = System::Word(0x212);
static const System::Word CS_AMQP_ERROR_NOT_IMPLEMENTED = System::Word(0x21c);
static const System::Word CS_AMQP_ERROR_INTERNAL = System::Word(0x21d);
static const System::Int8 CS_AMQP_ERROR_CLOSE_CONNECTION = System::Int8(-1);
static const System::WideChar CS_AMQP_DEFAULT_VIRTUAL_HOST = (System::WideChar)(0x2f);
#define CS_AMQP_DEFAULT_LOCALE L"en_US"
static const System::Word CS_AMQP_DEFAULT_MAX_CHANNELS = System::Word(0xffff);
static const int CS_AMQP_DEFAULT_MAX_FRAME_SIZE = int(0x7fffffff);
static const System::Word CS_AMQP_DEFAULT_TIMEOUT = System::Word(0x2710);
extern DELPHI_PACKAGE System::ResourceString _S_AMQP_ERROR_CHANNEL_ALREADY_EXISTS;
#define Sgcamqp_const_S_AMQP_ERROR_CHANNEL_ALREADY_EXISTS System::LoadResourceString(&Sgcamqp_const::_S_AMQP_ERROR_CHANNEL_ALREADY_EXISTS)
extern DELPHI_PACKAGE System::ResourceString _S_AMQP_ERROR_CHANNEL_NOT_EXISTS;
#define Sgcamqp_const_S_AMQP_ERROR_CHANNEL_NOT_EXISTS System::LoadResourceString(&Sgcamqp_const::_S_AMQP_ERROR_CHANNEL_NOT_EXISTS)
extern DELPHI_PACKAGE System::ResourceString _S_AMQP_ERROR_EXCHANGE_ALREADY_EXISTS;
#define Sgcamqp_const_S_AMQP_ERROR_EXCHANGE_ALREADY_EXISTS System::LoadResourceString(&Sgcamqp_const::_S_AMQP_ERROR_EXCHANGE_ALREADY_EXISTS)
extern DELPHI_PACKAGE System::ResourceString _S_AMQP_ERROR_EXCHANGE_NOT_EXISTS;
#define Sgcamqp_const_S_AMQP_ERROR_EXCHANGE_NOT_EXISTS System::LoadResourceString(&Sgcamqp_const::_S_AMQP_ERROR_EXCHANGE_NOT_EXISTS)
extern DELPHI_PACKAGE System::ResourceString _S_AMQP_ERROR_QUEUE_ALREADY_EXISTS;
#define Sgcamqp_const_S_AMQP_ERROR_QUEUE_ALREADY_EXISTS System::LoadResourceString(&Sgcamqp_const::_S_AMQP_ERROR_QUEUE_ALREADY_EXISTS)
extern DELPHI_PACKAGE System::ResourceString _S_AMQP_ERROR_QUEUE_NOT_EXISTS;
#define Sgcamqp_const_S_AMQP_ERROR_QUEUE_NOT_EXISTS System::LoadResourceString(&Sgcamqp_const::_S_AMQP_ERROR_QUEUE_NOT_EXISTS)
extern DELPHI_PACKAGE System::ResourceString _S_AMQP_ERROR_CONTENT_TOO_LARGE;
#define Sgcamqp_const_S_AMQP_ERROR_CONTENT_TOO_LARGE System::LoadResourceString(&Sgcamqp_const::_S_AMQP_ERROR_CONTENT_TOO_LARGE)
extern DELPHI_PACKAGE System::ResourceString _S_AMQP_ERROR_NO_CONSUMERS;
#define Sgcamqp_const_S_AMQP_ERROR_NO_CONSUMERS System::LoadResourceString(&Sgcamqp_const::_S_AMQP_ERROR_NO_CONSUMERS)
extern DELPHI_PACKAGE System::ResourceString _S_AMQP_ERROR_CONNECTION_FORCED;
#define Sgcamqp_const_S_AMQP_ERROR_CONNECTION_FORCED System::LoadResourceString(&Sgcamqp_const::_S_AMQP_ERROR_CONNECTION_FORCED)
extern DELPHI_PACKAGE System::ResourceString _S_AMQP_ERROR_INVALID_PATH;
#define Sgcamqp_const_S_AMQP_ERROR_INVALID_PATH System::LoadResourceString(&Sgcamqp_const::_S_AMQP_ERROR_INVALID_PATH)
extern DELPHI_PACKAGE System::ResourceString _S_AMQP_ERROR_ACCESS_REFUSED;
#define Sgcamqp_const_S_AMQP_ERROR_ACCESS_REFUSED System::LoadResourceString(&Sgcamqp_const::_S_AMQP_ERROR_ACCESS_REFUSED)
extern DELPHI_PACKAGE System::ResourceString _S_AMQP_ERROR_NOT_FOUND;
#define Sgcamqp_const_S_AMQP_ERROR_NOT_FOUND System::LoadResourceString(&Sgcamqp_const::_S_AMQP_ERROR_NOT_FOUND)
extern DELPHI_PACKAGE System::ResourceString _S_AMQP_ERROR_RESOURCE_LOCKED;
#define Sgcamqp_const_S_AMQP_ERROR_RESOURCE_LOCKED System::LoadResourceString(&Sgcamqp_const::_S_AMQP_ERROR_RESOURCE_LOCKED)
extern DELPHI_PACKAGE System::ResourceString _S_AMQP_ERROR_PRECONDITION_FAILED;
#define Sgcamqp_const_S_AMQP_ERROR_PRECONDITION_FAILED System::LoadResourceString(&Sgcamqp_const::_S_AMQP_ERROR_PRECONDITION_FAILED)
extern DELPHI_PACKAGE System::ResourceString _S_AMQP_ERROR_FRAME;
#define Sgcamqp_const_S_AMQP_ERROR_FRAME System::LoadResourceString(&Sgcamqp_const::_S_AMQP_ERROR_FRAME)
extern DELPHI_PACKAGE System::ResourceString _S_AMQP_ERROR_SYNTAX;
#define Sgcamqp_const_S_AMQP_ERROR_SYNTAX System::LoadResourceString(&Sgcamqp_const::_S_AMQP_ERROR_SYNTAX)
extern DELPHI_PACKAGE System::ResourceString _S_AMQP_ERROR_COMMAND_INVALID;
#define Sgcamqp_const_S_AMQP_ERROR_COMMAND_INVALID System::LoadResourceString(&Sgcamqp_const::_S_AMQP_ERROR_COMMAND_INVALID)
extern DELPHI_PACKAGE System::ResourceString _S_AMQP_ERROR_CHANNEL;
#define Sgcamqp_const_S_AMQP_ERROR_CHANNEL System::LoadResourceString(&Sgcamqp_const::_S_AMQP_ERROR_CHANNEL)
extern DELPHI_PACKAGE System::ResourceString _S_AMQP_ERROR_UNEXPECTED_FRAME;
#define Sgcamqp_const_S_AMQP_ERROR_UNEXPECTED_FRAME System::LoadResourceString(&Sgcamqp_const::_S_AMQP_ERROR_UNEXPECTED_FRAME)
extern DELPHI_PACKAGE System::ResourceString _S_AMQP_ERROR_RESOURCE;
#define Sgcamqp_const_S_AMQP_ERROR_RESOURCE System::LoadResourceString(&Sgcamqp_const::_S_AMQP_ERROR_RESOURCE)
extern DELPHI_PACKAGE System::ResourceString _S_AMQP_ERROR_NOT_ALLOWED;
#define Sgcamqp_const_S_AMQP_ERROR_NOT_ALLOWED System::LoadResourceString(&Sgcamqp_const::_S_AMQP_ERROR_NOT_ALLOWED)
extern DELPHI_PACKAGE System::ResourceString _S_AMQP_ERROR_NOT_IMPLEMENTED;
#define Sgcamqp_const_S_AMQP_ERROR_NOT_IMPLEMENTED System::LoadResourceString(&Sgcamqp_const::_S_AMQP_ERROR_NOT_IMPLEMENTED)
extern DELPHI_PACKAGE System::ResourceString _S_AMQP_ERROR_INTERNAL;
#define Sgcamqp_const_S_AMQP_ERROR_INTERNAL System::LoadResourceString(&Sgcamqp_const::_S_AMQP_ERROR_INTERNAL)
}	/* namespace Sgcamqp_const */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCAMQP_CONST)
using namespace Sgcamqp_const;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcamqp_constHPP
