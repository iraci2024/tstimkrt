{ ***************************************************************************
  sgcAMQP component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcAMQP_Const;

interface

{$I sgcVer.inc}

const
  CS_AMQP_MAJOR_VERSION = 0;
  CS_AMQP_MINOR_VERSION = 9;
  CS_AMQP_RELEASE_VERSION = 1;

const
  CS_AMQP_FRAME_HEADER_LENGTH = 7;
  CS_AMQP_FRAME_END_LENGTH = 1;
  CS_AMQP_FRAME_HEADER_SIZE = 8;

const
  CS_AMQP_FRAME_METHOD = 1;
  CS_AMQP_FRAME_HEADER = 2;
  CS_AMQP_FRAME_BODY = 3;
  CS_AMQP_FRAME_HEARTBEAT = 8;
  CS_AMQP_FRAME_MIN_SIZE = 4096;
  CS_AMQP_FRAME_END = 206;

const
  CS_AMQP_REPLY_SUCCESS = 200;

const
  CS_AMQP_ERROR_CONTENT_TOO_LARGE = 311;
  CS_AMQP_ERROR_NO_CONSUMERS = 313;
  CS_AMQP_ERROR_CONNECTION_FORCED = 320;
  CS_AMQP_ERROR_INVALID_PATH = 402;
  CS_AMQP_ERROR_ACCESS_REFUSED = 403;
  CS_AMQP_ERROR_NOT_FOUND = 404;
  CS_AMQP_ERROR_RESOURCE_LOCKED = 405;
  CS_AMQP_ERROR_PRECONDITION_FAILED = 406;

const
  CS_AMQP_ERROR_FRAME = 501;
  CS_AMQP_ERROR_SYNTAX = 502;
  CS_AMQP_ERROR_COMMAND_INVALID = 503;
  CS_AMQP_ERROR_CHANNEL = 504;
  CS_AMQP_ERROR_UNEXPECTED_FRAME = 505;
  CS_AMQP_ERROR_RESOURCE = 506;
  CS_AMQP_ERROR_NOT_ALLOWED = 530;
  CS_AMQP_ERROR_NOT_IMPLEMENTED = 540;
  CS_AMQP_ERROR_INTERNAL = 541;

const
  CS_AMQP_ERROR_CLOSE_CONNECTION = -1;

const
  CS_AMQP_DEFAULT_VIRTUAL_HOST = '/';
  CS_AMQP_DEFAULT_LOCALE = 'en_US';
  CS_AMQP_DEFAULT_MAX_CHANNELS = 65535;
  CS_AMQP_DEFAULT_MAX_FRAME_SIZE = 2147483647;

const
  CS_AMQP_DEFAULT_TIMEOUT = 10000;

resourcestring
  S_AMQP_ERROR_CHANNEL_ALREADY_EXISTS = 'Channel: %s already exists.';
  S_AMQP_ERROR_CHANNEL_NOT_EXISTS = 'Channel: %s not exists.';
  S_AMQP_ERROR_EXCHANGE_ALREADY_EXISTS = 'Exchange: %s already exists in channel: %s';
  S_AMQP_ERROR_EXCHANGE_NOT_EXISTS = 'Exchange: %s not exists in channel: %s';
  S_AMQP_ERROR_QUEUE_ALREADY_EXISTS = 'Queue: %s already exists in channel: %s';
  S_AMQP_ERROR_QUEUE_NOT_EXISTS = 'Queue: %s not exists in channel: %s';

resourcestring
  S_AMQP_ERROR_CONTENT_TOO_LARGE = 'Content Too Large';
  S_AMQP_ERROR_NO_CONSUMERS = 'No Consumers';
  S_AMQP_ERROR_CONNECTION_FORCED = 'Connection Forced';
  S_AMQP_ERROR_INVALID_PATH = 'Invalid Path';
  S_AMQP_ERROR_ACCESS_REFUSED = 'Access Refused';
  S_AMQP_ERROR_NOT_FOUND = 'Not Found';
  S_AMQP_ERROR_RESOURCE_LOCKED = 'Resource Locked';
  S_AMQP_ERROR_PRECONDITION_FAILED = 'Precondition Failed';

resourcestring
  S_AMQP_ERROR_FRAME = 'Frame Error';
  S_AMQP_ERROR_SYNTAX = 'Syntax Error';
  S_AMQP_ERROR_COMMAND_INVALID = 'Command Invalid';
  S_AMQP_ERROR_CHANNEL = 'Channel Error';
  S_AMQP_ERROR_UNEXPECTED_FRAME = 'Unexpected Frame';
  S_AMQP_ERROR_RESOURCE = 'Resource Error';
  S_AMQP_ERROR_NOT_ALLOWED = 'Now Allowed';
  S_AMQP_ERROR_NOT_IMPLEMENTED = 'Not Implemented';
  S_AMQP_ERROR_INTERNAL = 'Internal Error';

implementation

end.
