{******************************************************************************}
{                                                       	               }
{ Internet Protocol Helper API interface Unit for Object Pascal                }
{                                                       	               }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{ 								               }
{ The original file is: ipexport.h, released July 2000. The original Pascal    }
{ code is: IpExport.pas, released September 2000. The initial developer of the }
{ Pascal code is Marcel van Brakel (brakelm@chello.nl).                        }
{                                                                              }
{ Portions created by Marcel van Brakel are Copyright (C) 1999-2001            }
{ Marcel van Brakel. All Rights Reserved.                                      }
{ 								               }
{ Contributor(s): John C. Penman (jcp@craiglockhart.com)                       }
{                 Vladimir Vassiliev (voldemarv@hotpop.com)                    }
{ 								               }
{ Obtained through: Joint Endeavour of Delphi Innovators (Project JEDI)        }
{								               }
{ You may retrieve the latest version of this file at the Project JEDI home    }
{ page, located at http://delphi-jedi.org or my personal homepage located at   }
{ http://members.chello.nl/m.vanbrakel2                                        }
{								               }
{ The contents of this file are used with permission, subject to the Mozilla   }
{ Public License Version 1.1 (the "License"); you may not use this file except }
{ in compliance with the License. You may obtain a copy of the License at      }
{ http://www.mozilla.org/MPL/MPL-1.1.html                                      }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ Alternatively, the contents of this file may be used under the terms of the  }
{ GNU Lesser General Public License (the  "LGPL License"), in which case the   }
{ provisions of the LGPL License are applicable instead of those above.        }
{ If you wish to allow use of your version of this file only under the terms   }
{ of the LGPL License and not to allow others to use your version of this file }
{ under the MPL, indicate your decision by deleting  the provisions above and  }
{ replace  them with the notice and other provisions required by the LGPL      }
{ License.  If you do not delete the provisions above, a recipient may use     }
{ your version of this file under either the MPL or the LGPL License.          }
{ 								               }
{ For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html }
{ 								               }
{******************************************************************************}

unit IpExport;

{$WEAKPACKAGEUNIT}

{$HPPEMIT ''}
{$HPPEMIT '#include "ipexport.h"'}
{$HPPEMIT ''}

//{$I WINDEFINES.INC}

interface

uses
  Windows;

//
// IP type definitions.
//

type
  IPAddr = Cardinal;     // An IP address.
  {$EXTERNALSYM IPAddr}
  IPMask = Cardinal;     // An IP subnet mask.
  {$EXTERNALSYM IPMask}
  IP_STATUS = Cardinal;  // Status code returned from IP APIs.
  {$EXTERNALSYM IP_STATUS}

//
// The ip_option_information structure describes the options to be
// included in the header of an IP packet. The TTL, TOS, and Flags
// values are carried in specific fields in the header. The OptionsData
// bytes are carried in the options area following the standard IP header.
// With the exception of source route options, this data must be in the
// format to be transmitted on the wire as specified in RFC 791. A source
// route option should contain the full route - first hop thru final
// destination - in the route data. The first hop will be pulled out of the
// data and the option will be reformatted accordingly. Otherwise, the route
// option should be formatted as specified in RFC 791.
//

type
  ip_option_information = record
    Ttl: Byte;          // Time To Live
    Tos: Byte;          // Type Of Service
    Flags: Byte;        // IP header flags
    OptionsSize: Byte;  // Size in bytes of options data
    OptionsData: PByte; // Pointer to options data
  end;
  {$EXTERNALSYM ip_option_information}
  TIpOptionInformation = ip_option_information;
  PIpOptionInformation = ^ip_option_information;

//
// The icmp_echo_reply structure describes the data returned in response
// to an echo request.
//

  icmp_echo_reply = record
    Address: IPAddr;  // Replying address
    Status: Cardinal; // Reply IP_STATUS
    RoundTripTime: Cardinal; // RTT in milliseconds
    DataSize: Word;   // Reply data size in bytes
    Reserved: Word;   // Reserved for system use
    Data: Pointer;    // Pointer to the reply data
    Options: ip_option_information; // Reply options
  end;
  {$EXTERNALSYM icmp_echo_reply}
  PIP_OPTION_INFORMATION = ^IP_OPTION_INFORMATION;
  {$EXTERNALSYM PIP_OPTION_INFORMATION}
  PICMP_ECHO_REPLY = ^ICMP_ECHO_REPLY;
  {$EXTERNALSYM PICMP_ECHO_REPLY}
  TIcmpEchoReply = icmp_echo_reply;
  PIcmpEchoReply = PICMP_ECHO_REPLY;

  PARP_SEND_REPLY = ^ARP_SEND_REPLY;
  {$EXTERNALSYM PARP_SEND_REPLY}
  ArpRequestBuffer = record
    DestAddress: IPAddr;
    SrcAddress: IPAddr;
  end;
  {$EXTERNALSYM ArpRequestBuffer}
  ARP_SEND_REPLY = ArpRequestBuffer;
  {$EXTERNALSYM ARP_SEND_REPLY}
  TArpRequestBuffer = ARP_SEND_REPLY;
  PArpRequestBuffer = PARP_SEND_REPLY;

  _TCP_RESERVE_PORT_RANGE = record
    UpperRange: Word;
    LowerRange: Word;
  end;
  {$EXTERNALSYM _TCP_RESERVE_PORT_RANGE}
  TCP_RESERVE_PORT_RANGE = _TCP_RESERVE_PORT_RANGE;
  {$EXTERNALSYM TCP_RESERVE_PORT_RANGE}
  TTcpReservePortRange = _TCP_RESERVE_PORT_RANGE;
  PTcpReservePortRange = ^TCP_RESERVE_PORT_RANGE;

const
  MAX_ADAPTER_NAME = 128;
  {$EXTERNALSYM MAX_ADAPTER_NAME}

type
  PIP_ADAPTER_INDEX_MAP = ^IP_ADAPTER_INDEX_MAP;
  {$EXTERNALSYM PIP_ADAPTER_INDEX_MAP}
  _IP_ADAPTER_INDEX_MAP = record
    Index: ULONG;
    Name: array [0..MAX_ADAPTER_NAME - 1] of WCHAR;
  end;
  {$EXTERNALSYM _IP_ADAPTER_INDEX_MAP}
  IP_ADAPTER_INDEX_MAP = _IP_ADAPTER_INDEX_MAP;
  {$EXTERNALSYM IP_ADAPTER_INDEX_MAP}
  TIpAdapterIndexMap = IP_ADAPTER_INDEX_MAP;
  PIpAdapterIndexMap = PIP_ADAPTER_INDEX_MAP;

  PIP_INTERFACE_INFO = ^IP_INTERFACE_INFO;
  {$EXTERNALSYM PIP_INTERFACE_INFO}
  _IP_INTERFACE_INFO = record
    NumAdapters: Longint;
    Adapter: array [0..0] of IP_ADAPTER_INDEX_MAP;
  end;
  {$EXTERNALSYM _IP_INTERFACE_INFO}
  IP_INTERFACE_INFO = _IP_INTERFACE_INFO;
  {$EXTERNALSYM IP_INTERFACE_INFO}
  TIpInterfaceInfo = IP_INTERFACE_INFO;
  PIpInterfaceInfo = PIP_INTERFACE_INFO;

  PIP_UNIDIRECTIONAL_ADAPTER_ADDRESS = ^IP_UNIDIRECTIONAL_ADAPTER_ADDRESS;
  {$EXTERNALSYM PIP_UNIDIRECTIONAL_ADAPTER_ADDRESS}
  _IP_UNIDIRECTIONAL_ADAPTER_ADDRESS = record
    NumAdapters: ULONG;
    Address: array [0..0] of IPAddr;
  end;
  {$EXTERNALSYM _IP_UNIDIRECTIONAL_ADAPTER_ADDRESS}
  IP_UNIDIRECTIONAL_ADAPTER_ADDRESS = _IP_UNIDIRECTIONAL_ADAPTER_ADDRESS;
  {$EXTERNALSYM IP_UNIDIRECTIONAL_ADAPTER_ADDRESS}
  TIpUnidirectionalAdapterAddress = IP_UNIDIRECTIONAL_ADAPTER_ADDRESS;
  PIpUnidirectionalAdapterAddress = PIP_UNIDIRECTIONAL_ADAPTER_ADDRESS;

  PIP_ADAPTER_ORDER_MAP = ^IP_ADAPTER_ORDER_MAP;
  {$EXTERNALSYM PIP_ADAPTER_ORDER_MAP}
  _IP_ADAPTER_ORDER_MAP = record
    NumAdapters: ULONG;
    AdapterOrder: array [0..0] of ULONG;
  end;
  {$EXTERNALSYM _IP_ADAPTER_ORDER_MAP}
  IP_ADAPTER_ORDER_MAP = _IP_ADAPTER_ORDER_MAP;
  {$EXTERNALSYM IP_ADAPTER_ORDER_MAP}
  TIpAdapterOrderMap = IP_ADAPTER_ORDER_MAP;
  PIpAdapterOrderMap = PIP_ADAPTER_ORDER_MAP;

//
// IP_STATUS codes returned from IP APIs
//

const
  IP_STATUS_BASE = 11000;
  {$EXTERNALSYM IP_STATUS_BASE}

  IP_SUCCESS               = 0;
  {$EXTERNALSYM IP_SUCCESS}
  IP_BUF_TOO_SMALL         = IP_STATUS_BASE + 1;
  {$EXTERNALSYM IP_BUF_TOO_SMALL}
  IP_DEST_NET_UNREACHABLE  = IP_STATUS_BASE + 2;
  {$EXTERNALSYM IP_DEST_NET_UNREACHABLE}
  IP_DEST_HOST_UNREACHABLE = IP_STATUS_BASE + 3;
  {$EXTERNALSYM IP_DEST_HOST_UNREACHABLE}
  IP_DEST_PROT_UNREACHABLE = IP_STATUS_BASE + 4;
  {$EXTERNALSYM IP_DEST_PROT_UNREACHABLE}
  IP_DEST_PORT_UNREACHABLE = IP_STATUS_BASE + 5;
  {$EXTERNALSYM IP_DEST_PORT_UNREACHABLE}
  IP_NO_RESOURCES          = IP_STATUS_BASE + 6;
  {$EXTERNALSYM IP_NO_RESOURCES}
  IP_BAD_OPTION            = IP_STATUS_BASE + 7;
  {$EXTERNALSYM IP_BAD_OPTION}
  IP_HW_ERROR              = IP_STATUS_BASE + 8;
  {$EXTERNALSYM IP_HW_ERROR}
  IP_PACKET_TOO_BIG        = IP_STATUS_BASE + 9;
  {$EXTERNALSYM IP_PACKET_TOO_BIG}
  IP_REQ_TIMED_OUT         = IP_STATUS_BASE + 10;
  {$EXTERNALSYM IP_REQ_TIMED_OUT}
  IP_BAD_REQ               = IP_STATUS_BASE + 11;
  {$EXTERNALSYM IP_BAD_REQ}
  IP_BAD_ROUTE             = IP_STATUS_BASE + 12;
  {$EXTERNALSYM IP_BAD_ROUTE}
  IP_TTL_EXPIRED_TRANSIT   = IP_STATUS_BASE + 13;
  {$EXTERNALSYM IP_TTL_EXPIRED_TRANSIT}
  IP_TTL_EXPIRED_REASSEM   = IP_STATUS_BASE + 14;
  {$EXTERNALSYM IP_TTL_EXPIRED_REASSEM}
  IP_PARAM_PROBLEM         = IP_STATUS_BASE + 15;
  {$EXTERNALSYM IP_PARAM_PROBLEM}
  IP_SOURCE_QUENCH         = IP_STATUS_BASE + 16;
  {$EXTERNALSYM IP_SOURCE_QUENCH}
  IP_OPTION_TOO_BIG        = IP_STATUS_BASE + 17;
  {$EXTERNALSYM IP_OPTION_TOO_BIG}
  IP_BAD_DESTINATION       = IP_STATUS_BASE + 18;
  {$EXTERNALSYM IP_BAD_DESTINATION}

//
// The next group are status codes passed up on status indications to
// transport layer protocols.
//

  IP_ADDR_DELETED                    = IP_STATUS_BASE + 19;
  {$EXTERNALSYM IP_ADDR_DELETED}
  IP_SPEC_MTU_CHANGE                 = IP_STATUS_BASE + 20;
  {$EXTERNALSYM IP_SPEC_MTU_CHANGE}
  IP_MTU_CHANGE                      = IP_STATUS_BASE + 21;
  {$EXTERNALSYM IP_MTU_CHANGE}
  IP_UNLOAD                          = IP_STATUS_BASE + 22;
  {$EXTERNALSYM IP_UNLOAD}
  IP_ADDR_ADDED                      = IP_STATUS_BASE + 23;
  {$EXTERNALSYM IP_ADDR_ADDED}
  IP_MEDIA_CONNECT                   = IP_STATUS_BASE + 24;
  {$EXTERNALSYM IP_MEDIA_CONNECT}
  IP_MEDIA_DISCONNECT                = IP_STATUS_BASE + 25;
  {$EXTERNALSYM IP_MEDIA_DISCONNECT}
  IP_BIND_ADAPTER                    = IP_STATUS_BASE + 26;
  {$EXTERNALSYM IP_BIND_ADAPTER}
  IP_UNBIND_ADAPTER                  = IP_STATUS_BASE + 27;
  {$EXTERNALSYM IP_UNBIND_ADAPTER}
  IP_DEVICE_DOES_NOT_EXIST           = IP_STATUS_BASE + 28;
  {$EXTERNALSYM IP_DEVICE_DOES_NOT_EXIST}
  IP_DUPLICATE_ADDRESS               = IP_STATUS_BASE + 29;
  {$EXTERNALSYM IP_DUPLICATE_ADDRESS}
  IP_INTERFACE_METRIC_CHANGE         = IP_STATUS_BASE + 30;
  {$EXTERNALSYM IP_INTERFACE_METRIC_CHANGE}
  IP_RECONFIG_SECFLTR                = IP_STATUS_BASE + 31;
  {$EXTERNALSYM IP_RECONFIG_SECFLTR}
  IP_NEGOTIATING_IPSEC               = IP_STATUS_BASE + 32;
  {$EXTERNALSYM IP_NEGOTIATING_IPSEC}
  IP_INTERFACE_WOL_CAPABILITY_CHANGE = IP_STATUS_BASE + 33;
  {$EXTERNALSYM IP_INTERFACE_WOL_CAPABILITY_CHANGE}
  IP_DUPLICATE_IPADD                 = IP_STATUS_BASE + 34;
  {$EXTERNALSYM IP_DUPLICATE_IPADD}

  IP_GENERAL_FAILURE = IP_STATUS_BASE + 50;
  {$EXTERNALSYM IP_GENERAL_FAILURE}
  MAX_IP_STATUS      = IP_GENERAL_FAILURE;
  {$EXTERNALSYM MAX_IP_STATUS}
  IP_PENDING         = IP_STATUS_BASE + 255;
  {$EXTERNALSYM IP_PENDING}

//
// Values used in the IP header Flags field.
//

  IP_FLAG_DF = $2; // Don't fragment this packet.
  {$EXTERNALSYM IP_FLAG_DF}

//
// Supported IP Option Types.
//
// These types define the options which may be used in the OptionsData field
// of the ip_option_information structure.  See RFC 791 for a complete
// description of each.
//

  IP_OPT_EOL          = 0; // End of list option
  {$EXTERNALSYM IP_OPT_EOL}
  IP_OPT_NOP          = 1; // No operation
  {$EXTERNALSYM IP_OPT_NOP}
  IP_OPT_SECURITY     = $82; // Security option
  {$EXTERNALSYM IP_OPT_SECURITY}
  IP_OPT_LSRR         = $83; // Loose source route
  {$EXTERNALSYM IP_OPT_LSRR}
  IP_OPT_SSRR         = $89; // Strict source route
  {$EXTERNALSYM IP_OPT_SSRR}
  IP_OPT_RR           = $7; // Record route
  {$EXTERNALSYM IP_OPT_RR}
  IP_OPT_TS           = $44; // Timestamp
  {$EXTERNALSYM IP_OPT_TS}
  IP_OPT_SID          = $88; // Stream ID (obsolete)
  {$EXTERNALSYM IP_OPT_SID}
  IP_OPT_ROUTER_ALERT = $94; // Router Alert Option
  {$EXTERNALSYM IP_OPT_ROUTER_ALERT}

  MAX_OPT_SIZE = 40; // Maximum length of IP options in bytes
  {$EXTERNALSYM MAX_OPT_SIZE}

// Ioctls code exposed by Memphis tcpip stack.
// For NT these ioctls are define in ntddip.h  (private\inc)

  IOCTL_IP_RTCHANGE_NOTIFY_REQUEST        = 101;
  {$EXTERNALSYM IOCTL_IP_RTCHANGE_NOTIFY_REQUEST}
  IOCTL_IP_ADDCHANGE_NOTIFY_REQUEST       = 102;
  {$EXTERNALSYM IOCTL_IP_ADDCHANGE_NOTIFY_REQUEST}
  IOCTL_ARP_SEND_REQUEST                  = 103;
  {$EXTERNALSYM IOCTL_ARP_SEND_REQUEST}
  IOCTL_IP_INTERFACE_INFO                 = 104;
  {$EXTERNALSYM IOCTL_IP_INTERFACE_INFO}
  IOCTL_IP_GET_BEST_INTERFACE             = 105;
  {$EXTERNALSYM IOCTL_IP_GET_BEST_INTERFACE}
  IOCTL_IP_UNIDIRECTIONAL_ADAPTER_ADDRESS = 106;
  {$EXTERNALSYM IOCTL_IP_UNIDIRECTIONAL_ADAPTER_ADDRESS}

implementation

end.
