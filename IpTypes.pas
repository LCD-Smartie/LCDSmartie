{******************************************************************************}
{                                                       	               }
{ Internet Protocol Helper API interface Unit for Object Pascal                }
{                                                       	               }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{ 								               }
{ The original file is: iptypes.h, released July 2000. The original Pascal     }
{ code is: IpTypes.pas, released September 2000. The initial developer of the  }
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

unit IpTypes;

{$WEAKPACKAGEUNIT}

{$HPPEMIT ''}
{$HPPEMIT '#include "iptypes.h"'}
{$HPPEMIT ''}

//{$I WINDEFINES.INC}

interface

uses
  Windows;

type
  // #include <time.h>
  time_t = Longint;
  {$EXTERNALSYM time_t}

// Definitions and structures used by getnetworkparams and getadaptersinfo apis

const
  MAX_ADAPTER_DESCRIPTION_LENGTH = 128; // arb.
  {$EXTERNALSYM MAX_ADAPTER_DESCRIPTION_LENGTH}
  MAX_ADAPTER_NAME_LENGTH        = 256; // arb.
  {$EXTERNALSYM MAX_ADAPTER_NAME_LENGTH}
  MAX_ADAPTER_ADDRESS_LENGTH     = 8; // arb.
  {$EXTERNALSYM MAX_ADAPTER_ADDRESS_LENGTH}
  DEFAULT_MINIMUM_ENTITIES       = 32; // arb.
  {$EXTERNALSYM DEFAULT_MINIMUM_ENTITIES}
  MAX_HOSTNAME_LEN               = 128; // arb.
  {$EXTERNALSYM MAX_HOSTNAME_LEN}
  MAX_DOMAIN_NAME_LEN            = 128; // arb.
  {$EXTERNALSYM MAX_DOMAIN_NAME_LEN}
  MAX_SCOPE_ID_LEN               = 256; // arb.
  {$EXTERNALSYM MAX_SCOPE_ID_LEN}

//
// types
//

// Node Type

  BROADCAST_NODETYPE    = 1;
  {$EXTERNALSYM BROADCAST_NODETYPE}
  PEER_TO_PEER_NODETYPE = 2;
  {$EXTERNALSYM PEER_TO_PEER_NODETYPE}
  MIXED_NODETYPE        = 4;
  {$EXTERNALSYM MIXED_NODETYPE}
  HYBRID_NODETYPE       = 8;
  {$EXTERNALSYM HYBRID_NODETYPE}

// Adapter Type

  IF_OTHER_ADAPTERTYPE      = 0;
  {$EXTERNALSYM IF_OTHER_ADAPTERTYPE}
  IF_ETHERNET_ADAPTERTYPE   = 1;
  {$EXTERNALSYM IF_ETHERNET_ADAPTERTYPE}
  IF_TOKEN_RING_ADAPTERTYPE = 2;
  {$EXTERNALSYM IF_TOKEN_RING_ADAPTERTYPE}
  IF_FDDI_ADAPTERTYPE       = 3;
  {$EXTERNALSYM IF_FDDI_ADAPTERTYPE}
  IF_PPP_ADAPTERTYPE        = 4;
  {$EXTERNALSYM IF_PPP_ADAPTERTYPE}
  IF_LOOPBACK_ADAPTERTYPE   = 5;
  {$EXTERNALSYM IF_LOOPBACK_ADAPTERTYPE}
  IF_SLIP_ADAPTERTYPE       = 6;
  {$EXTERNALSYM IF_SLIP_ADAPTERTYPE}

//
// IP_ADDRESS_STRING - store an IP address as a dotted decimal string
//

type
  PIP_MASK_STRING = ^IP_MASK_STRING;
  {$EXTERNALSYM PIP_MASK_STRING}
  IP_ADDRESS_STRING = record
    S: array [0..15] of Char;
  end;
  {$EXTERNALSYM IP_ADDRESS_STRING}
  PIP_ADDRESS_STRING = ^IP_ADDRESS_STRING;
  {$EXTERNALSYM PIP_ADDRESS_STRING}
  IP_MASK_STRING = IP_ADDRESS_STRING;
  {$EXTERNALSYM IP_MASK_STRING}
  TIpAddressString = IP_ADDRESS_STRING;
  PIpAddressString = PIP_MASK_STRING;

//
// IP_ADDR_STRING - store an IP address with its corresponding subnet mask,
// both as dotted decimal strings
//

  PIP_ADDR_STRING = ^IP_ADDR_STRING;
  {$EXTERNALSYM PIP_ADDR_STRING}
  _IP_ADDR_STRING = record
    Next: PIP_ADDR_STRING;
    IpAddress: IP_ADDRESS_STRING;
    IpMask: IP_MASK_STRING;
    Context: DWORD;
  end;
  {$EXTERNALSYM _IP_ADDR_STRING}
  IP_ADDR_STRING = _IP_ADDR_STRING;
  {$EXTERNALSYM IP_ADDR_STRING}
  TIpAddrString = IP_ADDR_STRING;
  PIpAddrString = PIP_ADDR_STRING;

//
// ADAPTER_INFO - per-adapter information. All IP addresses are stored as
// strings
//

  PIP_ADAPTER_INFO = ^IP_ADAPTER_INFO;
  {$EXTERNALSYM PIP_ADAPTER_INFO}
  _IP_ADAPTER_INFO = record
    Next: PIP_ADAPTER_INFO;
    ComboIndex: DWORD;
    AdapterName: array [0..MAX_ADAPTER_NAME_LENGTH + 3] of Char;
    Description: array [0..MAX_ADAPTER_DESCRIPTION_LENGTH + 3] of Char;
    AddressLength: UINT;
    Address: array [0..MAX_ADAPTER_ADDRESS_LENGTH - 1] of BYTE;
    Index: DWORD;
    Type_: UINT;
    DhcpEnabled: UINT;
    CurrentIpAddress: PIP_ADDR_STRING;
    IpAddressList: IP_ADDR_STRING;
    GatewayList: IP_ADDR_STRING;
    DhcpServer: IP_ADDR_STRING;
    HaveWins: BOOL;
    PrimaryWinsServer: IP_ADDR_STRING;
    SecondaryWinsServer: IP_ADDR_STRING;
    LeaseObtained: time_t;
    LeaseExpires: time_t;
  end;
  {$EXTERNALSYM _IP_ADAPTER_INFO}
  IP_ADAPTER_INFO = _IP_ADAPTER_INFO;
  {$EXTERNALSYM IP_ADAPTER_INFO}
  TIpAdapterInfo = IP_ADAPTER_INFO;
  PIpAdapterInfo = PIP_ADAPTER_INFO;

//
// IP_PER_ADAPTER_INFO - per-adapter IP information such as DNS server list.
//

  PIP_PER_ADAPTER_INFO = ^IP_PER_ADAPTER_INFO;
  {$EXTERNALSYM PIP_PER_ADAPTER_INFO}
  _IP_PER_ADAPTER_INFO = record
    AutoconfigEnabled: UINT;
    AutoconfigActive: UINT;
    CurrentDnsServer: PIP_ADDR_STRING;
    DnsServerList: IP_ADDR_STRING;
  end;
  {$EXTERNALSYM _IP_PER_ADAPTER_INFO}
  IP_PER_ADAPTER_INFO = _IP_PER_ADAPTER_INFO;
  {$EXTERNALSYM IP_PER_ADAPTER_INFO}
  TIpPerAdapterInfo = IP_PER_ADAPTER_INFO;
  PIpPerAdapterInfo = PIP_PER_ADAPTER_INFO;

//
// FIXED_INFO - the set of IP-related information which does not depend on DHCP
//

  PFIXED_INFO = ^FIXED_INFO;
  {$EXTERNALSYM PFIXED_INFO}
  FIXED_INFO = record
    HostName: array [0..MAX_HOSTNAME_LEN + 3] of Char;
    DomainName: array[0..MAX_DOMAIN_NAME_LEN + 3] of Char;
    CurrentDnsServer: PIP_ADDR_STRING;
    DnsServerList: IP_ADDR_STRING;
    NodeType: UINT;
    ScopeId: array [0..MAX_SCOPE_ID_LEN + 3] of Char;
    EnableRouting: UINT;
    EnableProxy: UINT;
    EnableDns: UINT;
  end;
  {$EXTERNALSYM FIXED_INFO}
  TFixedInfo = FIXED_INFO;
  PFixedInfo = PFIXED_INFO;

implementation

end.
