# Simplex-IEC-104-Client
Simplex IEC 104 Client as an example of using the Simplex IEC 101/104 SDK.

http://www.simplexiec104.com

The SDK supports:
 Protocols	 IEC 60870-5-101, IEC 60870-5-104
 Platforms	 Win32, Win64, Android, iOSDevice32, iOSDevice64, iOSSimulator, OSX32, Linux64
 IDE	 Delphi: XE10.2, XE10.1, XE10, XE8, XE7, XE6, XE5, XE4, XE3, XE2

Supported ASDU
M_SP_NA: Single-point information
M_SP_TA: Single-point information with time tag (3 bytes)
M_DP_NA: Double-point information
M_DP_TA: Double-point information with time tag (3 bytes)
M_ST_NA: Step position information
M_ST_TA: Step position information with time tag (3 bytes)
M_BO_NA: Bitstring of 32 bit
M_BO_TA: Bitstring of 32 bit with time tag (3 bytes)
M_ME_NA: Measured value, normalized value
M_ME_TA: Measured value, normalized value with time tag (3 bytes)
M_ME_NB: Measured value, scaled value
M_ME_TB: Measured value, scaled value with time tag (3 bytes)
M_ME_NC: Measured value, short floating point number
M_ME_TC: Measured value, short floating point number with time tag (3 bytes)
M_IT_NA: Integrated totals
M_IT_TA: Integrated totals with time tag (3 bytes)
M_EP_TA: Event of protection equipment with time tag (3 bytes)
M_EP_TB: Packed start events of protection equipment with time tag (3 bytes)
M_EP_TC: Packed output circuit information of protection equipment with time tag
M_PS_NA: Packed single-point information with status change detection
M_ME_ND: Measured value, normalized value without quality descriptor
M_SP_TB: Single-point information with time tag (7 bytes)
M_DP_TB: Double-point information with time tag (7 bytes)
M_ST_TB: Step position information with time tag (7 bytes)
M_BO_TB: Bitstring of 32 bits with time tag (7 bytes)
M_ME_TD: Measured value, normalized value with time tag (7 bytes)
M_ME_TE: Measured value, scaled value with time tag (7 bytes)
M_ME_TF: Measured value, short floating point number with time tag (7 bytes)
M_IT_TB: Integrated totals with time tag (7 bytes)
M_EP_TD: Event of protection equipment with time tag (7 bytes)
M_EP_TE: Packed start events of protection equipment with time tag (7 bytes)
M_EP_TF: Packed output circuit information of protection equipment with time tag
M_EI_NA: End of initialization
C_CS_NA: Current time (Clock synchronization command)
C_IC_NA: Interrogation command
C_CI_NA: Counter interrogation command
C_RD_NA: Read command
C_SC_NA: Single command

Supported Features
No threads - simple and effective use
TCP and Serial channel (serial only for Win32/Win64). Adding your own channels
Automatic channel recovery
Configuring of protocol options
