# Copyright (c) [2025], RTE (https://www.rte-france.com)
#
# See AUTHORS.txt
#
# SPDX-License-Identifier: Apache-2.0
#
# This file is part of PSCAD import tool for IEEE/CIGRE DLLs,
# Tool for importing a DLL in IEEE CIGRE format into PSCAD 5.X software.

import ctypes

class IEEE_Cigre_DLLInterface_DataType_Enum(ctypes.c_int):
    IEEE_Cigre_DLLInterface_DataType_char_T = 1
    IEEE_Cigre_DLLInterface_DataType_int8_T = 2
    IEEE_Cigre_DLLInterface_DataType_uint8_T = 3
    IEEE_Cigre_DLLInterface_DataType_int16_T = 4
    IEEE_Cigre_DLLInterface_DataType_uint16_T = 5
    IEEE_Cigre_DLLInterface_DataType_int32_T = 6
    IEEE_Cigre_DLLInterface_DataType_uint32_T = 7
    IEEE_Cigre_DLLInterface_DataType_real32_T = 8
    IEEE_Cigre_DLLInterface_DataType_real64_T = 9
    IEEE_Cigre_DLLInterface_DataType_c_string_T = 10

class IEEE_Cigre_DLLInterface_Signal(ctypes.Structure):
    _fields_ = [
        ("Name", ctypes.c_char_p),
        ("Description", ctypes.c_char_p),
        ("Unit", ctypes.c_char_p),
        ("DataType", ctypes.c_int),
        ("Width", ctypes.c_int),
    ]

class DefaultValue(ctypes.Union):
    _fields_ = [
        ("Char_Val", ctypes.c_char),
        ("Char_Ptr", ctypes.c_char_p),
        ("Int8_Val", ctypes.c_int8),
        ("Uint8_Val", ctypes.c_uint8),
        ("Int16_Val", ctypes.c_int16),
        ("Uint16_Val", ctypes.c_uint16),
        ("Int32_Val", ctypes.c_int32),
        ("Uint32_Val", ctypes.c_uint32),
        ("Real32_Val", ctypes.c_float),
        ("Real64_Val", ctypes.c_double),
    ]

class MinMaxValue(ctypes.Union):
    _fields_ = [
        ("Char_Val", ctypes.c_char),
        ("Int8_Val", ctypes.c_int8),
        ("Uint8_Val", ctypes.c_uint8),
        ("Int16_Val", ctypes.c_int16),
        ("Uint16_Val", ctypes.c_uint16),
        ("Int32_Val", ctypes.c_int32),
        ("Uint32_Val", ctypes.c_uint32),
        ("Real32_Val", ctypes.c_float),
        ("Real64_Val", ctypes.c_double),
    ]

class IEEE_Cigre_DLLInterface_Parameter(ctypes.Structure):
    _fields_ = [
        ("Name", ctypes.c_char_p),
        ("GroupName", ctypes.c_char_p),
        ("Description", ctypes.c_char_p),
        ("Unit", ctypes.c_char_p),
        ("DataType", ctypes.c_int),
        ("FixedValue", ctypes.c_int),
        ("DefaultValue", DefaultValue),
        ("MinValue", MinMaxValue),
        ("MaxValue", MinMaxValue),
    ]

class IEEE_Cigre_DLLInterface_Model_Info(ctypes.Structure):
    _fields_ = [
        ("DLLInterfaceVersion", ctypes.c_ubyte * 4),
        ("ModelName", ctypes.c_char_p),
        ("ModelVersion", ctypes.c_char_p),
        ("ModelDescription", ctypes.c_char_p),
        ("GeneralInformation", ctypes.c_char_p),
        ("ModelCreated", ctypes.c_char_p),
        ("ModelCreator", ctypes.c_char_p),
        ("ModelLastModifiedDate", ctypes.c_char_p),
        ("ModelLastModifiedBy", ctypes.c_char_p),
        ("ModelModifiedComment", ctypes.c_char_p),
        ("ModelModifiedHistory", ctypes.c_char_p),
        ("FixedStepBaseSampleTime", ctypes.c_double),
        ("EMT_RMS_Mode", ctypes.c_ubyte),
        ("NumInputPorts", ctypes.c_int),
        ("InputPortsInfo", ctypes.POINTER(IEEE_Cigre_DLLInterface_Signal)),
        ("NumOutputPorts", ctypes.c_int),
        ("OutputPortsInfo", ctypes.POINTER(IEEE_Cigre_DLLInterface_Signal)),
        ("NumParameters", ctypes.c_int),
        ("ParametersInfo", ctypes.POINTER(IEEE_Cigre_DLLInterface_Parameter)),
        ("NumIntStates", ctypes.c_int),
        ("NumFloatStates", ctypes.c_int),
        ("NumDoubleStates", ctypes.c_int),
    ]