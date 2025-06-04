:: Copyright (c) [2025], RTE (https://www.rte-france.com)
::
:: See AUTHORS.txt
::
:: SPDX-License-Identifier: Apache-2.0
::
:: This file is part of PSCAD import tool for IEEE/CIGRE DLLs,
:: Tool for importing a DLL in IEEE CIGRE format into PSCAD 5.X software.

:: Select this bat file directory
cd "%~dp0"

CALL venv64\Scripts\activate

venv64\Scripts\pyinstaller.exe --onefile --noconsole IEEE_CIGRE_DLL_PSCAD_Import_Tool.py
