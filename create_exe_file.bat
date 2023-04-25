:: Select this bat file directory
cd "%~dp0"

CALL venv32\Scripts\activate

venv32\Scripts\pyinstaller.exe --onefile --noconsole IEEE_CIGRE_DLL_PSCAD_Import_Tool.py
