:: Select this bat file directory
cd "%~dp0"

CALL venv64\Scripts\activate

venv64\Scripts\pyinstaller.exe --onefile --noconsole IEEE_CIGRE_DLL_PSCAD_Import_Tool.py
