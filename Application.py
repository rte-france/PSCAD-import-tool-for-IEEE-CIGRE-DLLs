# For GUI
# tuto : https://python.sdv.univ-paris-diderot.fr/20_tkinter/
import tkinter as tk

#from tkinter import filedialog  # cesar
from tkinter import filedialog, ttk  # ttk provides better rendering for label, entry etc. compared to tk
import shutil  # to copy files

import os

# import IEEE_Cigre_DLLInterface
from IEEE_Cigre_DLLInterface import IEEE_Cigre_DLLInterface_Model_Info, IEEE_Cigre_DLLInterface_DataType_Enum

import ctypes

# for PSCAD automation V5 :
# https://www.pscad.com/webhelp-v501-al/index.html
# https://www.pscad.com/webhelp-v5-al/tutorial/index.html
import mhi.pscad
#from mhi.pscad import OverlayGraph, graphics, Component
from mhi.pscad.wizard import UserDefnWizard, Signal

# For PSCAD automation V4
# download https://www.pscad.com/software/pscad/automation-library
# setup: https://www.pscad.com/knowledge-base/article/369
# chapters: https://www.pscad.com/knowledge-base/topic-242/v-
# doc: https://www.pscad.com/webhelp/al-help/index.html
# doc reference manual: https://www.pscad.com/webhelp/al-help/automation/index.html
# import mhrc.automation


# Application inherits from the class tk.Tk
class Application(tk.Tk):

    def __init__(self, num_version):
        tk.Tk.__init__(self)

        # GUI widgets: all 'self' attributes must be declared in init
        self.button_generate_pscad_model = None
        self.button_go_to_folder = None
        self.button_refresh = None
        self.refresh_image = None
        self.go_to_folder_image = None
        self.combobox_pscad_projects = None
        self.button_browse_new_project = None
        self.entry_destination_folder = None
        self.radio_option = None
        self.label_dll_file_path = None
        self.button_browse_pscx_file_path = None
        self.entry_dll_file_path = None
        self.entry_destination_folder_placeholder = "Destination folder"  # gray info if empty
        self.combobox_pscad_projects_placeholder = "Select an open PSCAD project"
    
        # Other attibutes
        self.pscad = None
        self.pscad_projects_selected_value = None  # for option 2 only
        self.destination_folder = None
        self.pscad_project_name = None  # for both option 1 and 2
        self.num_version = num_version
        self.dll_file_path = None
        self.dll_file_name = None  # the name of the file 'modelName.dll'
        self.fortran_interface_file_path = None
        self.fortran_interface_file_name = None
        self.row_index = 0  # for GUI grid
        self.list_label_errors = []
        self.list_label_info = []
        self.Model_Info = None
        self.Model_Name = None
        self.Model_Name_Shortened = None
        self.DLLInterfaceVersion = None  # An array of 4 integers
        # No pandas df used because heavy package when building in exe
        #self.in_df = None  # Dataframe inputs
        #self.out_df = None  # Dataframe outputs
        self.in_names = []
        self.in_fortran_types = []
        self.in_pscad_types = []  # INTEGER or REAL
        self.in_width = []

        self.out_names = []
        self.out_units = []
        self.out_fortran_types = []
        self.out_pscad_types = []  # INTEGER or REAL
        self.out_width = []

        self.param_names = []
        self.param_fortran_types = []
        self.param_pscad_types = []  # For an IEEE CIGRE DLL, it can be INTEGER, REAL or CHARACTER(*)
        self.param_group_names = []
        self.param_descriptions = []
        self.param_units = []
        self.param_fixedValue = []  # int, keep it in case of calling Model_CheckParameters each time parameters have really changed
        self.param_default_values = []
        self.param_min_values = []
        self.param_max_values = []

        self.nb_inputs_total = 0
        self.nb_outputs_total = 0
        self.nb_params_total = 0

    # Returns open PSCAD project names
    def get_available_pscad_project_names(self):
        projects_list = self.pscad.projects()
        project_names = []
        for project in projects_list:
            if project['type'] == 'Case':  # The project with type "case" is a real project, not a library
                project_names.append(project['name'])
        return project_names  # All the available projects (the names as appear in PSCAD)

    # Open the directory of the selected project in the combobox
    # Do nothing if the project does not exist
    def go_to_selected_project_folder(self):
        project_name = self.pscad_projects_selected_value.get()

        try:
            pscad_project = self.pscad.project(name=project_name)
        except Exception:
            return

        project_filename = pscad_project.filename
        folder_path = os.path.dirname(project_filename)  # project folder
        if os.path.exists(folder_path):
            os.startfile(folder_path)

    # Get self.pscad
    # Raise Exception if PSCAD 5 not installed or unlicensed
    def init_pscad(self):
        label_loading_pscad = None
        try:
            if self.pscad is None:  # Means PSCAD never init
                # Display Loading message only if self.pscad is None

                self.display_info('Loading PSCAD')  # method 1
                # method 2
                # label_loading_pscad = tk.Label(self, text='Loading PSCAD', fg='#007934', font='Helvetica 10 bold')
                # label_loading_pscad.grid(row=3, column=0, pady=0)  # bellow radio button

                self.update_idletasks()  # force GUI refresh

                self.pscad = mhi.pscad.application()  # use PSCAD instance already open or launch a new instance

                # method 1
                self.list_label_info[-1].destroy()  # remove last info from GUI
                self.list_label_info.pop()  # remove last element from list
                self.row_index -= 1

                # label_loading_pscad.destroy()  # method 2
            if not self.pscad.licensed():
                raise Exception
        except Exception as e:
            if label_loading_pscad is not None:
                label_loading_pscad.destroy()
            error_message = "PSCAD V5.X is not installed on this computer or is unlicensed."
            raise Exception(error_message)

    # Fill combobox values with open PSCAD project names
    def refresh_pscad_projects(self):
        pscad_project_names = self.get_available_pscad_project_names()
        self.combobox_pscad_projects["values"] = pscad_project_names

    # Actions when clicking on radio button
    def click_radio_button(self):
        selected_option = self.radio_option.get()

        if selected_option == "Option 1":
            # disable option 2 entries
            self.combobox_pscad_projects.config(state=tk.DISABLED)
            self.combobox_pscad_projects.config(foreground="gray")  # text stay in black by default
            self.button_go_to_folder.config(state=tk.DISABLED)
            self.button_refresh.config(state=tk.DISABLED)
            # enable option 1 entries
            self.entry_destination_folder.config(state=tk.NORMAL)
            self.button_browse_new_project.config(state=tk.NORMAL)

        elif selected_option == "Option 2":
            self.entry_destination_folder.config(state=tk.DISABLED)
            self.button_browse_new_project.config(state=tk.DISABLED)
            self.combobox_pscad_projects.config(state=tk.NORMAL)
            self.button_go_to_folder.config(state=tk.NORMAL)
            self.button_refresh.config(state=tk.NORMAL)

            try:
                self.init_pscad()  # Get self.pscad. Raise Exception if not installed or unlicensed
            except Exception as e:
                # exception to display error because does not stop algo
                self.display_error('Cannot select an available project. ' + str(e))
                self.radio_option.set("Option 1")
                self.click_radio_button()
            else:  # no error
                self.refresh_pscad_projects()
                # set black font only if selected value != placeholder_value
                self.set_combobox_black_foreground(self.combobox_pscad_projects, self.pscad_projects_selected_value,
                                                   self.combobox_pscad_projects_placeholder)

    # Fill entry with the selected folder path
    def select_folder(self, entry):  # ***-->***
        initial_dir = entry.get()  # Get the current folder in the entry
        folder = filedialog.askdirectory(initialdir=initial_dir)  # Address to the selected folder
        if folder:
            entry.delete(0, tk.END)
            entry.insert(0, folder)
            entry.config(foreground="black")  # because can be gray if placeholder value

    # Run the program and display GUI
    def start(self):
        self.create_widgets()

    # Remove placeholder value when clicking on the entry
    def remove_placeholder(self, entry, placeholder_value):
        if entry.get() == placeholder_value:
            entry.delete(0, tk.END)
            entry.config(foreground="black")  # Changement de la couleur du texte

    # Display placeholder value if entry is left empty
    def display_placeholder(self, entry, placeholder_value):
        if not entry.get():  # if entry empty
            entry.insert(0, placeholder_value)
            entry.config(foreground="gray")

    # Display placeholder value for a combobox if no value selected
    def display_combobox_placeholder(self, combobox, combobox_selected_value, placeholder_value):
        if not combobox_selected_value.get():  # empty
            combobox_selected_value.set(placeholder_value)
            combobox.config(foreground="gray")

    def set_combobox_black_foreground(self, combobox, combobox_selected_value, placeholder_value):
        # For info, combobox_selected_value should never be empty
        if combobox_selected_value.get() != "" and combobox_selected_value.get() != placeholder_value:
            combobox.config(foreground="black")

    # Create all GUI labels, buttons, etc.
    def create_widgets(self):

        pady_value = (10, 0)  # 10 top, 0 bottom

        # Row for PSCX file
        self.label_dll_file_path = ttk.Label(self, text="DLL File Path")
        self.entry_dll_file_path = ttk.Entry(self, width=50)
        self.button_browse_pscx_file_path = ttk.Button(self, text="Browse",
                                                       command=lambda: self.open_file(self.entry_dll_file_path,
                                                                                     '.dll'))

        self.label_dll_file_path.grid(row=self.row_index, pady=pady_value)  # pady add spaces up and down
        self.entry_dll_file_path.grid(row=self.row_index, column=1, pady=pady_value)
        self.button_browse_pscx_file_path.grid(row=self.row_index, column=2, pady=pady_value)
        self.row_index += 1

        # Option 1:
        self.radio_option = tk.StringVar(
            value="Option 1")  # variable to store the selected value. Default value is option 1
        radio_button1 = ttk.Radiobutton(self, text="Create New Project", variable=self.radio_option,
                                        value="Option 1",
                                        command=self.click_radio_button)  # To switch between radio buttons
        # sticky="w" to left align radio buttons
        radio_button1.grid(row=self.row_index, column=0, pady=pady_value, sticky="w", padx=(10, 0))

        self.entry_destination_folder = ttk.Entry(self, width=50)  # The entry to select a folder as the destination for the new project
        self.entry_destination_folder.grid(row=self.row_index, column=1, pady=pady_value)
        self.entry_destination_folder.bind("<FocusIn>", lambda event, entry=self.entry_destination_folder, placeholder_value=self.entry_destination_folder_placeholder: self.remove_placeholder(entry, placeholder_value))
        self.entry_destination_folder.bind("<FocusOut>", lambda event, entry=self.entry_destination_folder, placeholder_value=self.entry_destination_folder_placeholder: self.display_placeholder(entry, placeholder_value))
        self.display_placeholder(self.entry_destination_folder, self.entry_destination_folder_placeholder)

        self.entry_destination_folder.config(state=tk.DISABLED)

        self.button_browse_new_project = ttk.Button(self, text="Browse",
                                                    command=lambda: self.select_folder(self.entry_destination_folder))
        self.button_browse_new_project.grid(row=self.row_index, column=2, pady=pady_value)
        self.button_browse_new_project.config(state=tk.DISABLED)

        self.row_index += 1

        # Option 2:
        radio_button2 = ttk.Radiobutton(self, text="Use Available Project", variable=self.radio_option,
                                        value="Option 2", command=self.click_radio_button)  # Switching to option 2
        # sticky="w" to left align
        radio_button2.grid(row=self.row_index, column=0, pady=pady_value, sticky="w", padx=(10, 0))

        self.pscad_projects_selected_value = tk.StringVar()  # Variable to store selected value
        self.combobox_pscad_projects = ttk.Combobox(self, width=47,
                                                    textvariable=self.pscad_projects_selected_value)
        self.combobox_pscad_projects.grid(row=self.row_index, column=1, pady=pady_value)
        self.combobox_pscad_projects.config(state=tk.DISABLED)
        self.pscad_projects_selected_value.set("")  # to define a default selected value
        self.display_combobox_placeholder(self.combobox_pscad_projects, self.pscad_projects_selected_value,
                                          self.combobox_pscad_projects_placeholder)
        self.combobox_pscad_projects.bind("<FocusIn>", lambda event,
                                                              combobox=self.combobox_pscad_projects,
                                                              combobox_selected_value=self.pscad_projects_selected_value,
                                                              placeholder_value=self.combobox_pscad_projects_placeholder: self.set_combobox_black_foreground(combobox, combobox_selected_value, placeholder_value))

        # 24x24px
        icon_base64_refresh = 'iVBORw0KGgoAAAANSUhEUgAAABgAAAAYCAYAAADgdz34AAAAAXNSR0IArs4c6QAAAcZJREFUSEvN1cvLTWEUBvDfFyKKhEQGbgPkkhiIAUoiURgwUkwkBiYG/AMykJSJxEARMsBAQkoxUC4pykTkVi4xYKLcWrU+bdnv3ufkfOUd7X3Oep9n3Z5n9xng0zfA+P4LgpXYgVmYiJ94hQc4jSv4XupEUwXTcBYLWtp4F2vwri6uRLAC5zEqLx7FRTxNkNlYha1Z1WvEnSc4h5mYE7F1BJPxGMNxFRvxpVBFJHAS6/ABe3Esn8eVCG5jMS5jLX40tOggXmAzFlXiDmN3HcFQvMdnzMtMSviR4dtCF6I9j0oVzMBXPOtAI5H1LmzC4Iy/h4X9d3ulg7HYloPfj2u9JigW26sKOiZYgn24iRMtQ+5gRH/r4CHm5s1vuIADCLW2nRGY0r89dTOYj/s1KKGDGOKnBobxmcQExKr/9qbqDI5gZwXkFk4hPGlPA3isZyh+ebZ2WTW2SvARo9M5D2FY+lH4TckqRuISlmZMOO7LEsGNVHGIZkOa1qC0guO4nh4Vv03FemzHmGxfvMdy/HGa1jRs+gymt0z3TnrR87q4Nh0MwWpsyQ/OpPSeN1lNzC0q71gHbavY9f9tFXQN2M0M/hk8AH4BquBSGZPuD3sAAAAASUVORK5CYII='
        # 24x24px
        icon_base64_go_to_folder = 'iVBORw0KGgoAAAANSUhEUgAAABgAAAAYCAQAAABKfvVzAAAAIGNIUk0AAHomAACAhAAA+gAAAIDoAAB1MAAA6mAAADqYAAAXcJy6UTwAAAACYktHRAD/h4/MvwAAAAd0SU1FB+cKEQwjJO4wwxUAAAGHSURBVDjLjdM/a1NhFAbw331zo21KapqotBa7qHTRD2Bx1G/gn0XQzcFFUBA/gNTNRUGcOinYQVz8g4NUXARBXCwOVYMYTNralkZJNfE69BprxXifM72H8zzv4Tnn0Atjph2UGUVTEjPGspXnTepIJO4o/U7HoGRCX5p576UEZ5wTwDE1F61tVDvinUceeuCFe3I4bkXSjY5LcuulORxyVsMJt90y56RBVXvNatsDXrlp2VanJKpw3VNH079GXDZpGFxI9afAAU9cI1bQZ9p9Q4JI21VBzqgfKqlIybhIy4xRhchj45oW5IRuRIJIWVki8tWySFDQMht564o3qVvJJmv/fHfsdz624LVnGUfZrxas2pl59rs1gkU7MhN2+Rg0bM9MGFEN6l37/oe8ig9BXVmUiTCguN5S0ZZMhG3y6sG8QndTe6OibSn4LDaQiTBsVTNYkRjMaOqitaDp+8aL6kn4RNDyxVDGlmrEvpk3Ye7XRf0DiYJ97hLhsBv6/9rUzYg9d9rSTyFpa7yLVQGQAAAAJXRFWHRkYXRlOmNyZWF0ZQAyMDIzLTEwLTE3VDEyOjM1OjI1KzAwOjAwuFkSYAAAACV0RVh0ZGF0ZTptb2RpZnkAMjAyMy0xMC0xN1QxMjozNToyNSswMDowMMkEqtwAAAAodEVYdGRhdGU6dGltZXN0YW1wADIwMjMtMTAtMTdUMTI6MzU6MzYrMDA6MDBjU5EAAAAAAElFTkSuQmCC'
        self.go_to_folder_image = tk.PhotoImage(data=icon_base64_go_to_folder)
        self.refresh_image = tk.PhotoImage(data=icon_base64_refresh)

        self.button_refresh = ttk.Button(self, image=self.refresh_image, command=lambda: self.refresh_pscad_projects())

        self.button_go_to_folder = ttk.Button(self, image=self.go_to_folder_image,
                                              command=lambda: self.go_to_selected_project_folder())
        self.button_refresh.grid(row=self.row_index, column=2, pady=pady_value, padx=(0, 40))
        self.button_go_to_folder.grid(row=self.row_index, column=2, pady=pady_value, padx=(40, 0))
        self.button_refresh.config(state=tk.DISABLED)
        self.button_go_to_folder.config(state=tk.DISABLED)

        self.row_index += 1

        self.button_generate_pscad_model = ttk.Button(self, text="Generate PSCAD Model",
                                                      command=lambda: self.generate_pscad_model())
        self.button_generate_pscad_model.grid(row=self.row_index, column=1, pady=10)

        self.row_index += 1

        self.click_radio_button()  # to change states of entries

        self.grid_columnconfigure(0, minsize=160)  # min width for labels (column 0)
        self.grid_columnconfigure(2, minsize=100)  # min width for Browse (column 2)

        # FOR DEBUG
        #self.entry_dll_file_path.insert(0, 'C:\\Users\\Cesar\\RTE\\CIGRE_IEEE_DLL\\Cesar\\SCRX9_dll_source_modif\\Release\\SCRX9.dll')
        #self.entry_dll_file_path.insert(0, 'C:\\Users\\Cesar\\RTE\\CIGRE_IEEE_DLL\\Cesar\\Gain_IEEE_CIGRE_DLL\\Release\\Gain_IEEE_CIGRE_DLL.dll')
        #self.button_generate_pscad_model.invoke()  # Appuyer sur le bouton
        # self.generate_pscad_model()

    # remove displayed errors
    def clean_errors_and_info(self):
        for label_error in self.list_label_errors:
            label_error.destroy()
            self.row_index -= 1
        for label_info in self.list_label_info:
            label_info.destroy()
            self.row_index -= 1

    # Reset list attributes
    def clean_list_attributes(self):
        self.in_names = []
        self.in_fortran_types = []
        self.in_pscad_types = []
        self.in_width = []

        self.out_names = []
        self.out_units = []
        self.out_fortran_types = []
        self.out_pscad_types = []
        self.out_width = []

        self.param_names = []
        self.param_fortran_types = []
        self.param_pscad_types = []
        self.param_group_names = []
        self.param_descriptions = []
        self.param_units = []
        self.param_default_values = []
        self.param_min_values = []
        self.param_max_values = []

        self.list_label_errors = []
        self.list_label_info = []

    # Main function, called when clicking on the generate button
    def generate_pscad_model(self):

        try:
            self.clean_errors_and_info()  # Remove displayed errors

            self.clean_list_attributes()  # Reset lists

            self.get_and_check_dll_file_path()  # fill dll_file_path

            self.get_dll_model_info()  # get self.Model_Info

            self.get_and_check_dll_interface_version()

            self.Model_Name = self.Model_Info.ModelName.decode("utf-8")
            self.Model_Name_Shortened = self.Model_Name[:50]  # take max 50 char

            self.get_and_check_pscad_project_name()  # get self.pscad_project_name

            self.get_destination_folder()  # Get self.destination_folder

            self.fortran_interface_file_name = self.Model_Name_Shortened + '_FINTERFACE_PSCAD.f90'
            self.fortran_interface_file_path = self.destination_folder + '\\' + self.fortran_interface_file_name

            # Create wrapper code and write it to the Fortran file
            self.fill_in_out_param_lists()
            buffer = self.create_fortran_code()
            with open(self.fortran_interface_file_path, 'w') as f:
                f.write(buffer)  # allow rewrite on existing file

            # Copy the DLL into the destination folder
            destination_dll_path = self.destination_folder + '\\' + self.dll_file_name
            if not os.path.exists(destination_dll_path):
                shutil.copy(self.dll_file_path, destination_dll_path)

            # Generate PSCAD projet and component
            self.generate_pscad_project()

        except Exception as e:
            #self.display_error(e.args[0])  # for old version of python
            #self.display_error(repr(e))  # contains Exception in the message...
            self.display_error(str(e))

    def remove_forbidden_char(self, str):
        str = str.replace(' ', '_')
        return str

    def fill_in_out_param_lists(self):
        for i in range(0, self.Model_Info.NumInputPorts):
            signal = self.Model_Info.InputPortsInfo[i]
            if signal.Name is None:
                raise Exception('One of the inputs has no Name, it is forbidden')
            name = signal.Name.decode("utf-8")
            name = self.remove_forbidden_char(name)
            datatype = signal.DataType  # ex: IEEE_Cigre_DLLInterface_DataType_int8_T
            if datatype is None:
                raise Exception('One of the inputs has no DataType, it is forbidden')
            self.in_names.append(name)
            try:
                width = signal.Width  # because parameters have no width attribute
                if width is None:
                    width = 1
            except Exception as e:
                width = 1
            self.in_width.append(width)

            fortran_type_str = self.ieee_cigre_datatype_to_fortran_type_str(datatype)
            self.in_fortran_types.append(fortran_type_str)
            pscad_type_str = self.ieee_cigre_datatype_to_pscad_inout_type_str(datatype)
            self.in_pscad_types.append(pscad_type_str)

        for i in range(0, self.Model_Info.NumOutputPorts):
            signal = self.Model_Info.OutputPortsInfo[i]
            if signal.Name is None:
                raise Exception('One of the outputs has no Name, it is forbidden')
            name = signal.Name.decode("utf-8")
            name = self.remove_forbidden_char(name)
            if signal.Unit is None:
                unit = ''
            else:
                unit = signal.Unit.decode("utf-8")
            self.out_units.append(unit)
            datatype = signal.DataType  # ex: IEEE_Cigre_DLLInterface_DataType_int8_T
            if datatype is None:
                raise Exception('One of the outputs has no DataType, it is forbidden')
            self.out_names.append(name)
            try:
                width = signal.Width  # because parameters have no width attribute
                if width is None:
                    width = 1
            except Exception as e:
                width = 1
            self.out_width.append(width)

            fortran_type_str = self.ieee_cigre_datatype_to_fortran_type_str(datatype)
            self.out_fortran_types.append(fortran_type_str)
            pscad_type_str = self.ieee_cigre_datatype_to_pscad_inout_type_str(datatype)
            self.out_pscad_types.append(pscad_type_str)

        for i in range(0, self.Model_Info.NumParameters):
            parameter = self.Model_Info.ParametersInfo[i]
            if parameter.Name is None:
                raise Exception('One of the parameters has no Name, it is forbidden')
            name = parameter.Name.decode("utf-8")
            name = self.remove_forbidden_char(name)
            datatype = parameter.DataType  # ex: IEEE_Cigre_DLLInterface_DataType_int8_T
            if datatype is None:
                raise Exception('One of the parameters has no DataType, it is forbidden')
            if parameter.GroupName is None:
                group_name = ''  # Will be in General group by default
            else:
                group_name = parameter.GroupName.decode("utf-8")
            if parameter.Description is None:
                description = ''
            else:
                description = parameter.Description.decode("utf-8")
            if parameter.Unit is None:
                unit = ''
            else:
                unit = parameter.Unit.decode("utf-8")
            if parameter.FixedValue is None:
                fixedValue = 0
            else:
                fixedValue = parameter.FixedValue  # is int, keep it in case of calling Model_CheckParameters each time parameters have really changed

            if parameter.DefaultValue is None:
                raise Exception('One of the parameters has no DefaultValue, it is forbidden')
            if parameter.MinValue is None:
                raise Exception('One of the parameters has no MinValue, it is forbidden')
            if parameter.MaxValue is None:
                raise Exception('One of the parameters has no MaxValue, it is forbidden')
            default_value = self.get_parameter_default_min_or_max_value(parameter.DefaultValue, datatype)
            # IEEE CIGRE DLL allows min max value for strings
            min_value = self.get_parameter_default_min_or_max_value(parameter.MinValue, datatype)
            max_value = self.get_parameter_default_min_or_max_value(parameter.MaxValue, datatype)
            """if not datatype == IEEE_Cigre_DLLInterface_DataType_Enum.IEEE_Cigre_DLLInterface_DataType_c_string_T:
                min_value = self.get_parameter_default_min_or_max_value(parameter.MinValue, datatype)
                max_value = self.get_parameter_default_min_or_max_value(parameter.MaxValue, datatype)
            else:  # no min max value
                min_value = None
                max_value = None"""

            self.param_names.append(name)
            fortran_type_str = self.ieee_cigre_datatype_to_fortran_type_str(datatype)
            self.param_fortran_types.append(fortran_type_str)
            pscad_type_str = self.ieee_cigre_datatype_to_pscad_param_type_str(datatype)
            self.param_pscad_types.append(pscad_type_str)
            self.param_group_names.append(group_name)
            self.param_descriptions.append(description)
            self.param_units.append(unit)
            self.param_fixedValue.append(fixedValue)
            self.param_default_values.append(default_value)
            self.param_min_values.append(min_value)
            self.param_max_values.append(max_value)

        self.nb_inputs_total = sum(self.in_width)
        self.nb_outputs_total = sum(self.out_width)
        self.nb_params_total = self.Model_Info.NumParameters

    # parameter is of type IEEE_Cigre_DLLInterface_Parameter
    def get_parameter_default_min_or_max_value(self, parameter_union, datatype):
        if datatype == IEEE_Cigre_DLLInterface_DataType_Enum.IEEE_Cigre_DLLInterface_DataType_char_T:
            return parameter_union.Char_Val
        elif datatype == IEEE_Cigre_DLLInterface_DataType_Enum.IEEE_Cigre_DLLInterface_DataType_int8_T:
            return parameter_union.Int8_Val
        elif datatype == IEEE_Cigre_DLLInterface_DataType_Enum.IEEE_Cigre_DLLInterface_DataType_uint8_T:
            return parameter_union.Uint8_Val
        elif datatype == IEEE_Cigre_DLLInterface_DataType_Enum.IEEE_Cigre_DLLInterface_DataType_int16_T:
            return parameter_union.Int16_Val
        elif datatype == IEEE_Cigre_DLLInterface_DataType_Enum.IEEE_Cigre_DLLInterface_DataType_uint16_T:
            return parameter_union.Uint16_Val
        elif datatype == IEEE_Cigre_DLLInterface_DataType_Enum.IEEE_Cigre_DLLInterface_DataType_int32_T:
            return parameter_union.Int32_Val
        elif datatype == IEEE_Cigre_DLLInterface_DataType_Enum.IEEE_Cigre_DLLInterface_DataType_uint32_T:
            return parameter_union.Uint32_Val
        elif datatype == IEEE_Cigre_DLLInterface_DataType_Enum.IEEE_Cigre_DLLInterface_DataType_real32_T:
            return parameter_union.Real32_Val
        elif datatype == IEEE_Cigre_DLLInterface_DataType_Enum.IEEE_Cigre_DLLInterface_DataType_real64_T:
            return parameter_union.Real64_Val
        elif datatype == IEEE_Cigre_DLLInterface_DataType_Enum.IEEE_Cigre_DLLInterface_DataType_c_string_T:
            try:
                return parameter_union.Char_Ptr.decode("utf-8")  # ok for DefaultValueU
            except Exception:
                # MinMaxValueU has no const char_T * const    Char_Ptr;
                return parameter_union.Char_Val
        else:
            raise Exception('get_parameter_default_min_or_max_value : error in dataType_enum: ' + str(datatype))


    """def generate_storfloat_declaration(self):
        buffer = ''
        NumFloatStates = self.Model_Info.NumFloatStates
        if NumFloatStates > 0:
            buffer += '\t! ---- Variables to use if STORFLOAT needed (for IEEE CIGRE FloatStates array) ----\n' \
              '\t! we can also try to use REAL*4, POINTER :: STORFLOAT(:) and store pointer address in STORI\n' \
              '\tREAL*4, DIMENSION (:), ALLOCATABLE :: STORFLOAT ! To store IEEE CIGRE FloatStates array\n' \
              '\tINTEGER :: NSTORFLOAT ! STORFLOAT index\n' \
              '\tINTEGER :: N_MAX_INSTANCE = 100  ! nb max of instances of this specific IEEE CIGRE Model\n' \
              '\tDOUBLE PRECISION :: TIME_PREV = -1  ! Used only to reset NSTORFLOAT to 1\n\n' \

        return buffer"""

    def generate_type(self, type_name, names, fortran_types, widths=None):
        buffer = ''
        buffer += '\t'
        buffer += 'TYPE ' + type_name
        for i in range(0, len(names)):
            buffer += '\n\t\t'

            name = names[i]
            fortran_type = fortran_types[i]
            width = 1
            if widths is not None:
                width = widths[i]

            buffer += fortran_type + ' :: ' + name
            if width > 1:
                buffer += '(' + str(width) + ')'

        buffer += '\n\t'
        buffer += 'END TYPE'
        return buffer

    def generate_finterface_function_prototype(self):
        buffer = ''
        buffer += '\tSUBROUTINE ' + self.Model_Name_Shortened + '_FINTERFACE_PSCAD('
        for name in self.in_names:
            buffer += name + '_pscad, '
        for name in self.param_names:
            buffer += name + '_pscad, '
        for name in self.out_names:
            buffer += name + '_pscad, '
        for name in self.out_names:
            buffer += name + '_init_pscad, '

        buffer += 'TRelease, DLL_Path, Use_Interpolation)'

        return buffer

    def generate_variables_from_pscad(self):
        buffer = ''
        buffer += self.generate_variables_declaration('IN', self.in_names, '_pscad', self.in_pscad_types, self.in_width)
        buffer += '\n'
        buffer += self.generate_variables_declaration('IN', self.param_names, '_pscad', self.param_pscad_types, None)
        buffer += '\n'
        out_init_names = [x + '_init' for x in self.out_names]
        buffer += self.generate_variables_declaration('IN', out_init_names, '_pscad', self.out_pscad_types, self.out_width)
        buffer += '\n'
        buffer += self.generate_variables_declaration('OUT', self.out_names, '_pscad', self.out_pscad_types, self.out_width)
        return buffer

    def generate_variables_declaration(self, intent_value, var_names, var_suffix, var_types, var_width=None):
        buffer = ''
        intent_str = ''
        if intent_value is not None and intent_value != '':
            intent_str = ', INTENT(' + intent_value + ')'

        for i in range(0, len(var_names)):
            if var_width is not None and var_width[i] > 1:
                buffer += ('\t' + var_types[i] + intent_str + ' :: ' + var_names[i] + var_suffix +
                           '(' + str(var_width[i]) + ')\n')
            else:
                buffer += '\t' + var_types[i] + intent_str + ' :: ' + var_names[i] + var_suffix + '\n'
        return buffer

    # No more used
    """def generate_storfloat_code(self):
        buffer = ''
        NumFloatStates = self.Model_Info.NumFloatStates
        if NumFloatStates > 0:
            buffer += '\t! Allocate STORFLOAT\n' \
                      '\tIF (FIRST_CALL_THIS_FILE) THEN  ! Cannot use FIRSTSTEP because of multi-instance\n' \
                      '\t\tALLOCATE(STORFLOAT(' + str(NumFloatStates) + '*N_MAX_INSTANCE))   ! ' + str(NumFloatStates) + ' float state arrays\n' \
                      '\t\tSTORFLOAT = 0 ! Init to zero\n' \
                      '\tENDIF\n\n' \
                      '\t! Check if PSCAD time has changed to reset NSTORFLOAT\n' \
                      '\tIF ( TIME .GT. TIME_PREV + EQUALITY_PRECISION ) THEN\n' \
                      '\t\tTIME_PREV = TIME\n' \
                      '\t\tNSTORFLOAT = 1\n' \
                      '\tENDIF'

        return buffer"""

    # Keep it in case of calling Model_CheckParameters each time parameters have really changed (but do not use PARAMETERS_new)
    """def generate_parameters_change_check(self):
        buffer = ''

        #param_names_that_change = []
        l1 = []  # will be ['PARAMETERS_new%param1 /= param1_pscad', 'PARAMETERS_new%param2 /= param2_pscad', ...]
        for i, value in enumerate(self.param_fixedValue):
            if value == 0:  # fixedValue = 0, means parameters can be modified at any time
                #param_names_that_change.append(self.param_names[i])
                l1.append('PARAMETERS_new%' + self.param_names[i] + ' /= ' + self.param_names[i] + '_pscad')

        # Add OR between elements and make a string
        check_str = ''
        if len(l1) > 0:
            check_str = ' .OR. '.join(l1)

        if check_str != '':
            buffer += '\t! Check if parameters have changed. Usefull to call Model_CheckParameters again or not\n'
            buffer += '\t! Check only parameters that may change (with .FixedValue = 0)\n'
            buffer += '\tIF (' + check_str + ') THEN\n'
            buffer += '\t\tParametersChanged = .TRUE.\n'
            buffer += '\tENDIF\n\n'

        return buffer"""

    """def generate_pointers_to_state_arrays(self):
        buffer = ''
        buffer += '\t! Setup Pointers to state variable storage\n'
        if self.Model_Info.NumIntStates > 0:
            buffer += '\tpInstance%IntStates    = c_loc(STATEI(1))\n'
        if self.Model_Info.NumFloatStates > 0:
            buffer += '\tpInstance%FloatStates  = c_loc(STATEF(1))\n'
        if self.Model_Info.NumDoubleStates > 0:
            buffer += '\tpInstance%DoubleStates = c_loc(STATED(1))\n'

        buffer += '\n'
        buffer += '\t! Copy values from STORx to STATEx\n'

        if self.Model_Info.NumIntStates > 0:
            buffer += '\tDO i = 1, ' + str(self.Model_Info.NumIntStates) + '\n'
            buffer += '\t\tSTATEI(i) = STORI(NSTORI + i - 1)\n'
            buffer += '\tEND DO\n'
        if self.Model_Info.NumFloatStates > 0:
            buffer += '\tDO i = 1, ' + str(self.Model_Info.NumFloatStates) + '\n'
            buffer += '\t\tSTATEF(i) = STORF(idx_start_statef + i - 1)\n'
            buffer += '\tEND DO\n'
        if self.Model_Info.NumDoubleStates > 0:
            buffer += '\tDO i = 1, ' + str(self.Model_Info.NumDoubleStates) + '\n'
            buffer += '\t\tSTATED(i) = STORF(idx_start_stated + i - 1)\n'
            buffer += '\tEND DO\n'

        buffer += '\n'
        return buffer"""

    """def generate_storfloat_to_storf(self):
        buffer = ''
        for i in range(self.Model_Info.NumFloatStates):
            buffer += '\tSTORF(NSTORF + 1 + ' + str(i) + ') = STORFLOAT(' + str(i + 1) + ')\n'

        return buffer"""

    """def generate_state_arrays_to_storf(self):
        buffer = ''
        buffer += '\t! Copy values from STATEx to STORx\n'

        if self.Model_Info.NumIntStates > 0:
            buffer += '\tDO i = 1, ' + str(self.Model_Info.NumIntStates) + '\n'
            buffer += '\t\tSTORI(NSTORI + i - 1) = STATEI(i)\n'
            buffer += '\tEND DO\n'
        if self.Model_Info.NumFloatStates > 0:
            buffer += '\tDO i = 1, ' + str(self.Model_Info.NumFloatStates) + '\n'
            buffer += '\t\tSTORF(idx_start_statef + i - 1) = STATEF(i)\n'
            buffer += '\tEND DO\n'
        if self.Model_Info.NumDoubleStates > 0:
            buffer += '\tDO i = 1, ' + str(self.Model_Info.NumDoubleStates) + '\n'
            buffer += '\t\tSTORF(idx_start_stated + i - 1) = STATED(i)\n'
            buffer += '\tEND DO\n'

        return buffer"""

    # names is self.in_names or self.param_names or self.out_names
    # same for widths, fortran_types and pscad_types
    # suffix is suffix used in varibales for part_2. If None, use STORF
    # fill_new_struct is boolean :
    # * True to fill INPUTS_new, OUTPUTS_new or PARAMETERS_new (INPUTS_new etc. on the left side)
    # * False to get values from them (INPUTS_new etc. on the right side)
    # new_struct is "INPUTS_new", "OUTPUTS_new" or "PARAMETERS_new"
    def generate_conversion(self, names, widths, fortran_types, pscad_types, suffix, fill_new_struct, new_struct):
        buffer = ''
        # width_sum = sum(widths)
        i_storf = 0
        for i in range(0, len(names)):
            if widths is not None:
                width = widths[i]
            else:
                width = 1
            name = names[i]
            pscad_type = pscad_types[i]
            fortran_type = fortran_types[i]

            part_1_base = new_struct + '%' + name  # ex: INPUTS_new%in_1

            for j in range(1, width + 1):  # work also if width == 1
                if width == 1:
                    part_1 = part_1_base
                else:
                    part_1 = part_1_base + '(' + str(j) + ')'

                if suffix is None:  # use STORF
                    """if self.Model_Info.NumDoubleStates > 0:
                        part_2 = 'STORF(NSTORF + 2 + ' + str(i_storf) + ')'
                    else:
                        part_2 = 'STORF(NSTORF + 1 + ' + str(i_storf) + ')'"""

                    part_2 = 'STORF(idx_start_outputs + ' + str(i_storf) + ')'
                    i_storf += 1

                    if fill_new_struct:  # STORF on the right side
                        type_from = 'DOUBLE PRECISION'  # REAL*8
                        type_to = fortran_type
                    else:
                        type_from = fortran_type
                        type_to = 'DOUBLE PRECISION'
                else:
                    part_2 = name + suffix
                    if width > 1:
                        part_2 += '(' + str(j) + ')'

                    if fill_new_struct:
                        type_from = pscad_type
                        type_to = fortran_type
                    else:
                        type_from = fortran_type
                        type_to = pscad_type

                conversion_part_1, conversion_part_2 = self.get_conversion(type_from, type_to)

                if fill_new_struct:
                    buffer += '\t' + part_1 + ' = ' + conversion_part_1 + part_2 + conversion_part_2
                else:
                    buffer += '\t' + part_2 + ' = ' + conversion_part_1 + part_1 + conversion_part_2

                buffer += '\n'

        return buffer

    def generate_storf_to_prev_inputs(self):
        buffer = ''
        # width_sum = sum(widths)
        i_storf = 0
        for i in range(0, self.Model_Info.NumInputPorts):
            width = self.in_width[i]
            name = self.in_names[i]
            pscad_type = self.in_pscad_types[i]
            #fortran_type = self.in_fortran_types[i]

            part_1_base = name + '_pscad_prev'

            for j in range(1, width + 1):  # work also if width == 1
                if width == 1:
                    part_1 = part_1_base
                else:
                    part_1 = part_1_base + '(' + str(j) + ')'

                part_2 = 'STORF(idx_start_inputs + ' + str(i_storf) + ')'
                i_storf += 1

                type_from = 'DOUBLE PRECISION'  # REAL*8
                type_to = pscad_type

                conversion_part_1, conversion_part_2 = self.get_conversion(type_from, type_to)

                buffer += '\t\t' + part_1 + ' = ' + conversion_part_1 + part_2 + conversion_part_2
                buffer += '\n'

        return buffer

    def generate_interpolated_inputs(self):
        buffer = ''
        #i_storf = 0
        for i in range(0, self.Model_Info.NumInputPorts):
            width = self.in_width[i]
            name = self.in_names[i]
            #pscad_type = self.in_pscad_types[i]
            fortran_type = self.in_fortran_types[i]

            part_1_base = 'INPUTS_new%' + name

            for j in range(1, width + 1):  # work also if width == 1
                if width == 1:
                    part_1 = part_1_base
                    part_2 = name + '_pscad_prev + (' + name + '_pscad - ' + name + '_pscad_prev) * delta_t2 / delta_t1'
                else:
                    part_1 = part_1_base + '(' + str(j) + ')'
                    part_2 = (name + '_pscad_prev(' + str(j) + ') + (' + name + '_pscad(' + str(j) + ') - ' +
                              name + '_pscad_prev(' + str(j) + ')) * delta_t2 / delta_t1')

                #i_storf += 1

                type_from = 'DOUBLE PRECISION'  # REAL*8
                type_to = fortran_type

                conversion_part_1, conversion_part_2 = self.get_conversion(type_from, type_to)

                buffer += '\t\t\t' + part_1 + ' = ' + conversion_part_1 + part_2 + conversion_part_2
                buffer += '\n'

        return buffer

    def generate_inputs_to_storf(self):
        buffer = ''
        # width_sum = sum(widths)
        i_storf = 0
        for i in range(0, self.Model_Info.NumInputPorts):
            width = self.in_width[i]
            name = self.in_names[i]
            pscad_type = self.in_pscad_types[i]
            # fortran_type = self.in_fortran_types[i]

            part_2_base = name + '_pscad'

            for j in range(1, width + 1):  # work also if width == 1

                part_1 = 'STORF(idx_start_inputs + ' + str(i_storf) + ')'

                if width == 1:
                    part_2 = part_2_base
                else:
                    part_2 = part_2_base + '(' + str(j) + ')'

                i_storf += 1

                type_from = pscad_type
                type_to = 'DOUBLE PRECISION'

                conversion_part_1, conversion_part_2 = self.get_conversion(type_from, type_to)

                buffer += '\t\t' + part_1 + ' = ' + conversion_part_1 + part_2 + conversion_part_2
                buffer += '\n'

        return buffer

    # types considered are :
    # 'INTEGER*2' 'INTEGER' 'INTEGER*4' 'REAL' 'REAL*4' 'REAL*8' 'DOUBLE PRECISION'
    # 'CHARACTER' 'CHARACTER(*)' 'CHARACTER (LEN=...)'
    def get_conversion(self, type_from, type_to):
        conversion_part_1 = None
        conversion_part_2 = None
        if type_from == 'INTEGER*2':
            if (type_to == 'INTEGER*2' or type_to == 'INTEGER' or type_to == 'INTEGER*4' or type_to == 'REAL' or
                    type_to == 'REAL*4' or type_to == 'REAL*8' or type_to == 'DOUBLE PRECISION'):
                conversion_part_1 = ''
                conversion_part_2 = ''
            elif 'CHARACTER' in type_to:  # 'CHARACTER' or 'CHARACTER(*)' or 'CHARACTER (LEN=1000)' for ex
                conversion_part_1 = 'CHAR('
                conversion_part_2 = ')'
        elif type_from == 'INTEGER' or type_from == 'INTEGER*4':
            if type_to == 'INTEGER*2':
                conversion_part_1 = 'INT('
                conversion_part_2 = ', 2)'
            elif (type_to == 'INTEGER' or type_to == 'INTEGER*4' or type_to == 'REAL' or
                    type_to == 'REAL*4' or type_to == 'REAL*8' or type_to == 'DOUBLE PRECISION'):
                conversion_part_1 = ''
                conversion_part_2 = ''
            elif 'CHARACTER' in type_to:  # 'CHARACTER' or 'CHARACTER(*)' or 'CHARACTER (LEN=1000)' for ex
                conversion_part_1 = 'CHAR('
                conversion_part_2 = ')'
        elif type_from == 'REAL' or type_from == 'REAL*4' or type_from == 'REAL*8' or type_from == 'DOUBLE PRECISION':
            if type_to == 'INTEGER*2':
                conversion_part_1 = 'INT('
                conversion_part_2 = ', 2)'
            elif type_to == 'INTEGER' or type_to == 'INTEGER*4':
                conversion_part_1 = 'INT('
                conversion_part_2 = ')'
            elif type_to == 'REAL' or type_to == 'REAL*4' or type_to == 'REAL*8' or type_to == 'DOUBLE PRECISION':
                conversion_part_1 = ''
                conversion_part_2 = ''
            elif 'CHARACTER' in type_to:  # 'CHARACTER' or 'CHARACTER(*)' or 'CHARACTER (LEN=1000)' for ex
                conversion_part_1 = 'CHAR(INT('
                conversion_part_2 = '))'
        elif type_from == 'CHARACTER':
            if (type_to == 'INTEGER*2' or type_to == 'INTEGER' or type_to == 'INTEGER*4' or type_to == 'REAL' or
                    type_to == 'REAL*4' or type_to == 'REAL*8' or type_to == 'DOUBLE PRECISION'):
                conversion_part_1 = 'ICHAR('
                conversion_part_2 = ')'
            elif 'CHARACTER' in type_to:  # 'CHARACTER' or 'CHARACTER(*)' or 'CHARACTER (LEN=1000)' for ex
                conversion_part_1 = ''
                conversion_part_2 = ''
        elif 'CHARACTER(' in type_from:  # 'CHARACTER(*)' or 'CHARACTER (LEN=1000)' for ex
            if 'CHARACTER' in type_to:
                # OK, character(len=100) to character(len=2) or to character works
                conversion_part_1 = ''
                conversion_part_2 = ''

        if conversion_part_1 is None or conversion_part_2 is None:
            raise Exception('get_conversion problem with type_from: ' + type_from + ' and type_to: ' + type_to)

        return conversion_part_1, conversion_part_2

    """# Only conversions that can appear in the wrapper
    def get_conversion(self, type_from, type_to):
        conversion_part_1 = None
        conversion_part_2 = None
        if type_from == 'INTEGER' or type_from == 'INTEGER*4':
            if type_to == 'CHARACTER':
                conversion_part_1 = 'CHAR('
                conversion_part_2 = ') ! INTEGER to CHARACTER'
            elif type_to == 'INTEGER*2':
                conversion_part_1 = 'INT('
                conversion_part_2 = ', 2) ! INTEGER to INTEGER*2. "INT" with précision 2'
            elif type_to == 'INTEGER*4' or type_to == 'INTEGER':
                conversion_part_1 = ''
                conversion_part_2 = ' ! INTEGER is INTEGER4.'
            elif type_to == 'DOUBLE PRECISION' or type_to == 'REAL*8' or type_to == 'REAL':
                conversion_part_1 = ''
                conversion_part_2 = ' ! INTEGER to DOUBLE PRECISION'
        elif type_from == 'DOUBLE PRECISION' or type_from == 'REAL*8' or type_from == 'REAL':
            if type_to == 'REAL*4':
                conversion_part_1 = ''
                conversion_part_2 = ' ! DOUBLE PRECISION (REAL*8) to REAL*4 : possible accuracy lost'
            elif type_to == 'DOUBLE PRECISION' or type_to == 'REAL*8' or type_to == 'REAL':
                conversion_part_1 = ''
                conversion_part_2 = ''
            elif type_to == 'CHARACTER':
                conversion_part_1 = 'ACHAR(INT('
                conversion_part_2 = ')) ! DOUBLE PRECISION (REAL*8) to CHARACTER'
            elif type_to == 'INTEGER*2':
                conversion_part_1 = 'INT('
                conversion_part_2 = ', 2) ! DOUBLE PRECISION (REAL*8) to INTEGER*2. "INT" with précision 2'
            elif type_to == 'INTEGER' or type_to == 'INTEGER*4':
                conversion_part_1 = 'INT('
                conversion_part_2 = ') ! DOUBLE PRECISION (REAL*8) to INTEGER*4'
        elif type_from == 'CHARACTER':
            if type_to == 'DOUBLE PRECISION' or type_to == 'REAL*8' or type_to == 'REAL':
                conversion_part_1 = 'ICHAR('
                conversion_part_2 = ') ! CHARACTER to DOUBLE PRECISION (REAL*8)'
            elif type_to == 'INTEGER' or type_to == 'INTEGER*4':
                conversion_part_1 = 'ICHAR('
                conversion_part_2 = ') ! CHARACTER to INTEGER'
        elif type_from == 'INTEGER*2':
            if type_to == 'DOUBLE PRECISION' or type_to == 'REAL*8' or type_to == 'REAL':
                conversion_part_1 = ''
                conversion_part_2 = ' ! INTEGER*2 to DOUBLE PRECISION (REAL*8)'
            elif type_to == 'INTEGER' or type_to == 'INTEGER*4':
                conversion_part_1 = ''
                conversion_part_2 = ' ! INTEGER*2 to INTEGER*4'
        elif type_from == 'REAL*4':
            if type_to == 'DOUBLE PRECISION' or type_to == 'REAL*8' or type_to == 'REAL':
                conversion_part_1 = ''
                conversion_part_2 = ' ! REAL*4 to DOUBLE PRECISION (REAL*8)'
        elif type_from == 'CHARACTER(*)':
            if type_to == 'DOUBLE PRECISION' or type_to == 'REAL*8' or type_to == 'REAL':
                conversion_part_1 = ''
                conversion_part_2 = ' ! REAL*4 to DOUBLE PRECISION (REAL*8)'

        if conversion_part_1 is None or conversion_part_2 is None:
            raise Exception('get_conversion problem with type_from: ' + type_from + ' and type_to: ' + type_to)

        return conversion_part_1, conversion_part_2"""

    # We could also convert a text file to a base64 encoded string and then save the base64 encoded string
    # as a Python module
    # import base64
    # with open('my_file.txt', 'rb') as f:
    #     encoded_file = base64.b64encode(f.read())
    # with open('my_file.py', 'w') as f:
    #     f.write('encoded_file = b"' + encoded_file.decode('ascii') + '"')
    # from my_file import encoded_file
    # fichier_decoded = base64.b64decode(encoded_file)
    def create_fortran_code(self):
        bf = ''
        bf += '\t! Common module for each instance of ' + self.Model_Name_Shortened + ' model\n' \
              '\t! Generated by RTE/TUD IEEE CIGRE DLL PSCAD Import tool version ' + str(self.num_version) + '\n' \
              '\tMODULE ' + self.Model_Name_Shortened + '_MOD\n\n' \
              '\tUSE, INTRINSIC :: iso_c_binding, only : c_ptr, c_double, c_int8_t, c_int32_t\n' \
              '\tUSE IFWIN ! To use INTEGER(handle)\n\n' \
              '\tINTEGER(handle) :: dllHandle = 0 ! Handle to the DLL\n' \
              '\tDOUBLE PRECISION :: EQUALITY_PRECISION = 1.0e-12  ! Used to test equality between two double variable\n' \
              '\tLOGICAL :: FIRST_CALL_THIS_FILE = .TRUE.  ! Used to allocate some arrays and set pointers to DLL functions\n' \
              '\tDOUBLE PRECISION   :: DELT_Model      ! The sampling time (Sec) needed by this model\n' \
              '\tINTEGER :: N_INSTANCE = 0  ! nb instances of this specific IEEE CIGRE Model, useful at last time-step to use FreeLibrary\n' \
              '\tINTEGER :: N_CALL_LAST_STEP = 0  ! nb call at last step, useful at last time-step to use FreeLibrary\n' \
              '\tINTEGER   :: IUNIT = 6 ! Unit to use for Write Statements\n' \
              '\tCHARACTER*' + str(len(self.Model_Name)) + '   :: OrigModelName = "' + self.Model_Name + '"  ! Model name, written by the DLL Import tool\n' \
              '\tCHARACTER*' + str(len(self.Model_Name_Shortened)) + '   :: OrigModelNameShortened = "' + self.Model_Name_Shortened + '"  ! Model name shortened to 50 char max, written by the DLL Import tool\n\n' \
              '\t!---------------------------------\n'\
              '\t!--- Types common to each model --\n'\
              '\t!---------------------------------\n'\
              '\tTYPE IEEE_Cigre_DLLInterface_Instance\n'\
              '\t\tTYPE(c_ptr) :: ExternalInputs\n'\
              '\t\tTYPE(c_ptr) :: ExternalOutputs\n'\
              '\t\tTYPE(c_ptr) :: Parameters\n'\
              '\t\tREAL(KIND=c_double) :: Time\n'\
              '\t\tCHARACTER   :: SimTool_EMT_RMS_Mode\n'\
              '\t\tTYPE(c_ptr) :: LastErrorMessage\n'\
              '\t\tTYPE(c_ptr) :: LastGeneralMessage\n'\
              '\t\tTYPE(c_ptr) :: IntStates\n'\
              '\t\tTYPE(c_ptr) :: FloatStates\n'\
              '\t\tTYPE(c_ptr) :: DoubleStates\n'\
              '\tEND TYPE\n\n'\
              '\tTYPE IEEE_Cigre_DLLInterface_Model_Info\n' \
              '\t\tINTEGER(KIND=c_int8_t), DIMENSION(4) :: DLLInterfaceVersion\n' \
              '\t\tTYPE(c_ptr) :: ModelName\n' \
              '\t\tTYPE(c_ptr) :: ModelVersion\n' \
              '\t\tTYPE(c_ptr) :: ModelDescription\n' \
              '\t\tTYPE(c_ptr) :: GeneralInformation\n' \
              '\t\tTYPE(c_ptr) :: ModelCreated\n' \
              '\t\tTYPE(c_ptr) :: ModelCreator\n' \
              '\t\tTYPE(c_ptr) :: ModelLastModifiedDate\n' \
              '\t\tTYPE(c_ptr) :: ModelLastModifiedBy\n' \
              '\t\tTYPE(c_ptr) :: ModelModifiedComment\n' \
              '\t\tTYPE(c_ptr) :: ModelModifiedHistory\n' \
              '\t\tREAL(KIND=c_double) :: FixedStepBaseSampleTime\n' \
              '\t\tINTEGER(KIND=c_int8_t) :: EMT_RMS_Mode\n' \
              '\t\tINTEGER(KIND=c_int32_t) :: NumInputPorts\n' \
              '\t\tTYPE(c_ptr) :: InputPortsInfo\n' \
              '\t\tINTEGER(KIND=c_int32_t) :: NumOutputPorts\n' \
              '\t\tTYPE(c_ptr) :: OutputPortsInfo\n' \
              '\t\tINTEGER(KIND=c_int32_t) :: NumParameters\n' \
              '\t\tTYPE(c_ptr) :: ParametersInfo\n' \
              '\t\tINTEGER(KIND=c_int32_t) :: NumIntStates\n' \
              '\t\tINTEGER(KIND=c_int32_t) :: NumFloatStates\n' \
              '\t\tINTEGER(KIND=c_int32_t) :: NumDoubleStates\n' \
              '\tEND TYPE\n\n' \
              '\t!---------------------------------\n' \
              '\t!--- Types specific to a model ---\n' \
              '\t!---------------------------------\n' \
              '\t! See Fortran C equivalent here: https://docs.oracle.com/cd/E19957-01/805-4940/z40009104412/index.html\n' \
              '\t! We could also used types with kind type parameters from the ISO_C_BINDING module\n'

        type_modelinputs = self.generate_type('ModelInputs', self.in_names, self.in_fortran_types, self.in_width)
        type_modeloutputs = self.generate_type('ModelOutputs', self.out_names, self.out_fortran_types, self.out_width)
        type_modelparameters = self.generate_type('ModelParameters', self.param_names, self.param_fortran_types)

        bf += type_modelinputs + '\n\n' + type_modeloutputs + '\n\n' + type_modelparameters + '\n\n'

        bf += '\t!----------------------------------\n' \
              '\t!--- Interface to DLL functions ---\n' \
              '\t!----------------------------------\n' \
              '\tINTERFACE\n' \
              '\t\tFUNCTION Model_GetInfo()\n' \
              '\t\t\tIMPORT :: c_ptr\n' \
              '\t\t\t!DEC$ ATTRIBUTES C :: Model_GetInfo\n' \
              '\t\t\t! Any input parameters still use the DEC statement here\n' \
              '\t\t\tTYPE(c_ptr) :: Model_GetInfo\n' \
              '\t\tEND FUNCTION\n' \
              '\t\tFUNCTION Model_Outputs(pInstance)\n' \
              '\t\t\tIMPORT :: c_int32_t, IEEE_Cigre_DLLInterface_Instance\n' \
              '\t\t\t!DEC$ ATTRIBUTES C :: Model_Outputs\n' \
              '\t\t\t!DEC$ ATTRIBUTES REFERENCE :: pInstance\n' \
              '\t\t\tTYPE(IEEE_Cigre_DLLInterface_Instance) pInstance\n' \
              '\t\t\tINTEGER(c_int32_t) :: Model_Outputs\n' \
              '\t\tEND FUNCTION\n' \
              '\t\tFUNCTION Model_Initialize(pInstance)\n' \
              '\t\t\tIMPORT :: c_int32_t, IEEE_Cigre_DLLInterface_Instance\n' \
              '\t\t\t!DEC$ ATTRIBUTES C :: Model_Initialize\n' \
              '\t\t\t!DEC$ ATTRIBUTES REFERENCE :: pInstance\n' \
              '\t\t\tTYPE(IEEE_Cigre_DLLInterface_Instance) pInstance\n' \
              '\t\t\tINTEGER(c_int32_t) :: Model_Initialize\n' \
              '\t\tEND FUNCTION\n' \
              '\t\tFUNCTION Model_CheckParameters(pInstance)\n' \
              '\t\t\tIMPORT :: c_int32_t, IEEE_Cigre_DLLInterface_Instance\n' \
              '\t\t\t!DEC$ ATTRIBUTES C :: Model_CheckParameters\n' \
              '\t\t\t!DEC$ ATTRIBUTES REFERENCE :: pInstance\n' \
              '\t\t\tTYPE(IEEE_Cigre_DLLInterface_Instance) pInstance\n' \
              '\t\t\tINTEGER(c_int32_t) :: Model_CheckParameters\n' \
              '\t\tEND FUNCTION\n' \
              '\t\tFUNCTION Model_PrintInfo()\n' \
              '\t\t\tIMPORT :: c_int32_t\n' \
              '\t\t\t!DEC$ ATTRIBUTES C :: Model_PrintInfo\n' \
              '\t\t\tINTEGER(c_int32_t) :: Model_PrintInfo\n' \
              '\t\tEND FUNCTION\n' \
              '\t\tFUNCTION Model_Terminate(pInstance)\n' \
              '\t\t\tIMPORT :: c_int32_t, IEEE_Cigre_DLLInterface_Instance\n' \
              '\t\t\t!DEC$ ATTRIBUTES C :: Model_Terminate\n' \
              '\t\t\t!DEC$ ATTRIBUTES REFERENCE :: pInstance\n' \
              '\t\t\tTYPE(IEEE_Cigre_DLLInterface_Instance) pInstance\n' \
              '\t\t\tINTEGER(c_int32_t) :: Model_Terminate\n' \
              '\t\tEND FUNCTION\n' \
              '\t\tFUNCTION Model_FirstCall(pInstance)\n' \
              '\t\t\tIMPORT :: c_int32_t, IEEE_Cigre_DLLInterface_Instance\n' \
              '\t\t\t!DEC$ ATTRIBUTES C :: Model_FirstCall\n' \
              '\t\t\t!DEC$ ATTRIBUTES REFERENCE :: pInstance\n' \
              '\t\t\tTYPE(IEEE_Cigre_DLLInterface_Instance) pInstance\n' \
              '\t\t\tINTEGER(c_int32_t) :: Model_FirstCall\n' \
              '\t\tEND FUNCTION\n' \
              '\t\tFUNCTION Model_Iterate(pInstance)\n' \
              '\t\t\tIMPORT :: c_int32_t, IEEE_Cigre_DLLInterface_Instance\n' \
              '\t\t\t!DEC$ ATTRIBUTES C :: Model_Iterate\n' \
              '\t\t\t!DEC$ ATTRIBUTES REFERENCE :: pInstance\n' \
              '\t\t\tTYPE(IEEE_Cigre_DLLInterface_Instance) pInstance\n' \
              '\t\t\tINTEGER(c_int32_t) :: Model_Iterate\n' \
              '\t\tEND FUNCTION\n' \
              '\tEND INTERFACE\n\n' \
              '\t! Pointer to DLL functions\n' \
              '\tPOINTER(pointer_Model_GetInfo,         Model_GetInfo)\n' \
              '\tPOINTER(pointer_Model_Outputs,         Model_Outputs)\n' \
              '\tPOINTER(pointer_Model_CheckParameters, Model_CheckParameters)\n' \
              '\tPOINTER(pointer_Model_Initialize,      Model_Initialize)\n' \
              '\tPOINTER(pointer_Model_PrintInfo,       Model_PrintInfo)\n' \
              '\tPOINTER(pointer_Model_Terminate,       Model_Terminate)\n' \
              '\tPOINTER(pointer_Model_FirstCall,       Model_FirstCall)\n' \
              '\tPOINTER(pointer_Model_Iterate,         Model_Iterate)\n\n' \
              '\tcontains ! For functions ans subroutines\n\n' \
              '\t\t! Write LastErrorMessage according to retval (return value of the DLL function)\n' \
              '\t\tsubroutine Handle_Message(pInstance, retval)\n\n' \
              '\t\t\timplicit none\n\n' \
              '\t\t\tTYPE(IEEE_Cigre_DLLInterface_Instance), INTENT(IN) :: pInstance\n' \
              '\t\t\tINTEGER, INTENT(IN) :: retval\n' \
              '\t\t\tCHARACTER(:), ALLOCATABLE :: msg\n\n' \
              '\t\t\t!INTEGER, PARAMETER :: IEEE_Cigre_DLLInterface_Return_OK = 0\n' \
              '\t\t\tINTEGER, PARAMETER :: IEEE_Cigre_DLLInterface_Return_Message = 1\n' \
              '\t\t\tINTEGER, PARAMETER :: IEEE_Cigre_DLLInterface_Return_Error = 2\n\n' \
              '\t\t\tIF (retval .eq. IEEE_Cigre_DLLInterface_Return_Error) THEN\n' \
              '\t\t\t\tmsg = Get_Fortran_String(pInstance%LastErrorMessage)\n' \
              '\t\t\t\tIF (LEN(msg) .GT. 1) THEN\n' \
              '\t\t\t\t\tWRITE(IUNIT,*) "Error in ",OrigModelNameShortened,": ", msg\n' \
              '\t\t\t\tELSE\n' \
              '\t\t\t\t\tWRITE(IUNIT,*) "Unspecified error in ",OrigModelNameShortened\n' \
              '\t\t\t\tENDIF\n' \
              '\t\t\t\tFLUSH(IUNIT)\n' \
              '\t\t\t\tSTOP  ! Stop the simulation\n' \
              '\t\t\tELSE IF (retval .eq. IEEE_Cigre_DLLInterface_Return_Message) THEN\n' \
              '\t\t\t\tmsg = Get_Fortran_String(pInstance%LastGeneralMessage)\n' \
              '\t\t\t\tIF (LEN(msg) .GT. 1) WRITE(IUNIT,*) OrigModelNameShortened,": ", msg\n' \
              '\t\t\tENDIF\n' \
              '\t\tend subroutine\n\n' \
              '\t\t! Load DLL into dllHandle\n' \
              '\t\tsubroutine Get_DLL_Handle(DLL_Path)\n' \
              '\t\t\tuse, intrinsic :: iso_c_binding, only : c_null_char\n' \
              '\t\t\timplicit none\n' \
              '\t\t\tcharacter*(*), INTENT(IN) :: DLL_Path\n' \
              '\t\t\tdllHandle = LoadLibrary(trim(DLL_Path) // C_NULL_CHAR)\n' \
              '\t\t\tif (dllHandle .eq. 0) then\n' \
              '\t\t\t\tWRITE(IUNIT,*) "==============================================================================="\n' \
              '\t\t\t\tWRITE(IUNIT,*) "*** ",OrigModelNameShortened," Error loading ", trim(DLL_Path)\n' \
              '\t\t\t\tWRITE(IUNIT,*) "==============================================================================="\n' \
              '\t\t\t\tSTOP\n' \
              '\t\t\tendif\n' \
              '\t\tend subroutine\n\n' \
              '\t\t! Get the address of the DLL function and store it in functionAd\n' \
              '\t\tsubroutine Get_Pointer_To_DLL_Function(functionName, functionAd)\n' \
              '\t\t\t!use IFWIN\n' \
              '\t\t\tuse, intrinsic :: iso_c_binding, only : c_null_char\n' \
              '\t\t\timplicit none\n' \
              '\t\t\tcharacter*(*), INTENT(IN) :: functionName\n' \
              '\t\t\tinteger(handle), INTENT(INOUT) :: functionAd\n' \
              '\t\t\tfunctionAd = GetProcAddress(dllHandle, trim(functionName)// C_NULL_CHAR)\n' \
              '\t\t\tif (functionAd .eq. 0) then\n' \
              '\t\t\t\tWRITE(IUNIT,*) "==============================================================================="\n' \
              '\t\t\t\tWRITE(IUNIT,*) "*** ",OrigModelNameShortened," Error finding DLL function ",trim(functionName)\n' \
              '\t\t\t\tWRITE(IUNIT,*) "==============================================================================="\n' \
              '\t\t\t\tstop\n' \
              '\t\t\tendif\n' \
              '\t\tend subroutine\n\n' \
              '\t\tfunction Get_Fortran_String(c_input) result(output)\n' \
              '\t\t\tuse, intrinsic :: iso_c_binding, only : c_ptr, c_f_pointer, c_null_char, c_associated\n' \
              '\t\t\timplicit none\n' \
              '\t\t\tTYPE(c_ptr) :: c_input\n' \
              '\t\t\tcharacter(:), allocatable :: output\n' \
              '\t\t\tcharacter(10000), pointer :: string_fp\n' \
              '\t\t\tinteger string_length\n' \
              '\t\t\toutput = ""\n' \
              '\t\t\tif(c_associated(c_input) ) then\n' \
              '\t\t\t\tcall c_f_pointer(c_input, string_fp)\n' \
              '\t\t\t\tstring_length = index(string_fp, c_null_char)\n' \
              '\t\t\t\tif (string_length .ne. 0) then\n' \
              '\t\t\t\t\toutput = string_fp(1:string_length-1)\n' \
              '\t\t\t\tendif\n' \
              '\t\t\tendif\n' \
              '\t\t\treturn\n' \
              '\t\tend function\n\n' \
              '\tEND MODULE ' + self.Model_Name_Shortened + '_MOD\n\n\n\n'

        finterface_function_prototype = self.generate_finterface_function_prototype()

        bf += finterface_function_prototype + '\n\n'

        bf += '\tUSE, INTRINSIC :: iso_c_binding, only : c_ptr, c_f_pointer, c_loc\n\n' \
              '\tUSE ' + self.Model_Name_Shortened + '_MOD\n\n' \
              '\tINCLUDE \'s1.h\' ! This include gives us access to TIME, TIMEZERO etc. variables\n' \
              '\tINCLUDE \'emtstor.h\' ! This include gives us access to STORI, STORF, NSTORI, NSTORF\n\n' \
              '\tIMPLICIT NONE\n\n' \
              '\t! Inputs, params and outputs variables, received from PSCAD\n'

        variables_from_pscad = self.generate_variables_from_pscad()

        bf += variables_from_pscad + '\n'

        bf += '\t! Other inputs\n' \
              '\tDOUBLE PRECISION, INTENT(IN) :: TRelease  ! Time to release initial output values\n' \
              '\tCHARACTER*(*), INTENT(IN) :: DLL_Path\n' \
              '\tLOGICAL, INTENT(IN) :: Use_Interpolation  ! if inputs interpolation is checked in the PSCAD component form\n\n' \
              '\t! --------------------------------\n' \
              '\t! Local variables\n' \
              '\t! --------------------------------\n' \
              '\tINTEGER   :: i  ! Used for loops\n' \
              '\tINTEGER   :: retval  ! return value of DLL functions\n' \
              '\tINTEGER   :: retValFreeLib     ! Only for FreeLibrary\n' \
              '\tTYPE(IEEE_Cigre_DLLInterface_Instance) :: pInstance\n' \
              '\tDOUBLE PRECISION :: Next_t_model ! To know when to call model output function\n' \
              '\tLOGICAL   :: First_Step_Model ! To know the first time model sampling time has been reached\n' \
              '\tTYPE(c_ptr)                                           :: Model_Info_cp ! The C pointer of Model_GetInfo\n' \
              '\tTYPE(IEEE_Cigre_DLLInterface_Model_Info), POINTER     :: Model_Info_fp ! The F pointer of Model_GetInfo\n' \
              '\tTYPE(ModelInputs), TARGET     :: INPUTS_new\n' \
              '\tTYPE(ModelOutputs), TARGET    :: OUTPUTS_new\n' \
              '\tTYPE(ModelParameters), TARGET :: PARAMETERS_new\n' \
              '\t! To check if the DLL has changed, the name and version will be checked against the name and version extracted from the DLL\n' \
              '\tTYPE(c_ptr)    :: DLLModelName_cp  ! The C pointer to the DLL ModelName\n' \
              '\tINTEGER(KIND=c_int8_t), DIMENSION(4) :: DLLInterfaceVersion  ! The DLL interface version of the DLL\n' \
              '\tINTEGER(KIND=c_int8_t), DIMENSION(4) :: OrigDLLInterfaceVersion = ' + str(self.DLLInterfaceVersion) + '  ! DLL interface version, written by the DLL Import tool\n'
              #'\tLOGICAL :: ParametersChanged  ! Indicates whether parameters have changed since the previous call\n'
              #'\tINTEGER(KIND=c_int8_t), DIMENSION(4) :: OrigDLLInterfaceVersion = [' + ', '.join(map(str, self.DLLInterfaceVersion)) + ']  ! DLL interface version, written by the DLL Import tool\n'
              #'\tCHARACTER*50   :: OrigModelVersion    = "' + self.DLLInterfaceVersionStr + '"  ! Model version, written by the DLL Import tool\n'

        if self.Model_Info.NumIntStates > 0:
            bf += '\tINTEGER, DIMENSION(' + str(self.Model_Info.NumIntStates) + '), TARGET :: STATEI  ! To store IEEE CIGRE IntStates array\n'
        if self.Model_Info.NumFloatStates > 0:
            bf += '\tREAL*4, DIMENSION(' + str(self.Model_Info.NumFloatStates) + '), TARGET :: STATEF  ! To store IEEE CIGRE FloatStates array\n'
        if self.Model_Info.NumDoubleStates > 0:
            bf += '\tDOUBLE PRECISION, DIMENSION(' + str(self.Model_Info.NumDoubleStates) + '), TARGET :: STATED  ! To store IEEE CIGRE DoubleStates array\n'

        bf += '\t! Variables for interpolation\n' \
              '\tDOUBLE PRECISION :: Prev_t_pscad  ! Previous PSCAD time\n' \
              '\tDOUBLE PRECISION ::delta_t1  ! Used to calculate interpolated inputs\n' \
              '\tDOUBLE PRECISION ::delta_t2  ! Used to calculate interpolated inputs\n'

        bf += self.generate_variables_declaration('', self.in_names, '_pscad_prev', self.in_pscad_types, self.in_width)
        bf += '\n' \
              '\tINTEGER :: idx_start_next_t_model  ! STORF start index to store Next_t_model\n' \
              '\tINTEGER :: idx_start_statef  ! STORF start index to store float state variables\n' \
              '\tINTEGER :: idx_start_stated  ! STORF start index to store double state variables\n' \
              '\tINTEGER :: idx_start_outputs  ! STORF start index to store outputs\n' \
              '\tINTEGER :: idx_start_prev_t  ! STORF start index to store Prev_t_pscad\n' \
              '\tINTEGER :: idx_start_inputs  ! STORF start index to store inputs\n' \
              '\tINTEGER :: idx_start_next_model_storf  ! STORF start index for the next model\n' \
              '\tINTEGER :: idx_start_first_step_model  ! STORI start index for First_Step_Model\n' \
              '\tINTEGER :: idx_start_statei  ! STORI start index to store integer state variables\n' \
              '\tINTEGER :: idx_start_next_model_stori  ! STORI start index for the next model\n\n' \
              '\t! --------------------------------\n' \
              '\t! First, load the DLL and its functions and check static information. Do this only once\n' \
              '\t! --------------------------------\n' \
              '\tIF (FIRST_CALL_THIS_FILE) THEN  ! Use FIRST_CALL_THIS_FILE to do following instruction once\n\n' \
              '\t\t! Load dllHandle\n' \
              '\t\tCALL Get_DLL_Handle(DLL_Path)\n\n' \
              '\t\t! Set pointers to DLL functions\n' \
              '\t\tCALL Get_Pointer_To_DLL_Function("Model_GetInfo", pointer_Model_GetInfo)\n' \
              '\t\tCALL Get_Pointer_To_DLL_Function("Model_Outputs", pointer_Model_Outputs)\n' \
              '\t\tCALL Get_Pointer_To_DLL_Function("Model_Initialize", pointer_Model_Initialize)\n' \
              '\t\tCALL Get_Pointer_To_DLL_Function("Model_CheckParameters", pointer_Model_CheckParameters)\n' \
              '\t\tCALL Get_Pointer_To_DLL_Function("Model_PrintInfo", pointer_Model_PrintInfo)\n' \
              '\t\tCALL Get_Pointer_To_DLL_Function("Model_Terminate", pointer_Model_Terminate)\n' \
              '\t\tCALL Get_Pointer_To_DLL_Function("Model_FirstCall", pointer_Model_FirstCall)\n' \
              '\t\tCALL Get_Pointer_To_DLL_Function("Model_Iterate", pointer_Model_Iterate)\n\n' \
              '\t\t! Get the C pointer to ModelInfo\n' \
              '\t\tModel_Info_cp = Model_GetInfo()\n\n' \
              '\t\t! Transfer the C pointer to the F pointer\n' \
              '\t\tcall c_f_pointer(Model_Info_cp, Model_Info_fp)\n\n' \
              '\t\t! Call DLL routine to Print DLL Model Information\n' \
              '\t\tretval = Model_PrintInfo()\n' \
              '\t\tcall Handle_Message(pInstance, retval)\n\n' \
              '\t\t! Extract the control code sampling step size (Seconds)\n' \
              '\t\tDELT_Model = Model_Info_fp.FixedStepBaseSampleTime\n\n' \
              '\t\t! Idea still under discussion but currently adopted: -1 for FixedStepBaseSampleTime indicates that the simulator time step is used as the model time step.\n' \
              '\t\t! -1 means inherited value\n' \
              '\t\tIF (DELT_Model .EQ. -1) THEN\n' \
              '\t\t\tDELT_Model = DELT\n' \
              '\t\tENDIF\n\n' \
              '\t\tDLLModelName_cp = Model_Info_fp.ModelName\n' \
              '\t\tIF (Get_Fortran_String(DLLModelName_cp) .NE. OrigModelName ) THEN\n' \
              '\t\t\tWRITE(IUNIT,*) "*** Warning - The model name in the fortran interface code generated by the DLLImport tools is: ", OrigModelName\n' \
              '\t\t\tWRITE(IUNIT,*) "but the model name in the DLL is: ", Get_Fortran_String(DLLModelName_cp)\n' \
              '\t\t\tWRITE(IUNIT,*) "They must match. Simulation continues anyway"\n' \
              '\t\tENDIF\n\n' \
              '\t\tDLLInterfaceVersion = Model_Info_fp.DLLInterfaceVersion  ! DLLInterfaceVersion got from the DLL\n' \
              '\t\t! Compare it to OrigDLLInterfaceVersion (DLL interface version, written by the DLL Import tool)\n' \
              '\t\tDO i = 1, 4\n' \
              '\t\t\tIF (DLLInterfaceVersion(i) /= OrigDLLInterfaceVersion(i)) THEN\n' \
              '\t\t\t\tWRITE(IUNIT,*) "*** Warning - The DLL interface version in the fortran interface code generated by the DLLImport tools is: ", OrigDLLInterfaceVersion(1), ".", OrigDLLInterfaceVersion(2), ".", OrigDLLInterfaceVersion(3), ".", OrigDLLInterfaceVersion(4)\n' \
              '\t\t\t\tWRITE(IUNIT,*) "but the DLL interface version in the DLL is: ", DLLInterfaceVersion(1), ".", DLLInterfaceVersion(2), ".", DLLInterfaceVersion(3), ".", DLLInterfaceVersion(4)\n' \
              '\t\t\t\tWRITE(IUNIT,*) "They must match. Simulation continues anyway"\n' \
              '\t\t\t\tEXIT\n' \
              '\t\t\tEND IF\n' \
              '\t\tEND DO\n\n' \
              '\t\tFIRST_CALL_THIS_FILE = .FALSE.\n\n' \
              '\tENDIF\n\n'

        bf += '\t! --------------------------------\n' \
              '\t! Initialize STORF start indexes\n' \
              '\t! --------------------------------\n' \
              '\tidx_start_next_t_model = NSTORF\n' \
              '\tidx_start_statef = idx_start_next_t_model + 1\n' \
              '\tidx_start_stated = idx_start_statef + ' + str(self.Model_Info.NumFloatStates) + '\n' \
              '\tidx_start_outputs = idx_start_stated + ' + str(self.Model_Info.NumDoubleStates) + '\n' \
              '\tidx_start_prev_t = idx_start_outputs + ' + str(self.nb_outputs_total) + '\n' \
              '\tidx_start_inputs = idx_start_prev_t + 1\n' \
              '\tidx_start_next_model_storf = idx_start_inputs + ' + str(self.nb_inputs_total) + '\n\n' \
              '\t! --------------------------------\n' \
              '\t! Initialize STORI start indexes\n' \
              '\t! --------------------------------\n' \
              '\tidx_start_first_step_model = NSTORI\n' \
              '\tidx_start_statei = NSTORI + 1\n' \
              '\tidx_start_next_model_stori = idx_start_statei + ' + str(self.Model_Info.NumIntStates) + '\n\n' \
              '\t! --------------------------------\n' \
              '\t! Set pointers\n' \
              '\t! --------------------------------\n' \
              '\t! Set pointers to input/output/parameter structs\n' \
              '\tpInstance%ExternalInputs     = c_loc(INPUTS_new)\n' \
              '\tpInstance%Parameters         = c_loc(PARAMETERS_new)\n' \
              '\tpInstance%ExternalOutputs    = c_loc(OUTPUTS_new)\n' \
              '\t! Set pointers to state variable storage\n'

        if self.Model_Info.NumIntStates > 0:
            bf += '\tpInstance%IntStates    = c_loc(STATEI(1))\n'
        if self.Model_Info.NumFloatStates > 0:
            bf += '\tpInstance%FloatStates  = c_loc(STATEF(1))\n'
        if self.Model_Info.NumDoubleStates > 0:
            bf += '\tpInstance%DoubleStates = c_loc(STATED(1))\n'

        bf += '\n' \
              '\t! --------------------------------\n' \
              '\t! Get saved values from STORI\n' \
              '\t! --------------------------------\n' \
              '\tFirst_Step_Model = (STORI(idx_start_first_step_model) /= 0)  ! TRUE if not equal to 0\n'

        bf += '\n' \
              '\t! --------------------------------\n' \
              '\t! Get saved values from STORF\n' \
              '\t! --------------------------------\n' \
              '\t! Get Next_t_model from STORF\n' \
              '\tNext_t_model = STORF(idx_start_next_t_model)\n' \
              '\tIF (TIMEZERO) THEN\n' \
              '\t\tNext_t_model = 0\n' \
              '\tENDIF\n' \
              '\t! Get state values from STORF\n'

        if self.Model_Info.NumIntStates > 0:
            bf += '\tDO i = 1, ' + str(self.Model_Info.NumIntStates) + '\n'
            bf += '\t\tSTATEI(i) = STORI(idx_start_statei + i - 1)\n'
            bf += '\tEND DO\n'
        if self.Model_Info.NumFloatStates > 0:
            bf += '\tDO i = 1, ' + str(self.Model_Info.NumFloatStates) + '\n'
            bf += '\t\tSTATEF(i) = STORF(idx_start_statef + i - 1)\n'
            bf += '\tEND DO\n'
        if self.Model_Info.NumDoubleStates > 0:
            bf += '\tDO i = 1, ' + str(self.Model_Info.NumDoubleStates) + '\n'
            bf += '\t\tSTATED(i) = STORF(idx_start_stated + i - 1)\n'
            bf += '\tEND DO\n'

        bf += '\t! Get output values from STORF\n'
        bf += '\t! Retrieve previous outputs values, can be used by the model in Model_Outputs function\n'

        outputs_storf_to_new_struct = self.generate_conversion(self.out_names, self.out_width, self.out_fortran_types,
                                                               self.out_pscad_types, None, True,
                                                               'OUTPUTS_new')
        bf += outputs_storf_to_new_struct
        bf += '\t! Get prev_t (previous time value of pscad) and previous values of pscad inputs\n' \
              '\tIF (Use_Interpolation) THEN\n' \
              '\t\tPrev_t_pscad = STORF(idx_start_prev_t)\n'
        bf += self.generate_storf_to_prev_inputs()
        bf += '\tENDIF\n\n' \
              '\tpInstance%Time = TIME  ! PSCAD current time\n\n' \
              '\t! Assign parameters from the simulation tool side to model parameters\n' \
              '\t! Done before Model_FirstCall because this function may use the parameters\n'

        params_pscad_to_new_struct = self.generate_conversion(self.param_names, None, self.param_fortran_types,
                                                              self.param_pscad_types, '_pscad', True,
                                                              'PARAMETERS_new')

        bf += params_pscad_to_new_struct + '\n'

        bf += '\t! FIRSTSTEP is True for first step starting from the Data file (so at T0) or Snapshot file.\n' \
              '\tIF (FIRSTSTEP) THEN\n' \
              '\t\tN_INSTANCE = N_INSTANCE + 1\n' \
              '\t\tFirst_Step_Model = .TRUE.\n' \
              '\tENDIF\n\n' \
              '\t! Check if the model sampling time has been reached\n' \
              '\t! True at T0\n' \
              '\tDO WHILE ( TIME .GE. Next_t_model - EQUALITY_PRECISION )\n\n' \
              '\t\t! assign inputs from the simulation tool side to model inputs\n' \
              '\t\t! Done before Model_FirstCall because this function may use the input values\n' \
              '\t\tIF (Use_Interpolation .AND. .NOT. TIMEZERO) THEN\n' \
              '\t\t\tdelta_t1 = TIME - Prev_t_pscad\n' \
              '\t\t\tdelta_t2 = Next_t_model - Prev_t_pscad\n\n' \
              '\t\t\tIF (delta_t1 .EQ. 0.0) THEN  ! TIME = Prev_t_pscad, should not happen\n' \
              '\t\t\t\tWRITE(IUNIT,*) "*** Error in ",OrigModelNameShortened,": division by 0"\n' \
              '\t\t\t\tSTOP\n' \
              '\t\t\tENDIF\n\n'

        bf += self.generate_interpolated_inputs()
        bf += '\n' \
              '\t\t\t! Update also time in the model (it can be used by the model)\n' \
              '\t\t\tpInstance%Time = Next_t_model  ! Time is not PSCAD current time but Next_t_model\n' \
              '\t\tELSE\n'

        inputs_pscad_to_new_struct = self.generate_conversion(self.in_names, self.in_width, self.in_fortran_types,
                                                              self.in_pscad_types, '_pscad', True,
                                                              'INPUTS_new')

        inputs_pscad_to_new_struct = inputs_pscad_to_new_struct.replace('\t', '\t\t\t')

        bf += inputs_pscad_to_new_struct

        bf += '\t\tENDIF\n\n' \
              '\t\t! Use First_Step_Model instead of FIRSTSTEP because FIRSTSTEP from snapshot may not fall on a model time step\n' \
              '\t\t! First_Step_Model is True from the Data file or Snapshot file, if this is the first time model sampling time has been reached\n' \
              '\t\tIF (First_Step_Model) THEN\n' \
              '\t\t\tretval = Model_FirstCall(pInstance)\n' \
              '\t\t\tcall Handle_Message(pInstance, retval)\n' \
              '\t\t\tFirst_Step_Model = .FALSE.\n' \
              '\t\tENDIF\n\n' \
              '\t\t! Model_CheckParameters is done only at t0 (to be called every time parameter values are modified in a future version, with GUI option)\n' \
              '\t\t! Model_CheckParameters must be called just after Model_FirstCall\n' \
              '\t\tIF (TIMEZERO) THEN\n' \
              '\t\t\tretval = Model_CheckParameters(pInstance)\n' \
              '\t\t\tcall Handle_Message(pInstance, retval)\n' \
              '\t\tENDIF\n\n' \
              '\t\t! Check if the model must be initialized\n' \
              '\t\tIF (TIMEZERO .OR. (TIME .LE. TRelease)) THEN\n' \
              '\t\t\t! assign initial values to model outputs provided through the component mask\n'

        outputs_init_pscad_to_new_struct = self.generate_conversion(self.out_names, self.out_width, self.out_fortran_types,
                                                                    self.out_pscad_types, '_init_pscad', True,
                                                                    'OUTPUTS_new')

        outputs_init_pscad_to_new_struct = outputs_init_pscad_to_new_struct.replace('\t', '\t\t\t')

        bf += outputs_init_pscad_to_new_struct + '\n'

        bf += '\t\t\tretval = Model_Initialize(pInstance)\n' \
              '\t\t\tcall Handle_Message(pInstance, retval)\n' \
              '\t\tENDIF\n\n' \
              '\t\t! Call ModelOutputs function every DELT_Model sampling time\n' \
              '\t\tretval = Model_Outputs(pInstance)\n' \
              '\t\tcall Handle_Message(pInstance, retval)\n\n' \
              '\t\tNext_t_model = Next_t_model + DELT_Model  ! Next_t_model indicates when the model will be called\n' \
              '\tEND DO\n\n' \
              '\t! Put back into STORI\n' \
              '\tSTORI(idx_start_first_step_model) = merge(1, 0, First_Step_Model) ! 1 if TRUE, 0 else\n\n' \
              '\t! --------------------------------\n' \
              '\t! Put back into STORF\n' \
              '\t! --------------------------------\n' \
              '\tSTORF(idx_start_next_t_model) = Next_t_model\n' \
              '\t! Copy values from STATEx to STORx\n'

        if self.Model_Info.NumIntStates > 0:
            bf += '\tDO i = 1, ' + str(self.Model_Info.NumIntStates) + '\n'
            bf += '\t\tSTORI(idx_start_statei + i - 1) = STATEI(i)\n'
            bf += '\tEND DO\n'
        if self.Model_Info.NumFloatStates > 0:
            bf += '\tDO i = 1, ' + str(self.Model_Info.NumFloatStates) + '\n'
            bf += '\t\tSTORF(idx_start_statef + i - 1) = STATEF(i)\n'
            bf += '\tEND DO\n'
        if self.Model_Info.NumDoubleStates > 0:
            bf += '\tDO i = 1, ' + str(self.Model_Info.NumDoubleStates) + '\n'
            bf += '\t\tSTORF(idx_start_stated + i - 1) = STATED(i)\n'
            bf += '\tEND DO\n'

        bf += '\t! Outputs into STORF\n'

        outputs_new_to_storf = self.generate_conversion(self.out_names, self.out_width, self.out_fortran_types,
                                                        self.out_pscad_types, None, False,
                                                        'OUTPUTS_new')

        bf += outputs_new_to_storf

        bf += '\t! Inputs and TIME into STORF\n' \
              '\tIF (Use_Interpolation) THEN\n' \
              '\t\tSTORF(idx_start_prev_t) = TIME\n'

        bf += self.generate_inputs_to_storf()

        bf += '\tENDIF\n\n' \
              '\t! Send back outputs values to PSCAD\n'

        outputs_new_to_pscad = self.generate_conversion(self.out_names, self.out_width, self.out_fortran_types,
                                                        self.out_pscad_types, '_pscad', False,
                                                        'OUTPUTS_new')

        bf += outputs_new_to_pscad + '\n'

        bf += '\t! Cleanup on the last time step\n' \
              '\tIF (LASTSTEP) THEN\n' \
              '\t\t! Call DLL routine Model_Terminate\n' \
              '\t\tretval =  Model_Terminate(pInstance)\n' \
              '\t\tcall Handle_Message(pInstance, retval)\n\n' \
              '\t\t! FreeLibrary only at LASTSTEP of the last instance\n' \
              '\t\tN_CALL_LAST_STEP= N_CALL_LAST_STEP + 1\n' \
              '\t\tIF (N_CALL_LAST_STEP .EQ. N_INSTANCE) THEN\n' \
              '\t\t\tretValFreeLib = FreeLibrary(dllHandle) ! Free DLL\n' \
              '\t\t\t! Reset some global variables for multiple runs\n' \
              '\t\t\tdllHandle = 0\n' \
              '\t\t\tFIRST_CALL_THIS_FILE = .TRUE.\n' \
              '\t\t\tN_INSTANCE = 0\n' \
              '\t\t\tN_CALL_LAST_STEP = 0 \n' \
              '\t\tENDIF\n' \
              '\tENDIF\n\n' \
              '\t! Increment STORx pointers (this must be done every time step)\n' \
              '\t! Note memory for these vectors must be allocated using #STORAGE syntax in the PSCAD component definition\n'

        # NumIntStates = self.Model_Info.NumIntStates
        # if NumIntStates > 0:
        #     bf += '\tNSTORI = NSTORI + ' + str(NumIntStates) + ' ! # of integer states used in the dll\n'

        bf += '\tNSTORI = idx_start_next_model_stori\n\n'

        bf += '\tNSTORF = idx_start_next_model_storf\n\n'
        bf += '\tEND\n\n'

        return bf

    # See : https://docs.oracle.com/cd/E19957-01/805-4940/z40009104412/index.html
    # We could also use types with kind type parameters from the ISO_C_BINDING module
    def ieee_cigre_datatype_to_fortran_type_str(self, datatype):
        if datatype == IEEE_Cigre_DLLInterface_DataType_Enum.IEEE_Cigre_DLLInterface_DataType_char_T:
            return 'CHARACTER'
        elif datatype == IEEE_Cigre_DLLInterface_DataType_Enum.IEEE_Cigre_DLLInterface_DataType_int8_T:
            return 'CHARACTER'
        elif datatype == IEEE_Cigre_DLLInterface_DataType_Enum.IEEE_Cigre_DLLInterface_DataType_uint8_T:
            return 'CHARACTER'
        elif datatype == IEEE_Cigre_DLLInterface_DataType_Enum.IEEE_Cigre_DLLInterface_DataType_int16_T:
            return 'INTEGER*2'
        elif datatype == IEEE_Cigre_DLLInterface_DataType_Enum.IEEE_Cigre_DLLInterface_DataType_uint16_T:
            return 'INTEGER*2'
        elif datatype == IEEE_Cigre_DLLInterface_DataType_Enum.IEEE_Cigre_DLLInterface_DataType_int32_T:
            return 'INTEGER*4'
        elif datatype == IEEE_Cigre_DLLInterface_DataType_Enum.IEEE_Cigre_DLLInterface_DataType_uint32_T:
            return 'INTEGER*4'
        elif datatype == IEEE_Cigre_DLLInterface_DataType_Enum.IEEE_Cigre_DLLInterface_DataType_real32_T:
            return 'REAL*4'
        elif datatype == IEEE_Cigre_DLLInterface_DataType_Enum.IEEE_Cigre_DLLInterface_DataType_real64_T:
            return 'DOUBLE PRECISION'
        elif datatype == IEEE_Cigre_DLLInterface_DataType_Enum.IEEE_Cigre_DLLInterface_DataType_c_string_T:
            return 'CHARACTER (LEN=1000)'  # TODO check
        else:
            raise Exception('ieee_cigre_datatype_to_fortran_type_str : error in dataType_enum: ' + str(datatype))

    # PSCAD types for inputs/outputs are : LOGICAL INTEGER OR REAL
    def ieee_cigre_datatype_to_pscad_inout_type_str(self, datatype):
        if datatype == IEEE_Cigre_DLLInterface_DataType_Enum.IEEE_Cigre_DLLInterface_DataType_char_T:
            return 'INTEGER'
        elif datatype == IEEE_Cigre_DLLInterface_DataType_Enum.IEEE_Cigre_DLLInterface_DataType_int8_T:
            return 'INTEGER'
        elif datatype == IEEE_Cigre_DLLInterface_DataType_Enum.IEEE_Cigre_DLLInterface_DataType_uint8_T:
            return 'INTEGER'
        elif datatype == IEEE_Cigre_DLLInterface_DataType_Enum.IEEE_Cigre_DLLInterface_DataType_int16_T:
            return 'INTEGER'
        elif datatype == IEEE_Cigre_DLLInterface_DataType_Enum.IEEE_Cigre_DLLInterface_DataType_uint16_T:
            return 'INTEGER'
        elif datatype == IEEE_Cigre_DLLInterface_DataType_Enum.IEEE_Cigre_DLLInterface_DataType_int32_T:
            return 'INTEGER'
        elif datatype == IEEE_Cigre_DLLInterface_DataType_Enum.IEEE_Cigre_DLLInterface_DataType_uint32_T:
            return 'INTEGER'
        elif datatype == IEEE_Cigre_DLLInterface_DataType_Enum.IEEE_Cigre_DLLInterface_DataType_real32_T:
            return 'REAL'
        elif datatype == IEEE_Cigre_DLLInterface_DataType_Enum.IEEE_Cigre_DLLInterface_DataType_real64_T:
            return 'REAL'
        elif datatype == IEEE_Cigre_DLLInterface_DataType_Enum.IEEE_Cigre_DLLInterface_DataType_c_string_T:
            raise Exception('No PSCAD input or output type equivalent to IEEE_Cigre_DLLInterface_DataType_c_string_T')
        else:
            raise Exception('ieee_cigre_datatype_to_pscad_type_str : error in dataType_enum: ' + str(datatype))

    # Allowed PSCAD types for parameters are : .text .logical .boolean .choice .integer .real
    # For an IEEE CIGRE DLL, it can be INTEGER, REAL or CHARACTER(*)
    def ieee_cigre_datatype_to_pscad_param_type_str(self, datatype):
        if datatype == IEEE_Cigre_DLLInterface_DataType_Enum.IEEE_Cigre_DLLInterface_DataType_char_T:
            return 'INTEGER'
        elif datatype == IEEE_Cigre_DLLInterface_DataType_Enum.IEEE_Cigre_DLLInterface_DataType_int8_T:
            return 'INTEGER'
        elif datatype == IEEE_Cigre_DLLInterface_DataType_Enum.IEEE_Cigre_DLLInterface_DataType_uint8_T:
            return 'INTEGER'
        elif datatype == IEEE_Cigre_DLLInterface_DataType_Enum.IEEE_Cigre_DLLInterface_DataType_int16_T:
            return 'INTEGER'
        elif datatype == IEEE_Cigre_DLLInterface_DataType_Enum.IEEE_Cigre_DLLInterface_DataType_uint16_T:
            return 'INTEGER'
        elif datatype == IEEE_Cigre_DLLInterface_DataType_Enum.IEEE_Cigre_DLLInterface_DataType_int32_T:
            return 'INTEGER'
        elif datatype == IEEE_Cigre_DLLInterface_DataType_Enum.IEEE_Cigre_DLLInterface_DataType_uint32_T:
            return 'INTEGER'
        elif datatype == IEEE_Cigre_DLLInterface_DataType_Enum.IEEE_Cigre_DLLInterface_DataType_real32_T:
            return 'REAL'
        elif datatype == IEEE_Cigre_DLLInterface_DataType_Enum.IEEE_Cigre_DLLInterface_DataType_real64_T:
            return 'REAL'
        elif datatype == IEEE_Cigre_DLLInterface_DataType_Enum.IEEE_Cigre_DLLInterface_DataType_c_string_T:
            return 'CHARACTER(*)'
        else:
            raise Exception('ieee_cigre_datatype_to_pscad_param_type_str : error in dataType_enum: ' + str(datatype))

    # Get and check self.dll_file_name and self.dll_file_path
    def get_and_check_dll_file_path(self):
        # Check DLL file path not empty
        self.dll_file_path = self.entry_dll_file_path.get()
        self.dll_file_name = os.path.basename(self.dll_file_path)
        if not self.dll_file_path:
            raise Exception("Please select a DLL")

    # Get self.Model_Info
    def get_dll_model_info(self):
        # Get and check DLL info
        dll_handle = ctypes.cdll.LoadLibrary(self.dll_file_path)
        dll_handle.Model_GetInfo.restype = ctypes.POINTER(IEEE_Cigre_DLLInterface_Model_Info)
        result = dll_handle.Model_GetInfo()  # pointer to struct "IEEE_Cigre_DLLInterface_Model_Info"
        self.Model_Info = result.contents  # use contents to get access

    # Check if this import tool is compatible with the version of the IEEE CIGRE DLL Interface
    # For now, it is only compatible with 1.1.0.0
    def get_and_check_dll_interface_version(self):
        self.DLLInterfaceVersion = [int(x) for x in self.Model_Info.DLLInterfaceVersion]  # ex: [1, 1, 0, 0]
        if self.DLLInterfaceVersion != [1, 1, 0, 0]:
            DLLInterfaceVersionStr = '.'.join(map(str, self.DLLInterfaceVersion))
            raise Exception('DLLInterfaceVersion is not correct. This import tool is compatible with 1.1.0.0 but the'
                            ' DLL interface version is ' + DLLInterfaceVersionStr)

    # Get self.pscad_project_name
    def get_and_check_pscad_project_name(self):
        # get destination_folder
        selected_option = self.radio_option.get()
        if selected_option == "Option 1":
            self.pscad_project_name = self.Model_Name_Shortened
        else:
            self.pscad_project_name = self.pscad_projects_selected_value.get()
            if not self.pscad_project_name or self.pscad_project_name == self.combobox_pscad_projects_placeholder:  # means = ""
                raise Exception("No PSCAD project selected")

    # Get self.destination_folder
    def get_destination_folder(self):
        # get destination_folder
        selected_option = self.radio_option.get()
        if selected_option == "Option 1":
            self.destination_folder = self.entry_destination_folder.get()
            # if entry is empty or equal to the placeholder value
            if not self.destination_folder or self.destination_folder == self.entry_destination_folder_placeholder:
                self.destination_folder = os.path.dirname(self.dll_file_path)  # get folder path of the DLL
        else:
            self.init_pscad()  # reload PSCAD if it has been closed. Cannot fail because already tested in click radio button
            project_filename = self.pscad.project(name=self.pscad_project_name).filename
            self.destination_folder = os.path.dirname(project_filename)  # project folder

    def display_error(self, message):

        if message is None or message == '':
            return

        message = 'Error : ' + message

        #label_message = tk.Label(self, text=message, fg='#D63C27', font='Helvetica 10 bold')
        label_message = ttk.Label(self, text=message, foreground='#D63C27', font='Helvetica 10 bold')
        label_message.grid(row=self.row_index, column=1, pady=5)
        self.list_label_errors.append(label_message)
        self.row_index += 1

    def display_info(self, message):

        if message is None or message == '':
            return

        #message = 'Info : ' + error

        #label_message = tk.Label(self, text=message, fg='#007934', font='Helvetica 10 bold')
        label_message = ttk.Label(self, text=message, foreground='#007934', font='Helvetica 10 bold')
        label_message.grid(row=self.row_index, column=1, pady=5)
        self.list_label_info.append(label_message)
        self.row_index += 1

    # Display dialog and ask for a PSCX file
    # Then fill entry with path
    def open_file(self, entry, ext):
        file = filedialog.askopenfile(mode='r', filetypes=[('IEEE CIGRE DLL', '*' + ext)])
        if file:
            file_path = file.name
            entry.delete(0, 'end')  # clear text first
            entry.insert(0, file_path)

    def generate_pscad_project(self):

        # PSCAD V4
        """pscad = mhrc.automation.launch_pscad()
        workspace = pscad.workspace()
        project = workspace.create_project('1', 'tesssst', self.dll_folder_path)
        main = project.user_canvas('Main')"""

        # PSCAD V5
        # versions = mhi.pscad.versions()  # get installed versions
        # pscad = mhi.pscad.launch(version='4.6.3', x64=True)  # does not work, even with 4.6.2 or 4.6.1

        try:
            self.init_pscad()  # depending on the radio option, PSCAD may not be initialized
        except Exception as e:
            # exception to display error because does not stop algo
            self.display_error(str(e) + ' Only the wrapper file is generated.')
            return

        workspace = self.pscad.workspace()  # Get workspace. For info, works also if no license
        selected_option = self.radio_option.get()
        if selected_option == "Option 1":  # create the project
            project = workspace.create_project(1, self.pscad_project_name, self.destination_folder)  # Fail if no license
        else:
            project = self.pscad.project(self.pscad_project_name)  # select the project

        canvas = project.canvas("Main")

        # Init Component Wizard
        wizard = UserDefnWizard(self.Model_Name_Shortened)

        # Description of the definition (is not read only)
        wizard.description = "IEEE CIGRE DLL - " + self.Model_Name_Shortened

        ##################################################################################
        # Adding control inputs and outputs to the created component
        ##################################################################################

        y_offset = 1

        x_coord = -7
        y_coord = 0
        for i in range(len(self.in_names)):
            in_name = self.in_names[i]
            in_width = self.in_width[i]
            if self.in_pscad_types[i] == 'REAL':
                data_type = Signal.REAL
            else:
                data_type = Signal.INTEGER
            wizard.port.input(x_coord, y_coord, in_name, data_type, in_width)
            y_coord += y_offset

        x_coord = +17
        y_coord = 0
        for i in range(len(self.out_names)):
            out_name = self.out_names[i]
            out_width = self.out_width[i]
            if self.out_pscad_types[i] == 'REAL':
                data_type = Signal.REAL
            else:
                data_type = Signal.INTEGER
            wizard.port.output(x_coord, y_coord, out_name, data_type, out_width)
            y_coord += y_offset

        ##################################################################################
        # Creating the mask menu (parameters form)
        ##################################################################################

        category = wizard.category

        # Configuration tab
        # Fill the parameter name, automatically created by PSCAD
        wizard.parameter["Name"].value = self.Model_Name_Shortened
        wizard.parameter["Name"].visible = False
        category["Configuration"].text("DLL_Path", description='DLL Path', value='..\\' + self.dll_file_name)
        # help does not work...
        # help_interp = ('Input signals can be interpolated to match exactly the instants when the model computation'
        #                ' is performed. This option is useful if the model time-step is not a multiple of the PSCAD time-step.'
        #                ' If selected, linear interpolation will be applied to all input signals. Integer input signals '
        #                'are also interpolated and then converted to an integer before being sent to the model. Select '
        #                'this option with care.')
        #category["Configuration"].logical("Use_Interpolation", description='Use linear interpolation of inputs',
        #                                  value='.FALSE.', help=help_interp)
        p = category["Configuration"].logical("Use_Interpolation", description='Use linear interpolation of inputs',
                                          value='.FALSE.')
        p._set_attr('.', 'content_type', 'Constant', str)

        model_parameters = category.add("Model Parameters")
        for i in range(len(self.param_names)):
            param_name = self.param_names[i]
            param_pscad_type = self.param_pscad_types[i]  # For an IEEE CIGRE DLL, it can be INTEGER, REAL or CHARACTER(*)
            param_group_name = self.param_group_names[i]
            param_description = param_name + ' - ' + self.param_descriptions[i]
            param_unit = self.param_units[i]
            param_fixedValue = self.param_fixedValue[i]
            #param_default_value = self.param_default_values[i]
            if param_unit != '':
                param_default_value = str(self.param_default_values[i]) + ' [' + param_unit + ']'
            else:
                param_default_value = self.param_default_values[i]
            param_min_value = self.param_min_values[i]
            param_max_value = self.param_max_values[i]
            if param_pscad_type == 'INTEGER':
                # No units for integer...
                p = model_parameters.integer(param_name, description=param_description, value=param_default_value,
                                         minimum=param_min_value, maximum=param_max_value, group=param_group_name)
                # See https://www.pscad.com/webhelp-v502-ol/PSCAD/Component_Design/Parameters_Section/Parameter_Form/Input_Field.htm
                # for 'Literal', 'Constant' or 'Variable' contents
                # PSCAD Support : We appear to have missed this in the implementation of the Automation Library. A bug has been created to put this into the next release. For now there is a workaround for you.
                # ex:
                # gain = config.real('Gain', description="Gain factor")
                # gain._set_attr('.', 'content_type', 'Constant', str)       # Or 'Variable' or 'Literal'
                if param_fixedValue == 0:
                    p._set_attr('.', 'content_type', 'Variable', str)  # 'Literal', 'Constant' or 'Variable'
                else:
                    p._set_attr('.', 'content_type', 'Constant', str)

            elif param_pscad_type == 'REAL':
                p = model_parameters.real(param_name, description=param_description, value=param_default_value,
                                      minimum=param_min_value, maximum=param_max_value, units=param_unit,
                                      group=param_group_name)
                if param_fixedValue == 0:
                    p._set_attr('.', 'content_type', 'Variable', str)  # 'Literal', 'Constant' or 'Variable'
                else:
                    p._set_attr('.', 'content_type', 'Constant', str)
            elif param_pscad_type == 'CHARACTER(*)':
                model_parameters.text(param_name, description=param_description, value=param_default_value,
                                      group=param_group_name)
            else:
                raise Exception('Unknown PSCAD type for parameter: ' + param_name)

        initial_conditions = category.add('Initial Conditions')
        p = initial_conditions.real("TRelease", description='TRelease - Time to release initial conditions (sec)',
                                    value='0 [sec]', minimum=0, maximum=1E+308, units='sec')
        p._set_attr('.', 'content_type', 'Constant', str)

        for i in range(len(self.out_names)):
            out_name = self.out_names[i]
            out_unit = self.out_units[i]
            # param_default_value = self.param_default_values[i]

            out_pscad_type = self.out_pscad_types[i]  # INTEGER or REAL
            p_name = out_name + '_init'
            p_description = p_name + ' - Initial value of the output: ' + out_name

            if out_unit != '':
                p_value = '0 [' + out_unit + ']'
            else:
                p_value = 0

            if out_pscad_type == 'INTEGER':
                p = initial_conditions.integer(p_name, description=p_description, value=p_value)
            elif out_pscad_type == 'REAL':
                p = initial_conditions.real(p_name, description=p_description, value=p_value)
            else:
                raise Exception('Unknown PSCAD type for output: ' + out_name)
            p._set_attr('.', 'content_type', 'Constant', str)

        # is not read only
        wizard.form_width = 600
        wizard.form_splitter = 40  # space between symbol name and value in PSCAD form
        #wizard.form_height = 500

        ##################################################################################
        # Adding text to the graphics canvas
        ##################################################################################

        # title inside the component
        label_height = 18
        total_width = 200
        y_offset = 1
        if len(self.Model_Name_Shortened) <= 25:
            wizard.graphics.text(self.Model_Name_Shortened, total_width // 2 - 10, y_offset * label_height)
        else:
            model_name_part_1 = self.Model_Name_Shortened[:25]
            model_name_part_2 = self.Model_Name_Shortened[25:]
            wizard.graphics.text(model_name_part_1, total_width // 2 - 10, y_offset * label_height)
            y_offset += 1
            wizard.graphics.text(model_name_part_2, total_width//2 - 10, y_offset * label_height)

        y_offset += 1
        wizard.graphics.text("IEEE CIGRE DLL", total_width//2 - 10, y_offset * label_height)
        y_offset += 1
        wizard.graphics.text("", total_width, y_offset * label_height)  # to set the width of the component

        ##################################################################################
        # Adding Simple Fortran code to the component script
        ##################################################################################

        script = ''

        # NumIntStates = self.Model_Info.NumIntStates
        # if NumIntStates > 0:
        #     script += '\t#STORAGE INTEGER:' + str(NumIntStates) + '\n'

        NumIntStates = self.Model_Info.NumIntStates
        storage_integer = 1 + NumIntStates  # 1 for First_Step_Model parameter
        script += '\t#STORAGE INTEGER:' + str(storage_integer) + '\n'

        NumFloatStates = self.Model_Info.NumFloatStates
        NumDoubleStates = self.Model_Info.NumDoubleStates
        # next_t_model + NumFloatStates + NumDoubleStates + nb_outputs_total + Prev_t_pscad + nb_inputs_total
        storage_double = 1 + NumFloatStates + NumDoubleStates + self.nb_outputs_total + 1 + self.nb_inputs_total
        script += '\t#STORAGE REAL:' + str(storage_double) + '\n'
        script += '\n'

        script += '\tCALL ' + self.Model_Name_Shortened + '_FINTERFACE_PSCAD('
        for name in self.in_names:
            script += '$' + name + ', '
        for name in self.param_names:
            script += '$' + name + ', '
        for name in self.out_names:
            script += '$' + name + ', '

        out_init_names = [x + '_init' for x in self.out_names]
        for name in out_init_names:
            script += '$' + name + ', '

        script += '$TRelease, "$DLL_Path", $Use_Interpolation)'

        wizard.script['Fortran'] = script

        ##################################################################################
        # Creating the definition
        ##################################################################################

        defn = wizard.create_definition(project)
        canvas.create_component(defn, 20, 2)  # will be on top of the canvas

        ##################################################################################
        # Creating the component to include the MODELNAME_FINTERFACE_PSCAD.f90
        ##################################################################################

        # See: https://www.pscad.com/webhelp-v5-ol/PSCAD/The_Application_Environment/The_Workspace/The_Primary_Window/Projects_Branch/Resources_Branch.htm
        # Introduced in PSCAD V5, the resources branch replaces the prior Additional Source files
        # (*.f, *.for, *.f90, *.c, *.cpp) and the Additional Library (*.lib) and Object (*.obj/*.o)
        # Files project settings fields, as well as the older File Reference component, as a means to attach,
        # link or display files. It provides a single entry point for adding, deleting and managing all external,
        # dependent files.

        resource_already_added = False
        for resource in project.resources():
            if resource.name == self.fortran_interface_file_name:
                resource_already_added = True
                break
        if not resource_already_added:
            # Add a resource in the project tree
            project.create_resource(self.fortran_interface_file_name)

        self.display_info('The ' + self.Model_Name_Shortened + ' component has been created in project ' +
                          self.pscad_project_name + ' located in\n' + self.destination_folder)

        # Save the project because No dialog box if PSCAD is closed and project not saved...
        project.save()















