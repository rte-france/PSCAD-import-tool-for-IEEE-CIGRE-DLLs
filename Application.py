# For GUI
# tuto : https://python.sdv.univ-paris-diderot.fr/20_tkinter/
import tkinter as tk
from tkinter import filedialog
import os

# import IEEE_Cigre_DLLInterface
from IEEE_Cigre_DLLInterface import IEEE_Cigre_DLLInterface_Model_Info, IEEE_Cigre_DLLInterface_DataType_Enum

import ctypes

# for PSCAD automation V5 :
# https://www.pscad.com/webhelp-v501-al/index.html
# https://www.pscad.com/webhelp-v5-al/tutorial/index.html
import mhi.pscad
from mhi.pscad import OverlayGraph, graphics, Component
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

        self.num_version = num_version
        self.dll_file_path = None
        self.dll_file_name = None  # the name of the file 'modelName.dll'
        self.dll_folder_path = None
        self.fortran_interface_file_path = None
        self.fortran_interface_file_name = None
        self.row_index = 0  # for GUI grid
        self.list_label_errors = []
        self.Model_Info = None
        self.Model_Name = None
        self.Model_Name_Shortened = None
        self.DLLInterfaceVersion = None  # An array of 4 integers
        #self.DLLInterfaceVersionStr = None  # The string version of DLLInterfaceVersion, ex: '1.1.0.0'
        # No pandas df used because heavy package when building in exe
        #self.in_df = None  # Dataframe inputs
        #self.out_df = None  # Dataframe outputs
        self.in_names = []
        self.in_fortran_types = []
        self.in_pscad_types = []  # INTEGER or REAL
        self.in_width = []

        self.out_names = []
        self.out_fortran_types = []
        self.out_pscad_types = []  # INTEGER or REAL
        self.out_width = []

        self.param_names = []
        self.param_fortran_types = []
        self.param_pscad_types = []  # For an IEEE CIGRE DLL, it can be INTEGER, REAL or CHARACTER(*)
        self.param_descriptions = []
        self.param_units = []
        self.param_fixedValue = []  # int
        self.param_default_values = []
        self.param_min_values = []
        self.param_max_values = []

    # Run the program and display GUI
    def start(self):
        self.create_widgets()

    # Create all GUI labels, buttons, etc.
    def create_widgets(self):

        # Row for PSCX file
        self.label_dll_file_path = tk.Label(self, text="DLL File Path")
        self.entry_dll_file_path = tk.Entry(self, width=50)
        self.button_browse_pscx_file_path = tk.Button(self, text="Browse",
                                                      command=lambda: self.open_file(self.entry_dll_file_path,
                                                                                     '.dll'))

        self.label_dll_file_path.grid(row=self.row_index, pady=10)  # pady add spaces up and down
        self.entry_dll_file_path.grid(row=self.row_index, column=1, pady=10)
        self.button_browse_pscx_file_path.grid(row=self.row_index, column=2, pady=10)
        self.row_index += 1

        # Row for the button Generate PSCAD Model
        self.button_generate_pscad_model = tk.Button(self, text="Generate PSCAD Model",
                                                 command=lambda: self.generate_pscad_model())
        self.button_generate_pscad_model.grid(row=self.row_index, column=1, pady=10)
        self.row_index += 1

        self.grid_columnconfigure(0, minsize=200)  # min width for labels (column 0)
        self.grid_columnconfigure(2, minsize=100)  # min width for Browse (column 2)

        # FOR DEBUG
        #self.entry_dll_file_path.insert(0, 'C:\\Users\\Cesar\\RTE\\CIGRE_IEEE_DLL\\Cesar\\SCRX9_dll_source_modif\\Release\\SCRX9.dll')
        #self.entry_dll_file_path.insert(0, 'C:\\Users\\Cesar\\RTE\\CIGRE_IEEE_DLL\\Cesar\\Gain_IEEE_CIGRE_DLL\\Release\\Gain_IEEE_CIGRE_DLL.dll')
        #self.button_generate_pscad_model.invoke()  # Appuyer sur le bouton
        # self.generate_pscad_model()

    def clean_attributes(self):
        self.in_names = []
        self.in_fortran_types = []
        self.in_pscad_types = []
        self.in_width = []

        self.out_names = []
        self.out_fortran_types = []
        self.out_pscad_types = []
        self.out_width = []

        self.param_names = []
        self.param_fortran_types = []
        self.param_pscad_types = []
        self.param_descriptions = []
        self.param_units = []
        self.param_default_values = []
        self.param_min_values = []
        self.param_max_values = []

        self.list_label_errors = []

    def generate_pscad_model(self):
        self.clean_attributes()

        self.dll_file_path = self.entry_dll_file_path.get()
        self.dll_file_name = os.path.basename(self.dll_file_path)
        self.dll_folder_path = os.path.dirname(self.dll_file_path)  # get folder path of a file
        try:
            dll_handle = ctypes.cdll.LoadLibrary(self.dll_file_path)
            dll_handle.Model_GetInfo.restype = ctypes.POINTER(IEEE_Cigre_DLLInterface_Model_Info)
            result = dll_handle.Model_GetInfo()  #  pointer to struct "IEEE_Cigre_DLLInterface_Model_Info"
            self.Model_Info = result.contents  # use contents to get access
            self.get_dll_interface_version()
            self.check_dll_interface_version()
            self.Model_Name = self.Model_Info.ModelName.decode("utf-8")
            self.Model_Name_Shortened = self.Model_Name[:50]  # take max 50 char
            self.fortran_interface_file_name = self.Model_Name_Shortened + '_FINTERFACE_PSCAD.f90'
            self.fortran_interface_file_path = self.dll_folder_path + '\\' + self.fortran_interface_file_name

            self.fill_in_out_param_lists()

            """type_modelinputs = self.generate_type('ModelInputs',
                                                  self.Model_Info.NumInputPorts, self.Model_Info.InputPortsInfo)

            type_modeloutputs = self.generate_type('ModelOutputs',
                                                   self.Model_Info.NumOutputPorts, self.Model_Info.OutputPortsInfo)

            type_modelparameters = self.generate_type('ModelParameters',
                                                      self.Model_Info.NumParameters, self.Model_Info.ParametersInfo)"""
            storfloat_declaration = self.generate_storfloat_declaration()
            type_modelinputs = self.generate_type('ModelInputs', self.in_names, self.in_fortran_types, self.in_width)
            type_modeloutputs = self.generate_type('ModelOutputs', self.out_names, self.out_fortran_types, self.out_width)
            type_modelparameters = self.generate_type('ModelParameters', self.param_names, self.param_fortran_types)
            finterface_function_prototype = self.generate_finterface_function_prototype()
            variables_from_pscad = self.generate_variables_from_pscad()
            # storfloat_code = self.generate_storfloat_code()
            pointers_to_state_arrays = self.generate_pointers_to_state_arrays()
            """params_pscad_to_new_struct = self.generate_conversion(self.param_names, None, self.param_fortran_types,
                                                                  self.param_pscad_types, False, True, 'PARAMETERS_new')
            outputs_storf_to_new_struct = self.generate_conversion(self.out_names, self.out_width, self.out_fortran_types,
                                                                  self.out_pscad_types, True, True, 'OUTPUTS_new')

            inputs_pscad_to_new_struct = self.generate_conversion(self.in_names, self.in_width,
                                                                   self.in_fortran_types,
                                                                   self.in_pscad_types, False, True, 'INPUTS_new')

            inputs_pscad_to_new_struct = inputs_pscad_to_new_struct.replace('\t', '\t\t')  # double tab for this part

            outputs_new_to_storf = self.generate_conversion(self.out_names, self.out_width,
                                                                  self.out_fortran_types,
                                                                  self.out_pscad_types, True, False, 'OUTPUTS_new')

            outputs_new_to_pscad = self.generate_conversion(self.out_names, self.out_width,
                                                            self.out_fortran_types,
                                                            self.out_pscad_types, False, False, 'OUTPUTS_new')"""

            params_change_check = self.generate_parameters_change_check()

            params_pscad_to_new_struct = self.generate_conversion(self.param_names, None, self.param_fortran_types,
                                                                  self.param_pscad_types, '_pscad', True, 'PARAMETERS_new')
            outputs_storf_to_new_struct = self.generate_conversion(self.out_names, self.out_width,
                                                                   self.out_fortran_types,
                                                                   self.out_pscad_types, None, True, 'OUTPUTS_new')

            outputs_init_pscad_to_new_struct = self.generate_conversion(self.out_names, self.out_width,
                                                                        self.out_fortran_types,
                                                                        self.out_pscad_types, '_init_pscad', True,
                                                                        'OUTPUTS_new')

            outputs_init_pscad_to_new_struct = outputs_init_pscad_to_new_struct.replace('\t',
                                                                                        '\t\t\t')  # triple tab for this part

            inputs_pscad_to_new_struct = self.generate_conversion(self.in_names, self.in_width,
                                                                  self.in_fortran_types,
                                                                  self.in_pscad_types, '_pscad', True, 'INPUTS_new')

            inputs_pscad_to_new_struct = inputs_pscad_to_new_struct.replace('\t', '\t\t')  # double tab for this part

            storfloat_to_storf = self.generate_storfloat_to_storf()

            outputs_new_to_storf = self.generate_conversion(self.out_names, self.out_width,
                                                            self.out_fortran_types,
                                                            self.out_pscad_types, None, False, 'OUTPUTS_new')

            outputs_new_to_pscad = self.generate_conversion(self.out_names, self.out_width,
                                                            self.out_fortran_types,
                                                            self.out_pscad_types, '_pscad', False, 'OUTPUTS_new')

            buffer = self.create_fortran_code(type_modelinputs, type_modeloutputs, type_modelparameters,
                                              finterface_function_prototype, variables_from_pscad,
                                              pointers_to_state_arrays, params_change_check, params_pscad_to_new_struct,
                                              outputs_storf_to_new_struct, outputs_init_pscad_to_new_struct,
                                              inputs_pscad_to_new_struct, storfloat_to_storf, outputs_new_to_storf,
                                              outputs_new_to_pscad)

            with open(self.fortran_interface_file_path, 'w') as f:
                f.write(buffer)  # allow rewrite on existing file

            self.generate_pscad_project()

        except Exception as e:
            self.display_error(e.args[0])

    def remove_forbidden_char(self, str):
        str = str.replace(' ', '_')
        return str

    def fill_in_out_param_lists(self):
        for i in range(0, self.Model_Info.NumInputPorts):
            signal = self.Model_Info.InputPortsInfo[i]
            name = signal.Name.decode("utf-8")
            name = self.remove_forbidden_char(name)
            datatype = signal.DataType  # ex: IEEE_Cigre_DLLInterface_DataType_int8_T
            self.in_names.append(name)
            try:
                width = signal.Width  # parameters have no width attribute
            except Exception as e:
                width = 1
            self.in_width.append(width)

            fortran_type_str = self.ieee_cigre_datatype_to_fortran_type_str(datatype)
            self.in_fortran_types.append(fortran_type_str)
            pscad_type_str = self.ieee_cigre_datatype_to_pscad_inout_type_str(datatype)
            self.in_pscad_types.append(pscad_type_str)

        for i in range(0, self.Model_Info.NumOutputPorts):
            signal = self.Model_Info.OutputPortsInfo[i]
            name = signal.Name.decode("utf-8")
            name = self.remove_forbidden_char(name)
            datatype = signal.DataType  # ex: IEEE_Cigre_DLLInterface_DataType_int8_T
            self.out_names.append(name)
            try:
                width = signal.Width  # parameters have no width attribute
            except Exception as e:
                width = 1
            self.out_width.append(width)

            fortran_type_str = self.ieee_cigre_datatype_to_fortran_type_str(datatype)
            self.out_fortran_types.append(fortran_type_str)
            pscad_type_str = self.ieee_cigre_datatype_to_pscad_inout_type_str(datatype)
            self.out_pscad_types.append(pscad_type_str)

        for i in range(0, self.Model_Info.NumParameters):
            parameter = self.Model_Info.ParametersInfo[i]
            name = parameter.Name.decode("utf-8")
            name = self.remove_forbidden_char(name)
            datatype = parameter.DataType  # ex: IEEE_Cigre_DLLInterface_DataType_int8_T
            description = parameter.Description.decode("utf-8")
            unit = parameter.Unit.decode("utf-8")
            fixedValue = parameter.FixedValue  # is int
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
            self.param_descriptions.append(description)
            self.param_units.append(unit)
            self.param_fixedValue.append(fixedValue)
            self.param_default_values.append(default_value)
            self.param_min_values.append(min_value)
            self.param_max_values.append(max_value)

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
            return parameter_union.Char_Ptr
        else:
            raise Exception('get_parameter_default_min_or_max_value : error in dataType_enum: ' + str(datatype))


    def generate_storfloat_declaration(self):
        buffer = ''
        NumFloatStates = self.Model_Info.NumFloatStates
        if NumFloatStates > 0:
            buffer += '\t! ---- Variables to use if STORFLOAT needed (for IEEE CIGRE FloatStates array) ----\n' \
              '\t! we can also try to use REAL*4, POINTER :: STORFLOAT(:) and store pointer address in STORI\n' \
              '\tREAL*4, DIMENSION (:), ALLOCATABLE :: STORFLOAT ! To store IEEE CIGRE FloatStates array\n' \
              '\tINTEGER :: NSTORFLOAT ! STORFLOAT index\n' \
              '\tINTEGER :: N_MAX_INSTANCE = 100  ! nb max of instances of this specific IEEE CIGRE Model\n' \
              '\tDOUBLE PRECISION :: TIME_PREV = -1  ! Used only to reset NSTORFLOAT to 1\n\n' \

        return buffer

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

        buffer += 'TRelease, DLL_Path)'

        return buffer

    def generate_variables_from_pscad(self):
        buffer = ''
        buffer += self.generate_variables_declaration('IN', self.in_names, self.in_pscad_types, self.in_width)
        buffer += '\n'
        buffer += self.generate_variables_declaration('IN', self.param_names, self.param_pscad_types, None)
        buffer += '\n'
        out_init_names = [x + '_init' for x in self.out_names]
        buffer += self.generate_variables_declaration('IN', out_init_names, self.out_pscad_types, self.out_width)
        buffer += '\n'
        buffer += self.generate_variables_declaration('OUT', self.out_names, self.out_pscad_types, self.out_width)
        return buffer

    def generate_variables_declaration(self, intent_value, var_names, var_pscad_types, var_width=None):
        buffer = ''
        for i in range(0, len(var_names)):
            if var_width is not None and var_width[i] > 1:
                buffer += '\t' + var_pscad_types[i] + ', INTENT(' + intent_value + ') :: ' + var_names[i] +\
                          '_pscad(' + str(var_width[i]) + ')\n'
            else:
                buffer += '\t' + var_pscad_types[i] + ', INTENT(' + intent_value + ') :: ' + var_names[i] + '_pscad\n'
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

    def generate_parameters_change_check(self):
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

        return buffer

    def generate_pointers_to_state_arrays(self):
        buffer = ''
        buffer += '\t! Setup Pointers to state variable storage\n'
        if self.Model_Info.NumIntStates > 0:
            buffer += '\tpInstance%IntStates    = c_loc(STORI(NSTORI))\n'
        if self.Model_Info.NumFloatStates > 0:
            buffer += '\tpInstance%FloatStates  = c_loc(STORFLOAT(1))\n'
        if self.Model_Info.NumDoubleStates > 0:
            buffer += '\tpInstance%DoubleStates  = c_loc(STORF(NSTORF + 1 + ' + str(self.Model_Info.NumFloatStates) + ')) !DoubleStates just after Next_t_model and FloatStates\n'

        for i in range(self.Model_Info.NumFloatStates):
            buffer += '\tSTORFLOAT(' + str(i+1) + ') = STORF(NSTORF + 1 + ' + str(i) + ') ! DOUBLE PRECISION (REAL*8) to REAL*4: possible accuracy lost\n'

        buffer += '\n'
        return buffer

    def generate_storfloat_to_storf(self):
        buffer = ''
        for i in range(self.Model_Info.NumFloatStates):
            buffer += '\tSTORF(NSTORF + 1 + ' + str(i) + ') = STORFLOAT(' + str(i + 1) + ')\n'

        return buffer

    # names is self.in_names or self.param_names or self.out_names
    # same for widths, fortran_types and pscad_types
    # suffix is suffix used in varibales for part_2. If None, use STORF
    # fill_new_struct is boolean :
    # * True to fill INPUTS_new, OUTPUTS_new or PARAMETERS_new (INPUTS_new etc. on the left side)
    # * False to get values from them (INPUTS_new etc. on the right side)
    # new_struct is "INPUTS_new", "OUTPUTS_new" or "PARAMETERS_new"
    def generate_conversion(self, names, widths, fortran_types, pscad_types, suffix, fill_new_struct,
                            new_struct):
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

                    part_2 = 'STORF(NSTORF + 1 + ' + str(self.Model_Info.NumFloatStates) + ' + ' + \
                             str(self.Model_Info.NumDoubleStates) + ' + ' + str(i_storf) + ')'
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

    """# names is self.in_names or self.param_names or self.out_names
    # same for widths, fortran_types and pscad_types
    # use_storf is boolean to use STORF or _pscad variables
    # fill_new_struct is boolean :
    # * True to fill INPUTS_new, OUTPUTS_new or PARAMETERS_new (INPUTS_new etc. on the left side)
    # * False to get values from them (INPUTS_new etc. on the right side)
    # new_struct is "INPUTS_new", "OUTPUTS_new" or "PARAMETERS_new"
    def generate_conversion(self, names, widths, fortran_types, pscad_types, use_storf, fill_new_struct, new_struct):
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

                if use_storf:
                    if self.Model_Info.NumDoubleStates > 0:
                        part_2 = 'STORF(NSTORF + 2 + ' + str(i_storf) + ')'
                    else:
                        part_2 = 'STORF(NSTORF + 1 + ' + str(i_storf) + ')'
                    i_storf += 1

                    if fill_new_struct:  # STORF on the right side
                        type_from = 'DOUBLE PRECISION'  # REAL*8
                        type_to = fortran_type
                    else:
                        type_from = fortran_type
                        type_to = 'DOUBLE PRECISION'
                else:
                    part_2 = name + '_pscad'
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

        return buffer"""

    """def generate_conversion(self, names, widths, fortran_types, pscad_types, use_storf, fill_new_struct, new_struct):
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

            if width == 1:
                part_1 = part_1_base
                if use_storf:
                    if self.Model_Info.NumDoubleStates > 0:
                        part_2 = 'STORF(NSTORF + 2 + ' + str(i_storf) + ')'
                    else:
                        part_2 = 'STORF(NSTORF + 1 + ' + str(i_storf) + ')'
                    i_storf += 1

                    if fill_new_struct:  # STORF on the right side
                        type_from = 'DOUBLE PRECISION'  # REAL*8
                        type_to = fortran_type
                    else:
                        type_from = fortran_type
                        type_to = 'DOUBLE PRECISION'
                else:
                    part_2 = name + '_pscad'
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

            else:  # width > 1
                for j in range(1, width + 1):
                    part_1 = part_1_base + '(' + str(j) + ')'
                    if use_storf:
                        if self.Model_Info.NumDoubleStates > 0:
                            part_2 = 'STORF(NSTORF + 2 + ' + str(i_storf) + ')'
                        else:
                            part_2 = 'STORF(NSTORF + 1 + ' + str(i_storf) + ')'
                        i_storf += 1

                        if fill_new_struct:  # STORF on the right side
                            type_from = 'DOUBLE PRECISION'  # REAL*8
                            type_to = fortran_type
                        else:
                            type_from = fortran_type
                            type_to = 'DOUBLE PRECISION'
                    else:
                        part_2 = name + '_pscad' + '(' + str(j) + ')'
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

        return buffer"""

    """def generate_conversion(self, names, fortran_types, pscad_types, use_storf, fill_new_struct, new_struct):

        for i in range(0, len(names)):
            part_1 = new_struct + '%' + names[i]
            if use_storf:
                if self.Model_Info.NumDoubleStates > 0:
                    part_2 = 'STORF(NSTORF + 2 + ' + str(i) + ')'
                else:
                    part_2 = 'STORF(NSTORF + 1 + ' + str(i) + ')'
            else:
                part_2 = names[i] + '_pscad'

            if use_storf:
                if fill_new_struct:  # STORF on the right side
                    type_from = 'DOUBLE PRECISION'  # REAL*8
                    type_to = fortran_types[i]
                else:
                    type_from = fortran_types[i]
                    type_to = 'DOUBLE PRECISION'
            else:
                if fill_new_struct:
                    type_from = pscad_types[i]
                    type_to = fortran_types[i]
                else:
                    type_from = fortran_types[i]
                    type_to = pscad_types[i]

            conversion_part_1, conversion_part_2 = self.get_conversion(type_from, type_to)

            if fill_new_struct:
                return part_1 + ' = ' + conversion_part_1 + part_2 + conversion_part_2
            else:
                return part_2 + ' = ' + conversion_part_1 + part_1 + conversion_part_2"""

    # Only conversions that can appear in the wrapper
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

        if conversion_part_1 is None or conversion_part_2 is None:
            raise Exception('get_conversion problem with type_from: ' + type_from + ' and type_to: ' + type_to)

        return conversion_part_1, conversion_part_2


    # We could also convert a text file to a base64 encoded string and then save the base64 encoded string
    # as a Python module
    # import base64
    # with open('my_file.txt', 'rb') as f:
    #     encoded_file = base64.b64encode(f.read())
    # with open('my_file.py', 'w') as f:
    #     f.write('encoded_file = b"' + encoded_file.decode('ascii') + '"')
    # from my_file import encoded_file
    # fichier_decoded = base64.b64decode(encoded_file)
    def create_fortran_code(self, type_modelinputs, type_modeloutputs, type_modelparameters,
                            finterface_function_prototype, variables_from_pscad,
                            pointers_to_state_arrays, params_change_check, params_pscad_to_new_struct, outputs_storf_to_new_struct,
                            outputs_init_pscad_to_new_struct, inputs_pscad_to_new_struct, storfloat_to_storf,
                            outputs_new_to_storf, outputs_new_to_pscad):
        bf = ''
        bf += ('\t! Common module for each instance of ' + self.Model_Name_Shortened + ' model\n' \
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
              '\t! We could also used types with kind type parameters from the ISO_C_BINDING module\n')

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

        bf += finterface_function_prototype + '\n\n'

        bf += '\tUSE, INTRINSIC :: iso_c_binding, only : c_ptr, c_f_pointer, c_loc\n\n' \
              '\tUSE ' + self.Model_Name_Shortened + '_MOD\n\n' \
              '\tINCLUDE \'s1.h\' ! This include gives us access to TIME, TIMEZERO etc. variables\n' \
              '\tINCLUDE \'emtstor.h\' ! This include gives us access to STORI, STORF, NSTORI, NSTORF\n\n' \
              '\tIMPLICIT NONE\n\n' \
              '\t! Inputs, params and outputs variables, received from PSCAD\n'

        bf += variables_from_pscad + '\n'

        bf += '\t! Other inputs\n' \
              '\tDOUBLE PRECISION, INTENT(IN) :: TRelease  ! Time to release initial output values\n' \
              '\tCHARACTER*(*), INTENT(IN) :: DLL_Path\n\n' \
              '\t! Local variables\n' \
              '\tINTEGER   :: i  ! Used for loops\n' \
              '\tINTEGER   :: retval  ! return value of DLL functions\n' \
              '\tINTEGER   :: retValFreeLib     ! Only for FreeLibrary\n' \
              '\tTYPE(IEEE_Cigre_DLLInterface_Instance) :: pInstance\n' \
              '\tDOUBLE PRECISION :: Next_t_model ! To know when to call model output function\n' \
              '\tLOGICAL        :: IsInitializing    ! bool when the model is initializing\n' \
              '\tTYPE(c_ptr)                                           :: Model_Info_cp ! The C pointer of Model_GetInfo\n' \
              '\tTYPE(IEEE_Cigre_DLLInterface_Model_Info), POINTER     :: Model_Info_fp ! The F pointer of Model_GetInfo\n' \
              '\tTYPE(ModelInputs)     :: INPUTS_new\n' \
              '\tTYPE(ModelOutputs)    :: OUTPUTS_new\n' \
              '\tTYPE(ModelParameters) :: PARAMETERS_new\n' \
              '\t! To check if the DLL has changed, the name and version will be checked against the name and version extracted from the DLL\n' \
              '\tTYPE(c_ptr)    :: DLLModelName_cp  ! The C pointer to the DLL ModelName\n' \
              '\tINTEGER(KIND=c_int8_t), DIMENSION(4) :: DLLInterfaceVersion  ! The DLL interface version of the DLL\n' \
              '\tINTEGER(KIND=c_int8_t), DIMENSION(4) :: OrigDLLInterfaceVersion = ' + str(self.DLLInterfaceVersion) + '  ! DLL interface version, written by the DLL Import tool\n'
              #'\tINTEGER(KIND=c_int8_t), DIMENSION(4) :: OrigDLLInterfaceVersion = [' + ', '.join(map(str, self.DLLInterfaceVersion)) + ']  ! DLL interface version, written by the DLL Import tool\n'
              #'\tCHARACTER*50   :: OrigModelVersion    = "' + self.DLLInterfaceVersionStr + '"  ! Model version, written by the DLL Import tool\n'

        if self.Model_Info.NumFloatStates > 0:
            bf += '\tREAL*4, DIMENSION(' + str(self.Model_Info.NumFloatStates) + ') :: STORFLOAT  ! To store IEEE CIGRE FloatStates array\n'

        bf += '\n' \
              '\t! Initialize Next_t_model.\n' \
              '\t! TIMEZERO is True when time t = 0.0\n' \
              '\t! If it\'s FIRSTSTEP but not TIMEZERO => Next_t_model = STORF(NSTORF)\n' \
              '\tNext_t_model  = STORF(NSTORF)\n' \
              '\tIF ( TIMEZERO ) THEN\n' \
              '\t\tNext_t_model = 0\n' \
              '\tENDIF\n\n' \
              '\t! Set pointers to input/output/parameter structs\n' \
              '\tpInstance%ExternalInputs     = c_loc(INPUTS_new)\n' \
              '\tpInstance%Parameters         = c_loc(PARAMETERS_new)\n' \
              '\tpInstance%ExternalOutputs    = c_loc(OUTPUTS_new)\n' \
              '\tpInstance%Time               = TIME\n\n'

        bf += pointers_to_state_arrays

        bf += '\tIF (FIRST_CALL_THIS_FILE) THEN  ! Use FIRST_CALL_THIS_FILE to do following instruction once\n\n' \
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
              '\t\t! Error if the time step required by the controls is smaller than the simulation time step\n' \
              '\t\tIF ( DELT_Model .LT. DELT) THEN\n' \
              '\t\t\tWRITE(IUNIT,*) "*** Error - The ",OrigModelNameShortened," controls require a time-step of ", DELT_Model\n' \
              '\t\t\tWRITE(IUNIT,*) "The current program time-step used is ", DELT\n' \
              '\t\t\tWRITE(IUNIT,*) "The controller sample time must be larger than the simulation time-step."\n' \
              '\t\t\tSTOP\n' \
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
              '\tENDIF\n\n' \

        bf += params_change_check

        bf += '\t! Assign parameters from the simulation tool side to model parameters\n' \
              '\t! Done before Model_FirstCall because this function may use the parameters\n'

        bf += params_pscad_to_new_struct + '\n'

        bf += '\t! Same for outputs (stored after DoubleStates in STORF)\n'

        bf += outputs_storf_to_new_struct + '\n'

        bf += '\t! FIRSTSTEP is True for first step starting from the Data file or Snapshot file.\n' \
              '\t! This part is outside the check if the model sampling time has been reached, by prevention.\n' \
              '\t! -> We need to check if TIME .GE. Next_t_model is called when starting from snapshot.\n' \
              '\tIF ( FIRSTSTEP ) THEN\n' \
              '\t\tN_INSTANCE = N_INSTANCE + 1\n' \
              '\t\tretval = Model_FirstCall(pInstance)\n' \
              '\t\tcall Handle_Message(pInstance, retval)\n' \
              '\tENDIF\n\n' \
              '\t! Check if the model sampling time has been reached\n' \
              '\tIF ( TIME .GE. Next_t_model - EQUALITY_PRECISION ) THEN\n\n' \
              '\t\t! Model_CheckParameters must be just after Model_FirstCall\n' \
              '\t\t! Do it only if parameters have changed\n' \
              '\t\tIF (ParametersChanged) THEN\n' \
              '\t\t\tretval = Model_CheckParameters(pInstance)\n' \
              '\t\t\tcall Handle_Message(pInstance, retval)\n' \
              '\t\tENDIF\n\n' \
              '\t\t! Determine when the model should be initializing\n' \
              '\t\tIsInitializing = .FALSE.\n' \
              '\t\t! IF ( TIME .LE. TRelease ) should also work\n' \
              '\t\tIF ( TIMEZERO .OR. (TIME .LE. TRelease) ) IsInitializing = .TRUE.\n\n' \
              '\t\t! Initialize the models\n' \
              '\t\tIF ( IsInitializing ) THEN\n' \
              '\t\t\t! assign initial values to model outputs provided through the component mask\n'

        bf += outputs_init_pscad_to_new_struct + '\n'

        bf += '\t\t\tretval = Model_Initialize(pInstance)\n' \
              '\t\t\tcall Handle_Message(pInstance, retval)\n' \
              '\t\tENDIF\n\n' \
              '\t\t! assign inputs from the simulation tool side to model inputs\n'

        bf += inputs_pscad_to_new_struct + '\n'

        bf += '\t\t! Call ModelOutputs function every DELT_Model sampling time\n' \
              '\t\tretval = Model_Outputs(pInstance)\n' \
              '\t\tcall Handle_Message(pInstance, retval)\n\n' \
              '\t\tNext_t_model = Next_t_model + DELT_Model  ! Next_t_model indicates when the model will be called\n' \
              '\tENDIF\n\n' \
              '\t! --------------------------------\n' \
              '\t! Put back into storage\n' \
              '\t! --------------------------------\n\n' \
              '\tSTORF(NSTORF) = Next_t_model\n\n'

        bf += storfloat_to_storf + '\n'

        bf += '\t! Outputs back into storage\n'

        bf += outputs_new_to_storf + '\n'

        bf += '\t! Send back outputs values to PSCAD\n'

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
              '\t\tENDIF\n' \
              '\tENDIF\n\n' \
              '\t! Increment STORx pointers (this must be done every time step)\n' \
              '\t! Note memory for these vectors must be allocated using #STORAGE syntax in the PSCAD component definition\n'

        NumIntStates = self.Model_Info.NumIntStates
        if NumIntStates > 0:
            bf += '\tNSTORI = NSTORI + ' + str(NumIntStates) + ' ! # of integer states used in the dll\n'

        NumFloatStates = self.Model_Info.NumFloatStates
        NumDoubleStates = self.Model_Info.NumDoubleStates
        nb_outputs_total = sum(self.out_width)
        bf += '\tNSTORF = NSTORF + 1 + ' + str(NumFloatStates) + ' + ' + str(NumDoubleStates) + ' + ' + str(nb_outputs_total) + ' ! STORF contains Next_t_model, float states, double states, outputs \n\n'

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

    def get_dll_interface_version(self):
        #self.DLLInterfaceVersion = self.Model_Info.DLLInterfaceVersion  # ex: [1, 1, 0, 0]
        self.DLLInterfaceVersion = [int(x) for x in self.Model_Info.DLLInterfaceVersion]  # ex: [1, 1, 0, 0]

        # example: 1.1.0.0
        #self.DLLInterfaceVersionStr = str(DLLInterfaceVersion[0]) + '.' + str(DLLInterfaceVersion[1]) + '.' + str(
        #    DLLInterfaceVersion[2]) + '.' + str(DLLInterfaceVersion[3])

    # Check if this import tool is compatible with the version of the IEEE CIGRE DLL Interface
    # For now, it is only compatible with 1.1.0.0
    def check_dll_interface_version(self):
        if self.DLLInterfaceVersion != [1, 1, 0, 0]:
            DLLInterfaceVersionStr = '.'.join(map(str, self.DLLInterfaceVersion))
            raise Exception('DLLInterfaceVersion is not correct. This import tool is compatible with 1.1.0.0 but the'
                            ' DLL interface version is ' + DLLInterfaceVersionStr)

        """if self.DLLInterfaceVersionStr != '1.1.0.0':
            raise Exception('DLLInterfaceVersion is not correct. This import tool is compatible with 1.1.0.0 but the'
                            ' DLL interface versin is ' + self.DLLInterfaceVersionStr)"""

        """DLLInterfaceVersion = self.Model_Info.DLLInterfaceVersion
        if DLLInterfaceVersion[0] != 1 or DLLInterfaceVersion[1] != 1 or DLLInterfaceVersion[2] != 0 \
                or DLLInterfaceVersion[3] != 0:
            raise Exception('DLLInterfaceVersion is not correct')"""

    def display_error(self, error):

        if error is None or error == '':
            return

        error = 'Error : ' + error

        label_error = tk.Label(self, text=error, fg='#D63C27', font='Helvetica 10 bold')
        label_error.grid(row=self.row_index, column=1, pady=5)
        self.list_label_errors.append(label_error)
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
            pscad = mhi.pscad.application()  # use PSCAD instance already open or launch a new instance
        except Exception as e:
            raise Exception(e.args[0] + ". PSCAD V5.X is not installed on this computer or is unlicensed."
                                        " Only the wrapper file is generated")

        workspace = pscad.workspace()  # Get workspace
        project = workspace.create_project(1, self.Model_Name_Shortened, self.dll_folder_path)
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

        model_parameters = category.add("Model Parameters")
        for i in range(len(self.param_names)):
            param_name = self.param_names[i]
            param_pscad_type = self.param_pscad_types[i]  # For an IEEE CIGRE DLL, it can be INTEGER, REAL or CHARACTER(*)
            param_description = param_name + ' - ' + self.param_descriptions[i]
            param_unit = self.param_units[i]
            param_default_value = self.param_default_values[i]
            param_min_value = self.param_min_values[i]
            param_max_value = self.param_max_values[i]
            if param_pscad_type == 'INTEGER':
                model_parameters.integer(param_name, description=param_description, value=param_default_value,
                                         minimum=param_min_value, maximum=param_max_value)
            elif param_pscad_type == 'REAL':
                model_parameters.real(param_name, description=param_description, value=param_default_value,
                                         minimum=param_min_value, maximum=param_max_value, units=param_unit)
            elif param_pscad_type == 'CHARACTER(*)':
                model_parameters.text(param_name, description=param_description, value=param_default_value)
            else:
                raise Exception('Unknown PSCAD type for parameter: ' + param_name)

        initial_conditions = category.add('Initial Conditions')
        initial_conditions.real("TRelease", description='TRelease - Time to release initial conditions (sec)',
                                value=0, minimum=0, maximum=1E+308, units='sec')

        for i in range(len(self.out_names)):
            out_name = self.out_names[i]
            out_pscad_type = self.out_pscad_types[i]  # INTEGER or REAL
            p_name = out_name + '_init'
            p_description = p_name + ' - Initial value of the output: ' + out_name
            p_value = 0

            if out_pscad_type == 'INTEGER':
                initial_conditions.integer(p_name, description=p_description, value=p_value)
            elif out_pscad_type == 'REAL':
                initial_conditions.real(p_name, description=p_description, value=p_value)
            else:
                raise Exception('Unknown PSCAD type for output: ' + out_name)

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
        NumIntStates = self.Model_Info.NumIntStates
        if NumIntStates > 0:
            script += '\t#STORAGE INTEGER:' + str(NumIntStates) + '\n'

        nb_outputs_total = sum(self.out_width)
        NumFloatStates = self.Model_Info.NumFloatStates
        NumDoubleStates = self.Model_Info.NumDoubleStates
        storage_double = 1 + NumFloatStates + NumDoubleStates + nb_outputs_total
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

        script += '$TRelease, "$DLL_Path")'

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

        # Add a resource in the project tree
        project.create_resource(self.fortran_interface_file_name)

        #canvas.add_component(defn, 20, 2)  # will be on top of the canvas












