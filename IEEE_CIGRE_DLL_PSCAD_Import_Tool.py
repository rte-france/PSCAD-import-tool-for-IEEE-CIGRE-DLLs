from Application import Application
import sys
import os

if __name__ == '__main__':

    if getattr(sys, 'frozen', False):
        application_path = os.path.dirname(sys.executable)
    elif __file__:
        application_path = os.path.dirname(__file__)
    os.chdir(application_path)

    # Application inherits from the class tk.Tk (tkinter)
    num_version = '2.4'
    app = Application(num_version)
    app.start()
    #app.title("PSCAD Import Tool (v" + num_version + ") of an IEEE/Cigre DLL format")
    app.title("PSCAD Import Tool (IEEE/Cigre DLL format) RTE/TUD v" + num_version)
    app.mainloop()  # display window