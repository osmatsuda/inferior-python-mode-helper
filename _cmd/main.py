import os

class inferior_python_mode_helper:
    def _user_home(self):
        return os.environ['HOME']
        
    @property
    def pwd(self):
        return os.getcwd()

    @property
    def cd(self):
        os.chdir(self._user_home())
        return None
    @cd.setter
    def cd(self, value):
        os.chdir(value)

    @property
    def cd_b(self):
        os.chdir(self._user_home())
        return None
    @cd_b.setter
    def cd_b(self, value):
        os.chdir(value)

    def seval(self):
        pass
    
    def __call__(self):
        pass

