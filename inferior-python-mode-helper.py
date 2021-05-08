import os, __main__

class inferior_python_mode_helper:
    @staticmethod
    def _identifier():
        return 'inferior-python-mode-helper-temp-id'

    @property
    def pwd(self):
        return os.getcwd()

    def _pwd(self):
        cwd = self.pwd
        self._raw_command_output("pwd", cwd, cwd)

    @property
    def cd(self):
        return None

    @property
    def cd_b(self):
        return None

    def _chdir(self, path=None, *, cmd):
        if not path:
            path = os.environ['HOME']
        os.chdir(path)
        self._raw_command_output(cmd, path)

    def _eval_sexp(self, value):
        return value
    eval_sexp = property(fset=_eval_sexp)

    @staticmethod
    def _raw_command_output(cmd, data=None, result=None):
        id = inferior_python_mode_helper._identifier()
        o = f'_{id}_output_start_\'(command "{cmd}"'
        if data is not None:
            o += f' data "{data}"'
        if result is not None:
            o += f' result "{result}"'
        o += f')_{id}_output_end_'
        print(o)

__main__.__dict__[inferior_python_mode_helper._identifier()] = inferior_python_mode_helper()
