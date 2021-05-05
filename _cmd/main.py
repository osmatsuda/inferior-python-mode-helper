import os

class inferior_python_mode_helper:
    @property
    def pwd(self):
        return os.getcwd()

    def _pwd(self):
        cwd = self.pwd
        raw_command_output("pwd", cwd, cwd)

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
        raw_command_output(cmd, path)

    def _eval_sexp(self, value):
        # when the value is a cons, this function returns iterator
        # if a symbol, then a string
        # if nil, then None 
        pass
    eval_sexp = property(fset=_eval_sexp)

def raw_command_output(cmd, data=None, result=None):
    o = f'_cmd_output_start_\'(command "{cmd}"'
    if data is not None:
        o += f' data "{data}"'
    if result is not None:
        o += f' result "{result}"'
    o += ')_cmd_output_end_'
    print(o)
    
