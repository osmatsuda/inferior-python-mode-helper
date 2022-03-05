import os, os.path, io, __main__, tempfile, atexit

class inferior_python_mode_helper:
    _tmp_files = []
    
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

    def write_b(self, *, buffername, value, end='\n'):
        val = value if isinstance(value, str) else str(value)
        end_mark = ''
        for c in end.encode('utf8'):
            end_mark += ' '+str(c)
        if buffername:
            self._raw_command_output(f'write_b[{end_mark[1:]}]', buffername, val)
        else:
            print(value)

    def _open_tmp(self, tmp_fname):
        inferior_python_mode_helper._tmp_files.append(tmp_fname)
        return open(tmp_fname)

    @property
    def eval_expr(self):
        pass

    @property
    def open_b(self):
        pass

    @property
    def open_f(self):
        pass

    @staticmethod
    def _raw_command_output(cmd, data=None, result=None):
        id = inferior_python_mode_helper._identifier()
        o = f'_{id}_output_beg_\'(command "{cmd}"'
        if data is not None:
            o += f' data "{data}"'
            
        if result is not None:
            if type(result) is not str:
                result = str(result)
                
            o += ' result ['
            for c in list(result.encode('utf8')):
                o += str(c) + ' '
            o = o[:-1] + ']'
            
        o += f')_{id}_output_end_'
        print(o)
        
    @staticmethod
    def _cleanup_tmps():
        for p in inferior_python_mode_helper._tmp_files:
            if os.path.exists(p):
                os.remove(p)

atexit.register(inferior_python_mode_helper._cleanup_tmps)    

__main__.__dict__[inferior_python_mode_helper._identifier()] = inferior_python_mode_helper()
print(f'loaded "{inferior_python_mode_helper._identifier()}"')
