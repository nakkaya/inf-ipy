import getpass
import subprocess
import configparser
import unittest
import io
from contextlib import redirect_stdout
from core.__main__ import *

class test_inf_ipy(unittest.TestCase):
    def setUp(self):
        config = configparser.ConfigParser()
        config['SERVER'] = {'host': '127.0.0.1'}
        with open('config.ini', 'w') as configfile:
            config.write(configfile)

    def tearDown(self):
        if os.path.exists('config.ini'):
            os.remove('config.ini')

    def test010_ssh(self):
        ssh, cfg = ssh_connect({'host': '127.0.0.1'})
        ssh.close()
        ssh, cfg = ssh_connect({'host': '127.0.0.1', 'user' : getpass.getuser()})
        ssh.close()

    def test020_kernel(self):
        args = {'host': '127.0.0.1',
                'file' : 'inf-ipy-unit-test-kernel'}
        ssh, cfg = ssh_connect(args)
        conn_file = start_kernel(ssh, args)
        fetch_conn_file(ssh, conn_file)
        local_conn_file(conn_file, cfg["hostname"])
        ssh.close()

        km = kernel(args['file'])

        f = io.StringIO()
        with redirect_stdout(f):
            execute(km, '2+2')
        self.assertTrue('4' in f.getvalue())

        f = io.StringIO()
        with redirect_stdout(f):
            execute(km, 'some_var = 2+2\n42')
        self.assertTrue('42' in f.getvalue())

        f = io.StringIO()
        with redirect_stdout(f):
            execute(km, 'import time\ntime.sleep(' + str(network_timeout * 1.5) + ' )\n42')
        self.assertTrue('42' in f.getvalue())

        km.shutdown();
        os.remove(args['file'])

if __name__ == '__main__':
    unittest.main()
