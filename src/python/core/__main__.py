#!/usr/bin/env python3

import logging
import argparse
import configparser
import paramiko
from sshtunnel import SSHTunnelForwarder
import re
import json
import uuid
import time
import os
import sys
import jupyter_client
from IPython.utils.capture import capture_output
from prompt_toolkit import prompt
from prompt_toolkit.history import FileHistory
from prompt_toolkit.auto_suggest import AutoSuggestFromHistory
from queue import Empty
import base64
import warnings
import tempfile

warnings.filterwarnings(action='ignore',module='.*paramiko.*')
logging.getLogger("paramiko").setLevel(logging.WARNING)

logging.basicConfig(level=os.environ.get("LOGLEVEL", "INFO"),
                    format='%(levelname)s: %(message)s',)

verbose = False
timeout = 5

def ssh_read_config(args):
    ssh_config = paramiko.SSHConfig()
    user_config_file = os.path.expanduser("~/.ssh/config")
    if os.path.exists(user_config_file):
        with open(user_config_file) as f:
            ssh_config.parse(f)

    cfg = {'hostname': args['host']}

    if args['user']:
        cfg['username'] = args['user']
    
    if args['pass']:
        cfg['password'] = args['pass']
    
    user_config = ssh_config.lookup(cfg['hostname'])
    cfg["hostname"] = user_config["hostname"]
    if "user" in user_config :
        cfg["username"] = user_config["user"]
    if 'proxycommand' in user_config:
        cfg["sock"] = paramiko.ProxyCommand(user_config['proxycommand'])
    return cfg


def ssh_connect(args):
    ssh = paramiko.SSHClient()
    ssh.load_system_host_keys()
    ssh.set_missing_host_key_policy(paramiko.AutoAddPolicy())
    cfg = ssh_read_config(args)
    logging.info("connecting " + str(cfg))
    ssh.connect(**cfg)
    return ssh, cfg

def runtime_dir(ssh):
    stdin, stdout, stderr = ssh.exec_command('jupyter --runtime-dir')
    return stdout.read().decode('ascii').strip()

def start_kernel(ssh, args):
    conn_file = "rk-" + str(uuid.uuid4()) + ".json"

    if args['file']:
        conn_file = args['file']

    cmd = 'ipython kernel --ip="*"' + ' --ConnectionFileMixin.connection_file="' + conn_file + '"'

    kernel = "IPython.kernel.zmq.ipkernel.IPythonKernel"

    if args["kernel"] :
        kernel = args["kernel"]

    if args["kernel"] :
        cmd += ' --IPKernelApp.kernel_class="' + kernel + '"'

    if verbose :
        logging.info('exec ' + cmd)
    else:
        logging.info('launching kernel')
        
    stdin, stdout, stderr = ssh.exec_command (cmd)

    while stdout.channel.recv_ready() is False:
        time.sleep(0.1)
    
    if stdout.channel.recv_stderr_ready() :
        logging.error('can not launch kernel')
        sys.exit(1)

    return conn_file

def fetch_conn_file(ssh, conn_file):
    rt_dir = runtime_dir(ssh)
    remote_file = rt_dir + "/" + conn_file
    local_file = './' + conn_file

    sftp = ssh.open_sftp()

    if verbose :
        logging.info('rt_dir ' + rt_dir)

    logging.info("waiting kernel")
    while fexists(sftp, remote_file) is False:
        time.sleep(0.5)

    logging.info("fetch connection file " + local_file[2:])
    sftp.get(remote_file, local_file)
    sftp.close()

def local_conn_file(file, host):
    with open(file, "r+") as jsonFile:
        data = json.load(jsonFile)

        tmp = data["ip"]
        data["ip"] = host

        jsonFile.seek(0)  # rewind
        json.dump(data, jsonFile)
        jsonFile.truncate()

def fexists(sftp, path):
    """os.path.exists for paramiko's SCP object
    """
    try:
        sftp.stat(path)
    except IOError:
        return False
    else:
        return True

#
# Local Kernel Interaction
#

def kernel(f):
    cf = jupyter_client.find_connection_file(f)
    km = jupyter_client.BlockingKernelClient(connection_file=cf)

    # load connection info and init communication
    km.load_connection_file()
    km.start_channels()
    
    try:
        km.wait_for_ready(timeout=timeout)
    except:
        logging.error("he's dead, jim")
        sys.exit(1)
    
    return km

def execute(kernel, code):
     msg_id = kernel.execute(code)
     output = {"msg_id": msg_id, "output": None, "image": None, "error": None}
     while True:
            try:
                reply = kernel.get_iopub_msg(timeout=timeout)
            except Empty:
                continue

            if "execution_state" in reply["content"]:
                if reply["content"]["execution_state"] == "idle" and reply["parent_header"]["msg_id"] == msg_id:
                    if reply["parent_header"]["msg_type"] == "execute_request":
                        return output
            elif reply["header"]["msg_type"] == "execute_result":
                output["output"] = reply["content"]["data"].get("text/plain", "")
            elif reply["header"]["msg_type"] == "display_data":
                output["image"] = reply["content"]["data"].get("image/png", "")
            elif reply["header"]["msg_type"] == "stream":
                output["output"] = reply["content"].get("text", "")
            elif reply["header"]["msg_type"] == "error":
                output["error"] = "\n".join(reply["content"]["traceback"])

def display(stdout):
    if stdout.get('image') != None:
        data = base64.b64decode(stdout['image'])
        fd, path = tempfile.mkstemp(suffix='.png')
        with os.fdopen(fd, 'wb') as f:
            f.write(data)
        print("<image " + path + ">")

    if stdout.get('output') != None:
        print(stdout['output'])

    if stdout.get('error') != None:
        print(stdout['error'])

def attach_repl(args):
    if not os.path.isfile(args['file']):
        ssh, cfg = ssh_connect(args)
        fetch_conn_file(ssh, args['file'])
        local_conn_file(args['file'], cfg["hostname"])
        ssh.close()
        
def req_arg(args, arg):
    if args[arg] is None:
        logging.error("--" + arg + " is required for operation")
        sys.exit()

def main(args=None):
    """The main routine."""
    global verbose

    parser = argparse.ArgumentParser(description='inf-ipy')
    ssh_group = parser.add_argument_group("SSH")
    ssh_group.add_argument('--host', type=str, help='SSH Host')
    ssh_group.add_argument('--user', type=str, help='SSH User')
    ssh_group.add_argument('--pass', type=str, help='SSH Password')
    kernel_group = parser.add_argument_group("Kernel")    
    kernel_group.add_argument('--start', help='Start Kernel', action='store_true')
    kernel_group.add_argument('--stop', help='Stop Kernel', action='store_true')
    kernel_group.add_argument('--file', type=str, help='Connection File')
    kernel_group.add_argument('--kernel', type=str, help='Select Kernel')
    ssh_group.add_argument('--forward', help='Forward Kernel Ports', action='store_true')
    repl_group = parser.add_argument_group("REPL")
    repl_group.add_argument('--repl', help='CLI REPL', action='store_true')
    repl_group.add_argument('--comint', help='Emacs REPL', action='store_true')
    parser.add_argument('--config', type=str, help='Config File')
    parser.add_argument('--verbose', help='Verbose Logging', action='store_true')

    args = parser.parse_args()
    args = vars(args)

    if args['config']:
        config_file = args['config']
        abs_path = os.path.abspath(os.path.realpath(os.path.expanduser(config_file)))
        work_dir = os.path.dirname(abs_path)
        config_file = os.path.basename(abs_path)
        os.chdir(work_dir)
    else:
        config_file = "config.ini"

    config_ini = {}
    if os.path.exists(config_file):
        config = configparser.ConfigParser()
        config.read(config_file)
        config_ini = config._sections['SERVER']

    args.update(config_ini)

    if len(sys.argv) < 2:
        parser.print_usage()
        sys.exit(1)

    if args['verbose']:
        verbose = True

    if args['start']:
        req_arg(args, 'host')

        ssh, cfg = ssh_connect(args)
        conn_file = start_kernel(ssh, args)
        fetch_conn_file(ssh, conn_file)
        local_conn_file(conn_file, cfg["hostname"])
        ssh.close()

    if args['stop']:
        req_arg(args, 'file')
        km = kernel(args['file'])
        km.shutdown();

    if args['forward']:
        req_arg(args, 'host')
        req_arg(args, 'file')

        ports = []
        with open(args['file'], "r+") as jsonFile:
            data = json.load(jsonFile)
            ports = [(key, value) for key, value in data.items() if key.endswith("_port")]
            ports = list(map(lambda x: ("127.0.0.1", x[1]), ports))
            ports = ports

        cfg = ssh_read_config(args)
        logging.info("forwarding " + args['file'] + " on " + cfg["hostname"])
        tunnel = SSHTunnelForwarder(
            (args["host"]),
            ssh_username = cfg["username"],
            ssh_pkey = "~/.ssh/id_rsa",
            local_bind_addresses = ports,
            remote_bind_addresses = ports
        )

        tunnel.start()
        local_conn_file(args['file'], "127.0.0.1")
        input("Press Enter to continue...")
        tunnel.stop()

    if args['repl']:
        req_arg(args, 'host')
        req_arg(args, 'file')

        attach_repl(args)
        km = kernel(args['file'])
        print("Press [Meta+Enter] or [Esc] followed by [Enter] to accept input.")

        try:
            while True:
                stdin = prompt('λ ',
                               multiline=True,
                               history=FileHistory('.inf-ipy-repl-history'),
                               auto_suggest=AutoSuggestFromHistory())
                stdout = execute(km, stdin)
                display(stdout)

        except:
            pass

    if args['comint']:
        req_arg(args, 'host')
        req_arg(args, 'file')

        attach_repl(args)
        km = kernel(args['file'])

        try:
            while True:
                buffer = []
                line = input("> ")
                if line == "inf-ipy-eoe":
                    continue
                buffer.append(line)
                while True:
                    line = input()
                    if line == "inf-ipy-eoe":
                        break
                    buffer.append(line)
                stdin = "\n".join(buffer)
                stdout = execute(km, stdin)
                display(stdout)
        except:
            pass

if __name__ == "__main__":
    main()
