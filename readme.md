# inf-ipy

Remote Kernel Manager.

### Installation

Put ```inf-ipy``` somewhere on your path or clone this repository and
execute ```pip install .```

Remote machines that will run the kernels only requires SSH and
IPython to be present.

#### Quickstart

Make sure that you can login to the remote host that will run the
kernel without entering password.

    ssh remote_user@remote_host
    
To start a kernel on a remote machine.

    inf-ipy --host remote_host --user remote_user --start
    
This will launch the remote kernel and download the connection file to
the current directory. 

To interact with the remote kernel on the local machine.

    ipython qtconsole --existing conn_file

To stop a kernel on a remote machine.

    inf-ipy --host remote_host --user remote_user --stop --file conn_file

To select a different kernel other than Python ```--kernel``` option
can be used. To start a Matlab kernel use.

    inf-ipy --host remote_host --start --kernel matlab_kernel.kernel.MatlabKernel

Server configuration can be kept in a config file called ```config.ini```.

    [SERVER]
    host = 10.9.0.150
    file = server.json

### Third Party Kernels

#### Matlab

Install MATLAB Engine API for Python - https://www.mathworks.com/help/matlab/matlab_external/install-the-matlab-engine-for-python.html

### Resources

 - https://docs.microsoft.com/en-us/windows-server/administration/openssh/openssh_install_firstuse
 - https://sehyoun.com/blog/20180904_using-matlab-with-jupyter-notebook.html
