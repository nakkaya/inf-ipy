import setuptools
import subprocess

version_minor = subprocess.run(['git', 'rev-list', 'HEAD', '--count'], stdout=subprocess.PIPE)

setuptools.setup(
    name='inf_ipy',
    version='0.1.' + version_minor.stdout.decode('utf-8'),
    author="Nurullah Akkaya",
    author_email="nurullah@nakkaya.com",
    description="A REPL interface to communicate with Jupyter kernels in Emacs or CLI.",
    url="https://github.com/nakkaya/inf-ipy/",
    
    packages=['core'],
    entry_points={
        'console_scripts': [
            'inf-ipy = core.__main__:main'
        ]
    },
    install_requires=[
        'paramiko',
        'jupyter_client',
        'IPython',
        'prompt_toolkit'
    ],
    
    classifiers=[
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: BSD License",
        "Operating System :: OS Independent",
    ],
)
