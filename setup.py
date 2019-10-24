import setuptools

with open("readme.md", "r") as fh:
    long_description = fh.read()
    setuptools.setup(
        name='inf_ipy',
        version='0.1',
        author="Nurullah Akkaya",
        author_email="nurullah@nakkaya.com",
        description="Manage Remote IPython Sessions",

        packages=['core'],
        entry_points={
            'console_scripts': [
                'inf-ipy = core.__main__:main'
            ]
        },
        install_requires=[
            'paramiko',
            'sshtunnel',
            'jupyter_client',
            'IPython',
        ],
        
        classifiers=[
            "Programming Language :: Python :: 3",
            "License :: BSD 2-Clause",
            "Operating System :: OS Independent",
        ],
    )
