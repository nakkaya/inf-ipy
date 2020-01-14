import os
from invoke import task
import shutil

@task
def clean(c, docs=False, bytecode=False, extra=''):
    fs = ['build/', 'dist/', 'inf_ipy.egg-info/', '.eggs/', 'core/__pycache__/']
    for f in fs:
        if os.path.isdir(f):
            shutil.rmtree(f)

@task
def test(c, docs=False):
    c.run("python tests.py")

@task
def testDocker(c, docs=False):
    cmd = ("docker run -v " + os.getcwd() +
           ":/inf-ipy/ -w /inf-ipy/ " +
           "nakkaya/inf-ipy-build:latest " +
           "/bin/bash -c " +
           "'service ssh start && pip3 install . && python3 tests.py'")
    c.run(cmd)

@task
def build(c, docs=False):
    c.run("python setup.py sdist bdist_wheel")
    c.run("twine check dist/*")

@task
def deploy(c, docs=False):
    c.run("twine upload dist/*")
