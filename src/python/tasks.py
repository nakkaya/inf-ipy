import os
from invoke import task
import shutil

@task
def clean(c, docs=False, bytecode=False, extra=''):
    fs = ['build/', 'dist/', 'inf_ipy.egg-info/', '.eggs/']
    for f in fs:
        if os.path.isdir(f):
            shutil.rmtree(f)
@task
def build(c, docs=False):
    c.run("python setup.py sdist bdist_wheel")
    c.run("twine check dist/*")

@task
def deploy(c, docs=False):
    c.run("twine upload dist/*")
