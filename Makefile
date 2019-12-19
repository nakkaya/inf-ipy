pypi: clean
	cd src/python/ && python3 setup.py sdist bdist_wheel
	cd src/python/ && twine check dist/*
	cd src/python/ && twine upload dist/*

clean:
	rm -rf src/python/build/ src/python/dist/ src/python/inf_ipy.egg-info/ src/python/.eggs/
