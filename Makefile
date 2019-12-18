pypi: clean
	cd src/python/ && python3 setup.py sdist bdist_wheel
	cd src/python/ && twine check dist/*
	cd src/python/ && twine upload --repository-url https://test.pypi.org/legacy/ dist/*

clean:
	rm -rf src/python/build/ src/python/dist/ src/python/inf_ipy.egg-info/
