from setuptools import setup

extras = {
   'exp-client': ['exp-client']
}
setup(
   name='python-superposition-client',
   version='0.1',
   description='python client for cac and experimentation',
   author='superposition',
   author_email='superposition@juspay.in',
   extras_require=extras,
   packages=['cac-client'],
)