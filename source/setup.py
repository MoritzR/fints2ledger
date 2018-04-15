from setuptools import setup

setup(name='fints2ledger',
      version='0.1',
      description='Pulling fromt fints apis and converting to ledger',
      url='https://github.com/MoritzR/fints2ledger',
      author='Moritz Rumpf',
      license='MIT',
      install_requires=[
          'mt-940',
          'fints'
      ],
      dependency_links=['https://github.com/raphaelm/python-fints/tarball/master/#egg=fints-0.2.1'],
      zip_safe=False)