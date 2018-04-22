from setuptools import setup

def readme():
    with open('README.md') as f:
        return f.read()

setup(name='fints2ledger',
      version='0.1',
      description='A tool for downloading transactions from FinTS banking APIs and sorting them into a ledger journal.',
      long_description=readme(),
      long_description_content_type='text/markdown',
      url='https://github.com/MoritzR/fints2ledger',
      author='Moritz Rumpf',
      license='MIT',

      entry_points={
          'console_scripts': ['fints2ledger=fints2ledger.main:main'],
      },

      install_requires=[
          'mt-940',
          'fints',
          'PyYAML'
      ],
      setup_requires=['green'],
      packages=['fints2ledger'],
      dependency_links=[
          'https://github.com/raphaelm/python-fints/tarball/master/#egg=fints-0.2.1'],
      zip_safe=False
      )
