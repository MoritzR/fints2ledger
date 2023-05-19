from setuptools import setup


def readme():
    with open('README.md') as f:
        return f.read()


setup(
    name='fints2ledger',
    version='0.8.2',
    description='A tool for downloading transactions from FinTS banking APIs and sorting them into a ledger journal.',
    long_description=readme(),
    long_description_content_type='text/markdown',
    url='https://github.com/MoritzR/fints2ledger',
    author='Moritz Rumpf',
    author_email='moritz.rumpf@gmail.com',
    license='MIT',
    python_requires='>=3.7.0',
    entry_points={
        'console_scripts': ['fints2ledger=fints2ledger.main:main'],
    },
    install_requires=['mt-940>=4.11,<5', 'fints>=4,<5', 'pyyaml>=6'],
    setup_requires=['green'],
    packages=['fints2ledger'],
    zip_safe=False,
    classifiers=[
        'Development Status :: 3 - Alpha',
        'Programming Language :: Python :: 3.7',
        'Programming Language :: Python :: 3.8',
        'Programming Language :: Python :: 3.9',
        'Programming Language :: Python :: 3.10',
        'Programming Language :: Python :: 3.11',
    ],
)
