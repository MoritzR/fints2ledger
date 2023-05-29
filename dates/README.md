dates README
============

[![Build Status](https://travis-ci.org/portnov/dates.svg?branch=master)](https://travis-ci.org/portnov/dates)

This package allows to parse many different formats of dates. Both absolute and relative dates are supported. Supported date formats are:

* `DD.MM.YYYY`
* `YYYY/MM/DD`
* `12 September 2012`
* `today`, `tomorrow`, `yesterday`
* `in 2 days`, `3 weeks ago`
* `last monday`, `next friday`
* `last month` (1th of this month), `next year` (1th of January of next year)

4-digits years may be abbreviated (such as 12 for 2012). Both 12-hour and 24-hour time formats are supported.

User-specified date formats are supported by Data.Dates.Formats module.

