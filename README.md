ParseText: Parse simple configuration files in Fortran
======================================================

ParseText is a Fortran module to parse simple configuration files.

Example configuration file:

    x = 1
    N= -5 2 10
    flag = T

The configuration file has to be loaded in a custom `PTo` variable (here, `config`):

    call PTparse(config,'example_file',5)

Then the values can be obtained via a set of typed commands:

    x = PTread_d(config, 'x')
    N = PTread_ivec(config, 'N', 3)
    flag = PTread_l(config, 'flag')

## Licensing, etc

ParseText is distributed under the BSD-3-clause license (see LICENSE) and
is written by [Pierre de Buyl](http://pdebuyl.be/).

Contributions have been made by the following persons:  
Peter Colberg
