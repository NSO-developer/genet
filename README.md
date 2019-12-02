# GENET

GENET (Generic Erlang Transform) for NSO and ConfD is a framework for building
complex YANG-to-YANG model transformations.


## Building and running with NSO

If used with NSO, deploy this as a NSO package using the standard means - `make
all` to compile it and `packages reload` to let NSO notice the package
presence.


## Building and running with ConfD

(to be completed)


## Configuring

The only configurable part of genet is how and where it logs:

 * `/ec-genet/logging/level` - a value from the range `off`..`debug` to tell
   genet desired logging verbosity
 * `/ec-genet/logging/destination` - a choice with children `log-file-name`,
   `developer-log` to log to NSO/ConfD's developer, `stdout`.


## Documentation

To learn about genet API, run `make docs` and open
`erlang-lib/ec_genet/edoc/index.html` in your browser.


## Further reading

This package is accompanied by an [example
package](https://github.com/NSO-developer/ec_map_example) that shows basic
concepts of genet mappings.
