# GENET

GENET (Generic Erlang Transform) for NSO and ConfD is a framework for building
complex YANG-to-YANG model transformations.


## Building and running with NSO

If used with NSO, deploy this as a NSO package using the standard means - `make
all` to compile it and `packages reload` to let NSO notice the package
presence.


## Building and running with ConfD

To compile the package with ConfD, make sure that `$CONFD_DIR` points to the
confd installation directory (and also that `$NCS_DIR` does not exist); with
that, `make all` compiles all that is needed.

So as to run the transform code ConfD needs two paths to be added to its load
path, e.g. using `--addloadpath`:

 * path to the root of the package; ConfD will notice the `erlang-lib`
   subdirectory load the genet application from there;
 * path to `load-dir` to point ConfD to the genet configuration data model.


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
