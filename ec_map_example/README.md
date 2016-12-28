This is an example package that uses ec_genet.  How to use it:

   * create an initial ncs directory (e.g. `ncs-setup --dest example`)
   * copy the full project directory to `example/packages` like
     `cp -r . example/packages/ec_genent`
   * copy the `ec_map_example` directory to `packages`
   * compile the two packages `make -C example/packages/ec_genet` and
     `make -C example/packages/ec_map_example`
   * start ncs in the `example` directory

(Note that copying is necessary, symlinking may not work.)
