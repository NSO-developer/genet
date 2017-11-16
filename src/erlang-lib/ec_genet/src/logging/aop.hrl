-record(pointcut, {
          module = []::string(),
          function = []::string(),
          arity = 0::integer(),
          type = public::local|public|global
         }).

-record(advice, {
          type::before|after_return|after_throw|after_final|around,
          module::atom(),
          function::atom(),
          arguments::list(atom()|integer())
         }).

-record(aspect, {
          advice::#advice{},
          pointcuts::list(#pointcut{})
         }).

