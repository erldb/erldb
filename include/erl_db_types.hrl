-record('MODEL', {
          name,
          backend,
          fields,
          functions
         }).

-record('FIELD', {
          name,
          validator,
          line
         }).
