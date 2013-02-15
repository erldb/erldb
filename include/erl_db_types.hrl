-record('MODEL', {
          imports,
          name,
          backend,
          fields,
          functions
         }).

-record('FIELD', {
          name,
          type,
          arguments,
          line
         }).
