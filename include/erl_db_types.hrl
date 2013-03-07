-record('MODEL', {
          imports,
          name,
          backend,
          fields,
          functions
         }).

-record('IMPORT', {
          model
         }).

-record('BACKEND', {
          name,
          arguments
         }).

-record('FIELD', {
          name,
          type,
          arguments
         }).
