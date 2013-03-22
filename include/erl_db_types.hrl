-record('MODEL', {
          imports,
          version,
          name,
          backend,
          fields,
          functions
         }).

-record('VERSION', {
          value
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
