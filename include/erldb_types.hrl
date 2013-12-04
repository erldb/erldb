-record('ATTRIBUTE', {
          key,
          value,
          arguments
         }).


-record('FIELD', {
          name,
          type,
          arguments
         }).


-record('FIELD_REF', {
          model,
          field
         }).
