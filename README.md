# erldb

ORM (Object-relational mapping) application implemented in Erlang.

# Rework

Right now we are aiming todo a 0.1 release. When it is done we will
write new readme on how it works.

# Model files
Check 'testmodel.model' in the examples/ directory

#Usage:

A model is a description of how your data looks like in the database and is stored within a text-file, one model per file.
Here's an example:

     name: blog_entry
     backend: production()

     fields:
        id :: primary_key(auto_increment)
        title :: string(max_length = 255, index)
        author :: one_to_one(user.id)
        tags :: one_to_many(tags.id)
        created :: datetime()
        text :: string()

First of all we define the name of the model, in this case it's 'blog_entry'. Then we say that this model uses the 'production'-backend (We'll talk more about this
under 'Configuration') with no additional arguments. Then we have the field-defintions. A field is composed of three different things;
name, type and arguments. We have looked a great deal on how [Django](https://www.djangoproject.com/) have done, and hopefully we'll be able to map their model types.

#Configuration:

If you are using rebar (And of course you are) you should put your settings within the env-row of your erl_db.app.src-file.
Here is an example of how this might look like:

       {application, erl_db,
           ...
           {env, [{db_pools, [
                              {production, %% The name we're using in our models
                               erl_db_ets, %% The backend module
                               [{size, 5}, {max_overflow, 10}], %% How many workers we start / can overflow with
                               []} %% Additional arguments to the backend module
                             ]}]}
        }

In our example we've defined a backend which we call 'production' and is using the 'erl_db_ets'-module as backend. We have also defined a size of 5 and an overflow
of 10. This means that when we start erl_db, 5 workers will be spawned, and if we can spawn an additional of 10 workers if the initial 5 gets to busy. The size and overflow
arguments are specific to [poolboy](https://github.com/devinus/poolboy). The fourth argument is parameters that will be sent to the backend-module on start.


#Compilation of modules

There is an experimental rebar plugin but it's still a bit unstable so for testing purpose you'll have to use the erlang console.

       $ erl -pa ebin deps/*/ebin
       Erlang R16B01 (erts-5.10.2) [source] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false]

       Eshell V5.10.2  (abort with ^G)
       1> erl_db_compiler:compile("examples/tags.model").
       ################## info ###################
       Saved field-declaration file in "include/tags.hrl"
       ################## info ###################
       Compiled model at: "ebin/tags.beam"

#Usage

Now you have a file "tags.hrl" in your include directory that's the record-definition of your model.

       1> application:start(erl_db).
       ...
       2> rr("include/tags.hrl").
       [tags]
       3> A = #tags{}.
       #tags{id = id,tag = "world"}
       4> A:save().
       {ok,#tags{id = 4,tag = "world"}}

If your configuration for the backend is correct the record is now saved to the database and can be searched for;

       5> erl_db:find(tags, [{tag, "world"}]).
       [#tags{id = 1,tag = "world"},
        #tags{id = 2,tag = "world"},
        #tags{id = 4,tag = "world"}]
