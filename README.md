# Erl_DB
ORM (Object-relational mapping) application implemented in Erlang.

# Model files
Check 'testmodel.model' in the examples/ directory

#Usage:

This is the example model that is in examples directory:

     import: users, tags

     name: blog_entry
     backend: mnesia

     /* This is a test comment */

     fields:
        id :: primary_key(auto_increment)
        title :: string(max_length = 255, index)
        author :: string(max_length = 100)
        created :: datetime()
        text :: string()


To get your workers they are stored as deps in rebar.config in our config we have a mnesia worker.

       {erl_db_mnesia, ",*", {git, "git@github.com:Taure/erl_db_mnesia.git", "master"}}

This is a worker that help us to do basic things with mnesia to get it use:

        rebar get-deps

To compile your code and to compile your models.

        rebar compile model

Then start a shell (I did it with erl -pa ebin -pa deps/*/ebin)

        Eshell V5.9.2  (abort with ^G)
        1> application:start(erl_db).
        21:41:44.883 [info] Application lager started on node nonode@nohost
        21:41:44.951 [info] Application mnesia started on node nonode@nohost
        21:41:44.951 [info] Application erl_db started on node nonode@nohost
        ok

Here you will see that mnesia is started. It is started in the worker so now we know that the worker is loaded. (Maybe we should say something like a log message)

        2> FirstEntry = blog_entry:new(id, "My title", "Mr Imsobad", now(), "This is my first blog entry. And it is stored in mnesia. Hope I don't forget that").
        {blog_entry,1,"My title","Mr Imsobad",
                          {1361,393556,541209},
                          "This is my first blog entry. And it is stored in mnesia. Hope I don't forget that"}

This is our first entry in our blog, now we will try to store it.

        3> erl_db:create_table(blog_entry).
        {atomic,ok}
        4> erl_db:save(FirstEntry).
        {atomic,ok}
        5>  erl_db:find(blog_entry, 1).
        {atomic,[{blog_entry,1,"My title","Mr Imsobad",
                     {1361,469740,519505},
                     "This is my first blog entry. And it is stored in mnesia. Hope I don't forget that"}]}