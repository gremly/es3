es3
=====

An OTP application

Build
-----

    $ rebar3 compile

Usage
-----

For testing purposes we can start various nodes to store
our file.

    $ rebar3 shell --sname node_a@localhost

Then we can create a file to be stored.

    (node_a@localhost)1> {ok, MyFile} = file:read_file('/path/to/my_file.ext').

Using the es3 API we can store and retrive our previous file.

    (node_a@localhost)2> es3:write("FileName.ext", MyFile).

    (node_a@localhost)3> es3:read("FileName.ext").
