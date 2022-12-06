erlbbs
=====

An OTP application

Build
-----

    $ rebar3 compile

Run local server
-----

    $ rebar3 shell
    $ # and open http://127.0.0.1:60000 in web browser

Configure DB connection
-----
create `config/secret.config` file. example:

```
[{
    db, [
        {address, "10.0.0.2"},
        {port, 8087}
    ]
}].
```
