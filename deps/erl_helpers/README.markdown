## erl_helpers

It's a set of Erlang "helper" modules, nothing any more.

### Usage

Clone it inside your project (maybe in "deps" directory), and...

    $ cd erl_helpers
    $ make ungit
    $ mkdir ebin
    $ make

And finally, put it in the code path of your application.

### 3rd party modules

__mochijason2__ and __struct__ weren't built by me. However I added three new functions in __struct__: from_json/1, from_json/2, to_json/1.

### Licence

There's no formal LICENSE. Use as you want.

Copyright (c) 2011 Leandro Silva (CodeZone) <leandrodoze@gmail.com>.
